(ns sme-clj.core
  "Structure mapping engine core functionality.

  Reimplements SME (as described in [1]) for the most part, in Clojure instead
  of (rather ancient and difficult to read) Common Lisp. There are some
  differences in certain areas, such as GMap merging and scoring. It is also
  likely to be slower, as it has not been profiled or optimised at all.

  The main differences:
    - GMap merging differs in two of the steps, but results should be more
      complete than the original algorithm, if they differ at all.

    - Inference generation has some limitations, and has not received as much
      development and testing as other areas (as it was not used in the research
      for which this SME implementation was developed).

    - Scoring of GMaps is much simpler. It does not use complex evidence rules
      in a belief maintenance system as in [1]. Instead, GMaps are scored on
      structure using a trickle-down algorithm that rewards large structures.

  The basic usage is as follows:
     1. Define predicates, entities, and concept graphs using 'sme-clj.typedef.
     2. Run 'sme-clj.core/match with the base and target graphs as arguments.
     3. The 'match function returns the resulting GMaps.

  See the 'match docstring for more.
  Also look at the example in the sme-clj.example namespace.

  If you are not yet familiar with the general structure and terminology of the
  original SME algorithm, it will be hard to understand this stuff, so I would
  advise you read [1] first.

    [1] Falkenhainer, B., Forbus, K. & Gentner, D. (1989). The structure-mapping
          engine: algorithm and examples. Artificial Intelligence, 41, 1-62."
  (:require [clojure.math.combinatorics :as comb]
            [clojure.set :as set]
            [mops.core :as mops]
            #?(:clj [mops.records]
               :cljs [mops.records :refer [MopMap]])
            [sme-clj.ruledef :as rules]
            [sme-clj.typedef :as types]
            [taoensso.timbre :as log])
  (:import #?(:clj mops.records.MopMap)))

;; Note: "mh" = match hypothesis

(defn trace-quantity
  [starting ending]
  (log/trace "Started with:" (count starting) "Ended with:" (count ending))
  ending)

(defn apply-filter-rules
  "Apply :filter rules from a ruleset to the base and target expressions. Return distinct match hypotheses."
  [kg rules mhs]
  (log/info "Applying filter rules")
  (trace-quantity
   mhs
   (->> rules
        vals
        (filter (comp (partial = :filter) :type))
        (mapcat (fn [rule]
                  ;; map rule over every possible expression
                  ;; pairing
                  (mapcat (partial rules/apply-rule kg rule) mhs)))
        (remove nil?)
        distinct)))

(defn apply-intern-rules
  "Apply :intern rules to a set of match hypotheses generated by :filter rules,
  and see if new MHs are created. For every new MH, also apply the :intern rules
  to it. Return the resulting set of MHs."
  [kg rules mhs]
  (log/info "Applying intern rules")
  (let [rules (->> rules
                   vals
                   (filter (comp (partial = :intern) :type)))]
    (loop [mhs    mhs
           result (hash-set)]
      (trace-quantity mhs result)
      (let [[mh & rest-mhs] mhs]
        (if mh
          (let [result  (conj result mh)
                new-mhs (->> rules
                             (mapcat #(rules/apply-rule kg % mh))
                             (remove nil?)
                             set
                             (#(set/difference % result)))]
            (recur (lazy-cat rest-mhs new-mhs) result))
          ;; set of all MHs tested on intern rules
          result)))))

(defn create-match-hypotheses
  "Apply rules from a ruleset to base and target to generate match hypotheses
  for the graphs."
  [kg base target rules]
  (log/info "Creating match hypotheses")
  (->>
   (for [b (types/expressions kg base) t (types/expressions kg target)] [b t])
   (apply-filter-rules kg rules)
   (apply-intern-rules kg rules)))

(defn xor [a b] (and (not (and a b)) (or a b)))

(defn find-no-good
  "nogood is every mh mapping same target or base"
  [mhs root]
  (filter (fn [mh] (apply xor (map = root mh))) mhs))

(defn all-emaps [kg mhs] (remove (partial types/expression? kg) mhs))

;; The below proves useful when checking consistency and such.

(defn consistent?
  "True if an MH is consistent, meaning none of its emaps are in its nogoods."
  ([kg mhs mh]
   (let [emaps   (set (all-emaps kg mhs))
         no-good (set (find-no-good mhs mh))]
     #_(log/debug "E-maps:" emaps "No-good:" no-good)
     (empty? (set/intersection emaps no-good)))))

(defn direct-children
  [kg mh]
  (->> mh
       (map #(->> %
                  (types/expression-args kg)
                  (map second)))
       (apply map vector)
       set))

(defn find-roots
  "Returns only the root hypotheses, ie. those that are not children of any
  other hypothesis."
  [kg mhs]
  (remove (->> mhs
               (mapcat (partial direct-children kg))
               set)
          mhs))

(defn emap? [kg mh] (every? (partial types/entity? kg) mh))

(defn dfs
  [G v neighbor-fn]
  (loop [stack      (list v)
         discovered []]
    (if stack
      (let [[v & S] stack
            found?  ((set discovered) v)]
        (recur (cond-> S (not found?) (into (neighbor-fn G v)))
               (cond-> discovered (not found?) (conj v))))
      discovered)))

(comment (dfs {:A [:B :C :E]
               :B [:D :F :A]
               :C [:A :G]
               :D [:B]
               :E [:A :F]
               :F [:E :B]
               :G [:C]}
              :A
              (fn [G v] (get G v))))
(defn all-children
  "Returns a set of all descendants of a root."
  [kg root]
  (set (dfs kg root direct-children)))

(defn split-into-mhs-sets
  "Given match hypothesis information, builds a set of initial gmaps. Returns a
  map with the and the :gmaps set."
  [kg all-mhs]
  (log/info "Splitting into mhs sets")
  (let [mhs-sets (->> all-mhs
                      (find-roots kg)
                      (reduce (fn form-mhs-set [mhs-sets root]
                                (let [children (all-children kg root)]
                                  (log/trace "Children:" root children)
                                  (if (consistent? kg children root)
                                    (conj mhs-sets children)
                                    (->> root
                                         (direct-children kg)
                                         (mapcat #(form-mhs-set #{} %))
                                         set
                                         (set/union mhs-sets)))))
                              #{})
                      vec)]
    (log/trace "MHS sets:" (count mhs-sets) (frequencies (map count mhs-sets)))
    mhs-sets))

(defn all-no-good [all-mhs mhs] (mapcat (partial find-no-good all-mhs) mhs))

(defn mhs-sets-consistent?
  "Two gmaps are consistent if none of their elements are in the NoGood set of
  the other."
  [all-mhs mhs-a mhs-b]
  (every? (fn [[x y]]
            (->> y
                 (all-no-good all-mhs)
                 set
                 (set/intersection x)
                 empty?))
          [[mhs-a mhs-b] [mhs-b mhs-a]]))

(defn mhs-sets-internally-consistent?
  "True if the given set of gmaps is internally consistent."
  [all-mhs mhs-set]
  (every? (fn [mhs-a] (every? #(mhs-sets-consistent? all-mhs mhs-a %) mhs-set))
          mhs-set))

(defn strict-subset? [set1 set2] (and (not= set1 set2) (set/subset? set1 set2)))

;; NOTE: SME's second merge step seems rather complex compared to its benefits.
;; Its results will already be generated in a third step we will be performing
;; that is more exhaustive than SME performs at that point. Therefore step 2 is
;; unnecessary here and we skip it.

;; The below is a very naive implementation, performance-wise.

(defn maximal-valid-subsets
  [valid? s]
  (let [checked (atom #{})]
    (log/debug "Start" s)
    (letfn [(f [s]
               (loop [t (count s)]
                 (cond (zero? t) nil
                       (= 1 t)   (map hash-set s)
                       :else     (let [combs-1 (->> (comb/combinations s t)
                                                    (map set)
                                                    (remove @checked))
                                       combs   (group-by valid? combs-1)]
                                   (log/debug "Here" combs-1 @checked)
                                   (swap! checked into combs-1)
                                   (if-let [valid-combs (seq (get combs true))]
                                     (->> (get combs false)
                                          (remove (set/difference @checked
                                                                  combs-1))
                                          (mapcat f)
                                          (concat valid-combs))
                                     (recur (dec t)))))))]
      (f s))))

(maximal-valid-subsets
 (fn [s]
   (and (contains? s :a)
        (or (not (contains? s :b))
            (and (not (contains? s :c)) (not (contains? s :d))))))
 #{:a :b :c :d :e})
[#{:a :c :d :e} #{:a :b :e}]
(comb/combinations #{1 2 3 4} 3)

(defn consistent-combs-of-mhs-sets
  "Combine all gmaps in all maximal, consistent ways."
  [all-mhs mhs]
  (log/info "Finding consistent combinations of MHS sets")
  (let [consistent-sets (->> mhs
                             vec
                             (maximal-valid-subsets
                              (partial mhs-sets-internally-consistent? all-mhs))
                             #_#_#_comb/subsets (remove empty?)
                               (filter (partial mhs-sets-internally-consistent?
                                                all-mhs))
                             (map set))
        combs-of-sets   (->> consistent-sets
                             (remove (fn [mhs-a]
                                       (some (partial strict-subset? mhs-a)
                                             consistent-sets)))
                             (map vec))]
    (log/trace "Combs of MHS sets:" (count combs-of-sets))
    combs-of-sets))

(defn merge-mhs-sets
  "Given a collection of sets of gmaps, merges the gmaps in each set into a
  single gmap."
  [gmap-sets]
  (log/info "Merging MHS sets")
  (letfn [(reduce-to-gm [gm-set] (let [args (set (apply concat gm-set))] args))]
    (let [merged-sets (map reduce-to-gm gmap-sets)]
      (log/trace "Merged sets:" (count merged-sets))
      merged-sets)))

(defn emaps-equal?
  "Special equals function for entities that rounds floating point numbers to
   two decimals before comparing them, to avoid rounding errors affecting
   equality."
  [a b]
  (letfn [(round [n]
                 #?(:clj (.setScale (bigdec n) 2 BigDecimal/ROUND_HALF_UP)
                    :cljs (.toFixed n 2)))]
    (and (= (keys a) (keys b))
         (keep
          (fn [x y]
            (if (and (number? x) (number? y)) (== (round x) (round y)) (= x y)))
          (vals a)
          (vals b)))))


(defn matching-emaps
  "Returns seq of MHs that are emaps of which the entities are equal."
  [kg {:keys [mhs]} & {:keys [unmatched-keys]}]
  (filter (fn [[base target :as mh]]
            (and (emap? kg mh)
                 ;; Entities may have keys that are implementation details.
                 ;; Unmatched keys is a
                 ;; seq of those keys to ignore them in emap matching.
                 (emaps-equal? (apply dissoc (get kg base) unmatched-keys)
                               (apply dissoc (get kg target) unmatched-keys))))
          mhs))

(defn score-gmap
  "Computes SES and emap scores for a gmap. The emap score is not in the
  original SME. It simply counts how many entities match in their content."
  [kg mhs gm]
  (letfn [(score-mh [mh depth]
                    ;; simplified trickle-down SES
                    (if-let [kids (seq (direct-children kg mh))]
                      (reduce + depth (map #(score-mh % (inc depth)) kids))
                      depth))]
    {:mhs          gm
     :score        (->> gm
                        (find-roots kg)
                        (map #(score-mh % 0))
                        (reduce + (count gm)))
     :emap-matches (count (matching-emaps kg gm))}))

;;; Inference generation below. Not used in TCG model.

(defn gmap-inferences
  "Generates maximal inferences for a given gmap, based on SME algorithm."
  [kg base-name gmap]
  (let [mh-bases  (set (map first gmap))
        unmatched (set/difference (set (types/expressions kg base-name))
                                  mh-bases)
        ancestors (set/select #(types/ancestor? kg mh-bases %) unmatched)]
    (set/difference (set (mapcat (partial types/get-descendants kg) ancestors))
                    mh-bases)))

(defn generate-inferences
  "Appends :inferences to gmaps in given data, for a given base graph."
  [kg gmaps]
  (->> gmaps
       (map
        #(assoc %
                :inferences
                (gmap-inferences kg (get-in % [:mapping :base]) (:mhs %))))))

;; On inference integration, recursive method:
;;
;; Starting at inference root:
;; 1.  If expr/pred exists as :base of an MH
;; 1.a then return :target of the MH
;; 1.b else return (cons pred (map f args))
;;
;; This will return a relation expression for in the target gmap. All new parts
;; of the inference expression (ie. the inferences in Falkenhainer's terms)
;; will
;; be included in the new relation, with all base predicates that were mapped
;; replaced with their targets. That way, the inferences are linked up in the
;; target graph.


;; Note that the behaviour of the original SME that concerns the creation of
;; "skolem" entities when transfering inferences is not implemented.
;;
;; Inference generation in general was not as relevant in the original purpose
;; of this implementation, hence it is less tested etc.

(defn transfer-gmap-inferences
  "Attempt to transfer inferences to the target of the gmap."
  [kg
   {:as   gmap
    :keys [inferences mhs]}]
  ;; This exception setup is ugly, but is a simple and efficient way of aborting
  ;; the whole inference transferring process for this gmap. Monads would
  ;; perhaps work as well (or CPS).
  (try (let [pairs    (into {} mhs)
             transfer (fn transfer [expr]
                        (or (get pairs expr)
                            (if (types/entity? kg expr)
                              (throw (ex-info {} "cannot infer entities"))
                              (->> expr
                                   (types/expression-functor kg)
                                   (conj (->> expr
                                              (types/expression-args kg)
                                              (map second)
                                              (map transfer)))))))]
         (assoc gmap :transferred (set (map transfer inferences))))
       (catch #?(:cljs :default
                 :clj Exception)
         _
         gmap)))

(defn transfer-inferences
  [kg gmaps]
  (map (partial transfer-gmap-inferences kg) gmaps))

(defn finalize-gmaps
  "Computes additional information about the gmaps we have found and stores it
  in the gmaps."
  [kg base-name target-name mhs gmaps]
  (log/info "Finalizing gmaps")
  (->> gmaps
       (map #(score-gmap kg mhs %)) ; scores
       (map #(assoc %
                    :mapping
                    {:base   base-name
                     :target target-name}))))

(defn perform-inference
  "
    :inferences   Maximal set of potential inferences.

    :transferred  Inferences transferred to target, ready to be added into the
                  target's concept graph.
  "
  [kg gmaps]
  (->> gmaps
       (generate-inferences kg)
       (transfer-inferences kg)))

(defn match
  "Attempts to find a structure mapping between base and target using an
  implementation of the SME algorithm. Returns a map with the following:

    Collection of GMaps, which represent analogical mappings.

  Keys available in the returned GMaps:
    :mhs          Collection of match hypotheses that form the GMap.


    :score        Structural evaluation score (SES), simple implementation.


  For example: (map :score (match b t)) -> seq of gmap scores."
  ([kg base target rules]
   (let [all-mhs (create-match-hypotheses kg base target rules)]
     (->> all-mhs
          (split-into-mhs-sets kg)
          (consistent-combs-of-mhs-sets all-mhs)
          merge-mhs-sets
          (finalize-gmaps kg base target all-mhs))))
  ([kg base target]
   (match kg
     base target
     rules/literal-similarity)))
