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
            [mops :as mops]
            [sme-clj.ruledef :as rules]
            [sme-clj.typedef :as types])
  (:import mop_records.MopMap))

;;;;
;;;; GENERATING MATCH HYPOTHESES
;;;;

;; Note: "mh" = match hypothesis

(defn apply-filter-rules
  "Apply :filter rules from a ruleset to the base and target expressions. Return
  distinct match hypotheses."
  [kg rules mhs]
  (println "Filter rules")
  (->> rules
    vals
    (filter (comp (partial = :filter) :type))
    (mapcat (fn [rule]
              ;; map rule over every possible expression pairing
              (mapcat (partial rules/apply-rule kg rule) mhs)))
    (remove nil?)
    distinct))

(defn apply-intern-rules
  "Apply :intern rules to a set of match hypotheses generated by :filter rules,
  and see if new MHs are created. For every new MH, also apply the :intern rules
  to it. Return the resulting set of MHs."
  [kg rules mhs]
  (println "Intern rules")
  (let [rules (->> rules vals (filter (comp (partial = :intern) :type)))]
    (loop [mhs    mhs
           result (hash-set)]
      (let [[mh & rest-mhs] mhs]
        (if mh
          ;; test this MH on all intern rules, adding new matches to todo list
          (let [result  (conj result mh)
                new-mhs (->> rules
                          (mapcat #(rules/apply-rule kg % mh))
                          (remove nil?)
                          set
                          (#(set/difference % result)))]
            (recur
              (lazy-cat rest-mhs new-mhs)
              result))
          ;; set of all MHs tested on intern rules
          result)))))

(defmulti get-concept-graph-expressions
  (fn [kg _ ] (type kg)))

(defmethod get-concept-graph-expressions MopMap
  [kg concept-graph-name]
  (-> kg
    :mops
    (map  (mops/mop-ids kg))
    (->> (filter #(= #{concept-graph-name} (mops/filler % :concept-graph))))
    (->> (map :id))))

(defmethod get-concept-graph-expressions :default
  [kg concept-graph-name]
  (->> kg
    vals
    (filter #(= concept-graph-name (:concept-graph %)))
    (map :name)))

(defn create-match-hypotheses
  "Apply rules from a ruleset to base and target to generate match hypotheses
  for the graphs."
  [kg base target rules]
  (->> (for [b (get-concept-graph-expressions kg base)
             t (get-concept-graph-expressions kg target)]
         [b t])
    (map (partial apply types/make-match-hypothesis))
    (apply-filter-rules kg rules)
    (apply-intern-rules kg rules)))

;;;;
;;;; FORMING GMAPS
;;;;

(defn find-children [kg mhs [base target]]
  (let [[bmap tmap] (map #(group-by % mhs) [first second])]
    (->> [base target]
      (map #(types/lookup kg % :args))
      (apply mapcat (fn [b t]
                      (set/intersection
                        (set (get bmap b))
                        (set (get tmap t)))))
      set)))

(defn find-nogood
  "nogood is every mh mapping same target or base"
  [mhs [base target :as mh]]
  (let [[bmap tmap] (map #(group-by % mhs) [first second])]
    (disj (set/union
            (set (get bmap base))
            (set (get tmap target)))
      mh)))

(defn find-emaps
  [kg [base :as mh]]
  (when-not (= :expression (types/lookup kg base :type))
    #{mh}))

(defn all-emaps
  [kg mhs]
  (->> mhs
    (map (partial find-emaps kg))
    (reduce set/union)))

;; The below proves useful when checking consistency and such.

(defn consistent?
  "True if an MH is consistent, meaning none of its emaps are in its nogoods."
  ([kg mhs mh]
   (empty? (set/intersection
             (find-emaps kg mh)
             (find-nogood mhs mh)))))

(defn find-roots
  "Returns only the root hypotheses, ie. those that are not children of any
  other hypothesis."
  [kg mhs]
  (let [all-children (reduce #(set/union %1
                                (find-children kg mhs %2))
                       #{} mhs)]
    (filter #(not (contains? all-children %)) mhs)))

(defn is-emap? [kg {:keys [base target]}]
  (and
    (= :entity (types/lookup kg base :type))
    (= :entity (types/lookup kg target :type))))

(defn collect-children
  "Returns a set of all descendants of a root."
  [kg root mhs]
  (letfn [(collect [mh]
            (if (is-emap? kg mh)
              [mh]
              (cons mh (mapcat collect
                         (find-children kg mhs mh)))))]
    (set (collect root))))

(defn make-mhs-set
  "Returns a gmap with the root and all of its descendants."
  [kg root all-mhs]
  (collect-children kg root all-mhs))

(defn split-into-mhs-sets
  "Given match hypothesis information, builds a set of initial gmaps. Returns a
  map with the and the :gmaps set."
  [kg all-mhs]
  (->> all-mhs
    (find-roots kg)
    (reduce (fn form-mhs-set [mhs-sets root]
              (if (consistent? kg all-mhs root)
                (->> all-mhs
                  (make-mhs-set kg root)
                  (conj mhs-sets))
                (if-let [kids (seq (find-children kg all-mhs root))]
                  (->> kids
                    (mapcat #(form-mhs-set #{} %))
                    set
                    (set/union mhs-sets))
                  mhs-sets)))
      #{})
    vec))

(defn all-nogood
  [mhs1 mhs]
  (->> mhs
    (map (partial find-nogood mhs1))
    (reduce set/union)))

(defn mhs-sets-consistent?
  "Two gmaps are consistent if none of their elements are in the NoGood set of
  the other."
  [all-mhs mhs-a mhs-b]
  (and
    (empty? (set/intersection mhs-a (all-nogood all-mhs mhs-b)))
    (empty? (set/intersection mhs-b (all-nogood all-mhs mhs-a)))))

(defn mhs-sets-internally-consistent?
  "True if the given set of gmaps is internally consistent."
  [all-mhs mhs-set]
  (every? (fn [mhs-a]
            (every? #(mhs-sets-consistent? all-mhs mhs-a %) mhs-set))
    mhs-set))

(defn strict-subset? [set1 set2]
  (and (not= set1 set2)
    (set/subset? set1 set2)))

;; NOTE: SME's second merge step seems rather complex compared to its benefits.
;; Its results will already be generated in a third step we will be performing
;; that is more exhaustive than SME performs at that point. Therefore step 2 is
;; unnecessary here and we skip it.


;; The below is a very naive implementation, performance-wise.

(defn consistent-combs-of-mhs-sets
  "Combine all gmaps in all maximal, consistent ways."
  [all-mhs mhs]
  (let [consistent-sets (->> mhs
                          vec
                          comb/subsets
                          (remove empty?)
                          (filter (partial mhs-sets-internally-consistent? all-mhs))
                          (map set))]
    (->> consistent-sets
      (remove (fn [mhs-a]
                (some (partial strict-subset? mhs-a)
                  consistent-sets)))
      (map vec))))

(defn merge-mhs-sets
  "Given a collection of sets of gmaps, merges the gmaps in each set into a
  single gmap."
  [gmap-sets]
  (letfn [(gather-gm [mhs gm]
            (set/union mhs gm))

          (reduce-to-gm [gm-set]
            (let [args (reduce gather-gm #{} gm-set)]
              args))]
    (map reduce-to-gm gmap-sets)))

(letfn [(round [n]
          (.setScale (bigdec n) 2 BigDecimal/ROUND_HALF_UP))]
  (defn emaps-equal?
    "Special equals function for entities that rounds floating point numbers to
   two decimals before comparing them, to avoid rounding errors affecting
   equality."
    [a b]
    (and (= (keys a) (keys b))
      (every? true?
        (map (fn [x y]
               (if (and (number? x) (number? y))
                 (== (round x) (round y))
                 (= x y)))
          (vals a)
          (vals b))))))

;; Entities may have keys that are implementation details. Bind this var to a
;; seq of those keys to ignore them in emap matching.
(def unmatched-keys nil)

(defn matching-emaps
  "Returns seq of MHs that are emaps of which the entities are equal."
  [kg {:keys [mhs]}]
  (filter (fn [[base target :as mh]]
            (and (is-emap? kg mh)
              (emaps-equal?
                (apply dissoc (get kg base) unmatched-keys)
                (apply dissoc (get kg target) unmatched-keys))))
    mhs))

(defn score-gmap
  "Computes SES and emap scores for a gmap. The emap score is not in the
  original SME. It simply counts how many entities match in their content."
  [kg mhs gm]
  (letfn [(score-mh [mh depth]
            ;; simplified trickle-down SES
            (if-let [kids (seq (find-children kg mhs mh))]
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
        unmatched (set/difference (set (get-concept-graph-expressions kg base-name)) mh-bases)
        ancestors (set/select #(types/ancestor? kg mh-bases %) unmatched)]
    (set/difference (set (mapcat (partial types/get-descendants kg) ancestors))
      mh-bases)))

(defn generate-inferences
  "Appends :inferences to gmaps in given data, for a given base graph."
  [kg base gmaps]
  (->> gmaps
    (map #(assoc % :inferences (gmap-inferences kg base (:mhs %))))))

;; On inference integration, recursive method:
;;
;; Starting at inference root:
;; 1.  If expr/pred exists as :base of an MH
;; 1.a then return :target of the MH
;; 1.b else return (cons pred (map f args))
;;
;; This will return a relation expression for in the target gmap. All new parts
;; of the inference expression (ie. the inferences in Falkenhainer's terms) will
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
  [kg {:as   gmap,
       :keys [inferences mhs]}]
  ;; This exception setup is ugly, but is a simple and efficient way of aborting
  ;; the whole inference transferring process for this gmap. Monads would
  ;; perhaps work as well (or CPS).
  (try
    (let [pairs    (zipmap (map first mhs) (map second mhs))
          transfer (fn transfer [expr]
                     (if-let [t (get pairs expr)]
                       t
                       (if (= :entity (types/lookup kg expr :type))
                         (throw (RuntimeException.
                                  "cannot infer entities"))
                         (cons (types/lookup kg expr :functor)
                           (doall (map transfer (types/lookup kg expr :args)))))))]
      (assoc gmap
        :transferred (set (doall (map transfer inferences)))))
    (catch RuntimeException _
      gmap)))

(defn transfer-inferences
  [kg gmaps]
  (map (partial transfer-gmap-inferences kg) gmaps))

(defn finalize-gmaps
  "Computes additional information about the gmaps we have found and stores it
  in the gmaps."
  [kg base-name target-name mhs gmaps]
  (->> gmaps
    (map #(score-gmap kg mhs %)) ; scores
    (map #(assoc % :mapping {:base base-name :target target-name}))))

(defn perform-inference
  "
    :inferences   Maximal set of potential inferences.

    :transferred  Inferences transferred to target, ready to be added into the
                  target's concept graph.
  "
  [kg base gmaps]
  (->> gmaps
    (generate-inferences kg base)
    (transfer-inferences kg)))

(defn match
  "Attempts to find a structure mapping between base and target using an
  implementation of the SME algorithm. Returns a map with the following:

    Collection of GMaps, which represent analogical mappings.

  Keys available in the returned GMaps:
    :mhs          Collection of match hypotheses that form the GMap.


    :score        Structural evaluation score (SES), simple implementation.


  For example: (map :score (match b t)) -> seq of gmap scores."
  ([kg rules base target]
   (let [[base target] (->> [base target]
                         (map :graph)
                         (map keys))
         all-mhs       (create-match-hypotheses kg base target rules)]
     (->> all-mhs
       (split-into-mhs-sets kg)
       (consistent-combs-of-mhs-sets all-mhs)
       merge-mhs-sets
       (finalize-gmaps kg base target all-mhs))))
  ([kg base target]
   (match kg rules/literal-similarity base target)))
