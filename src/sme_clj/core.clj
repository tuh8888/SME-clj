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
            [sme-clj.ruledef :as rules]
            [sme-clj.typedef :as types]))

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

(defn create-match-hypotheses
  "Apply rules from a ruleset to base and target to generate match hypotheses
  for the graphs."
  [kg base target rules]
  (let [[base target] (->> [base target]
                        (map :graph)
                        (map keys))]
    (->> (for [b base
               t target]
           [b t])
      (map (partial apply types/make-match-hypothesis))
      (apply-filter-rules kg rules)
      (apply-intern-rules kg rules))))

;;;;
;;;; FORMING GMAPS
;;;;

;; The below proves useful when checking consistency and such.
(defn build-hypothesis-structure
  "Creates a map from MHs to their structural information. So retrieving the
  match hypotheses is done with 'keys, while the structural info can be accessed
  with 'vals or 'get."
  [kg mhs]
  (let [add-as                     (fn [m k mh] (update-in m [k (k mh)] set/union #{mh}))
        ;; cache of base/target expressions mapped to their mh
        {bmap :base, tmap :target} (reduce (fn [s mh] (-> s
                                                       (add-as :base mh)
                                                       (add-as :target mh)))
                                     {:base {}, :target {}}
                                     mhs)]
    (reduce (fn build-mh-structure [structure mh]
              (assoc structure
                mh

                ;; initial emaps is just ourselves if we are one, for the rest
                ;; this will be filled later
                {:emaps (if (= :expression (types/lookup kg (:base mh) :type)) #{} #{mh})

                 ;; nogood is every mh mapping same target or base
                 :nogood (-> (set/union
                               (get bmap (:base mh) #{})
                               (get tmap (:target mh)) #{})
                           ;; not nogood of ourselves
                           (disj mh))

                 ;; our children are mhs that map our arguments (so children
                 ;; does not equal our entire set of descendants)
                 :children (if (= :expression (types/lookup kg (:base mh) :type))
                             (->> (types/lookup kg (:target mh) :args)
                               (mapcat (fn [b t]
                                         (set/intersection (get bmap b #{})
                                           (get tmap t #{})))
                                 (types/lookup kg (:base mh) :args))
                               set)
                             #{})}))
      {}
      mhs)))

;; For each expression without emaps, recursively add the union of its
;; children's emaps and nogoods.
(defn propagate-from-emaps
  "Extends structural MH information of each expression without emaps by
  recursively adding the union of its children's emaps and nogoods to
  it. Essentially flows up the structural information."
  [mh-structure]
  (letfn [(propagate [mstr mh]
            (if (seq (:emaps (get mstr mh)))
              mstr
              (let [kids (:children (get mstr mh))
                    mstr-kids (reduce propagate mstr kids)
                    kids-struct (vals (select-keys mstr-kids kids))]
                (update-in mstr-kids
                           [mh]
                           #(merge-with set/union %1 %2)
                           {:emaps
                            (reduce set/union (map :emaps kids-struct))

                            :nogood
                            (reduce set/union (map :nogood kids-struct))}))))]
    (reduce propagate
            mh-structure
            (keys mh-structure))))

(defn consistent?
  "True if an MH is consistent, meaning none of its emaps are in its nogoods."
  ([{:keys [emaps nogood] :as mstr-entry}]
     (empty? (set/intersection emaps nogood)))
  ([mh mstr]
     (consistent? (get mstr mh))))

(defn find-roots
  "Returns only the root hypotheses, ie. those that are not children of any
  other hypothesis."
  [mh-structure]
  (let [all-children (reduce #(set/union %1 (:children %2))
                       #{}
                       (vals mh-structure))]
    (filter #(not (contains? all-children %)) (keys mh-structure))))

(defn is-emap? [kg {:keys [base target]}]
  (and
    (= :entity (types/lookup kg base :type))
    (= :entity (types/lookup kg target :type))))

(defn collect-children
  "Returns a set of all descendants of a root."
  [kg root mh-structure]
  (letfn [(collect [mh]
            (if (is-emap? kg mh)
              [mh]
              (cons mh (mapcat collect
                         (:children (get mh-structure mh))))))]
    (set (collect root))))

(defn make-gmap
  "Returns a gmap with the root and all of its descendants."
  [kg root mh-structure]
  {:mhs       (collect-children kg root mh-structure)
   :structure (merge {:roots #{root}}
                 ;; gmap's nogoods/emaps are those of its root(s)
                 (select-keys (get mh-structure root) [:nogood :emaps]))})

(defn compute-initial-gmaps
  "Given match hypothesis information, builds a set of initial gmaps. Returns a
  map with the :mh-structure and the :gmaps set."
  [kg mh-structure]
  {:mh-structure mh-structure
   :gmaps        (->>
                   (find-roots mh-structure)
                   (reduce (fn form-gmap [gmaps root]
                             (if (consistent? (get mh-structure root))
                               (->> mh-structure
                                 (make-gmap kg root)
                                 (conj gmaps))
                               (if-let [kids (seq (:children (get mh-structure root)))]
                                 (->> kids
                                   (mapcat #(form-gmap #{} %))
                                   set
                                   (set/union gmaps))
                                 gmaps)))
                     #{})
                   vec)})

(defn gmaps-consistent?
  "Two gmaps are consistent if none of their elements are in the NoGood set of
  the other."
  [gm-a gm-b]
  (and
    (empty? (set/intersection (:mhs gm-a) (get-in gm-b [:structure :nogood])))
    (empty? (set/intersection (:mhs gm-b) (get-in gm-a [:structure :nogood])))))

(defn gmap-set-internally-consistent?
  "True if the given set of gmaps is internally consistent."
  [gmap-set]
  (every? (fn [gm-b]
            (every? #(gmaps-consistent? gm-b %) gmap-set))
    gmap-set))

(defn strict-subset? [set1 set2]
  (and (not= set1 set2)
    (set/subset? set1 set2)))

;; NOTE: SME's second merge step seems rather complex compared to its benefits.
;; Its results will already be generated in a third step we will be performing
;; that is more exhaustive than SME performs at that point. Therefore step 2 is
;; unnecessary here and we skip it.


;; The below is a very naive implementation, performance-wise.

(defn combine-gmaps
  "Combine all gmaps in all maximal, consistent ways."
  [data]
  (update data :gmaps (fn [gmaps]
                        (let [consistent-sets (->> gmaps
                                                vec
                                                comb/subsets
                                                (remove empty?)
                                                (filter gmap-set-internally-consistent?)
                                                (map set))]
                          (->> consistent-sets
                            (remove (fn [gms-a]
                                      (some (partial strict-subset? gms-a)
                                        consistent-sets)))
                            (map vec))))))

(defn merge-gmaps
  "Given a collection of sets of gmaps, merges the gmaps in each set into a
  single gmap."
  [data]
  (letfn [(gather-gm [{:keys [mhs structure] :as total} gm]
            (assoc total
              :mhs (set/union mhs (:mhs gm))
              :structure (merge-with set/union structure (:structure gm))))

          (reduce-to-gm [gm-set]
            (let [args (reduce gather-gm {:mhs #{} :structure {}} gm-set)]
              {:mhs       (:mhs args)
               :structure (:structure args)}))]
    (update data :gmaps (partial map reduce-to-gm))))


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
  [kg {:as   gmap,
       :keys [mhs]}]
  (filter #(and (is-emap? kg %)
             (emaps-equal? (apply dissoc (get kg (:base %)) unmatched-keys)
               (apply dissoc (get kg (:target %)) unmatched-keys)))
    mhs))

(defn score-gmap
  "Computes SES and emap scores for a gmap. The emap score is not in the
  original SME. It simply counts how many entities match in their content."
  [kg {:keys [mh-structure] :as data} gm]
  (letfn [(score-mh [mh depth]
            ;; simplified trickle-down SES
            (if-let [kids (seq (:children (get mh-structure mh)))]
              (reduce + depth (map #(score-mh % (inc depth)) kids))
              depth))]
    (assoc gm
      :score (reduce + (count (:mhs gm))
               (map #(score-mh % 0) (:roots (:structure gm))))
      :emap-matches (count (matching-emaps kg gm)))))

;;; Inference generation below. Not used in TCG model.

(defn gmap-inferences
  "Generates maximal inferences for a given gmap, based on SME algorithm."
  [kg {base :graph} gmap]
  (let [mh-bases  (set (map :base (:mhs gmap)))
        unmatched (set/difference (set (keys base)) mh-bases)
        ancestors (set/select #(types/ancestor? kg mh-bases %) unmatched)]
    (set/difference (set (mapcat (partial types/get-descendants kg) ancestors))
      mh-bases)))

(defn generate-inferences
  "Appends :inferences to gmaps in given data, for a given base graph."
  [kg base data]
  (update data :gmaps (partial map #(assoc % :inferences (gmap-inferences kg base %)))))

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
    (let [pairs    (zipmap (map :base mhs) (map :target mhs))
          transfer (fn transfer [expr]
                     (if-let [t (get pairs expr)]
                       t
                       (if (= :entity (types/lookup kg expr :type))
                         (throw (RuntimeException.
                                  "cannot infer entities"))
                         (cons (types/lookup kg expr :functor)
                           (doall (map transfer (types/lookup kg expr :args)))))))]
      (assoc gmap
        :transferred (doall (map transfer inferences))))
    (catch RuntimeException e
      gmap)))

(defn transfer-inferences
  [kg data]
  (update data :gmaps (partial map (partial transfer-gmap-inferences kg))))

(defn finalize-gmaps
  "Computes additional information about the gmaps we have found and stores it
  in the gmaps."
  [kg {base :name} {target :name} data]
  (update data :gmaps (fn [gmaps]
                        (->> gmaps
                          (map #(score-gmap kg data %)) ; scores
                          (map #(assoc % :mapping {:base base :target target}))))))

(defn match
  "Attempts to find a structure mapping between base and target using an
  implementation of the SME algorithm. Returns a map with the following:

    :mh-structure All MHs created for the mapping.

    :gmaps        Collection of GMaps, which represent analogical mappings.

  Keys available in the returned GMaps:
    :mhs          Collection of match hypotheses that form the GMap.

    :structure    Map from each MH to a map with structural information.
                  Keys: :emaps, :nogoods, :children.

    :mapping      Original structures that were mapped, :base and :target.

    :score        Structural evaluation score (SES), simple implementation.

    :inferences   Maximal set of potential inferences.

    :transferred  Inferences transferred to target, ready to be added into the
                  target's concept graph.

  For example: (map :score (match b t)) -> seq of gmap scores."
  ([kg rules base target]
   (->> (create-match-hypotheses kg base target rules)
     (build-hypothesis-structure kg)
     propagate-from-emaps
     (compute-initial-gmaps kg)
     combine-gmaps
     merge-gmaps
     (finalize-gmaps kg base target)
     (generate-inferences kg base)
     (transfer-inferences kg)))
  ([kg base target]
   (match kg rules/literal-similarity base target)))
