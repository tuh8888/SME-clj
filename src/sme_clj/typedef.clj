(ns sme-clj.typedef
  "Data type definitions and protocols for SME, as well as facilities to create
   instances of types and manipulate them."
  (:require
            [clojure.repl :refer [demunge]]
            [clojure.test :refer [function?]]
            [clojure.walk :as walk]
            [mop-records]
            [mops :as mops]
            [sme-clj.util :as util])
  (:import [mop_records MopMap]))

;;; ENTITY AND PREDICATE

;; This base Entity is primarily here for testing mapping mechanics. Real
;; concept graphs will have specialised data types with value slots.

(defn make-entity [id & slots]
  {:id    id
   :type  ::Entity
   :slots slots})

(defn make-predicate [id & {:keys [type arity ordered?]
                            :or   {type ::Relation, arity 2, ordered? true}}]
  {:id id
   :type     type
   :arity    (if (= type ::Relation) arity 1)
   :ordered? ordered?})

(defn predicate? [{:keys [type]}] (#{::Function ::Attribute ::Relation} type))

;;; EXPRESSION

;; Originally expressions were simply seqs, with as first element the
;; functor. However, this meant two relations with the same predicate and same
;; entities were equal as per value equality. Though this problem is not
;; detrimental in every situation, they are sufficiently common to change the
;; representation to a uniquely identifiable one including a unique id.
;;
;; An Expression's id is not guaranteed to be related to any implicit ordering
;; of the expressions within a graph, but in practice often is.

(defmulti make-expression (comp type first vector))
(defn make-expression [id functor & args]
  {:id      id
   :type    ::Expression
   :functor functor
   :args    args})

(defn lookup [kg k & props]
  (reduce (fn [k prop]
            (get-in kg [k prop]))
    k props))

(defmulti attribute? (comp type first vector))

(defmethod attribute? :default
  [kg k]
  ((comp (partial = ::Attribute) #(lookup kg % :type)) k))

(defmethod attribute? MopMap
  [kg k]
  (mops/abstr? kg k ::Attribute))

(defmulti entity? (comp type first vector))

(defmethod entity? :default
  [kg k]
  ((comp (partial = ::Entity) #(lookup kg % :type)) k))

(defmethod entity? MopMap
  [kg k]
  (mops/abstr? kg k ::Entity))

(defmulti expression? (comp type first vector))

(defmethod expression? :default
  [kg k]
  ((comp (partial = ::Expression) #(lookup kg % :type)) k))

(defmethod expression? MopMap
  [kg k]
  (mops/abstr? kg k ::Expression))

(defmulti expression-args (comp type first vector))

(defmethod expression-args :default
  [kg k]
  (when (expression? kg k)
    (map (partial vector :arg) (lookup kg k :args))))

(defmethod expression-args MopMap
  [kg k]
  (when (expression? kg k)
    (let [mop (mops/get-mop kg k)]
      (->> mop
        mops/roles
        (remove (into mops/reserved-roles [:concept-graph :functor]))
        (map (partial mops/slot mop))
        (map (juxt first (comp first second)))))))

(defmulti expression-functor (comp type first vector))

(defmethod expression-functor MopMap
  [kg k]
  (let [mop (mops/get-mop kg k)]
    (first (mops/filler mop :functor))))

(defmethod expression-functor :default
  [kg k]
  (when (expression? kg k)
    (lookup kg k :functor)))

(defmulti type-function? (comp type first vector))

(defmethod type-function? MopMap
  [kg k]
  (mops/abstr? kg k ::Function))


(defmethod type-function? :default
  [kg k]
  (= ::Function (lookup kg k :type)))

(defmulti ordered? (comp type first vector))

(defmethod ordered? MopMap
  [kg k]
  (when (mops/get-mop kg k)
    (mops/inherit-filler kg k :ordered?)))

(defmethod ordered? :default
  [kg k]
  (lookup kg k :ordered?))


(defn ancestor?
  "Returns true if a given expression is an ancestor of one of the expressions
  in the base set."
  [kg base-set expr]
  (and (expression? kg expr)
    (or (contains? base-set expr)
      (some #(ancestor? kg base-set %) (map second (expression-args kg expr))))))

(defn get-descendants
  "Returns the seq of descendants of the given expression."
  [kg expr]
  (tree-seq (partial expression? kg) (comp  (partial map second) (partial expression-args kg)) expr))

;;; CONCEPT GRAPH

(defn- pretty-demunge
  [fn-object]
  (if (function? fn-object)
    (let [dem-fn (demunge (str fn-object))
          pretty (last (re-find #"(.*?\/(.*?))[\-\-|@].*" dem-fn))]
      (if pretty pretty dem-fn))
    fn-object))

(defn combine-ids [ids]
  (->> ids
    (map (fn [id]
           (cond-> id
             (coll? id)          ((comp #(str "[" % "]") #(subs % 1) str combine-ids (partial mapv (comp keyword pretty-demunge))))
             (not (keyword? id)) ((comp keyword str)))))
    (map str)
    (map #(subs % 1))
    (interpose "-")
    (apply str)
    keyword))

(defmulti add-entity (comp type first vector))

(defmethod add-entity :default
  [m args]
  (let [{:keys [id] :as e} (apply make-entity args)]
    (assoc m id e)))

(defmulti add-predicate (comp type first vector))

(defmethod add-predicate :default
  [m args]
  (let [{:keys [id] :as p} (apply make-predicate args)]
    (assoc m id p)))

(defmulti add-concept-graph (comp type first vector))

(defmethod add-concept-graph :default
  [m concept-graph-id & expressions]
  (let [e-map (atom [])]
    (letfn [(add-expr! [args]
              (let [id (combine-ids args)]
                (swap! e-map conj (assoc (apply make-expression id args)
                                    :concept-graph concept-graph-id))
                id))]
      ;; Doseq is used here instead of passing all expressions to postwalk to prevent the
      ;; entire set of expressions for the concept graph being counted as an expression.
      (doseq [expression expressions]
        (walk/postwalk #(cond->> % (coll? %) add-expr!) expression))
      (as-> m m
        (reduce (fn [m {:keys [id] :as e}]
                  (assoc m id e))
          m @e-map)
        (assoc m concept-graph-id {:id   concept-graph-id
                                   :type ::ConceptGraph
                                   :spec expressions})))))

(defn args->slots
  [args]
  (->> args
    (map-indexed (comp (juxt (comp keyword (partial str "e") inc first)
                      second)
                   vector))
    (into {})))

(defn add-mop-entity
  [m id parent slots & args]
  (mops/add-mop m (mops/->mop id (-> args
                                   args->slots
                                   (merge slots)
                                   (assoc :parents #{parent})))))

(defmulti initialize-kg (comp type first vector))

(defmethod initialize-kg :default
  [_]
  {})

(defmethod initialize-kg MopMap
  [m]
  (reduce (partial apply add-mop-entity)
    m
    [[::Expression :thing nil]
     [::Entity :thing nil]
     [::Functor ::Expression nil]
     [::Relation ::Functor {:ordered? true}]
     [::Attribute ::Functor nil]
     [::Function ::Functor nil]]))

(defmethod add-concept-graph MopMap
  [m concept-graph-id & expressions]
  (let [e-map (atom m)]
    (letfn [(add-expr! [& args]
              (let [[[functor & function-args :as all-e]] args
                    id                                    (combine-ids all-e)]

                (swap! e-map mops/add-mop (-> id
                                            (mops/->mop (args->slots function-args))
                                            (mops/add-slot :parents ::Expression)
                                            (mops/add-slot :functor functor)
                                            (mops/add-slot :concept-graph concept-graph-id)))
                id))]
      (doseq [expression expressions]
        (walk/postwalk #(cond->> % (coll? %) add-expr!) expression))
      (mops/add-mop @e-map (mops/->mop concept-graph-id nil)))))

;;; GMAP

(defn matched-goal
  [gmap]
  (get-in gmap [:mapping 0]))

(defn matched-goals
  "Returns the set of distinct goals that are mapped in the given collection of
  gmaps."
  [gmaps]
  (->> gmaps
    (map matched-goal)
    set))

(defn filter-predicates
  "Returns a seq of relations for which the root predicate matches the given
  predicate."
  [predicate coll]
  (filter #(= predicate (:functor %)) coll))

(defn some-predicate
  "Returns the first relation for which the root predicate matches the given
  predicate, or nil if none is found."
  [predicate coll]
  (->> coll
    (map :functor)
    (some #{predicate})))
