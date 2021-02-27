(ns sme-clj.typedef
  "Data type definitions and protocols for SME, as well as facilities to create
   instances of types and manipulate them."
  (:require [clojure.repl :refer [demunge]]
            [clojure.test :refer [function?]]
            [clojure.walk :as walk]
            [mop-records]
            [mops :as mops]
            [sme-clj.util :as util])
  (:import [mop_records MopMap]))

;;; ENTITY AND PREDICATE

;; This base Entity is primarily here for testing mapping mechanics. Real
;; concept graphs will have specialised data types with value slots.

(defn make-entity [name & slots]
  {:name  name
   :type  :entity
   :slots slots})

(defn make-predicate [name & {:keys [type arity ordered?]
                              :or   {type ::Relation, arity 2, ordered? true}}]
  {:name     name
   :type     type
   :arity    (if (= type ::Relation) 1 arity)
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

(defn make-expression [id functor & args]
  {:name    id
   :type    ::Expression
   :functor functor
   :args    args})

(defn lookup [kg k & props]
  (reduce (fn [k prop]
            (get-in kg [k prop]))
    k props))

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
        (remove (conj mops/reserved-roles :concept-graph))
        (map (partial mops/slot mop))
        (map (juxt first (comp first second)))))))

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

(defn make-concept-graph [name & expressions]
  (let [e-map (atom [])]
    (letfn [(add-expr! [x]
              (let [new-x (combine-ids x)]
                (swap! e-map conj (apply make-expression new-x x) )
                new-x))]
      ;; Doseq is used here instead of passing all expressions to postwalk to prevent the
      ;; entire set of expressions for the concept graph being counted as an expression.
      (doseq [expression expressions]
        (walk/postwalk #(cond->> % (coll? %) add-expr!) expression))
      {:name  name
       :graph (util/vals-as-keys :name @e-map)
       :spec  expressions})))

;;; MATCH HYPOTHESIS

(defn make-match-hypothesis [base target]
  [base target])

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
