(ns sme-clj.typedef
  "Data type definitions and protocols for SME, as well as facilities to create
   instances of types and manipulate them."
  (:require [clojure.walk :as walk]
            [sme-clj.util :refer :all]))

;;; ENTITY AND PREDICATE

(defrecord Concept [name type])

;; This base Entity is primarily here for testing mapping mechanics. Real
;; concept graphs will have specialised data types with value slots.

(defn make-entity [name & slots]
  (merge (->Concept name :entity) {:slots slots}))

(defn entity? [{:keys [type]}] (= :entity type))

(defn make-predicate [name & {:keys [type arity ordered?]
                              :or   {type :relation, arity 2, ordered? true}}]
  (merge (->Concept name type) {:arity    (if (= type :relation) 1 arity)
                                :ordered? ordered?}))

(defn function? [{:keys [type]}] (= :function type))
(defn attribute? [{:keys [type]}] (= :attribute type))

(defn predicate? [{:keys [type]}] (#{:function :attribute :relation} type))

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
  (merge (->Concept id :expression) {:functor functor
                                     :args    args}))

(defn expression? [{:keys [type]}] (= type :expression))

(defn ancestor?
  "Returns true if a given expression is an ancestor of one of the expressions
  in the base set."
  [base-set expr]
  (and (expression? expr)
    (or (contains? base-set expr)
      (some #(ancestor? base-set %) (:args expr)))))

(defn get-descendants
  "Returns the seq of descendants of the given expression."
  [expr]
  (tree-seq expression? :args expr))


(comment
  (defn predicate-type? [t x] (and (predicate? x) (= t (:ptype x))))
  (defn function? [x] (predicate-type? :function x))
  (defn attribute? [x] (predicate-type? :attribute x)))

;;; CONCEPT GRAPH

(defn make-concept-graph [name & expressions]
  (let [id-idx (atom -1)
        e-map  (atom {})]
    (letfn [(id []
              (swap! id-idx inc)
              (keyword (str "e" @id-idx)))
            (add-expr! [x]
              (let [new-x (id)]
                (swap! e-map assoc new-x x)
                new-x))]
      ;; Doseq is used here instead of passing all expressions to postwalk to prevent the
      ;; entire set of expressions for the concept graph being counted as an expression.
      (doseq [expression expressions]
        (walk/postwalk #(cond->> % (coll? %) add-expr!) expression))
      {:name  name
       :graph @e-map
       :spec  expressions})))

;;; MATCH HYPOTHESIS

(defprotocol AMatchHypothesis
  (is-expression? [mh] "Is this MH an expression?")
  (is-emap?       [mh] "Is this MH an emap?"))

(defrecord MatchHypothesis
  [base target]
  
  AMatchHypothesis
  (is-expression? [_] (expression? base))
  (is-emap?       [_] (and (entity? base) (entity? target))))

(defmake MatchHypothesis [base target])

(def make-MH make-MatchHypothesis)

;;; GMAP

(defrecord GMap
  [mhs structure])

(defmake GMap [m s])

(defn matched-goal
  [gmap]
  (:base (:mapping gmap)))

(defn matched-goals
  "Returns the set of distinct goals that are mapped in the given collection of
  gmaps."
  [gmaps]
  (set (map matched-goal gmaps)))

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
