(ns sme-clj.predicate-calculus
  (:require [sme-clj.typedef :as types]
            [clojure.walk :as walk]))

(defmethod types/initialize-kg :default [_] {})

(defn lookup [kg k & props] (reduce (fn [k prop] (get-in kg [k prop])) k props))

(defmethod types/attribute? :default
  [kg k]
  ((comp (partial = ::types/Attribute) #(lookup kg % :type)) k))

(defmethod types/entity? :default
  [kg k]
  ((comp (partial = ::types/Entity) #(lookup kg % :type)) k))

(defmethod types/expression? :default
  [kg k]
  ((comp (partial = ::types/Expression) #(lookup kg % :type)) k))

(defmethod types/expression-args :default
  [kg k]
  (when (types/expression? kg k)
    (map (partial vector :arg) (lookup kg k :args))))

(defmethod types/expression-functor :default
  [kg k]
  (when (types/expression? kg k) (lookup kg k :functor)))

(defmethod types/type-function? :default
  [kg k]
  (= ::types/Function (lookup kg k :type)))

(defmethod types/ordered? :default [kg k] (lookup kg k :ordered?))

(defmethod types/add-entity :default
  [m id & slots]
  (let [{:keys [id]
         :as   e}
        {:id    id
         :type  ::types/Entity
         :slots slots}]
    (assoc m id e)))

(defmethod types/add-predicate :default
  [m
   [id
    &
    {:keys [type arity ordered?]
     :or   {type     ::types/Relation
            arity    2
            ordered? true}}]]
  (let [p {:id       id
           :type     type
           :arity    (if (= type ::types/Relation) arity 1)
           :ordered? ordered?}]
    (assoc m id p)))

(defmethod types/add-concept-graph :default
  [m concept-graph-id & expressions]
  (let [e-map (atom [])]
    (letfn [(add-expr! [[functor & other-args :as args]]
                       (let [id (types/combine-ids args)]
                         (swap! e-map conj
                           (assoc {:id      id
                                   :type    ::types/Expression
                                   :functor functor
                                   :args    other-args}
                                  :concept-graph
                                  concept-graph-id))
                         id))]
      ;; Doseq is used here instead of passing all expressions to postwalk to
      ;; prevent the
      ;; entire set of expressions for the concept graph being counted as an
      ;; expression.
      (doseq [expression expressions]
        (walk/postwalk #(cond->> % (coll? %) add-expr!) expression))
      (as-> m m
        (reduce (fn [m
                     {:keys [id]
                      :as   e}]
                  (assoc m id e))
                m
                @e-map)
        (assoc m
               concept-graph-id
               {:id   concept-graph-id
                :type ::types/ConceptGraph
                :spec expressions})))))

(defmethod types/extract-common-role-fillers :default
  [_ & ms]
  (->> ms
       (map (partial map second))
       (apply map vector)))
