(ns sme-clj.mops
  "Data type definitions and protocols for SME, as well as facilities to create
   instances of types and manipulate them."
  (:require [clojure.walk :as walk]
            #?(:clj [mops.records]
               :cljs [mops.records :refer [MopMap]])
            [mops.core :as mops]
            [sme-clj.typedef :as types])
  (:import #?(:clj [mops.records MopMap])))

(defmethod types/attribute? MopMap [kg k] (mops/abstr? kg k ::types/Attribute))

(defmethod types/entity? MopMap [kg k] (mops/abstr? kg k ::types/Entity))

(defmethod types/expression? MopMap
  [kg k]
  (mops/abstr? kg k ::types/Expression))

(defmethod types/expression-args MopMap
  [kg k]
  (when (types/expression? kg k)
    (let [mop (mops/get-mop kg k)]
      (->> mop
           mops/roles
           (remove (into mops/reserved-roles [:concept-graph :functor]))
           (map (partial mops/slot mop))
           (map (juxt first (comp first second)))))))

(defmethod types/expression-functor MopMap
  [kg k]
  (let [mop (mops/get-mop kg k)] (first (mops/filler mop :functor))))

(defmethod types/type-function? MopMap
  [kg k]
  (mops/abstr? kg k ::types/Function))

(defmethod types/ordered? MopMap
  [kg k]
  (when (mops/get-mop kg k) (mops/inherit-filler kg k :ordered?)))

(defmethod types/add-entity MopMap
  [m id parent slots & args]
  (let [mop (mops/->mop id
                        {}
                        (-> args
                            types/args->slots
                            (merge slots)
                            (assoc :parents #{parent})))]
    (mops/add-mop m mop)))

(defmethod types/add-concept-graph MopMap
  [m concept-graph-id & expressions]
  (let [e-map (atom m)]
    (letfn [(add-expr!
             [& args]
             (let [[[functor & function-args :as all-e]] args
                   id                                    (types/combine-ids
                                                          all-e)]
               (swap! e-map mops/add-mop
                 (-> id
                     (mops/->mop {} (types/args->slots function-args))
                     (mops/add-slot :parents ::types/Expression)
                     (mops/add-slot :functor functor)
                     (mops/add-slot :concept-graph concept-graph-id)))
               id))]
      (doseq [expression expressions]
        (walk/postwalk #(cond->> % (coll? %) add-expr!) expression))
      (mops/add-mop
       @e-map
       (mops/->mop concept-graph-id {} {:parents #{::types/ConceptGraph}})))))

(defmethod types/initialize-kg MopMap
  [m]
  (reduce (partial apply types/add-entity)
          m
          [[::types/Expression :thing nil]
           [::types/Entity :thing nil]
           [::types/Functor ::types/Expression nil]
           [::types/Relation ::types/Functor {:ordered? true}]
           [::types/Attribute ::types/Functor nil]
           [::types/Function ::types/Functor nil]]))
