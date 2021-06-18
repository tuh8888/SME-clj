(ns sme-clj.mops
  "Data type definitions and protocols for SME, as well as facilities to create
   instances of types and manipulate them."
  (:require [clojure.walk :as walk]
            #?(:clj [mops.records]
               :cljs [mops.records :refer [MopMap]])
            [mops.core :as mops]
            [sme-clj.typedef :as types]
            [clojure.set :as set])
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
           (remove #{:concept-graph :functor :parents})
           (map (juxt (comp first (partial mops/slot mop))
                      (comp first (partial mops/filler-value mop))))))))

(defmethod types/expression-functor MopMap
  [kg k]
  (let [mop (mops/get-mop kg k)] (first (mops/filler-value mop :functor))))

(defmethod types/type-function? MopMap
  [kg k]
  (mops/abstr? kg k ::types/Function))

(defmethod types/ordered? MopMap
  [kg k]
  (when (mops/get-mop kg k) (mops/inherit-filler kg k :ordered?)))

(defmethod types/add-entity MopMap
  [m id parent slots & args]
  (let [mop (mops/add-slot (mops/->mop id
                                       {}
                                       (-> args
                                           types/args->slots
                                           (merge slots)))
                           :parents
                           parent)]
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
      (mops/add-mop @e-map
                    (-> concept-graph-id
                        (mops/->mop {} {})
                        (mops/add-slot :parents ::types/ConceptGraph))))))

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

(defmethod types/extract-common-role-fillers MopMap
  [_ & ms]
  (let [ms           (map (partial into {}) ms)
        common-roles (->> ms
                          (map keys)
                          (map set)
                          (apply set/intersection))]
    (map (apply juxt ms) common-roles)))

(defmethod types/expressions MopMap
  [kg concept-graph-name]
  (-> kg
      :mops
      (map (mops/mop-ids kg))
      (->> (filter (comp #{concept-graph-name}
                         first
                         #(mops/filler-value % :concept-graph)))
           (map (comp :id meta)))))
