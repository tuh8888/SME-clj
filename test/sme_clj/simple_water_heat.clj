(ns sme-clj.simple-water-heat
  (:require [mop-records :as mr]
            [mops :as mops]
            [sme-clj.typedef :as types]
            [sme-clj.util :as util])
  (:import mop_records.MopMap))

(defn map-vals
  [f m]
  (->> m
    (map (juxt key (comp f val)))
    (into {})))

(defmulti add-concept-graph (comp type first vector))

(defmethod add-concept-graph MopMap
  [m k & expressions]
  (reduce (fn [m [functor & slots]]
            (let [id  (types/combine-ids (-> slots
                                           (->> (map second))
                                           (conj functor)))
                  mop (mops/->mop id (into {} slots))]
              (-> m
                (mops/add-mop mop)
                (mops/add-slot-to-mop id :parents ::types/Expression)
                (mops/add-slot-to-mop id :functor functor)
                (mops/add-slot-to-mop id :concept-graph k))))
    (mops/add-mop m (mops/->mop k nil)) expressions))

(defmethod add-concept-graph :default
  [kg k & expressions]
  (let [concept-graph (apply types/add-concept-graph k expressions)]
    (-> (merge-with (fn [v1 v2]
                      (throw (ex-info "Value already in kg"
                               {:v1 v1 :v2 v2})))
          kg
          (->> (:graph concept-graph)
            (map-vals #(assoc % :concept-graph k))))
      (assoc k {:id k
                :type :ConceptGraph
                :spec (:spec concept-graph)}))))
(def kg (as-> (types/initialize-kg {}) m
          (reduce (fn [m args]
                    (types/add-entity m args))
            m
            [[:Coffee]
             [:Icecube]
             [:Bar]
             [:Heat]

             [:Water]
             [:Beaker]
             [:Vial]
             [:Pipe]])
          (reduce (fn [m args]
                    (types/add-predicate m args))
            m
            [[:flow :type ::types/Relation :arity 4]
             [:greater :type ::types/Relation :arity 2]
             [:cause :type ::types/Relation :arity 2]
             [:temperature :type ::types/Function]
             [:flat-top :type ::types/Function]
             [:pressure :type ::types/Function]
             [:diameter :type ::types/Function]
             [:liquid :type ::types/Attribute]
             [:clear :type ::types/Attribute]])
          (-> m
            (add-concept-graph :simple-water-flow
              [:cause
               [:greater [:pressure :Beaker] [:pressure :Vial]]
               [:flow :Beaker :Vial :Water :Pipe]]
              [:greater [:diameter :Beaker] [:diameter :Vial]]
              [:clear :Beaker]
              [:flat-top :Water]
              [:liquid :Water])
            (add-concept-graph :simple-heat-flow
              [:flow :Coffee :Icecube :Heat :Bar]
              [:greater [:temperature :Coffee] [:temperature :Icecube]]
              [:flat-top :Coffee]
              [:liquid :Coffee]))))

(def mops-kg (-> (reduce (partial apply types/add-mop-entity)
                   (types/initialize-kg (mr/make-mop-map))
                   [[:cause ::types/Relation nil ::types/Expression ::types/Expression]
                    [:greater ::types/Relation nil ::types/Expression ::types/Expression]
                    [:flow ::types/Relation nil ::types/Entity ::types/Entity ::types/Entity ::types/Entity]
                    [:pressure ::types/Function nil ::types/Entity]
                    [:diameter ::types/Function nil ::types/Entity]
                    [:clear ::types/Attribute nil ::types/Entity]
                    [:temperature ::types/Function nil ::types/Entity]
                    [:flat-top ::types/Function nil ::types/Entity]
                    [:liquid ::types/Attribute nil ::types/Entity]

                    [:Coffee ::types/Entity nil]
                    [:Water ::types/Entity nil]
                    [:Heat ::types/Entity nil]
                    [:Pipe ::types/Entity nil]
                    [:Vial ::types/Entity nil]
                    [:Icecube ::types/Entity nil]
                    [:Bar ::types/Entity nil]
                    [:Beaker ::types/Entity nil]])

               (types/add-concept-graph :simple-water-flow
                 [:cause
                  [:greater [:pressure :Beaker] [:pressure :Vial]]
                  [:flow :Beaker :Vial :Water :Pipe]]
                 [:greater [:diameter :Beaker] [:diameter :Vial]]
                 [:clear :Beaker]
                 [:flat-top :Water]
                 [:liquid :Water])
               (types/add-concept-graph :simple-heat-flow
                 [:flow :Coffee :Icecube :Heat :Bar]
                 [:greater [:temperature :Coffee] [:temperature :Icecube]]
                 [:flat-top :Coffee]
                 [:liquid :Coffee])
               mops/infer-hierarchy))
