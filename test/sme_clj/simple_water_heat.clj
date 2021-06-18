(ns sme-clj.simple-water-heat
  (:require [mops.records :as mr]
            [mops.core :as mops]
            [sme-clj.typedef :as types]))

(def kg
  (as-> (types/initialize-kg {}) m
    (reduce
     (partial apply types/add-entity)
     m
     [[:Coffee] [:Icecube] [:Bar] [:Heat] [:Water] [:Beaker] [:Vial] [:Pipe]])
    (reduce (fn [m args] (types/add-predicate m args))
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
    (->
      m
      (types/add-concept-graph :simple-water-flow
                               [:cause
                                [:greater [:pressure :Beaker] [:pressure :Vial]]
                                [:flow :Beaker :Vial :Water :Pipe]]
                               [:greater [:diameter :Beaker] [:diameter :Vial]]
                               [:clear :Beaker]
                               [:flat-top :Water]
                               [:liquid :Water])
      (types/add-concept-graph
       :simple-heat-flow
       [:flow :Coffee :Icecube :Heat :Bar]
       [:greater [:temperature :Coffee] [:temperature :Icecube]]
       [:flat-top :Coffee]
       [:liquid :Coffee]))))

(def mops-kg
  (-> (reduce
       (partial apply types/add-entity)
       (types/initialize-kg (mr/make-mop-map))
       [[:cause ::types/Relation nil ::types/Expression ::types/Expression]
        [:greater ::types/Relation nil ::types/Expression ::types/Expression]
        [:flow ::types/Relation nil ::types/Entity ::types/Entity ::types/Entity
         ::types/Entity]
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
      (types/add-concept-graph
       :simple-heat-flow
       [:flow :Coffee :Icecube :Heat :Bar]
       [:greater [:temperature :Coffee] [:temperature :Icecube]]
       [:flat-top :Coffee]
       [:liquid :Coffee])
      mops/infer-hierarchy))
