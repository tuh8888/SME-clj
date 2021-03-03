(ns water-heat
  (:require [mop-records :as mr]
            [mops :as mops]
            [sme-clj.core :as sme]
            [sme-clj.typedef :as types]))

(def kg (-> (reduce (partial apply types/add-mop-entity)
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

(sme/match kg :simple-water-flow :simple-heat-flow)

(->> (sme/match kg :simple-water-flow :simple-heat-flow)
  (sme/perform-inference kg :simple-water-flow)
  (map :transferred))
