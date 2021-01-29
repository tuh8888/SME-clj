(ns sme-clj.core-test
  (:require [clojure.pprint :as pp]
            [sme-clj.core :as SUT]
            [sme-clj.ruledef :as rules]
            [sme-clj.typedef :as types]
            [sme-clj.util :as util]
            [clojure.set  :as set ]
            [clojure.test :refer [is deftest]]))

(reset! types/id-idx -1)

;; Predicate definitions
(def predicate-map (util/vals-as-keys :name [(types/make-predicate :flow :type :relation :arity 4)
                                             (types/make-predicate :greater :type :relation :arity 2)
                                             (types/make-predicate :cause :type :relation :arity 2)
                                             (types/make-predicate :temperature :type :function)
                                             (types/make-predicate :flat-top :type :function)
                                             (types/make-predicate :pressure :type :function)
                                             (types/make-predicate :diameter :type :function)
                                             (types/make-predicate :liquid :type :attribute)
                                             (types/make-predicate :clear :type :attribute)]))


;; Entities
(def entity-map (util/vals-as-keys :name [(types/make-entity :Coffee)
                                          (types/make-entity :Icecube)
                                          (types/make-entity :Bar)
                                          (types/make-entity :Heat)

                                          (types/make-entity :Water)
                                          (types/make-entity :Beaker)
                                          (types/make-entity :Vial)
                                          (types/make-entity :Pipe)]))

;; Concept graph definitions
(def simple-heat-flow (types/make-concept-graph (keyword "simple-heat-flow")
                        [:flow :Coffee :Icecube :Heat :Bar]
                        [:greater [:temperature :Coffee] [:temperature :Icecube]]
                        [:flat-top :Coffee]
                        [:liquid :Coffee]))

(def simple-water-flow (types/make-concept-graph (keyword "simple water flow")
                         [:cause
                          [:greater [:pressure :Beaker] [:pressure :Vial]]
                          [:flow :Beaker :Vial :Water :Pipe]]
                         [:greater [:diameter :Beaker] [:diameter :Vial]]
                         [:clear :Beaker]
                         [:flat-top :Water]
                         [:liquid :Water]))

(def kg (merge-with
          (fn [v1 v2]
            {:help (vector v1 v2)})
          entity-map
          predicate-map
          (:graph simple-heat-flow)
          (:graph simple-water-flow)))

(def expected-match-hypotheses #{{:base :Water, :target :Heat}
                                 {:base :e11, :target :e1}
                                 {:base :e15, :target :e4}
                                 {:base :Beaker, :target :Coffee}
                                 {:base :e8, :target :e3}
                                 {:base :e7, :target :e2}
                                 {:base :Water, :target :Coffee}
                                 {:base :e6, :target :e1}
                                 {:base :e9, :target :e0}
                                 {:base :e16, :target :e5}
                                 {:base :Pipe, :target :Bar}
                                 {:base :e12, :target :e2}
                                 {:base :e13, :target :e3}
                                 {:base :Vial, :target :Icecube}})

(def expected-hypothesis-structure {{:base :e11, :target :e1}
                                    {:emaps    #{},
                                     :nogood   #{{:base :e6, :target :e1}},
                                     :children #{{:base :Beaker, :target :Coffee}}},
                                    {:base :e15, :target :e4}
                                    {:emaps #{}, :nogood #{}, :children #{{:base :Water, :target :Coffee}}},
                                    {:base :Water, :target :Heat}
                                    {:emaps    #{{:base :Water, :target :Heat}},
                                     :nogood   #{{:base :Water, :target :Coffee}},
                                     :children #{}},
                                    {:base :e7, :target :e2}
                                    {:emaps    #{},
                                     :nogood   #{{:base :e12, :target :e2}},
                                     :children #{{:base :Vial, :target :Icecube}}},
                                    {:base :e8, :target :e3}
                                    {:emaps    #{},
                                     :nogood   #{{:base :e13, :target :e3}},
                                     :children #{{:base :e7, :target :e2} {:base :e6, :target :e1}}},
                                    {:base :Beaker, :target :Coffee}
                                    {:emaps    #{{:base :Beaker, :target :Coffee}},
                                     :nogood   #{{:base :Water, :target :Coffee}},
                                     :children #{}},
                                    {:base :Water, :target :Coffee}
                                    {:emaps    #{{:base :Water, :target :Coffee}},
                                     :nogood   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                     :children #{}},
                                    {:base :e9, :target :e0}
                                    {:emaps  #{},
                                     :nogood #{},
                                     :children
                                     #{{:base :Water, :target :Heat}
                                       {:base :Beaker, :target :Coffee}
                                       {:base :Pipe, :target :Bar}
                                       {:base :Vial, :target :Icecube}}},
                                    {:base :e6, :target :e1}
                                    {:emaps    #{},
                                     :nogood   #{{:base :e11, :target :e1}},
                                     :children #{{:base :Beaker, :target :Coffee}}},
                                    {:base :Pipe, :target :Bar}
                                    {:emaps #{{:base :Pipe, :target :Bar}}, :nogood #{}, :children #{}},
                                    {:base :e16, :target :e5}
                                    {:emaps #{}, :nogood #{}, :children #{{:base :Water, :target :Coffee}}},
                                    {:base :Vial, :target :Icecube}
                                    {:emaps #{{:base :Vial, :target :Icecube}}, :nogood #{}, :children #{}},
                                    {:base :e13, :target :e3}
                                    {:emaps    #{},
                                     :nogood   #{{:base :e8, :target :e3}},
                                     :children #{{:base :e11, :target :e1} {:base :e12, :target :e2}}},
                                    {:base :e12, :target :e2}
                                    {:emaps    #{},
                                     :nogood   #{{:base :e7, :target :e2}},
                                     :children #{{:base :Vial, :target :Icecube}}}})

(set/difference expected-match-hypotheses)

(deftest heat-water-test
  (is (=
        expected-match-hypotheses
        (->> rules/literal-similarity
          (SUT/create-match-hypotheses kg simple-water-flow simple-heat-flow)
          (map #(into {} %))
          set)))
  (is (=
        expected-hypothesis-structure
        (SUT/build-hypothesis-structure kg expected-match-hypotheses))))
