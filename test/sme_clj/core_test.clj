(ns sme-clj.core-test
  (:require [clojure.pprint :as pp]
            [sme-clj.core :as SUT]
            [sme-clj.ruledef :as rules]
            [sme-clj.typedef :as types]
            [sme-clj.util :as util]
            [clojure.set  :as set ]
            [clojure.test :refer [is deftest]]
            [clojure.data :as data]))

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

(def simple-water-flow (types/make-concept-graph (keyword "simple-water-flow")
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

(def expected-propagated-from-emaps {{:base :e11, :target :e1}
                                     {:emaps    #{{:base :Beaker, :target :Coffee}},
                                      :nogood   #{{:base :Water, :target :Coffee} {:base :e6, :target :e1}},
                                      :children #{{:base :Beaker, :target :Coffee}}},
                                     {:base :e15, :target :e4}
                                     {:emaps    #{{:base :Water, :target :Coffee}},
                                      :nogood   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                      :children #{{:base :Water, :target :Coffee}}},
                                     {:base :Water, :target :Heat}
                                     {:emaps    #{{:base :Water, :target :Heat}},
                                      :nogood   #{{:base :Water, :target :Coffee}},
                                      :children #{}},
                                     {:base :e7, :target :e2}
                                     {:emaps    #{{:base :Vial, :target :Icecube}},
                                      :nogood   #{{:base :e12, :target :e2}},
                                      :children #{{:base :Vial, :target :Icecube}}},
                                     {:base :e8, :target :e3}
                                     {:emaps    #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}},
                                      :nogood
                                      #{{:base :e11, :target :e1}
                                        {:base :Water, :target :Coffee}
                                        {:base :e13, :target :e3}
                                        {:base :e12, :target :e2}},
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
                                     {:emaps
                                      #{{:base :Water, :target :Heat}
                                        {:base :Beaker, :target :Coffee}
                                        {:base :Pipe, :target :Bar}
                                        {:base :Vial, :target :Icecube}},
                                      :nogood #{{:base :Water, :target :Coffee}},
                                      :children
                                      #{{:base :Water, :target :Heat}
                                        {:base :Beaker, :target :Coffee}
                                        {:base :Pipe, :target :Bar}
                                        {:base :Vial, :target :Icecube}}},
                                     {:base :e6, :target :e1}
                                     {:emaps    #{{:base :Beaker, :target :Coffee}},
                                      :nogood   #{{:base :e11, :target :e1} {:base :Water, :target :Coffee}},
                                      :children #{{:base :Beaker, :target :Coffee}}},
                                     {:base :Pipe, :target :Bar}
                                     {:emaps #{{:base :Pipe, :target :Bar}}, :nogood #{}, :children #{}},
                                     {:base :e16, :target :e5}
                                     {:emaps    #{{:base :Water, :target :Coffee}},
                                      :nogood   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                      :children #{{:base :Water, :target :Coffee}}},
                                     {:base :Vial, :target :Icecube}
                                     {:emaps #{{:base :Vial, :target :Icecube}}, :nogood #{}, :children #{}},
                                     {:base :e13, :target :e3}
                                     {:emaps    #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}},
                                      :nogood
                                      #{{:base :e7, :target :e2}
                                        {:base :e8, :target :e3}
                                        {:base :Water, :target :Coffee}
                                        {:base :e6, :target :e1}},
                                      :children #{{:base :e11, :target :e1} {:base :e12, :target :e2}}},
                                     {:base :e12, :target :e2}
                                     {:emaps    #{{:base :Vial, :target :Icecube}},
                                      :nogood   #{{:base :e7, :target :e2}},
                                      :children #{{:base :Vial, :target :Icecube}}}})

(def expected-computed-initial-gmaps {:gmaps
                                      #{{:mhs
                                         #{{:base :Water, :target :Heat}
                                           {:base :Beaker, :target :Coffee}
                                           {:base :e9, :target :e0}
                                           {:base :Pipe, :target :Bar}
                                           {:base :Vial, :target :Icecube}},
                                         :structure
                                         {:roots  #{{:base :e9, :target :e0}},
                                          :nogood #{{:base :Water, :target :Coffee}},
                                          :emaps
                                          #{{:base :Water, :target :Heat}
                                            {:base :Beaker, :target :Coffee}
                                            {:base :Pipe, :target :Bar}
                                            {:base :Vial, :target :Icecube}}}}
                                        {:mhs #{{:base :e15, :target :e4} {:base :Water, :target :Coffee}},
                                         :structure
                                         {:roots  #{{:base :e15, :target :e4}},
                                          :nogood #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                          :emaps  #{{:base :Water, :target :Coffee}}}}
                                        {:mhs
                                         #{{:base :e11, :target :e1}
                                           {:base :Beaker, :target :Coffee}
                                           {:base :Vial, :target :Icecube}
                                           {:base :e13, :target :e3}
                                           {:base :e12, :target :e2}},
                                         :structure
                                         {:roots #{{:base :e13, :target :e3}},
                                          :nogood
                                          #{{:base :e7, :target :e2}
                                            {:base :e8, :target :e3}
                                            {:base :Water, :target :Coffee}
                                            {:base :e6, :target :e1}},
                                          :emaps
                                          #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}}}}
                                        {:mhs #{{:base :Water, :target :Coffee} {:base :e16, :target :e5}},
                                         :structure
                                         {:roots  #{{:base :e16, :target :e5}},
                                          :nogood #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                          :emaps  #{{:base :Water, :target :Coffee}}}}
                                        {:mhs
                                         #{{:base :e7, :target :e2}
                                           {:base :e8, :target :e3}
                                           {:base :Beaker, :target :Coffee}
                                           {:base :e6, :target :e1}
                                           {:base :Vial, :target :Icecube}},
                                         :structure
                                         {:roots #{{:base :e8, :target :e3}},
                                          :nogood
                                          #{{:base :e11, :target :e1}
                                            {:base :Water, :target :Coffee}
                                            {:base :e13, :target :e3}
                                            {:base :e12, :target :e2}},
                                          :emaps
                                          #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}}}}},
                                      :mh-structure
                                      {{:base :e11, :target :e1}
                                       {:emaps    #{{:base :Beaker, :target :Coffee}},
                                        :nogood   #{{:base :Water, :target :Coffee} {:base :e6, :target :e1}},
                                        :children #{{:base :Beaker, :target :Coffee}}},
                                       {:base :e15, :target :e4}
                                       {:emaps    #{{:base :Water, :target :Coffee}},
                                        :nogood   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                        :children #{{:base :Water, :target :Coffee}}},
                                       {:base :Water, :target :Heat}
                                       {:emaps    #{{:base :Water, :target :Heat}},
                                        :nogood   #{{:base :Water, :target :Coffee}},
                                        :children #{}},
                                       {:base :e7, :target :e2}
                                       {:emaps    #{{:base :Vial, :target :Icecube}},
                                        :nogood   #{{:base :e12, :target :e2}},
                                        :children #{{:base :Vial, :target :Icecube}}},
                                       {:base :e8, :target :e3}
                                       {:emaps    #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}},
                                        :nogood
                                        #{{:base :e11, :target :e1}
                                          {:base :Water, :target :Coffee}
                                          {:base :e13, :target :e3}
                                          {:base :e12, :target :e2}},
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
                                       {:emaps
                                        #{{:base :Water, :target :Heat}
                                          {:base :Beaker, :target :Coffee}
                                          {:base :Pipe, :target :Bar}
                                          {:base :Vial, :target :Icecube}},
                                        :nogood #{{:base :Water, :target :Coffee}},
                                        :children
                                        #{{:base :Water, :target :Heat}
                                          {:base :Beaker, :target :Coffee}
                                          {:base :Pipe, :target :Bar}
                                          {:base :Vial, :target :Icecube}}},
                                       {:base :e6, :target :e1}
                                       {:emaps    #{{:base :Beaker, :target :Coffee}},
                                        :nogood   #{{:base :e11, :target :e1} {:base :Water, :target :Coffee}},
                                        :children #{{:base :Beaker, :target :Coffee}}},
                                       {:base :Pipe, :target :Bar}
                                       {:emaps #{{:base :Pipe, :target :Bar}}, :nogood #{}, :children #{}},
                                       {:base :e16, :target :e5}
                                       {:emaps    #{{:base :Water, :target :Coffee}},
                                        :nogood   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                        :children #{{:base :Water, :target :Coffee}}},
                                       {:base :Vial, :target :Icecube}
                                       {:emaps #{{:base :Vial, :target :Icecube}}, :nogood #{}, :children #{}},
                                       {:base :e13, :target :e3}
                                       {:emaps    #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}},
                                        :nogood
                                        #{{:base :e7, :target :e2}
                                          {:base :e8, :target :e3}
                                          {:base :Water, :target :Coffee}
                                          {:base :e6, :target :e1}},
                                        :children #{{:base :e11, :target :e1} {:base :e12, :target :e2}}},
                                       {:base :e12, :target :e2}
                                       {:emaps    #{{:base :Vial, :target :Icecube}},
                                        :nogood   #{{:base :e7, :target :e2}},
                                        :children #{{:base :Vial, :target :Icecube}}}}})

(def expected-combined-gmaps {:gmaps
                              '(#{{:mhs
                                   #{{:base :Water, :target :Heat}
                                     {:base :Beaker, :target :Coffee}
                                     {:base :e9, :target :e0}
                                     {:base :Pipe, :target :Bar}
                                     {:base :Vial, :target :Icecube}},
                                   :structure
                                   {:roots  #{{:base :e9, :target :e0}},
                                    :nogood #{{:base :Water, :target :Coffee}},
                                    :emaps
                                    #{{:base :Water, :target :Heat}
                                      {:base :Beaker, :target :Coffee}
                                      {:base :Pipe, :target :Bar}
                                      {:base :Vial, :target :Icecube}}}}
                                  {:mhs
                                   #{{:base :e11, :target :e1}
                                     {:base :Beaker, :target :Coffee}
                                     {:base :Vial, :target :Icecube}
                                     {:base :e13, :target :e3}
                                     {:base :e12, :target :e2}},
                                   :structure
                                   {:roots #{{:base :e13, :target :e3}},
                                    :nogood
                                    #{{:base :e7, :target :e2}
                                      {:base :e8, :target :e3}
                                      {:base :Water, :target :Coffee}
                                      {:base :e6, :target :e1}},
                                    :emaps
                                    #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}}}}}
                                 #{{:mhs
                                    #{{:base :Water, :target :Heat}
                                      {:base :Beaker, :target :Coffee}
                                      {:base :e9, :target :e0}
                                      {:base :Pipe, :target :Bar}
                                      {:base :Vial, :target :Icecube}},
                                    :structure
                                    {:roots  #{{:base :e9, :target :e0}},
                                     :nogood #{{:base :Water, :target :Coffee}},
                                     :emaps
                                     #{{:base :Water, :target :Heat}
                                       {:base :Beaker, :target :Coffee}
                                       {:base :Pipe, :target :Bar}
                                       {:base :Vial, :target :Icecube}}}}
                                   {:mhs
                                    #{{:base :e7, :target :e2}
                                      {:base :e8, :target :e3}
                                      {:base :Beaker, :target :Coffee}
                                      {:base :e6, :target :e1}
                                      {:base :Vial, :target :Icecube}},
                                    :structure
                                    {:roots #{{:base :e8, :target :e3}},
                                     :nogood
                                     #{{:base :e11, :target :e1}
                                       {:base :Water, :target :Coffee}
                                       {:base :e13, :target :e3}
                                       {:base :e12, :target :e2}},
                                     :emaps
                                     #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}}}}}
                                 #{{:mhs #{{:base :e15, :target :e4} {:base :Water, :target :Coffee}},
                                    :structure
                                    {:roots #{{:base :e15, :target :e4}},
                                     :nogood
                                     #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                     :emaps #{{:base :Water, :target :Coffee}}}}
                                   {:mhs #{{:base :Water, :target :Coffee} {:base :e16, :target :e5}},
                                    :structure
                                    {:roots #{{:base :e16, :target :e5}},
                                     :nogood
                                     #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                     :emaps #{{:base :Water, :target :Coffee}}}}}),
                              :mh-structure
                              {{:base :e11, :target :e1}
                               {:emaps    #{{:base :Beaker, :target :Coffee}},
                                :nogood   #{{:base :Water, :target :Coffee} {:base :e6, :target :e1}},
                                :children #{{:base :Beaker, :target :Coffee}}},
                               {:base :e15, :target :e4}
                               {:emaps    #{{:base :Water, :target :Coffee}},
                                :nogood   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                :children #{{:base :Water, :target :Coffee}}},
                               {:base :Water, :target :Heat}
                               {:emaps    #{{:base :Water, :target :Heat}},
                                :nogood   #{{:base :Water, :target :Coffee}},
                                :children #{}},
                               {:base :e7, :target :e2}
                               {:emaps    #{{:base :Vial, :target :Icecube}},
                                :nogood   #{{:base :e12, :target :e2}},
                                :children #{{:base :Vial, :target :Icecube}}},
                               {:base :e8, :target :e3}
                               {:emaps    #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}},
                                :nogood
                                #{{:base :e11, :target :e1}
                                  {:base :Water, :target :Coffee}
                                  {:base :e13, :target :e3}
                                  {:base :e12, :target :e2}},
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
                               {:emaps
                                #{{:base :Water, :target :Heat}
                                  {:base :Beaker, :target :Coffee}
                                  {:base :Pipe, :target :Bar}
                                  {:base :Vial, :target :Icecube}},
                                :nogood #{{:base :Water, :target :Coffee}},
                                :children
                                #{{:base :Water, :target :Heat}
                                  {:base :Beaker, :target :Coffee}
                                  {:base :Pipe, :target :Bar}
                                  {:base :Vial, :target :Icecube}}},
                               {:base :e6, :target :e1}
                               {:emaps    #{{:base :Beaker, :target :Coffee}},
                                :nogood   #{{:base :e11, :target :e1} {:base :Water, :target :Coffee}},
                                :children #{{:base :Beaker, :target :Coffee}}},
                               {:base :Pipe, :target :Bar}
                               {:emaps #{{:base :Pipe, :target :Bar}}, :nogood #{}, :children #{}},
                               {:base :e16, :target :e5}
                               {:emaps    #{{:base :Water, :target :Coffee}},
                                :nogood   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                :children #{{:base :Water, :target :Coffee}}},
                               {:base :Vial, :target :Icecube}
                               {:emaps #{{:base :Vial, :target :Icecube}}, :nogood #{}, :children #{}},
                               {:base :e13, :target :e3}
                               {:emaps    #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}},
                                :nogood
                                #{{:base :e7, :target :e2}
                                  {:base :e8, :target :e3}
                                  {:base :Water, :target :Coffee}
                                  {:base :e6, :target :e1}},
                                :children #{{:base :e11, :target :e1} {:base :e12, :target :e2}}},
                               {:base :e12, :target :e2}
                               {:emaps    #{{:base :Vial, :target :Icecube}},
                                :nogood   #{{:base :e7, :target :e2}},
                                :children #{{:base :Vial, :target :Icecube}}}}})

(def expected-merged-gmaps {:gmaps
                            '({:mhs
                               #{{:base :e11, :target :e1}
                                 {:base :Water, :target :Heat}
                                 {:base :Beaker, :target :Coffee}
                                 {:base :e9, :target :e0}
                                 {:base :Pipe, :target :Bar}
                                 {:base :Vial, :target :Icecube}
                                 {:base :e13, :target :e3}
                                 {:base :e12, :target :e2}},
                               :structure
                               {:roots #{{:base :e9, :target :e0} {:base :e13, :target :e3}},
                                :nogood
                                #{{:base :e7, :target :e2}
                                  {:base :e8, :target :e3}
                                  {:base :Water, :target :Coffee}
                                  {:base :e6, :target :e1}},
                                :emaps
                                #{{:base :Water, :target :Heat}
                                  {:base :Beaker, :target :Coffee}
                                  {:base :Pipe, :target :Bar}
                                  {:base :Vial, :target :Icecube}}}}
                              {:mhs
                               #{{:base :Water, :target :Heat}
                                 {:base :e7, :target :e2}
                                 {:base :e8, :target :e3}
                                 {:base :Beaker, :target :Coffee}
                                 {:base :e9, :target :e0}
                                 {:base :e6, :target :e1}
                                 {:base :Pipe, :target :Bar}
                                 {:base :Vial, :target :Icecube}},
                               :structure
                               {:roots #{{:base :e8, :target :e3} {:base :e9, :target :e0}},
                                :nogood
                                #{{:base :e11, :target :e1}
                                  {:base :Water, :target :Coffee}
                                  {:base :e13, :target :e3}
                                  {:base :e12, :target :e2}},
                                :emaps
                                #{{:base :Water, :target :Heat}
                                  {:base :Beaker, :target :Coffee}
                                  {:base :Pipe, :target :Bar}
                                  {:base :Vial, :target :Icecube}}}}
                              {:mhs
                               #{{:base :e15, :target :e4}
                                 {:base :Water, :target :Coffee}
                                 {:base :e16, :target :e5}},
                               :structure
                               {:roots  #{{:base :e15, :target :e4} {:base :e16, :target :e5}},
                                :nogood #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                :emaps  #{{:base :Water, :target :Coffee}}}}),
                            :mh-structure
                            {{:base :e11, :target :e1}
                             {:emaps    #{{:base :Beaker, :target :Coffee}},
                              :nogood   #{{:base :Water, :target :Coffee} {:base :e6, :target :e1}},
                              :children #{{:base :Beaker, :target :Coffee}}},
                             {:base :e15, :target :e4}
                             {:emaps    #{{:base :Water, :target :Coffee}},
                              :nogood   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                              :children #{{:base :Water, :target :Coffee}}},
                             {:base :Water, :target :Heat}
                             {:emaps    #{{:base :Water, :target :Heat}},
                              :nogood   #{{:base :Water, :target :Coffee}},
                              :children #{}},
                             {:base :e7, :target :e2}
                             {:emaps    #{{:base :Vial, :target :Icecube}},
                              :nogood   #{{:base :e12, :target :e2}},
                              :children #{{:base :Vial, :target :Icecube}}},
                             {:base :e8, :target :e3}
                             {:emaps    #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}},
                              :nogood
                              #{{:base :e11, :target :e1}
                                {:base :Water, :target :Coffee}
                                {:base :e13, :target :e3}
                                {:base :e12, :target :e2}},
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
                             {:emaps
                              #{{:base :Water, :target :Heat}
                                {:base :Beaker, :target :Coffee}
                                {:base :Pipe, :target :Bar}
                                {:base :Vial, :target :Icecube}},
                              :nogood #{{:base :Water, :target :Coffee}},
                              :children
                              #{{:base :Water, :target :Heat}
                                {:base :Beaker, :target :Coffee}
                                {:base :Pipe, :target :Bar}
                                {:base :Vial, :target :Icecube}}},
                             {:base :e6, :target :e1}
                             {:emaps    #{{:base :Beaker, :target :Coffee}},
                              :nogood   #{{:base :e11, :target :e1} {:base :Water, :target :Coffee}},
                              :children #{{:base :Beaker, :target :Coffee}}},
                             {:base :Pipe, :target :Bar}
                             {:emaps #{{:base :Pipe, :target :Bar}}, :nogood #{}, :children #{}},
                             {:base :e16, :target :e5}
                             {:emaps    #{{:base :Water, :target :Coffee}},
                              :nogood   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                              :children #{{:base :Water, :target :Coffee}}},
                             {:base :Vial, :target :Icecube}
                             {:emaps #{{:base :Vial, :target :Icecube}}, :nogood #{}, :children #{}},
                             {:base :e13, :target :e3}
                             {:emaps    #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}},
                              :nogood
                              #{{:base :e7, :target :e2}
                                {:base :e8, :target :e3}
                                {:base :Water, :target :Coffee}
                                {:base :e6, :target :e1}},
                              :children #{{:base :e11, :target :e1} {:base :e12, :target :e2}}},
                             {:base :e12, :target :e2}
                             {:emaps    #{{:base :Vial, :target :Icecube}},
                              :nogood   #{{:base :e7, :target :e2}},
                              :children #{{:base :Vial, :target :Icecube}}}}})

(def expected-gmap-score {:mhs
                          #{{:base :e11, :target :e1}
                            {:base :Water, :target :Heat}
                            {:base :Beaker, :target :Coffee}
                            {:base :e9, :target :e0}
                            {:base :Pipe, :target :Bar}
                            {:base :Vial, :target :Icecube}
                            {:base :e13, :target :e3}
                            {:base :e12, :target :e2}},
                          :structure
                          {:roots #{{:base :e9, :target :e0} {:base :e13, :target :e3}},
                           :nogood
                           #{{:base :e7, :target :e2}
                             {:base :e8, :target :e3}
                             {:base :Water, :target :Coffee}
                             {:base :e6, :target :e1}},
                           :emaps
                           #{{:base :Water, :target :Heat}
                             {:base :Beaker, :target :Coffee}
                             {:base :Pipe, :target :Bar}
                             {:base :Vial, :target :Icecube}}},
                          :score        18,
                          :emap-matches 0})

(def expected-finalized-gmaps {:gmaps
                               '({:mhs
                                  #{{:base :e11, :target :e1}
                                    {:base :Water, :target :Heat}
                                    {:base :Beaker, :target :Coffee}
                                    {:base :e9, :target :e0}
                                    {:base :Pipe, :target :Bar}
                                    {:base :Vial, :target :Icecube}
                                    {:base :e13, :target :e3}
                                    {:base :e12, :target :e2}},
                                  :structure
                                  {:roots #{{:base :e9, :target :e0} {:base :e13, :target :e3}},
                                   :nogood
                                   #{{:base :e7, :target :e2}
                                     {:base :e8, :target :e3}
                                     {:base :Water, :target :Coffee}
                                     {:base :e6, :target :e1}},
                                   :emaps
                                   #{{:base :Water, :target :Heat}
                                     {:base :Beaker, :target :Coffee}
                                     {:base :Pipe, :target :Bar}
                                     {:base :Vial, :target :Icecube}}},
                                  :score        18,
                                  :emap-matches 0}
                                 {:mhs
                                  #{{:base :Water, :target :Heat}
                                    {:base :e7, :target :e2}
                                    {:base :e8, :target :e3}
                                    {:base :Beaker, :target :Coffee}
                                    {:base :e9, :target :e0}
                                    {:base :e6, :target :e1}
                                    {:base :Pipe, :target :Bar}
                                    {:base :Vial, :target :Icecube}},
                                  :structure
                                  {:roots #{{:base :e8, :target :e3} {:base :e9, :target :e0}},
                                   :nogood
                                   #{{:base :e11, :target :e1}
                                     {:base :Water, :target :Coffee}
                                     {:base :e13, :target :e3}
                                     {:base :e12, :target :e2}},
                                   :emaps
                                   #{{:base :Water, :target :Heat}
                                     {:base :Beaker, :target :Coffee}
                                     {:base :Pipe, :target :Bar}
                                     {:base :Vial, :target :Icecube}}},
                                  :score        18,
                                  :emap-matches 0}
                                 {:mhs
                                  #{{:base :e15, :target :e4}
                                    {:base :Water, :target :Coffee}
                                    {:base :e16, :target :e5}},
                                  :structure
                                  {:roots  #{{:base :e15, :target :e4} {:base :e16, :target :e5}},
                                   :nogood #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                   :emaps  #{{:base :Water, :target :Coffee}}},
                                  :score        5,
                                  :emap-matches 0}),
                               :mh-structure
                               {{:base :e11, :target :e1}
                                {:emaps    #{{:base :Beaker, :target :Coffee}},
                                 :nogood   #{{:base :Water, :target :Coffee} {:base :e6, :target :e1}},
                                 :children #{{:base :Beaker, :target :Coffee}}},
                                {:base :e15, :target :e4}
                                {:emaps    #{{:base :Water, :target :Coffee}},
                                 :nogood   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                 :children #{{:base :Water, :target :Coffee}}},
                                {:base :Water, :target :Heat}
                                {:emaps    #{{:base :Water, :target :Heat}},
                                 :nogood   #{{:base :Water, :target :Coffee}},
                                 :children #{}},
                                {:base :e7, :target :e2}
                                {:emaps    #{{:base :Vial, :target :Icecube}},
                                 :nogood   #{{:base :e12, :target :e2}},
                                 :children #{{:base :Vial, :target :Icecube}}},
                                {:base :e8, :target :e3}
                                {:emaps    #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}},
                                 :nogood
                                 #{{:base :e11, :target :e1}
                                   {:base :Water, :target :Coffee}
                                   {:base :e13, :target :e3}
                                   {:base :e12, :target :e2}},
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
                                {:emaps
                                 #{{:base :Water, :target :Heat}
                                   {:base :Beaker, :target :Coffee}
                                   {:base :Pipe, :target :Bar}
                                   {:base :Vial, :target :Icecube}},
                                 :nogood #{{:base :Water, :target :Coffee}},
                                 :children
                                 #{{:base :Water, :target :Heat}
                                   {:base :Beaker, :target :Coffee}
                                   {:base :Pipe, :target :Bar}
                                   {:base :Vial, :target :Icecube}}},
                                {:base :e6, :target :e1}
                                {:emaps    #{{:base :Beaker, :target :Coffee}},
                                 :nogood   #{{:base :e11, :target :e1} {:base :Water, :target :Coffee}},
                                 :children #{{:base :Beaker, :target :Coffee}}},
                                {:base :Pipe, :target :Bar}
                                {:emaps #{{:base :Pipe, :target :Bar}}, :nogood #{}, :children #{}},
                                {:base :e16, :target :e5}
                                {:emaps    #{{:base :Water, :target :Coffee}},
                                 :nogood   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                 :children #{{:base :Water, :target :Coffee}}},
                                {:base :Vial, :target :Icecube}
                                {:emaps #{{:base :Vial, :target :Icecube}}, :nogood #{}, :children #{}},
                                {:base :e13, :target :e3}
                                {:emaps    #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}},
                                 :nogood
                                 #{{:base :e7, :target :e2}
                                   {:base :e8, :target :e3}
                                   {:base :Water, :target :Coffee}
                                   {:base :e6, :target :e1}},
                                 :children #{{:base :e11, :target :e1} {:base :e12, :target :e2}}},
                                {:base :e12, :target :e2}
                                {:emaps    #{{:base :Vial, :target :Icecube}},
                                 :nogood   #{{:base :e7, :target :e2}},
                                 :children #{{:base :Vial, :target :Icecube}}}}})


(def expected-generated-inferences {:gmaps
                                    '({:mhs
                                       #{{:base :e11, :target :e1}
                                         {:base :Water, :target :Heat}
                                         {:base :Beaker, :target :Coffee}
                                         {:base :e9, :target :e0}
                                         {:base :Pipe, :target :Bar}
                                         {:base :Vial, :target :Icecube}
                                         {:base :e13, :target :e3}
                                         {:base :e12, :target :e2}},
                                       :structure
                                       {:roots #{{:base :e9, :target :e0} {:base :e13, :target :e3}},
                                        :nogood
                                        #{{:base :e7, :target :e2}
                                          {:base :e8, :target :e3}
                                          {:base :Water, :target :Coffee}
                                          {:base :e6, :target :e1}},
                                        :emaps
                                        #{{:base :Water, :target :Heat}
                                          {:base :Beaker, :target :Coffee}
                                          {:base :Pipe, :target :Bar}
                                          {:base :Vial, :target :Icecube}}},
                                       :score        18,
                                       :emap-matches 0,
                                       :inferences   #{}}
                                      {:mhs
                                       #{{:base :Water, :target :Heat}
                                         {:base :e7, :target :e2}
                                         {:base :e8, :target :e3}
                                         {:base :Beaker, :target :Coffee}
                                         {:base :e9, :target :e0}
                                         {:base :e6, :target :e1}
                                         {:base :Pipe, :target :Bar}
                                         {:base :Vial, :target :Icecube}},
                                       :structure
                                       {:roots #{{:base :e8, :target :e3} {:base :e9, :target :e0}},
                                        :nogood
                                        #{{:base :e11, :target :e1}
                                          {:base :Water, :target :Coffee}
                                          {:base :e13, :target :e3}
                                          {:base :e12, :target :e2}},
                                        :emaps
                                        #{{:base :Water, :target :Heat}
                                          {:base :Beaker, :target :Coffee}
                                          {:base :Pipe, :target :Bar}
                                          {:base :Vial, :target :Icecube}}},
                                       :score        18,
                                       :emap-matches 0,
                                       :inferences   #{}}
                                      {:mhs
                                       #{{:base :e15, :target :e4}
                                         {:base :Water, :target :Coffee}
                                         {:base :e16, :target :e5}},
                                       :structure
                                       {:roots  #{{:base :e15, :target :e4} {:base :e16, :target :e5}},
                                        :nogood #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                        :emaps  #{{:base :Water, :target :Coffee}}},
                                       :score        5,
                                       :emap-matches 0,
                                       :inferences   #{}}),
                                    :mh-structure
                                    {{:base :e11, :target :e1}
                                     {:emaps    #{{:base :Beaker, :target :Coffee}},
                                      :nogood   #{{:base :Water, :target :Coffee} {:base :e6, :target :e1}},
                                      :children #{{:base :Beaker, :target :Coffee}}},
                                     {:base :e15, :target :e4}
                                     {:emaps    #{{:base :Water, :target :Coffee}},
                                      :nogood   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                      :children #{{:base :Water, :target :Coffee}}},
                                     {:base :Water, :target :Heat}
                                     {:emaps    #{{:base :Water, :target :Heat}},
                                      :nogood   #{{:base :Water, :target :Coffee}},
                                      :children #{}},
                                     {:base :e7, :target :e2}
                                     {:emaps    #{{:base :Vial, :target :Icecube}},
                                      :nogood   #{{:base :e12, :target :e2}},
                                      :children #{{:base :Vial, :target :Icecube}}},
                                     {:base :e8, :target :e3}
                                     {:emaps    #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}},
                                      :nogood
                                      #{{:base :e11, :target :e1}
                                        {:base :Water, :target :Coffee}
                                        {:base :e13, :target :e3}
                                        {:base :e12, :target :e2}},
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
                                     {:emaps
                                      #{{:base :Water, :target :Heat}
                                        {:base :Beaker, :target :Coffee}
                                        {:base :Pipe, :target :Bar}
                                        {:base :Vial, :target :Icecube}},
                                      :nogood #{{:base :Water, :target :Coffee}},
                                      :children
                                      #{{:base :Water, :target :Heat}
                                        {:base :Beaker, :target :Coffee}
                                        {:base :Pipe, :target :Bar}
                                        {:base :Vial, :target :Icecube}}},
                                     {:base :e6, :target :e1}
                                     {:emaps    #{{:base :Beaker, :target :Coffee}},
                                      :nogood   #{{:base :e11, :target :e1} {:base :Water, :target :Coffee}},
                                      :children #{{:base :Beaker, :target :Coffee}}},
                                     {:base :Pipe, :target :Bar}
                                     {:emaps #{{:base :Pipe, :target :Bar}}, :nogood #{}, :children #{}},
                                     {:base :e16, :target :e5}
                                     {:emaps    #{{:base :Water, :target :Coffee}},
                                      :nogood   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                      :children #{{:base :Water, :target :Coffee}}},
                                     {:base :Vial, :target :Icecube}
                                     {:emaps #{{:base :Vial, :target :Icecube}}, :nogood #{}, :children #{}},
                                     {:base :e13, :target :e3}
                                     {:emaps    #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}},
                                      :nogood
                                      #{{:base :e7, :target :e2}
                                        {:base :e8, :target :e3}
                                        {:base :Water, :target :Coffee}
                                        {:base :e6, :target :e1}},
                                      :children #{{:base :e11, :target :e1} {:base :e12, :target :e2}}},
                                     {:base :e12, :target :e2}
                                     {:emaps    #{{:base :Vial, :target :Icecube}},
                                      :nogood   #{{:base :e7, :target :e2}},
                                      :children #{{:base :Vial, :target :Icecube}}}}})

(def expected-transferred ::fail)
(def expected-matches ::fail)

(defn remove-mapping [result]
  (update result :gmaps (partial map #(dissoc % :mapping))))

(deftest heat-water-test
  ;; Water flow is the base, heat flow the target

  (is (=
        expected-match-hypotheses
        (->> rules/literal-similarity
          (SUT/create-match-hypotheses kg simple-water-flow simple-heat-flow)
          (map #(into {} %))
          set)))

  (is (=
        expected-hypothesis-structure
        (SUT/build-hypothesis-structure kg expected-match-hypotheses)))

  (is (=
        expected-propagated-from-emaps
        (SUT/propagate-from-emaps expected-hypothesis-structure)))

  (is (=
        expected-computed-initial-gmaps
        (SUT/compute-initial-gmaps kg expected-propagated-from-emaps)))

  (is (=
        expected-combined-gmaps
        (SUT/combine-gmaps expected-computed-initial-gmaps)))

  (is (=
        expected-merged-gmaps
        (SUT/merge-gmaps expected-combined-gmaps)))


  (is (=
        '({:base :Water, :target :Water})
        (filter (fn [{:keys [base target]}] (SUT/emaps-equal?
                                             (get kg base)
                                             (get kg target)))
          '({:base :Water, :target :Water}
            {:base :Beaker, :target :Coffee}
            {:base :Pipe, :target :Bar}
            {:base :Vial, :target :Icecube}))))

  (is (=
        '({:base :Water, :target :Heat}
          {:base :Beaker, :target :Coffee}
          {:base :Pipe, :target :Bar}
          {:base :Vial, :target :Icecube})
        (filter (partial SUT/is-emap? kg) (-> expected-merged-gmaps :gmaps first :mhs))))

  ;;Fails for now
  #_(is (=
          ::fail
          (SUT/matching-emaps kg (-> expected-merged-gmaps :gmaps first))))

  (is (=
        expected-gmap-score
        (SUT/score-gmap kg expected-merged-gmaps (-> expected-merged-gmaps :gmaps first))))

  (is (=
        expected-finalized-gmaps
        (-> (SUT/finalize-gmaps kg simple-water-flow simple-heat-flow expected-merged-gmaps)
          remove-mapping)))

  (is (=
        expected-generated-inferences
        (SUT/generate-inferences kg simple-water-flow expected-finalized-gmaps)))

  (def expected-transferred-inferences ::fail)
  (is (=
        expected-transferred-inferences
        (SUT/transfer-inferences expected-generated-inferences)))

  ;; Should show the cause relation between the greater temperature
  ;; relation and the heat flow relation. This relation has been inferred
  ;; based on the analogical cause relation in the water flow graph.
  (is (= expected-transferred (-> (SUT/match kg rules/literal-similarity simple-water-flow simple-heat-flow)
                                :gmaps
                                first
                                :transferred)))

  (is (= expected-matches (SUT/match kg rules/literal-similarity simple-water-flow simple-heat-flow))))
