(ns sme-clj.typedef-test
  (:require [clojure.test :refer [deftest is testing]]
            [mop-records :as mr]
            [sme-clj.typedef :as sut]))

(defn map-vals
  [f m]
  (->> m
    (map (juxt key (comp f val)))
    (into {})))

(deftest make-concept-graph-test
  (testing "Vector definition -> SME"
    (is (= {:diameter-Vial     {:id            :diameter-Vial
                                :type          ::sut/Expression
                                :functor       :diameter
                                :args          [:Vial]
                                :concept-graph :simple-water-flow}
            :flow-Beaker-Vial-Water-Pipe
            {:id            :flow-Beaker-Vial-Water-Pipe
             :type          ::sut/Expression
             :functor       :flow
             :args          [:Beaker :Vial :Water :Pipe]
             :concept-graph :simple-water-flow}
            :diameter-Beaker   {:id            :diameter-Beaker
                                :type          ::sut/Expression
                                :functor       :diameter
                                :args          [:Beaker]
                                :concept-graph :simple-water-flow}
            :clear-Beaker      {:id            :clear-Beaker
                                :type          ::sut/Expression
                                :functor       :clear
                                :args          [:Beaker]
                                :concept-graph :simple-water-flow}
            :pressure-Vial     {:id            :pressure-Vial
                                :type          ::sut/Expression
                                :functor       :pressure
                                :args          [:Vial]
                                :concept-graph :simple-water-flow}
            :cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe
            {:id
             :cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe
             :type          ::sut/Expression
             :functor       :cause
             :args
             [:greater-pressure-Beaker-pressure-Vial
              :flow-Beaker-Vial-Water-Pipe]
             :concept-graph :simple-water-flow}
            :greater-pressure-Beaker-pressure-Vial
            {:id            :greater-pressure-Beaker-pressure-Vial
             :type          ::sut/Expression
             :functor       :greater
             :args          [:pressure-Beaker :pressure-Vial]
             :concept-graph :simple-water-flow}
            :liquid-Water      {:id            :liquid-Water
                                :type          ::sut/Expression
                                :functor       :liquid
                                :args          [:Water]
                                :concept-graph :simple-water-flow}
            :greater-diameter-Beaker-diameter-Vial
            {:id            :greater-diameter-Beaker-diameter-Vial
             :type          ::sut/Expression
             :functor       :greater
             :args          [:diameter-Beaker :diameter-Vial]
             :concept-graph :simple-water-flow}
            :pressure-Beaker   {:id            :pressure-Beaker
                                :type          ::sut/Expression
                                :functor       :pressure
                                :args          [:Beaker]
                                :concept-graph :simple-water-flow}
            :flat-top-Water    {:id            :flat-top-Water
                                :type          ::sut/Expression
                                :functor       :flat-top
                                :args          [:Water]
                                :concept-graph :simple-water-flow}
            :simple-water-flow {:id   :simple-water-flow
                                :type ::sut/ConceptGraph
                                :spec '([:cause
                                         [:greater [:pressure :Beaker] [:pressure :Vial]]
                                         [:flow :Beaker :Vial :Water :Pipe]]
                                        [:greater [:diameter :Beaker] [:diameter :Vial]]
                                        [:clear :Beaker]
                                        [:flat-top :Water]
                                        [:liquid :Water])}}
          (sut/add-concept-graph {} :simple-water-flow
            [:cause
             [:greater [:pressure :Beaker] [:pressure :Vial]]
             [:flow :Beaker :Vial :Water :Pipe]]
            [:greater [:diameter :Beaker] [:diameter :Vial]]
            [:clear :Beaker]
            [:flat-top :Water]
            [:liquid :Water]))))
  (testing "Vector definition -> MopMap"
    (is (= {:ids  {}
            :mops {:diameter-Vial               {:id            :diameter-Vial
                                                 :inst?         false
                                                 :e1            #{:Vial}
                                                 :names         #{}
                                                 :parents       #{::sut/Expression}
                                                 :functor       #{:diameter}
                                                 :concept-graph #{:simple-water-flow}}
                   :flow-Beaker-Vial-Water-Pipe {:id            :flow-Beaker-Vial-Water-Pipe
                                                 :e1            #{:Beaker}
                                                 :concept-graph #{:simple-water-flow}
                                                 :e2            #{:Vial}
                                                 :e3            #{:Water}
                                                 :e4            #{:Pipe}
                                                 :parents       #{::sut/Expression}
                                                 :functor       #{:flow}
                                                 :names         #{}
                                                 :inst?         false}
                   :simple-water-flow           {:id      :simple-water-flow
                                                 :parents #{::sut/ConceptGraph}
                                                 :inst?   false
                                                 :names   #{}}
                   :diameter-Beaker             {:id            :diameter-Beaker
                                                 :inst?         false
                                                 :e1            #{:Beaker}
                                                 :names         #{}
                                                 :parents       #{::sut/Expression}
                                                 :functor       #{:diameter}
                                                 :concept-graph #{:simple-water-flow}}
                   :clear-Beaker                {:id            :clear-Beaker
                                                 :inst?         false
                                                 :e1            #{:Beaker}
                                                 :names         #{}
                                                 :parents       #{::sut/Expression}
                                                 :functor       #{:clear}
                                                 :concept-graph #{:simple-water-flow}}
                   :pressure-Vial               {:id            :pressure-Vial
                                                 :inst?         false
                                                 :e1            #{:Vial}
                                                 :names         #{}
                                                 :parents       #{::sut/Expression}
                                                 :functor       #{:pressure}
                                                 :concept-graph #{:simple-water-flow}}
                   :cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe
                   {:id
                    :cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe
                    :inst?         false
                    :e1            #{:greater-pressure-Beaker-pressure-Vial}
                    :e2            #{:flow-Beaker-Vial-Water-Pipe}
                    :names         #{}
                    :parents       #{::sut/Expression}
                    :functor       #{:cause}
                    :concept-graph #{:simple-water-flow}}
                   :greater-pressure-Beaker-pressure-Vial
                   {:id            :greater-pressure-Beaker-pressure-Vial
                    :inst?         false
                    :e1            #{:pressure-Beaker}
                    :e2            #{:pressure-Vial}
                    :names         #{}
                    :parents       #{::sut/Expression}
                    :functor       #{:greater}
                    :concept-graph #{:simple-water-flow}}
                   :liquid-Water
                   {:id            :liquid-Water
                    :inst?         false
                    :e1            #{:Water}
                    :names         #{}
                    :parents       #{::sut/Expression}
                    :functor       #{:liquid}
                    :concept-graph #{:simple-water-flow}}

                   :greater-diameter-Beaker-diameter-Vial
                   {:id            :greater-diameter-Beaker-diameter-Vial
                    :inst?         false
                    :e1            #{:diameter-Beaker}
                    :e2            #{:diameter-Vial}
                    :names         #{}
                    :parents       #{::sut/Expression}
                    :functor       #{:greater}
                    :concept-graph #{:simple-water-flow}}
                   :pressure-Beaker
                   {:id            :pressure-Beaker
                    :inst?         false
                    :e1            #{:Beaker}
                    :names         #{}
                    :parents       #{::sut/Expression}
                    :functor       #{:pressure}
                    :concept-graph #{:simple-water-flow}}
                   :flat-top-Water
                   {:id            :flat-top-Water
                    :inst?         false
                    :e1            #{:Water}
                    :names         #{}
                    :parents       #{::sut/Expression}
                    :functor       #{:flat-top}
                    :concept-graph #{:simple-water-flow}}}}
          (-> (mr/make-mop-map)
            (sut/add-concept-graph :simple-water-flow
              [:cause
               [:greater [:pressure :Beaker] [:pressure :Vial]]
               [:flow :Beaker :Vial :Water :Pipe]]
              [:greater [:diameter :Beaker] [:diameter :Vial]]
              [:clear :Beaker]
              [:flat-top :Water]
              [:liquid :Water])
            (->> (into {}))
            (update :mops (partial map-vals (partial into {}))))))
    (is (= {:ids  {}
            :mops {:simple-heat-flow {:id      :simple-heat-flow
                                      :inst?   false
                                      :names   #{}
                                      :parents #{::sut/ConceptGraph}}
                   :liquid-Coffee    {:id            :liquid-Coffee
                                      :inst?         false
                                      :e1            #{:Coffee}
                                      :names         #{}
                                      :parents       #{::sut/Expression}
                                      :functor       #{:liquid}
                                      :concept-graph #{:simple-heat-flow}}
                   :greater-temperature-Coffee-temperature-Icecube
                   {:id            :greater-temperature-Coffee-temperature-Icecube
                    :inst?         false
                    :e1            #{:temperature-Coffee}
                    :e2            #{:temperature-Icecube}
                    :names         #{}
                    :parents       #{::sut/Expression}
                    :functor       #{:greater}
                    :concept-graph #{:simple-heat-flow}}
                   :flat-top-Coffee
                   {:id            :flat-top-Coffee
                    :inst?         false
                    :e1            #{:Coffee}
                    :names         #{}
                    :parents       #{::sut/Expression}
                    :functor       #{:flat-top}
                    :concept-graph #{:simple-heat-flow}}
                   :temperature-Coffee
                   {:id            :temperature-Coffee
                    :inst?         false
                    :e1            #{:Coffee}
                    :names         #{}
                    :parents       #{::sut/Expression}
                    :functor       #{:temperature}
                    :concept-graph #{:simple-heat-flow}}
                   :temperature-Icecube
                   {:id            :temperature-Icecube
                    :inst?         false
                    :e1            #{:Icecube}
                    :names         #{}
                    :parents       #{::sut/Expression}
                    :functor       #{:temperature}
                    :concept-graph #{:simple-heat-flow}}
                   :flow-Coffee-Icecube-Heat-Bar
                   {:id            :flow-Coffee-Icecube-Heat-Bar
                    :e1            #{:Coffee}
                    :concept-graph #{:simple-heat-flow}
                    :e2            #{:Icecube}
                    :e3            #{:Heat}
                    :e4            #{:Bar}
                    :parents       #{::sut/Expression}
                    :functor       #{:flow}
                    :names         #{}
                    :inst?         false}}}
          (-> (mr/make-mop-map)
            (sut/add-concept-graph :simple-heat-flow
              [:flow :Coffee :Icecube :Heat :Bar]
              [:greater [:temperature :Coffee] [:temperature :Icecube]]
              [:flat-top :Coffee]
              [:liquid :Coffee])
            (->> (into {}))
            (update :mops (partial map-vals (partial into {}))))))))

(deftest add-entity-test
  (testing "SME"
    (is (= {:a {:id    :a
                :type  ::sut/Entity
                :slots nil}}
          (sut/add-entity {} [:a])))))

(deftest add-predicate-test
  (testing "SME"
    (is (= {:greater {:id       :greater
                      :type     ::sut/Relation
                      :arity    3
                      :ordered? true}}
          (sut/add-predicate {} [:greater :type ::sut/Relation :arity 3])))))
