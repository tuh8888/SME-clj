(ns sme-clj.typedef-test
  (:require [clojure.test :refer [deftest is testing]]
            [mops.records :as mr]
            [sme-clj.typedef :as sut]
            [sme-clj.mops]
            [sme-clj.predicate-calculus]
            [taoensso.timbre :as log]
            [mops.core :as mops]))

(deftest make-concept-graph-test
  (log/with-level
   :warn
   (testing "Vector definition -> Predicate calculus"
    (is
     (=
      {:diameter-Vial {:id            :diameter-Vial
                       :type          ::sut/Expression
                       :functor       :diameter
                       :args          [:Vial]
                       :concept-graph :simple-water-flow}
       :flow-Beaker-Vial-Water-Pipe {:id            :flow-Beaker-Vial-Water-Pipe
                                     :type          ::sut/Expression
                                     :functor       :flow
                                     :args          [:Beaker :Vial :Water :Pipe]
                                     :concept-graph :simple-water-flow}
       :diameter-Beaker {:id            :diameter-Beaker
                         :type          ::sut/Expression
                         :functor       :diameter
                         :args          [:Beaker]
                         :concept-graph :simple-water-flow}
       :clear-Beaker {:id            :clear-Beaker
                      :type          ::sut/Expression
                      :functor       :clear
                      :args          [:Beaker]
                      :concept-graph :simple-water-flow}
       :pressure-Vial {:id            :pressure-Vial
                       :type          ::sut/Expression
                       :functor       :pressure
                       :args          [:Vial]
                       :concept-graph :simple-water-flow}
       :cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe
       {:id
        :cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe
        :type ::sut/Expression
        :functor :cause
        :args [:greater-pressure-Beaker-pressure-Vial
               :flow-Beaker-Vial-Water-Pipe]
        :concept-graph :simple-water-flow}
       :greater-pressure-Beaker-pressure-Vial
       {:id            :greater-pressure-Beaker-pressure-Vial
        :type          ::sut/Expression
        :functor       :greater
        :args          [:pressure-Beaker :pressure-Vial]
        :concept-graph :simple-water-flow}
       :liquid-Water {:id            :liquid-Water
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
       :pressure-Beaker {:id            :pressure-Beaker
                         :type          ::sut/Expression
                         :functor       :pressure
                         :args          [:Beaker]
                         :concept-graph :simple-water-flow}
       :flat-top-Water {:id            :flat-top-Water
                        :type          ::sut/Expression
                        :functor       :flat-top
                        :args          [:Water]
                        :concept-graph :simple-water-flow}
       :simple-water-flow {:id :simple-water-flow
                           :type ::sut/ConceptGraph
                           :spec
                           '([:cause
                              [:greater [:pressure :Beaker] [:pressure :Vial]]
                              [:flow :Beaker :Vial :Water :Pipe]]
                             [:greater [:diameter :Beaker] [:diameter :Vial]]
                             [:clear :Beaker]
                             [:flat-top :Water]
                             [:liquid :Water])}}
      (sut/add-concept-graph {}
                             :simple-water-flow
                             [:cause
                              [:greater [:pressure :Beaker] [:pressure :Vial]]
                              [:flow :Beaker :Vial :Water :Pipe]]
                             [:greater [:diameter :Beaker] [:diameter :Vial]]
                             [:clear :Beaker]
                             [:flat-top :Water]
                             [:liquid :Water]))))
   (testing "Vector definition -> MopMap"
    (is
     (=
      {:ids {}
       :mops
       {:diameter-Vial {:e1            {:Vial {}}
                        :parents       {::sut/Expression {}}
                        :functor       {:diameter {}}
                        :concept-graph {:simple-water-flow {}}}
        :flow-Beaker-Vial-Water-Pipe {:e1            {:Beaker {}}
                                      :concept-graph {:simple-water-flow {}}
                                      :e2            {:Vial {}}
                                      :e3            {:Water {}}
                                      :e4            {:Pipe {}}
                                      :parents       {::sut/Expression {}}
                                      :functor       {:flow {}}}
        :simple-water-flow {:parents {::sut/ConceptGraph {}}}
        :diameter-Beaker {:e1            {:Beaker {}}
                          :parents       {::sut/Expression {}}
                          :functor       {:diameter {}}
                          :concept-graph {:simple-water-flow {}}}
        :clear-Beaker {:e1            {:Beaker {}}
                       :parents       {::sut/Expression {}}
                       :functor       {:clear {}}
                       :concept-graph {:simple-water-flow {}}}
        :pressure-Vial {:e1            {:Vial {}}
                        :parents       {::sut/Expression {}}
                        :functor       {:pressure {}}
                        :concept-graph {:simple-water-flow {}}}
        :cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe
        {:e1            {:greater-pressure-Beaker-pressure-Vial {}}
         :e2            {:flow-Beaker-Vial-Water-Pipe {}}
         :parents       {::sut/Expression {}}
         :functor       {:cause {}}
         :concept-graph {:simple-water-flow {}}}
        :greater-pressure-Beaker-pressure-Vial {:e1 {:pressure-Beaker {}}
                                                :e2 {:pressure-Vial {}}
                                                :parents {::sut/Expression {}}
                                                :functor {:greater {}}
                                                :concept-graph
                                                {:simple-water-flow {}}}
        :liquid-Water {:e1            {:Water {}}
                       :parents       {::sut/Expression {}}
                       :functor       {:liquid {}}
                       :concept-graph {:simple-water-flow {}}}
        :greater-diameter-Beaker-diameter-Vial {:e1 {:diameter-Beaker {}}
                                                :e2 {:diameter-Vial {}}
                                                :parents {::sut/Expression {}}
                                                :functor {:greater {}}
                                                :concept-graph
                                                {:simple-water-flow {}}}
        :pressure-Beaker {:e1            {:Beaker {}}
                          :parents       {::sut/Expression {}}
                          :functor       {:pressure {}}
                          :concept-graph {:simple-water-flow {}}}
        :flat-top-Water {:e1            {:Water {}}
                         :parents       {::sut/Expression {}}
                         :functor       {:flat-top {}}
                         :concept-graph {:simple-water-flow {}}}}}
      (->
        (mr/make-mop-map)
        (sut/add-concept-graph :simple-water-flow
                               [:cause
                                [:greater [:pressure :Beaker] [:pressure :Vial]]
                                [:flow :Beaker :Vial :Water :Pipe]]
                               [:greater [:diameter :Beaker] [:diameter :Vial]]
                               [:clear :Beaker]
                               [:flat-top :Water]
                               [:liquid :Water])
        mr/remove-mopiness)))
    (is
     (=
      {:ids  {}
       :mops {:simple-heat-flow {:parents {::sut/ConceptGraph {}}}
              :liquid-Coffee {:e1            {:Coffee {}}
                              :parents       {::sut/Expression {}}
                              :functor       {:liquid {}}
                              :concept-graph {:simple-heat-flow {}}}
              :greater-temperature-Coffee-temperature-Icecube
              {:e1            {:temperature-Coffee {}}
               :e2            {:temperature-Icecube {}}
               :parents       {::sut/Expression {}}
               :functor       {:greater {}}
               :concept-graph {:simple-heat-flow {}}}
              :flat-top-Coffee {:e1            {:Coffee {}}
                                :parents       {::sut/Expression {}}
                                :functor       {:flat-top {}}
                                :concept-graph {:simple-heat-flow {}}}
              :temperature-Coffee {:e1            {:Coffee {}}
                                   :parents       {::sut/Expression {}}
                                   :functor       {:temperature {}}
                                   :concept-graph {:simple-heat-flow {}}}
              :temperature-Icecube {:e1            {:Icecube {}}
                                    :parents       {::sut/Expression {}}
                                    :functor       {:temperature {}}
                                    :concept-graph {:simple-heat-flow {}}}
              :flow-Coffee-Icecube-Heat-Bar {:e1            {:Coffee {}}
                                             :concept-graph {:simple-heat-flow
                                                             {}}
                                             :e2            {:Icecube {}}
                                             :e3            {:Heat {}}
                                             :e4            {:Bar {}}
                                             :parents       {::sut/Expression
                                                             {}}
                                             :functor       {:flow {}}}}}
      (-> (mr/make-mop-map)
          (sut/add-concept-graph
           :simple-heat-flow
           [:flow :Coffee :Icecube :Heat :Bar]
           [:greater [:temperature :Coffee] [:temperature :Icecube]]
           [:flat-top :Coffee]
           [:liquid :Coffee])
          mr/remove-mopiness))))))

(deftest add-entity-test
  (log/with-level :warn
                  (testing "Predicate calculus"
                   (is (= {:a {:id    :a
                               :type  ::sut/Entity
                               :slots nil}}
                          (sut/add-entity {} :a))))))

(deftest add-predicate-test
  (log/with-level
   :warn
   (testing "Predicate calculus"
    (is (= {:greater {:id       :greater
                      :type     ::sut/Relation
                      :arity    3
                      :ordered? true}}
           (sut/add-predicate {} [:greater :type ::sut/Relation :arity 3]))))))
