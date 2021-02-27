(ns sme-clj.core-test
  (:require [clojure.test :as t]
            [sme-clj.core :as sut]
            [sme-clj.ruledef :as rules]
            [sme-clj.simple-water-heat :refer [kg mops-kg]]))

(def expected-concept-graph-expressions
  #{:diameter-Vial
    :flow-Beaker-Vial-Water-Pipe
    :diameter-Beaker
    :clear-Beaker
    :liquid-Coffee
    :pressure-Vial
    :cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe
    :greater-temperature-Coffee-temperature-Icecube
    :flat-top-Coffee
    :greater-pressure-Beaker-pressure-Vial
    :temperature-Coffee
    :liquid-Water
    :temperature-Icecube
    :greater-diameter-Beaker-diameter-Vial
    :pressure-Beaker
    :flat-top-Water
    :flow-Coffee-Icecube-Heat-Bar})

(t/deftest get-concept-graph-expressions-test
  (t/testing "SME"
    (t/is (= expected-concept-graph-expressions
            (into #{}
              (lazy-cat
                (sut/get-concept-graph-expressions kg :simple-water-flow)
                (sut/get-concept-graph-expressions kg :simple-heat-flow))))))

  (t/testing "Mops"
    (t/is (= expected-concept-graph-expressions
            (into #{}
              (lazy-cat
                (sut/get-concept-graph-expressions mops-kg :simple-heat-flow)
                (sut/get-concept-graph-expressions mops-kg :simple-water-flow)))))))

(def expected-match-hypotheses #{[:Water :Heat]
                                 [:Water :Coffee]
                                 [:flat-top-Water :flat-top-Coffee]
                                 [:liquid-Water :liquid-Coffee]
                                 [:greater-diameter-Beaker-diameter-Vial :greater-temperature-Coffee-temperature-Icecube]
                                 [:diameter-Beaker :temperature-Coffee]
                                 [:diameter-Vial :temperature-Icecube]
                                 [:flow-Beaker-Vial-Water-Pipe :flow-Coffee-Icecube-Heat-Bar]
                                 [:greater-pressure-Beaker-pressure-Vial :greater-temperature-Coffee-temperature-Icecube]
                                 [:Beaker :Coffee]
                                 [:Pipe :Bar]
                                 [:pressure-Beaker :temperature-Coffee]
                                 [:pressure-Vial :temperature-Icecube]
                                 [:Vial :Icecube]})

(t/deftest create-match-hypotheses-test
  ;; Water flow is the base heat flow the target
  (t/testing "SME"
    (t/is (= expected-match-hypotheses
            (sut/create-match-hypotheses kg :simple-water-flow :simple-heat-flow rules/literal-similarity))))
  (t/testing "Mops"
    (t/is (= expected-match-hypotheses
            (sut/create-match-hypotheses mops-kg :simple-water-flow :simple-heat-flow rules/mops-literal-similarity)))))

(t/deftest find-roots-test
  (t/testing "SME"
    (t/is (= #{[:flow-Beaker-Vial-Water-Pipe :flow-Coffee-Icecube-Heat-Bar]
               [:greater-diameter-Beaker-diameter-Vial :greater-temperature-Coffee-temperature-Icecube]
               [:greater-pressure-Beaker-pressure-Vial :greater-temperature-Coffee-temperature-Icecube]
               [:liquid-Water :liquid-Coffee]
               [:flat-top-Water :flat-top-Coffee]}
            (set (sut/find-roots kg expected-match-hypotheses))))))

(def diameter-gmap #{[:greater-diameter-Beaker-diameter-Vial
                      :greater-temperature-Coffee-temperature-Icecube]
                     [:diameter-Beaker :temperature-Coffee]
                     [:diameter-Vial :temperature-Icecube]
                     [:Beaker :Coffee]
                     [:Vial :Icecube]})
(def flow-gmap #{[:Water :Heat]
                 [:Beaker :Coffee]
                 [:Pipe :Bar]
                 [:flow-Beaker-Vial-Water-Pipe :flow-Coffee-Icecube-Heat-Bar]
                 [:Vial :Icecube]})

(def pressure-gmap #{[:Beaker :Coffee]
                     [:greater-pressure-Beaker-pressure-Vial
                      :greater-temperature-Coffee-temperature-Icecube]
                     [:Vial :Icecube]
                     [:pressure-Beaker :temperature-Coffee]
                     [:pressure-Vial :temperature-Icecube]})

(def liquid-gmap #{[:Water :Coffee]
                   [:liquid-Water :liquid-Coffee]})
(def flat-top-gmap #{[:Water :Coffee]
                     [:flat-top-Water :flat-top-Coffee]})

(def expected-computed-initial-gmaps [pressure-gmap
                                      flow-gmap
                                      flat-top-gmap
                                      liquid-gmap
                                      diameter-gmap])

(t/deftest split-into-mhs-sets-test
  (t/testing "SME"
    (t/is (= expected-computed-initial-gmaps
            (sut/split-into-mhs-sets kg expected-match-hypotheses)))))

(def expected-combined-gmaps [[pressure-gmap flow-gmap]
                              [flow-gmap diameter-gmap]
                              [flat-top-gmap liquid-gmap]])

(t/deftest concistent-combs-of-mhs-sets-test
  (t/testing "SME"
    (t/is (= expected-combined-gmaps
            (sut/consistent-combs-of-mhs-sets expected-match-hypotheses expected-computed-initial-gmaps)))))

(def expected-merged-gmaps
  [#{[:Beaker :Coffee]
     [:Water :Heat]
     [:pressure-Beaker :temperature-Coffee]
     [:Pipe :Bar]
     [:greater-pressure-Beaker-pressure-Vial
      :greater-temperature-Coffee-temperature-Icecube]
     [:flow-Beaker-Vial-Water-Pipe :flow-Coffee-Icecube-Heat-Bar]
     [:Vial :Icecube]
     [:pressure-Vial :temperature-Icecube]}
   #{[:Beaker :Coffee]
     [:diameter-Vial :temperature-Icecube]
     [:Water :Heat]
     [:Pipe :Bar]
     [:diameter-Beaker :temperature-Coffee]
     [:greater-diameter-Beaker-diameter-Vial
      :greater-temperature-Coffee-temperature-Icecube]
     [:flow-Beaker-Vial-Water-Pipe :flow-Coffee-Icecube-Heat-Bar]
     [:Vial :Icecube]}
   #{[:Water :Coffee]
     [:liquid-Water :liquid-Coffee]
     [:flat-top-Water :flat-top-Coffee]}])

(t/deftest merge-mhs-sets-test
  (t/testing "SME"
    (t/is (= expected-merged-gmaps
            (sut/merge-mhs-sets expected-combined-gmaps)))))

(def expected-finalized-gmaps (map (fn [mhs score]
                                     (assoc score
                                       :mhs  mhs))
                                expected-merged-gmaps
                                [{:score        18
                                  :emap-matches 0
                                  :mapping      {:base :simple-water-flow :target :simple-heat-flow}}
                                 {:score        18
                                  :emap-matches 0
                                  :mapping      {:base :simple-water-flow :target :simple-heat-flow}}
                                 {:score        5
                                  :emap-matches 0
                                  :mapping      {:base :simple-water-flow :target :simple-heat-flow}}]))

(t/deftest finalize-gmaps-tets
  (t/testing "SME"
    (t/is (= expected-finalized-gmaps
            (sut/finalize-gmaps kg :simple-water-flow :simple-heat-flow expected-match-hypotheses expected-merged-gmaps)))))

(def expected-generated-inferences [#{:cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe}
                                    #{:cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe
                                      :pressure-Vial
                                      :pressure-Beaker
                                      :greater-pressure-Beaker-pressure-Vial}
                                    #{}])

(t/deftest generate-inferences-test
  (t/testing "SME"
    (t/is (= expected-generated-inferences
            (->> expected-finalized-gmaps
              (sut/generate-inferences kg :simple-water-flow)
              (map :inferences))))))

(def expected-transferred-inferences [#{[:cause :greater-temperature-Coffee-temperature-Icecube :flow-Coffee-Icecube-Heat-Bar]}
                                      #{[:greater [:pressure :Coffee] [:pressure :Icecube]]
                                        [:pressure :Icecube]
                                        [:pressure :Coffee]
                                        [:cause [:greater [:pressure :Coffee] [:pressure :Icecube]]
                                         :flow-Coffee-Icecube-Heat-Bar]}
                                      #{}])

(t/deftest transfer-inferences-test
  (t/testing "SME"
    (t/is (= expected-transferred-inferences
            (->> expected-generated-inferences
              (map #(assoc %1 :inferences %2) expected-finalized-gmaps)
              (sut/transfer-inferences kg)
              (map :transferred))))))

(t/deftest match-test
  (t/testing "SME"
    (t/is (= expected-finalized-gmaps
            (sut/match kg :simple-water-flow :simple-heat-flow)))))

                                        ; LocalWords:  gmaps
