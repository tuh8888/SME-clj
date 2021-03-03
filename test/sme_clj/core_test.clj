(ns sme-clj.core-test
  (:require [clojure.test :refer [testing deftest is]]
            [sme-clj.core :as sut]
            [sme-clj.ruledef :as rules]
            [sme-clj.simple-water-heat :refer [kg mops-kg]]
            [mops :as mops]
            [sme-clj.typedef :as types]))

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

(deftest get-concept-graph-expressions-test
  (testing "SME"
    (is (= expected-concept-graph-expressions
          (into #{}
            (lazy-cat
              (sut/get-concept-graph-expressions kg :simple-water-flow)
              (sut/get-concept-graph-expressions kg :simple-heat-flow))))))

  (testing "Mops"
    (is (= expected-concept-graph-expressions
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

(deftest create-match-hypotheses-test
  ;; Water flow is the base heat flow the target
  (testing "SME"
    (is (= expected-match-hypotheses
            (sut/create-match-hypotheses kg :simple-water-flow :simple-heat-flow rules/literal-similarity))))
  (testing "Mops"
    (is (= expected-match-hypotheses
            (sut/create-match-hypotheses mops-kg :simple-water-flow :simple-heat-flow rules/literal-similarity)))))

(deftest find-children-test
  (let [expected-found-children1 #{}
        expected-found-children2 #{[:Beaker :Coffee] [:Water :Heat] [:Pipe :Bar] [:Vial :Icecube]}  ]
    (testing "SME"
      (is (= expected-found-children1
              (set (sut/find-children kg expected-match-hypotheses [:Beaker :Coffee]))))
      (is (= expected-found-children2
              (set (sut/find-children kg expected-match-hypotheses [:flow-Beaker-Vial-Water-Pipe :flow-Coffee-Icecube-Heat-Bar])))))
    (testing "Mops"
      (is (= expected-found-children1
              (set (sut/find-children mops-kg expected-match-hypotheses [:Beaker :Coffee]))))
      (is (= expected-found-children2
              (set (sut/find-children mops-kg expected-match-hypotheses [:flow-Beaker-Vial-Water-Pipe :flow-Coffee-Icecube-Heat-Bar])))))))

(def expected-found-roots #{[:flow-Beaker-Vial-Water-Pipe :flow-Coffee-Icecube-Heat-Bar]
                            [:greater-diameter-Beaker-diameter-Vial :greater-temperature-Coffee-temperature-Icecube]
                            [:greater-pressure-Beaker-pressure-Vial :greater-temperature-Coffee-temperature-Icecube]
                            [:liquid-Water :liquid-Coffee]
                            [:flat-top-Water :flat-top-Coffee]})
(deftest find-roots-test
  (testing "SME"
    (is (= expected-found-roots
            (set (sut/find-roots kg expected-match-hypotheses)))))
  (testing "Mops"
    (is (= expected-found-roots
            (set (sut/find-roots mops-kg expected-match-hypotheses))))))

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

(deftest split-into-mhs-sets-test
  (testing "SME"
    (is (= expected-computed-initial-gmaps
            (sut/split-into-mhs-sets kg expected-match-hypotheses))))

  (testing "Mops"
    (is (= expected-computed-initial-gmaps
            (sut/split-into-mhs-sets mops-kg expected-match-hypotheses)))))

(def expected-combined-gmaps [[pressure-gmap flow-gmap]
                              [flow-gmap diameter-gmap]
                              [flat-top-gmap liquid-gmap]])

(deftest concistent-combs-of-mhs-sets-test
  (testing "SME and Mops"
    (is (= expected-combined-gmaps
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

(deftest merge-mhs-sets-test
  (testing "SME"
    (is (= expected-merged-gmaps
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

(deftest finalize-gmaps-tets
  (testing "SME"
    (is (= expected-finalized-gmaps
            (sut/finalize-gmaps kg :simple-water-flow :simple-heat-flow expected-match-hypotheses expected-merged-gmaps))))

  (testing "Mops"
    (is (= expected-finalized-gmaps
            (sut/finalize-gmaps mops-kg :simple-water-flow :simple-heat-flow expected-match-hypotheses expected-merged-gmaps)))))

(def expected-generated-inferences [#{:cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe}
                                    #{:cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe
                                      :pressure-Vial
                                      :pressure-Beaker
                                      :greater-pressure-Beaker-pressure-Vial}
                                    #{}])

(deftest generate-inferences-test
  (testing "SME"
    (is (= expected-generated-inferences
            (->> expected-finalized-gmaps
              (sut/generate-inferences kg :simple-water-flow)
              (map :inferences)))))

  (testing "Mops"
    (is (= expected-generated-inferences
            (->> expected-finalized-gmaps
              (sut/generate-inferences mops-kg :simple-water-flow)
              (map :inferences))))))

(def expected-transferred-inferences [#{[:cause :greater-temperature-Coffee-temperature-Icecube :flow-Coffee-Icecube-Heat-Bar]}
                                      #{[:greater [:pressure :Coffee] [:pressure :Icecube]]
                                        [:pressure :Icecube]
                                        [:pressure :Coffee]
                                        [:cause [:greater [:pressure :Coffee] [:pressure :Icecube]]
                                         :flow-Coffee-Icecube-Heat-Bar]}
                                      #{}])

(deftest transfer-inferences-test
  (testing "SME"
    (is (= expected-transferred-inferences
          (->> expected-generated-inferences
            (map #(assoc %1 :inferences %2) expected-finalized-gmaps)
            (sut/transfer-inferences kg)
            (map :transferred)))))

  (testing "Mops"
    (is (= expected-transferred-inferences
          (->> expected-generated-inferences
            (map #(assoc %1 :inferences %2) expected-finalized-gmaps)
            (sut/transfer-inferences mops-kg)
            (map :transferred))))))

(deftest perform-inference-test
  (testing "SME"
    (is (= expected-transferred-inferences
          (->> expected-finalized-gmaps
            (sut/perform-inference kg :simple-water-flow)
            (map :transferred)))))

  (testing "Mops"
    (is (= expected-transferred-inferences
          (->> expected-finalized-gmaps
            (sut/perform-inference mops-kg :simple-water-flow)
            (map :transferred))))))

(deftest match-test
  (testing "SME"
    (is (= expected-finalized-gmaps
          (sut/match kg :simple-water-flow :simple-heat-flow)))
    (is (= expected-finalized-gmaps
          (sut/match kg :simple-water-flow :simple-heat-flow rules/literal-similarity))))

  (testing "Mops"
    (is (= expected-finalized-gmaps
          (sut/match mops-kg :simple-water-flow :simple-heat-flow)))))

                                        ; LocalWords:  gmaps
