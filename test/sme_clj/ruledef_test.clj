(ns sme-clj.ruledef-test
  (:require [clojure.test :as t]
            [sme-clj.ruledef :as sut]
            [sme-clj.simple-water-heat :refer [kg mops-kg]]))

(def expected-same-functor-matches
  #{[:diameter-Vial :diameter-Vial]
    [:diameter-Vial :diameter-Beaker]
    [:flow-Beaker-Vial-Water-Pipe :flow-Beaker-Vial-Water-Pipe]
    [:flow-Beaker-Vial-Water-Pipe :flow-Coffee-Icecube-Heat-Bar]
    [:diameter-Beaker :diameter-Vial]
    [:diameter-Beaker :diameter-Beaker]
    [:clear-Beaker :clear-Beaker]
    [:liquid-Coffee :liquid-Coffee]
    [:liquid-Coffee :liquid-Water]
    [:pressure-Vial :pressure-Vial]
    [:pressure-Vial :pressure-Beaker]
    [:cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe
     :cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe]
    [:greater-temperature-Coffee-temperature-Icecube
     :greater-temperature-Coffee-temperature-Icecube]
    [:greater-temperature-Coffee-temperature-Icecube
     :greater-pressure-Beaker-pressure-Vial]
    [:greater-temperature-Coffee-temperature-Icecube
     :greater-diameter-Beaker-diameter-Vial]
    [:flat-top-Coffee :flat-top-Coffee]
    [:flat-top-Coffee :flat-top-Water]
    [:greater-pressure-Beaker-pressure-Vial
     :greater-temperature-Coffee-temperature-Icecube]
    [:greater-pressure-Beaker-pressure-Vial
     :greater-pressure-Beaker-pressure-Vial]
    [:greater-pressure-Beaker-pressure-Vial
     :greater-diameter-Beaker-diameter-Vial]
    [:temperature-Coffee :temperature-Coffee]
    [:temperature-Coffee :temperature-Icecube]
    [:liquid-Water :liquid-Coffee]
    [:liquid-Water :liquid-Water]
    [:temperature-Icecube :temperature-Coffee]
    [:temperature-Icecube :temperature-Icecube]
    [:greater-diameter-Beaker-diameter-Vial
     :greater-temperature-Coffee-temperature-Icecube]
    [:greater-diameter-Beaker-diameter-Vial
     :greater-pressure-Beaker-pressure-Vial]
    [:greater-diameter-Beaker-diameter-Vial
     :greater-diameter-Beaker-diameter-Vial]
    [:pressure-Beaker :pressure-Vial]
    [:pressure-Beaker :pressure-Beaker]
    [:flat-top-Water :flat-top-Coffee]
    [:flat-top-Water :flat-top-Water]
    [:flow-Coffee-Icecube-Heat-Bar :flow-Beaker-Vial-Water-Pipe]
    [:flow-Coffee-Icecube-Heat-Bar :flow-Coffee-Icecube-Heat-Bar]})

(def expected-compatible-args-matches #{[:temperature-Icecube :temperature-Icecube]
                                        [:diameter-Beaker :diameter-Beaker]
                                        [:pressure-Beaker :diameter-Beaker]
                                        [:Beaker :Beaker]
                                        [:Pipe :Pipe]
                                        [:temperature-Coffee :temperature-Coffee]
                                        [:Water :Coffee]
                                        [:Coffee :Water]
                                        [:Icecube :Vial]
                                        [:Icecube :Water]
                                        [:temperature-Coffee :diameter-Beaker]
                                        [:Icecube :Beaker]
                                        [:Coffee :Vial]
                                        [:Beaker :Icecube]
                                        [:Beaker :Coffee]
                                        [:Water :Icecube]
                                        [:temperature-Coffee :pressure-Beaker]
                                        [:Icecube :Icecube]
                                        [:Vial :Beaker]
                                        [:diameter-Vial :temperature-Icecube]
                                        [:Beaker :Water]
                                        [:Vial :Coffee]
                                        [:temperature-Icecube :diameter-Vial]
                                        [:Vial :Water]
                                        [:Water :Vial]
                                        [:diameter-Vial :diameter-Vial]
                                        [:Coffee :Coffee]
                                        [:Water :Heat]
                                        [:Vial :Vial]
                                        [:pressure-Beaker :temperature-Coffee]
                                        [:diameter-Beaker :pressure-Beaker]
                                        [:Water :Beaker]
                                        [:Pipe :Bar]
                                        [:diameter-Vial :pressure-Vial]
                                        [:diameter-Beaker :temperature-Coffee]
                                        [:temperature-Icecube :pressure-Vial]
                                        [:Heat :Heat]
                                        [:Beaker :Vial]
                                        [:Coffee :Beaker]
                                        [:Water :Water]
                                        [:pressure-Beaker :pressure-Beaker]
                                        [:Vial :Icecube]
                                        [:pressure-Vial :diameter-Vial]
                                        [:Heat :Water]
                                        [:Bar :Pipe]
                                        [:pressure-Vial :temperature-Icecube]
                                        [:Icecube :Coffee]
                                        [:Coffee :Icecube]
                                        [:pressure-Vial :pressure-Vial]
                                        [:Bar :Bar]})

(def expected-commutative-args #{})

(t/deftest literal-similarity-test

  (let [mhs              (for [b (keys kg)
                               t (keys kg)]
                           [b t])
        wrapped-rule     (fn [rules rule-id kg mhs]
                           (->> mhs
                             (mapcat (partial sut/apply-rule kg (rules rule-id)))
                             (remove nil?)
                             set))
        wrapped-rule-lit (partial wrapped-rule sut/literal-similarity)]
    (t/testing "Same functor"
      (t/testing "SME"
        (t/is (= expected-same-functor-matches
                (wrapped-rule-lit :same-functor kg mhs))))
      (t/testing "Mops"
        (t/is (= expected-same-functor-matches
                (wrapped-rule-lit :same-functor mops-kg mhs)))))

    (t/testing "Compatible args"
      (t/testing "SME"
        (t/is (= expected-compatible-args-matches
                (wrapped-rule-lit :compatible-args kg mhs))))
      (t/testing "Mops"
        (t/is (= expected-compatible-args-matches
                (wrapped-rule-lit :compatible-args mops-kg mhs)))))

    (t/testing "Commutative args"
      (t/testing "SME"
        (t/is (= expected-commutative-args
                (wrapped-rule-lit :commutative-args kg mhs))))
      (t/testing "Mops"))))
