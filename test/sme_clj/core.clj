(ns sme-clj.core-test
  (:require [clojure.test  :refer :all]
            [sme-clj.core :refer  :all]
            [sme-clj.util :as util]
            [sme-clj.ruledef :as rules]
            [sme-clj.typedef :as types]
            [clojure.set :as set]
            [clojure.pprint :as pp]))

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
(pp/pprint (sort-by :base (seq expected-match-hypotheses)))
(pp/pprint (sort-by :base (seq (create-match-hypotheses kg simple-water-flow simple-heat-flow rules/literal-similarity))))
(def expected-hypothesis-structure [nil])
(set/difference expected-match-hypotheses)
(deftest heat-water
  (is (=
        expected-match-hypotheses
        (->> rules/literal-similarity
          (create-match-hypotheses kg simple-water-flow simple-heat-flow)
          (map #(into {} %))
          set)))
  (is (= expected-hypothesis-structure (build-hypothesis-structure kg expected-match-hypotheses))))
