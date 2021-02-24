(ns sme-clj.core-test
  (:require [sme-clj.test-util :refer [undiff]]
            [clojure.test :refer [deftest testing is]]
            [mop-records :as mr]
            [mops :as mops]
            [sme-clj.core :as SUT]
            [sme-clj.ruledef :as rules]
            [sme-clj.typedef :as types]
            [sme-clj.util :as util]))

#_(= new-var (apply undiff old-var (take 2 (data/diff new-var old-var))))

(def predicates [(types/make-predicate :flow :type :relation :arity 4)
                 (types/make-predicate :greater :type :relation :arity 2)
                 (types/make-predicate :cause :type :relation :arity 2)
                 (types/make-predicate :temperature :type :function)
                 (types/make-predicate :flat-top :type :function)
                 (types/make-predicate :pressure :type :function)
                 (types/make-predicate :diameter :type :function)
                 (types/make-predicate :liquid :type :attribute)
                 (types/make-predicate :clear :type :attribute)])

(def entities [(types/make-entity :Coffee)
               (types/make-entity :Icecube)
               (types/make-entity :Bar)
               (types/make-entity :Heat)

               (types/make-entity :Water)
               (types/make-entity :Beaker)
               (types/make-entity :Vial)
               (types/make-entity :Pipe)])

;; Concept graph definitions
(def simple-water-flow (types/make-concept-graph :simple-water-flow
                         [:cause
                          [:greater [:pressure :Beaker] [:pressure :Vial]]
                          [:flow :Beaker :Vial :Water :Pipe]]
                         [:greater [:diameter :Beaker] [:diameter :Vial]]
                         [:clear :Beaker]
                         [:flat-top :Water]
                         [:liquid :Water]))

(def simple-heat-flow (types/make-concept-graph :simple-heat-flow
                        [:flow :Coffee :Icecube :Heat :Bar]
                        [:greater [:temperature :Coffee] [:temperature :Icecube]]
                        [:flat-top :Coffee]
                        [:liquid :Coffee]))


(defn make-mop
  ([m id slots]
   (mops/add-mop m (mops/->mop id slots)))
  ([m [parent & slots]]
   (mops/add-mop m (mops/->mop (types/combine-ids (-> slots
                                                    (->> (map second))
                                                    (conj parent)))
                     (into {} slots)))))

(deftest mop-representation
  (let [kg                (-> (mr/make-mop-map)
                            (make-mop [:cause       {:upstream   :Expression
                                                     :downstream :Expression
                                                     :parents    #{:Expression}}])
                            (make-mop :greater     {:less    :Expression
                                                    :more    :Expression
                                                    :parents #{:Expression}})
                            (make-mop :flow        {:from    :Entity
                                                    :to      :Entity
                                                    :flow-er :Entity
                                                    :via     :Entity
                                                    :parents #{:Expression}})
                            (make-mop :pressure    {:container :Entity})
                            (make-mop :temperature {:thermal-entity :Entity
                                                    :parents        #{:Expression}})
                            (make-mop :flat-top    {:parents    #{:Expression}
                                                    :surface-of :Entity})
                            (make-mop :liquid      {:parents #{:Expression}
                                                    :entity  :Entity})
                            (make-mop :Coffee      {:parents #{:Entity}})
                            (make-mop :Water       {:parents #{:Entity}})
                            (make-mop :Heat        {:parents #{:Entity}})
                            (make-mop :Pipe        {:parents #{:Entity}})
                            (make-mop :Vial        {:parents #{:Entity}})
                            (make-mop :Icecube     {:parents #{:Entity}})
                            (make-mop :Bar         {:parents #{:Entity}})
                            (make-mop :Beaker      {:parents #{:Entity}}))
        simple-water-flow (-> (mr/make-mop-map)
                            (make-mop [:flat-top [:surface-of :Water]])
                            (make-mop [:liquid [:entity  :Water]])
                            (make-mop [:cause
                                       [:upstream :greater-pressure-Beaker-pressure-Vial]
                                       [:downstream :flow-Beaker-Vial-Water-Pipe]])
                            (make-mop [:greater
                                       [:more :pressure-Beaker]
                                       [:less :pressure-Vial]])
                            (make-mop [:pressure [:container :Beaker]])
                            (make-mop [:pressure [:container :Vial]])
                            (make-mop [:flow
                                       [:from    :Beaker]
                                       [:to      :Vial]
                                       [:flow-er :Water]
                                       [:via     :Pipe]]))
        simple-heat-flow  (-> (mr/make-mop-map)
                            (make-mop [:flow
                                       [:from    :Coffee]
                                       [:to      :Icecube]
                                       [:flow-er :Heat]
                                       [:via     :Bar]])
                            (make-mop [:greater
                                       [:more    :temperature-Coffee]
                                       [:less    :temperature-Icecube]])
                            (make-mop [:termerature [:thermal-entity :Coffee]])
                            (make-mop [:temperature [:thermal-entity :Icecube]])
                            (make-mop [:flat-top [:surface-of :Coffee]])
                            (make-mop [:liquid [:entity  :Coffee]]))
        full-kg           (update kg :mops merge (:mops simple-heat-flow) (:mops simple-water-flow))]


    (testing "Creating match hypotheses"
      (is (= expected-match-hypotheses
            (SUT/create-match-hypotheses full-kg (-> simple-water-flow :mops keys) (-> simple-heat-flow :mops keys) rules/literal-similarity))))))

(def kg (merge-with (fn [v1 v2]
                      {:help (vector v1 v2)})
          (util/vals-as-keys :name entities)
          (util/vals-as-keys :name predicates)
          (:graph simple-heat-flow)
          (:graph simple-water-flow)))

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
(def expected-computed-initial-gmaps
  [pressure-gmap
   flow-gmap
   flat-top-gmap
   liquid-gmap
   diameter-gmap])

(def expected-combined-gmaps
  [[pressure-gmap flow-gmap]
   [flow-gmap diameter-gmap]
   [flat-top-gmap liquid-gmap]])

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

(def expected-finalized-gmaps (map
                                (fn [mhs score]
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

(def expected-generated-inferences [#{:cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe}
                                    #{:cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe
                                      :pressure-Vial
                                      :pressure-Beaker
                                      :greater-pressure-Beaker-pressure-Vial}
                                    #{}])

(def expected-transferred-inferences [#{[:cause :greater-temperature-Coffee-temperature-Icecube :flow-Coffee-Icecube-Heat-Bar]}
                                      #{[:greater [:pressure :Coffee] [:pressure :Icecube]]
                                        [:pressure :Icecube]
                                        [:pressure :Coffee]
                                        [:cause [:greater [:pressure :Coffee] [:pressure :Icecube]]
                                         :flow-Coffee-Icecube-Heat-Bar]}
                                      #{}])

(deftest heat-water-test
  ;; Water flow is the base heat flow the target

  (testing "Creating match hypotheses"
    (is (= expected-match-hypotheses
          (SUT/create-match-hypotheses kg (-> simple-water-flow :graph keys) (-> simple-heat-flow :graph keys) rules/literal-similarity))))

  (testing "Computing initial gmaps"
    (is (= #{[:flow-Beaker-Vial-Water-Pipe :flow-Coffee-Icecube-Heat-Bar]
             [:greater-diameter-Beaker-diameter-Vial :greater-temperature-Coffee-temperature-Icecube]
             [:greater-pressure-Beaker-pressure-Vial :greater-temperature-Coffee-temperature-Icecube]
             [:liquid-Water :liquid-Coffee]
             [:flat-top-Water :flat-top-Coffee]}
          (set (SUT/find-roots kg expected-match-hypotheses))))

    (is (= expected-computed-initial-gmaps
          (SUT/split-into-mhs-sets kg expected-match-hypotheses))))

  (testing "Combining gmaps"
    (is (= expected-combined-gmaps
          (SUT/consistent-combs-of-mhs-sets expected-match-hypotheses expected-computed-initial-gmaps))))

  (testing "Merging gmaps"
    (is (= expected-merged-gmaps
          (SUT/merge-mhs-sets expected-combined-gmaps))))

  (testing "Finalizing gmaps"
    (is (= expected-finalized-gmaps
          (SUT/finalize-gmaps kg simple-water-flow simple-heat-flow expected-match-hypotheses expected-merged-gmaps))))

  (testing "Generating inferences"
    (is (= expected-generated-inferences
          (->> expected-finalized-gmaps
            (SUT/generate-inferences kg simple-water-flow)
            (map :inferences)))))

  (testing "Transferring inferences"
    (is (= expected-transferred-inferences
          (->> expected-generated-inferences
            (map #(assoc %1 :inferences %2) expected-finalized-gmaps)
            (SUT/transfer-inferences kg)
            (map :transferred))))))

                                        ; LocalWords:  gmaps
