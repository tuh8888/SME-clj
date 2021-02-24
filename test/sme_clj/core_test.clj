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

(let [kg                {:cause       {:upstream   :Expression
                                       :downstream :Expression
                                       :parents    #{:Expression}}
                         :greater     {:less    :Expression
                                       :more    :Expression
                                       :parents #{:Expression}}
                         :flow        {:from    :Entity
                                       :to      :Entity
                                       :flow-er :Entity
                                       :via     :Entity
                                       :parents #{:Expression}}
                         :pressure    {:container :Entity}
                         :temperature {:thermal-entity :Entity
                                       :parents        #{:Expression}}
                         :flat-top    {:parents    #{:Expression}
                                       :surface-of :Entity}
                         :liquid      {:parents #{:Expression}
                                       :entity  :Entity}
                         :Coffee      {:parents #{:Entity}}
                         :Water       {:parents #{:Entity}}
                         :Heat        {:parents #{:Entity}}
                         :Pipe        {:parents #{:Entity}}
                         :Vial        {:parents #{:Entity}}
                         :Icecube     {:parents #{:Entity}}
                         :Bar         {:parents #{:Entity}}
                         :Beaker      {:parents #{:Entity}}
                         }
      simple-water-flow {:ftw             {:parents    #{:flat-top}
                                           :surface-of :Water}
                         :lw              {:parents #{:liquid}
                                           :entity  :Water}
                         ;;cause-greater-beaker-pressure-vial-pressure-flow-from-beaker-to-vial-flow-er-water-via-pipe
                         :cgbpvpffbtvfwvp {:upstream   :gbpvp
                                           :downstream :ffbtvfwvp}
                         :gbpvp           {:more    :bp
                                           :less    :vp
                                           :parents #{:greater}}
                         :bp              {:parents   #{:pressure}
                                           :container :Beaker}
                         :vp              {:parents   #{:pressure}
                                           :container :Vial}
                         :ffbtvfwvp       {:from    :Beaker
                                           :to      :Vial
                                           :flow-er :Water
                                           :via     :Pipe
                                           :parents #{:flow}}}
      simple-heat-flow  {:ffctifhvb {:from    :Coffee
                                     :to      :Icecube
                                     :flow-er :Heat
                                     :via     :Bar
                                     :parents #{:flow}}
                         :gctit     {:more    :ct
                                     :less    :it
                                     :parents #{:greater}}
                         :ct        {:parents        #{:termerature}
                                     :thermal-entity :Coffee}
                         :it        {:thermal-entity :Icecube
                                     :parents        #{:temperature}}
                         :ftc       {:surface-of :Coffee
                                     :parents    #{:flat-top}}
                         :lc        {:entity  :Coffee
                                     :parents #{:liquid}}}])

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

(def expected-hypothesis-structure
  {[:Beaker :Coffee]                                            {:emaps  #{[:Beaker :Coffee]}
                                                                 :nogood #{[:Water :Coffee]}}
   [:Vial :Icecube]                                             {:emaps #{[:Vial :Icecube]}}
   [:Water :Heat]                                               {:emaps  #{[:Water :Heat]}
                                                                 :nogood #{[:Water :Coffee]}}
   [:Pipe :Bar]                                                 {:emaps #{[:Pipe :Bar]}}
   [:Water :Coffee]                                             {:emaps  #{[:Water :Coffee]}
                                                                 :nogood #{[:Water :Heat]
                                                                           [:Beaker :Coffee]}}
   [:flat-top-Water :flat-top-Coffee]                           nil
   [:liquid-Water :liquid-Coffee]                               nil
   [:pressure-Vial :temperature-Icecube]                        {:nogood #{[:diameter-Vial :temperature-Icecube]}}
   [:pressure-Beaker :temperature-Coffee]                       {:nogood #{[:diameter-Beaker :temperature-Coffee]}}
   [:diameter-Beaker :temperature-Coffee]                       {:nogood #{[:pressure-Beaker :temperature-Coffee]}}
   [:diameter-Vial :temperature-Icecube]                        {:nogood #{[:pressure-Vial :temperature-Icecube]}}
   [:greater-diameter-Beaker-diameter-Vial
    :greater-temperature-Coffee-temperature-Icecube]
   {:nogood #{[:greater-pressure-Beaker-pressure-Vial
               :greater-temperature-Coffee-temperature-Icecube]}}
   [:flow-Beaker-Vial-Water-Pipe :flow-Coffee-Icecube-Heat-Bar] nil
   [:greater-pressure-Beaker-pressure-Vial :greater-temperature-Coffee-temperature-Icecube]
   {:nogood #{[:greater-diameter-Beaker-diameter-Vial
               :greater-temperature-Coffee-temperature-Icecube]}}})


(def expected-propagated-from-emaps
  (undiff expected-hypothesis-structure
    {[:greater-pressure-Beaker-pressure-Vial :greater-temperature-Coffee-temperature-Icecube]
     {:emaps  #{[:Beaker :Coffee] [:Vial :Icecube]}
      :nogood #{[:diameter-Beaker :temperature-Coffee]
                [:diameter-Vial :temperature-Icecube]
                [:Water :Coffee]}}
     [:diameter-Vial :temperature-Icecube]                        {:emaps #{[:Vial :Icecube]}}
     [:diameter-Beaker :temperature-Coffee]                       {:emaps  #{[:Beaker :Coffee]}
                                                                   :nogood #{[:Water :Coffee]}}
     [:pressure-Beaker :temperature-Coffee]                       {:emaps  #{[:Beaker :Coffee]}
                                                                   :nogood #{[:Water :Coffee]}}
     [:flat-top-Water :flat-top-Coffee]                           {:emaps  #{[:Water :Coffee]}
                                                                   :nogood #{[:Beaker :Coffee]
                                                                             [:Water :Heat]}}
     [:liquid-Water :liquid-Coffee]                               {:emaps  #{[:Water :Coffee]}
                                                                   :nogood #{[:Beaker :Coffee]
                                                                             [:Water :Heat]}}
     [:pressure-Vial :temperature-Icecube]                        {:emaps #{[:Vial :Icecube]}}
     [:greater-diameter-Beaker-diameter-Vial :greater-temperature-Coffee-temperature-Icecube]
     {:emaps  #{[:Beaker :Coffee]
                [:Vial :Icecube]}
      :nogood #{[:pressure-Vial :temperature-Icecube]
                [:pressure-Beaker :temperature-Coffee]
                [:Water :Coffee]}}
     [:flow-Beaker-Vial-Water-Pipe :flow-Coffee-Icecube-Heat-Bar] {:emaps  #{[:Water :Heat]
                                                                             [:Beaker :Coffee]
                                                                             [:Pipe :Bar]
                                                                             [:Vial :Icecube]}
                                                                   :nogood #{[:Water :Coffee]}}}
    nil))


(def diameter-gmap {:mhs       #{[:greater-diameter-Beaker-diameter-Vial
                                  :greater-temperature-Coffee-temperature-Icecube]
                                 [:diameter-Beaker :temperature-Coffee]
                                 [:diameter-Vial :temperature-Icecube]
                                 [:Beaker :Coffee]
                                 [:Vial :Icecube]}
                    :structure {:nogood #{[:pressure-Vial :temperature-Icecube]
                                          [:pressure-Beaker :temperature-Coffee]
                                          [:greater-pressure-Beaker-pressure-Vial
                                           :greater-temperature-Coffee-temperature-Icecube]
                                          [:Water :Coffee]}
                                :emaps  #{[:Beaker :Coffee]
                                          [:Vial :Icecube]}}})
(def flow-gmap {:mhs       #{[:Water :Heat]
                             [:Beaker :Coffee]
                             [:Pipe :Bar]
                             [:flow-Beaker-Vial-Water-Pipe :flow-Coffee-Icecube-Heat-Bar]
                             [:Vial :Icecube]}
                :structure {:nogood #{[:Water :Coffee]}
                            :emaps  #{[:Water :Heat]
                                      [:Pipe :Bar]
                                      [:Beaker :Coffee]
                                      [:Vial :Icecube]}}})

(def pressure-gmap {:mhs       #{[:Beaker :Coffee]
                                 [:greater-pressure-Beaker-pressure-Vial
                                  :greater-temperature-Coffee-temperature-Icecube]
                                 [:Vial :Icecube]
                                 [:pressure-Beaker :temperature-Coffee]
                                 [:pressure-Vial :temperature-Icecube]}
                    :structure {:nogood #{[:diameter-Beaker :temperature-Coffee]
                                          [:diameter-Vial :temperature-Icecube]
                                          [:greater-diameter-Beaker-diameter-Vial
                                           :greater-temperature-Coffee-temperature-Icecube]
                                          [:Water :Coffee]}
                                :emaps  #{[:Beaker :Coffee]
                                          [:Vial :Icecube]}}})

(def liquid-gmap {:mhs       #{[:Water :Coffee]
                               [:liquid-Water :liquid-Coffee]}
                  :structure {:nogood #{[:Beaker :Coffee]
                                        [:Water :Heat]}
                              :emaps  #{[:Water :Coffee]}}})
(def flat-top-gmap {:mhs       #{[:Water :Coffee]
                                 [:flat-top-Water :flat-top-Coffee]}
                    :structure {:nogood #{[:Beaker :Coffee]
                                          [:Water :Heat]}
                                :emaps  #{[:Water :Coffee]}}})
(def expected-computed-initial-gmaps
  {:mh-structure expected-propagated-from-emaps
   :gmaps        [flat-top-gmap
                  pressure-gmap
                  diameter-gmap
                  flow-gmap
                  liquid-gmap]})

(def expected-combined-gmaps
  {:mh-structure expected-propagated-from-emaps
   :gmaps        [[flat-top-gmap liquid-gmap]
                  [pressure-gmap flow-gmap]
                  [diameter-gmap flow-gmap]]})

(def expected-merged-gmaps
  {:gmaps        [{:mhs       #{[:Water :Coffee]
                                [:liquid-Water :liquid-Coffee]
                                [:flat-top-Water :flat-top-Coffee]},
                   :structure {:nogood #{[:Beaker :Coffee] [:Water :Heat]},
                               :emaps  #{[:Water :Coffee]}}}
                  {:mhs       #{[:Beaker :Coffee]
                                [:Water :Heat]
                                [:pressure-Beaker :temperature-Coffee]
                                [:Pipe :Bar]
                                [:greater-pressure-Beaker-pressure-Vial
                                 :greater-temperature-Coffee-temperature-Icecube]
                                [:flow-Beaker-Vial-Water-Pipe :flow-Coffee-Icecube-Heat-Bar]
                                [:Vial :Icecube]
                                [:pressure-Vial :temperature-Icecube]},
                   :structure {:nogood #{[:Water :Coffee]
                                         [:diameter-Vial :temperature-Icecube]
                                         [:diameter-Beaker :temperature-Coffee]
                                         [:greater-diameter-Beaker-diameter-Vial
                                          :greater-temperature-Coffee-temperature-Icecube]},
                               :emaps  #{[:Beaker :Coffee]
                                         [:Water :Heat]
                                         [:Pipe :Bar]
                                         [:Vial :Icecube]}}}
                  {:mhs       #{[:Beaker :Coffee]
                                [:diameter-Vial :temperature-Icecube]
                                [:Water :Heat]
                                [:Pipe :Bar]
                                [:diameter-Beaker :temperature-Coffee]
                                [:greater-diameter-Beaker-diameter-Vial
                                 :greater-temperature-Coffee-temperature-Icecube]
                                [:flow-Beaker-Vial-Water-Pipe :flow-Coffee-Icecube-Heat-Bar]
                                [:Vial :Icecube]},
                   :structure {:nogood #{[:Water :Coffee]
                                         [:pressure-Beaker :temperature-Coffee]
                                         [:greater-pressure-Beaker-pressure-Vial
                                          :greater-temperature-Coffee-temperature-Icecube]
                                         [:pressure-Vial :temperature-Icecube]},
                               :emaps  #{[:Beaker :Coffee]
                                         [:Water :Heat]
                                         [:Pipe :Bar]
                                         [:Vial :Icecube]}}}]
   :mh-structure expected-propagated-from-emaps})

(def expected-finalized-gmaps (undiff expected-merged-gmaps
                                {:gmaps [{:score        5
                                          :emap-matches 0
                                          :mapping      {:base :simple-water-flow :target :simple-heat-flow}}
                                         {:score        18
                                          :emap-matches 0
                                          :mapping      {:base :simple-water-flow :target :simple-heat-flow}}
                                         {:score        18
                                          :emap-matches 0
                                          :mapping      {:base :simple-water-flow :target :simple-heat-flow}}]}
                                nil))

(def expected-generated-inferences (undiff expected-finalized-gmaps
                                     {:gmaps [{:inferences #{}}
                                              {:inferences #{:cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe}}
                                              {:inferences #{:cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe
                                                             :pressure-Vial
                                                             :pressure-Beaker
                                                             :greater-pressure-Beaker-pressure-Vial}}]}
                                     nil))

(def expected-transferred2 #{[:greater [:pressure :Coffee] [:pressure :Icecube]]
                             [:pressure :Icecube]
                             [:pressure :Coffee]
                             [:cause [:greater [:pressure :Coffee] [:pressure :Icecube]]
                              :flow-Coffee-Icecube-Heat-Bar]})
(def expected-transferred1 #{[:cause :greater-temperature-Coffee-temperature-Icecube :flow-Coffee-Icecube-Heat-Bar]})

(def expected-transferred-inferences (undiff expected-generated-inferences
                                       {:gmaps [{:transferred #{}}
                                                {:transferred expected-transferred1}
                                                {:transferred expected-transferred2}]}
                                       nil))

(deftest heat-water-test
  ;; Water flow is the base heat flow the target

  (testing "Creating match hypotheses"
    (is (= expected-match-hypotheses
          (SUT/create-match-hypotheses kg simple-water-flow simple-heat-flow rules/literal-similarity))))

  (testing "Building hypothesis structure"
    (is (= expected-hypothesis-structure
          (SUT/build-hypothesis-structure kg expected-match-hypotheses))))

  (testing "Propagating emaps"
    (is (= expected-propagated-from-emaps
          (SUT/propagate-from-emaps kg expected-hypothesis-structure))))

  (testing "Computing initial gmaps"
    (is (= #{[:flow-Beaker-Vial-Water-Pipe :flow-Coffee-Icecube-Heat-Bar]
             [:greater-diameter-Beaker-diameter-Vial :greater-temperature-Coffee-temperature-Icecube]
             [:greater-pressure-Beaker-pressure-Vial :greater-temperature-Coffee-temperature-Icecube]
             [:liquid-Water :liquid-Coffee]
             [:flat-top-Water :flat-top-Coffee]}
          (set (SUT/find-roots kg expected-propagated-from-emaps))))

    (is (= expected-computed-initial-gmaps
          (SUT/compute-initial-gmaps kg expected-propagated-from-emaps))))

  (testing "Combining gmaps"
    (is (= expected-combined-gmaps
          (SUT/combine-gmaps expected-computed-initial-gmaps))))

  (testing "Merging gmaps"
    (is (= expected-merged-gmaps
          (SUT/merge-gmaps expected-combined-gmaps))))

  (testing "Finalizing gmaps"
    (is (= expected-finalized-gmaps
          (SUT/finalize-gmaps kg simple-water-flow simple-heat-flow expected-merged-gmaps))))

  (testing "Generating inferences"
    (is (= expected-generated-inferences
          (SUT/generate-inferences kg simple-water-flow expected-finalized-gmaps))))

  (testing "Transferring inferences"
    (is (= expected-transferred-inferences
          (SUT/transfer-inferences kg expected-generated-inferences))))

  (testing "Full match pipeline"
    (let [result (SUT/match kg rules/literal-similarity simple-water-flow simple-heat-flow)]
      ;; Should show the cause relation between the greater temperature
      ;; relation and the heat flow relation. This relation has been inferred
      ;; based on the analogical cause relation in the water flow graph.
      (is (= expected-transferred1
            (-> result
              :gmaps
              (nth 1)
              :transferred)))

      (is (= expected-transferred-inferences result)))))

                                        ; LocalWords:  gmaps
