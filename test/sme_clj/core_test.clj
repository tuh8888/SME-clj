(ns sme-clj.core-test
  (:require [sme-clj.test-util :refer [undiff]]
            [clojure.test :refer [deftest is]]
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
                          [:flow :Beaker :Vial :Water #_:Pipe]]
                         #_[:greater [:diameter :Beaker] [:diameter :Vial]]
                         #_[:clear :Beaker]
                         #_[:flat-top :Water]
                         #_[:liquid :Water]))

(def simple-heat-flow (types/make-concept-graph :simple-heat-flow
                        [:flow :Coffee :Icecube :Heat #_:Bar]
                        [:greater [:temperature :Coffee] [:temperature :Icecube]]
                        #_[:flat-top :Coffee]
                        #_[:liquid :Coffee]))

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

(def expected-match-hypotheses #{{:base :Water :target :Heat}
                                 {:base :flow-Beaker-Vial-Water :target :flow-Coffee-Icecube-Heat}
                                 {:base :greater-pressure-Beaker-pressure-Vial :target :greater-temperature-Coffee-temperature-Icecube}
                                 {:base :Beaker :target :Coffee}
                                 {:base :pressure-Beaker :target :temperature-Coffee}
                                 {:base :pressure-Vial :target :temperature-Icecube}
                                 {:base :Vial :target :Icecube}})

(def expected-hypothesis-structure {{:base :Beaker :target :Coffee} {:emaps    #{{:base :Beaker :target :Coffee}}
                                                                     :nogood   #{}
                                                                     :children #{}}
                                    {:base :Vial :target :Icecube}  {:emaps    #{{:base :Vial :target :Icecube}}
                                                                     :nogood   #{}
                                                                     :children #{}}
                                    {:base :Water :target :Heat}    {:emaps    #{{:base :Water :target :Heat}}
                                                                     :nogood   #{}
                                                                     :children #{}}
                                    {:base :flow-Beaker-Vial-Water :target :flow-Coffee-Icecube-Heat}
                                    {:emaps    #{}
                                     :nogood   #{}
                                     :children #{{:base :Beaker :target :Coffee}
                                                 {:base :Vial :target :Icecube}
                                                 {:base :Water :target :Heat}}}
                                    {:base :pressure-Vial :target :temperature-Icecube}
                                    {:emaps    #{}
                                     :nogood   #{}
                                     :children #{{:base :Vial :target :Icecube}}}
                                    {:base :pressure-Beaker :target :temperature-Coffee}
                                    {:emaps    #{}
                                     :nogood   #{}
                                     :children #{{:base :Beaker :target :Coffee}}}
                                    {:base :greater-pressure-Beaker-pressure-Vial :target :greater-temperature-Coffee-temperature-Icecube}
                                    {:emaps    #{}
                                     :nogood   #{}
                                     :children #{{:base :pressure-Vial :target :temperature-Icecube}
                                                 {:base :pressure-Beaker :target :temperature-Coffee}}}})


(def expected-propagated-from-emaps (undiff expected-hypothesis-structure
                                      {{:base :greater-pressure-Beaker-pressure-Vial, :target :greater-temperature-Coffee-temperature-Icecube}
                                       {:emaps #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}}},
                                       {:base :pressure-Beaker, :target :temperature-Coffee}              {:emaps #{{:base :Beaker, :target :Coffee}}},
                                       {:base :pressure-Vial, :target :temperature-Icecube}               {:emaps #{{:base :Vial, :target :Icecube}}},
                                       {:base :flow-Beaker-Vial-Water, :target :flow-Coffee-Icecube-Heat} {:emaps #{{:base :Water, :target :Heat}
                                                                                                                    {:base :Beaker, :target :Coffee}
                                                                                                                    {:base :Vial, :target :Icecube}}}}
                                      {{:base :greater-pressure-Beaker-pressure-Vial, :target :greater-temperature-Coffee-temperature-Icecube}
                                       {:emaps nil},
                                       {:base :pressure-Beaker, :target :temperature-Coffee}              {:emaps nil},
                                       {:base :flow-Beaker-Vial-Water, :target :flow-Coffee-Icecube-Heat} {:emaps nil}}))


(def expected-computed-initial-gmaps
  {:mh-structure expected-propagated-from-emaps
   :gmaps        [{:mhs       #{{:base :Water, :target :Heat}
                                {:base :Beaker, :target :Coffee}
                                {:base :flow-Beaker-Vial-Water, :target :flow-Coffee-Icecube-Heat}
                                {:base :Vial, :target :Icecube}},
                   :structure {:roots  #{{:base :flow-Beaker-Vial-Water, :target :flow-Coffee-Icecube-Heat}},
                               :nogood #{},
                               :emaps  #{{:base :Water, :target :Heat}
                                         {:base :Beaker, :target :Coffee}
                                         {:base :Vial, :target :Icecube}}}}
                  {:mhs       #{{:base   :greater-pressure-Beaker-pressure-Vial,
                                 :target :greater-temperature-Coffee-temperature-Icecube}
                                {:base :Beaker, :target :Coffee}
                                {:base :Vial :target :Icecube}
                                {:base :pressure-Beaker, :target :temperature-Coffee}
                                {:base :pressure-Vial, :target :temperature-Icecube}},
                   :structure {:roots  #{{:base   :greater-pressure-Beaker-pressure-Vial,
                                          :target :greater-temperature-Coffee-temperature-Icecube}},
                               :nogood #{},
                               :emaps  #{{:base :Beaker, :target :Coffee}
                                         {:base :Vial, :target :Icecube}}}}]})

(def expected-combined-gmaps
  (update expected-computed-initial-gmaps :gmaps vector))

(def expected-merged-gmaps #_(undiff expected-merged-gmaps)
  {:gmaps        [{:mhs       #{{:base :Beaker :target :Coffee}
                                {:base :Vial :target :Icecube}
                                {:base :Water :target :Heat}
                                {:base :flow-Beaker-Vial-Water :target :flow-Coffee-Icecube-Heat}
                                {:base :pressure-Beaker :target :temperature-Coffee}
                                {:base :pressure-Vial, :target :temperature-Icecube}
                                {:base :greater-pressure-Beaker-pressure-Vial :target :greater-temperature-Coffee-temperature-Icecube}}
                   :structure {:roots  #{{:base   :flow-Beaker-Vial-Water
                                          :target :flow-Coffee-Icecube-Heat}
                                         {:base   :greater-pressure-Beaker-pressure-Vial
                                          :target :greater-temperature-Coffee-temperature-Icecube}}
                               :nogood #{}
                               :emaps  #{{:base   :Beaker
                                          :target :Coffee}
                                         {:base   :Vial
                                          :target :Icecube}
                                         {:base   :Water
                                          :target :Heat}}}}]
   :mh-structure expected-propagated-from-emaps})

(def expected-finalized-gmaps (undiff expected-merged-gmaps
                                {:gmaps [{:score        16,
                                          :emap-matches 0,
                                          :mapping      {:base :simple-water-flow, :target :simple-heat-flow}}]}
                                nil))

(def expected-generated-inferences (undiff expected-finalized-gmaps
                                     {:gmaps [{:inferences #{:cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water}}]}
                                     nil))

(def expected-transferred [[:cause :greater-temperature-Coffee-temperature-Icecube :flow-Coffee-Icecube-Heat]])

(def expected-transferred-inferences (undiff expected-generated-inferences
                                       {:gmaps [{:transferred expected-transferred}]}
                                       nil))

(deftest heat-water-test
  ;; Water flow is the base heat flow the target

  (is (= expected-match-hypotheses
        (SUT/create-match-hypotheses kg simple-water-flow simple-heat-flow rules/literal-similarity)))

  (is (= expected-hypothesis-structure
        (SUT/build-hypothesis-structure kg expected-match-hypotheses)))

  (is (= expected-propagated-from-emaps
        (SUT/propagate-from-emaps expected-hypothesis-structure)))

  (is (= [{:base :flow-Beaker-Vial-Water, :target :flow-Coffee-Icecube-Heat}
          {:base :greater-pressure-Beaker-pressure-Vial, :target :greater-temperature-Coffee-temperature-Icecube}]
        (SUT/find-roots expected-propagated-from-emaps)))

  (is (= expected-computed-initial-gmaps
        (SUT/compute-initial-gmaps kg expected-propagated-from-emaps)))

  (is (= expected-combined-gmaps
        (SUT/combine-gmaps expected-computed-initial-gmaps)))

  (is (= expected-merged-gmaps
        (SUT/merge-gmaps expected-combined-gmaps)))

  (is (= expected-finalized-gmaps
        (SUT/finalize-gmaps kg simple-water-flow simple-heat-flow expected-merged-gmaps)))

  (is (= expected-generated-inferences
        (SUT/generate-inferences kg simple-water-flow expected-finalized-gmaps)))

  (is (= expected-transferred-inferences
        (SUT/transfer-inferences kg expected-generated-inferences)))

  (is (= expected-transferred-inferences
        (SUT/match kg rules/literal-similarity simple-water-flow simple-heat-flow)))

  ;; Should show the cause relation between the greater temperature
  ;; relation and the heat flow relation. This relation has been inferred
  ;; based on the analogical cause relation in the water flow graph.
  (is (= expected-transferred (-> (SUT/match kg rules/literal-similarity simple-water-flow simple-heat-flow)
                                :gmaps
                                first
                                :transferred))))

; LocalWords:  gmaps
