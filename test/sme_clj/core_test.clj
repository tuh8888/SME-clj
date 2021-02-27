(ns sme-clj.core-test
  (:require [clojure.test :as t]
            [mop-records :as mr]
            [mops :as mops]
            [sme-clj.core :as sut]
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
(defn map-vals
  [f m]
  (->> m
    (map (juxt key (comp f val)))
    (into {})))

(defn add-concept-graph
  [kg k & expressions]
  (let [concept-graph (apply types/make-concept-graph k expressions)]
    (-> (merge-with (fn [v1 v2]
                      (throw (ex-info "Value already in kg"
                               {:v1 v1 :v2 v2})))
          kg
          (->> (:graph concept-graph)
            (map-vals #(assoc % :concept-graph k))))
      (assoc k {:name k
                :type :ConceptGraph
                :spec (:spec concept-graph)}))))
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

(def kg (-> (merge-with (fn [v1 v2]
                          {:help (vector v1 v2)})
              (util/vals-as-keys :name entities)
              (util/vals-as-keys :name predicates))
          (add-concept-graph :simple-water-flow
            [:cause
             [:greater [:pressure :Beaker] [:pressure :Vial]]
             [:flow :Beaker :Vial :Water :Pipe]]
            [:greater [:diameter :Beaker] [:diameter :Vial]]
            [:clear :Beaker]
            [:flat-top :Water]
            [:liquid :Water])
          (add-concept-graph :simple-heat-flow
            [:flow :Coffee :Icecube :Heat :Bar]
            [:greater [:temperature :Coffee] [:temperature :Icecube]]
            [:flat-top :Coffee]
            [:liquid :Coffee])))


(def expected-concept-graph-expressions #{:diameter-Vial
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

(t/deftest heat-water-test
  ;; Water flow is the base heat flow the target

  (t/testing "Creating match hypotheses"
    (t/is (= expected-concept-graph-expressions
            (into #{}
              (lazy-cat
                (sut/get-concept-graph-expressions kg :simple-water-flow)
                (sut/get-concept-graph-expressions kg :simple-heat-flow)))))
    (t/is (= expected-match-hypotheses
            (sut/create-match-hypotheses kg
              :simple-water-flow
              :simple-heat-flow
              rules/literal-similarity))))

  (t/testing "Computing initial gmaps"
    (t/is (= #{[:flow-Beaker-Vial-Water-Pipe :flow-Coffee-Icecube-Heat-Bar]
               [:greater-diameter-Beaker-diameter-Vial :greater-temperature-Coffee-temperature-Icecube]
               [:greater-pressure-Beaker-pressure-Vial :greater-temperature-Coffee-temperature-Icecube]
               [:liquid-Water :liquid-Coffee]
               [:flat-top-Water :flat-top-Coffee]}
            (set (sut/find-roots kg expected-match-hypotheses))))

    (t/is (= expected-computed-initial-gmaps
            (sut/split-into-mhs-sets kg expected-match-hypotheses))))

  (t/testing "Combining gmaps"
    (t/is (= expected-combined-gmaps
            (sut/consistent-combs-of-mhs-sets expected-match-hypotheses expected-computed-initial-gmaps))))

  (t/testing "Merging gmaps"
    (t/is (= expected-merged-gmaps
            (sut/merge-mhs-sets expected-combined-gmaps))))

  (t/testing "Finalizing gmaps"
    (t/is (= expected-finalized-gmaps
            (sut/finalize-gmaps kg simple-water-flow simple-heat-flow expected-match-hypotheses expected-merged-gmaps))))

  (t/testing "Generating inferences"
    (t/is (= expected-generated-inferences
            (->> expected-finalized-gmaps
              (sut/generate-inferences kg simple-water-flow)
              (map :inferences)))))

  (t/testing "Transferring inferences"
    (t/is (= expected-transferred-inferences
            (->> expected-generated-inferences
              (map #(assoc %1 :inferences %2) expected-finalized-gmaps)
              (sut/transfer-inferences kg)
              (map :transferred))))))


(defn make-mop
  [m id parent & [slots]]
  (let [mop (mops/->mop id slots)]
    (-> m
      (mops/add-mop mop)
      (mr/-add-slot-to-mop id :parents #{parent}))))

(let [m (-> (mr/make-mop-map)
          (make-mop :cause :Expression {:e1 :Expression
                                        :e2 :Expression}))]
  (mops/infer-hierarchy m)
  #_(get-in m [:mops :cause :parents])
  #_(->> m
      :mops
      (reduce
        (fn [m [id {:keys [parents]}]]
          parents
          (let [parents (cond-> parents
                          (not (or (nil? parents) (coll? parents))) hash-set)]
            (mops/link-abstrs m id parents)
            #_(conj m parents)))
        (assoc m :hierarchy (make-hierarchy)))))
(defn mops-add-concept-graph
  [m k & expressions]
  (reduce (fn [m [parent & slots]]
            (let [id  (types/combine-ids (-> slots
                                           (->> (map second))
                                           (conj parent)))
                  mop (mops/->mop id (into {} slots))]
              (-> m
                (mops/add-mop mop)
                (mops/add-slot-to-mop id :parents parent)
                (mops/add-slot-to-mop id :concept-graph k))))
    m expressions))

(t/deftest mop-representation
  (let [mops-kg (-> (mr/make-mop-map)
                  (make-mop :cause :Expression {:e1 :Expression
                                                :e2 :Expression})
                  (make-mop :greater :Expression {:e1 :Expression
                                                  :e2 :Expression})
                  (make-mop :flow :Expression {:e1 :Entity
                                               :e2 :Entity
                                               :e3 :Entity
                                               :e4 :Entity})
                  (make-mop :Function :Expression)
                  (make-mop :pressure :Function {:e1 :Entity})
                  (make-mop :diameter :Function {:e1 :Entity})
                  (make-mop :clear :Expression {:e1 :Entity})
                  (make-mop :temperature :Function {:e1 :Entity})
                  (make-mop :flat-top :Function {:e1 :Entity})
                  (make-mop :liquid :Expression {:e1 :Entity})
                  (make-mop :Coffee :Entity)
                  (make-mop :Water :Entity)
                  (make-mop :Heat :Entity)
                  (make-mop :Pipe :Entity)
                  (make-mop :Vial :Entity)
                  (make-mop :Icecube :Entity)
                  (make-mop :Bar :Entity)
                  (make-mop :Beaker :Entity)

                  (mops-add-concept-graph :simple-water-flow
                    [:flat-top [:e1 :Water]]
                    [:liquid [:e1 :Water]]
                    [:cause
                     [:e1 :greater-pressure-Beaker-pressure-Vial]
                     [:e2 :flow-Beaker-Vial-Water-Pipe]]
                    [:greater
                     [:e1 :pressure-Beaker]
                     [:e2 :pressure-Vial]]
                    [:greater
                     [:e1 :diameter-Beaker]
                     [:e2 :diameter-Vial]]
                    [:clear [:e1 :Beaker]]
                    [:diameter [:e1 :Beaker]]
                    [:diameter [:e1 :Vial]]
                    [:pressure [:e1 :Beaker]]
                    [:pressure [:e1 :Vial]]
                    [:flow
                     [:e1 :Beaker]
                     [:e2 :Vial]
                     [:e3 :Water]
                     [:e4 :Pipe]])

                  (mops-add-concept-graph :simple-heat-flow
                    [:flow
                     [:e1 :Coffee]
                     [:e2 :Icecube]
                     [:e3 :Heat]
                     [:e4 :Bar]]
                    [:greater
                     [:e1 :temperature-Coffee]
                     [:e2 :temperature-Icecube]]
                    [:temperature [:e1 :Coffee]]
                    [:temperature [:e1 :Icecube]]
                    [:flat-top [:e1 :Coffee]]
                    [:liquid [:e1  :Coffee]])
                  mops/infer-hierarchy)]

    (t/testing "Creating match hypotheses"
      (t/is (= expected-concept-graph-expressions
              (into #{}
                (lazy-cat
                  (sut/get-concept-graph-expressions mops-kg :simple-heat-flow)
                  (sut/get-concept-graph-expressions mops-kg :simple-water-flow)))))

      (t/is (= expected-match-hypotheses
              (sut/create-match-hypotheses mops-kg :simple-water-flow :simple-heat-flow rules/mops-literal-similarity))))))
                                        ; LocalWords:  gmaps
