(ns sme-clj.core-test
  (:require [clojure.pprint :as pp]
            [sme-clj.core :as SUT]
            [sme-clj.ruledef :as rules]
            [sme-clj.typedef :as types]
            [sme-clj.util :as util]
            [clojure.set  :as set ]
            [clojure.test :refer [is deftest]]
            [clojure.data :as data]))


#_(= new-var (apply undiff old-var (take 2 (data/diff new-var old-var))))
(defn- seqzip
  "returns a sequence of [[ value-left] [value-right]....]  padding with nulls for shorter sequences "
  [left right]
  (loop [list [] a left b right]
    (if (or (seq a) (seq b))
      (recur (conj list [(first a) (first b)] ) (rest a) (rest b))
      list)))

(defn- recursive-diff-merge
  " Merge two structures recusively , taking non-nil values from sequences and maps and merging sets"
  [part-state original-state]
  (cond
    (sequential? part-state) (map (fn [[l r]] (recursive-diff-merge l r)) (seqzip part-state original-state))
    (map? part-state) (merge-with recursive-diff-merge part-state original-state)
    (set? part-state) (set/union part-state original-state)
    (nil? part-state ) original-state
    :default part-state))

(defn undiff
  "returns the state of x after reversing the changes described by a diff against
   an earlier state (where before and after are the first two elements of the diff)"
  [x before after]
  (let [[a _ _] (clojure.data/diff x after)]
    (recursive-diff-merge a before)))


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

(def simple-water-flow (types/make-concept-graph (keyword "simple-water-flow")
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

(def expected-hypothesis-structure {{:base :e11, :target :e1}        {:emaps    #{},
                                                                      :nogood   #{{:base :e6, :target :e1}},
                                                                      :children #{{:base :Beaker, :target :Coffee}}},
                                    {:base :e15, :target :e4}        {:emaps    #{},
                                                                      :nogood   #{},
                                                                      :children #{{:base :Water, :target :Coffee}}},
                                    {:base :Water, :target :Heat}    {:emaps    #{{:base :Water, :target :Heat}},
                                                                      :nogood   #{{:base :Water, :target :Coffee}},
                                                                      :children #{}},
                                    {:base :e7, :target :e2}         {:emaps    #{},
                                                                      :nogood   #{{:base :e12, :target :e2}},
                                                                      :children #{{:base :Vial, :target :Icecube}}},
                                    {:base :e8, :target :e3}         {:emaps    #{},
                                                                      :nogood   #{{:base :e13, :target :e3}},
                                                                      :children #{{:base :e7, :target :e2} {:base :e6, :target :e1}}},
                                    {:base :Beaker, :target :Coffee} {:emaps    #{{:base :Beaker, :target :Coffee}},
                                                                      :nogood   #{{:base :Water, :target :Coffee}},
                                                                      :children #{}},
                                    {:base :Water, :target :Coffee}  {:emaps    #{{:base :Water, :target :Coffee}},
                                                                      :nogood   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                                                      :children #{}},
                                    {:base :e9, :target :e0}         {:emaps    #{},
                                                                      :nogood   #{},
                                                                      :children #{{:base :Water, :target :Heat}
                                                                                  {:base :Beaker, :target :Coffee}
                                                                                  {:base :Pipe, :target :Bar}
                                                                                  {:base :Vial, :target :Icecube}}},
                                    {:base :e6, :target :e1}         {:emaps    #{},
                                                                      :nogood   #{{:base :e11, :target :e1}},
                                                                      :children #{{:base :Beaker, :target :Coffee}}},
                                    {:base :Pipe, :target :Bar}      {:emaps    #{{:base :Pipe, :target :Bar}},
                                                                      :nogood   #{},
                                                                      :children #{}},
                                    {:base :e16, :target :e5}        {:emaps    #{},
                                                                      :nogood   #{},
                                                                      :children #{{:base :Water, :target :Coffee}}},
                                    {:base :Vial, :target :Icecube}  {:emaps    #{{:base :Vial, :target :Icecube}},
                                                                      :nogood   #{},
                                                                      :children #{}},
                                    {:base :e13, :target :e3}        {:emaps    #{},
                                                                      :nogood   #{{:base :e8, :target :e3}},
                                                                      :children #{{:base :e11, :target :e1} {:base :e12, :target :e2}}},
                                    {:base :e12, :target :e2}        {:emaps    #{},
                                                                      :nogood   #{{:base :e7, :target :e2}},
                                                                      :children #{{:base :Vial, :target :Icecube}}}})

(def expected-propagated-from-emaps (undiff expected-hypothesis-structure
                                      {{:base :e11, :target :e1} {:nogood #{{:base :Water, :target :Coffee}},
                                                                  :emaps  #{{:base :Beaker, :target :Coffee}}},
                                       {:base :e15, :target :e4} {:nogood #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                                                  :emaps  #{{:base :Water, :target :Coffee}}},
                                       {:base :e7, :target :e2}  {:emaps #{{:base :Vial, :target :Icecube}}},
                                       {:base :e8, :target :e3}  {:nogood
                                                                  #{{:base :e11, :target :e1}
                                                                    {:base :Water, :target :Coffee}
                                                                    {:base :e12, :target :e2}},
                                                                  :emaps #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}}},
                                       {:base :e9, :target :e0}  {:nogood #{{:base :Water, :target :Coffee}},
                                                                  :emaps
                                                                  #{{:base :Water, :target :Heat}
                                                                    {:base :Beaker, :target :Coffee}
                                                                    {:base :Pipe, :target :Bar}
                                                                    {:base :Vial, :target :Icecube}}},
                                       {:base :e6, :target :e1}  {:nogood #{{:base :Water, :target :Coffee}},
                                                                  :emaps  #{{:base :Beaker, :target :Coffee}}},
                                       {:base :e16, :target :e5} {:nogood #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                                                  :emaps  #{{:base :Water, :target :Coffee}}},
                                       {:base :e13, :target :e3} {:nogood
                                                                  #{{:base :e7, :target :e2}
                                                                    {:base :Water, :target :Coffee}
                                                                    {:base :e6, :target :e1}},
                                                                  :emaps #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}}},
                                       {:base :e12, :target :e2} {:emaps #{{:base :Vial, :target :Icecube}}}}
                                      {{:base :e11, :target :e1} {:emaps nil},
                                       {:base :e15, :target :e4} {:nogood nil, :emaps nil},
                                       {:base :e7, :target :e2}  {:emaps nil},
                                       {:base :e8, :target :e3}  {:emaps nil},
                                       {:base :e9, :target :e0}  {:nogood nil, :emaps nil},
                                       {:base :e6, :target :e1}  {:emaps nil},
                                       {:base :e16, :target :e5} {:nogood nil, :emaps nil},
                                       {:base :e13, :target :e3} {:emaps nil},
                                       {:base :e12, :target :e2} {:emaps nil}}))


(def expected-computed-initial-gmaps (undiff expected-propagated-from-emaps
                                       {:mh-structure
                                        {{:base :e11, :target :e1}
                                         {:emaps    #{{:base :Beaker, :target :Coffee}},
                                          :nogood   #{{:base :Water, :target :Coffee} {:base :e6, :target :e1}},
                                          :children #{{:base :Beaker, :target :Coffee}}},
                                         {:base :e15, :target :e4}
                                         {:emaps    #{{:base :Water, :target :Coffee}},
                                          :nogood   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                          :children #{{:base :Water, :target :Coffee}}},
                                         {:base :Water, :target :Heat}
                                         {:emaps    #{{:base :Water, :target :Heat}},
                                          :nogood   #{{:base :Water, :target :Coffee}},
                                          :children #{}},
                                         {:base :e7, :target :e2}
                                         {:emaps    #{{:base :Vial, :target :Icecube}},
                                          :nogood   #{{:base :e12, :target :e2}},
                                          :children #{{:base :Vial, :target :Icecube}}},
                                         {:base :e8, :target :e3}
                                         {:emaps    #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}},
                                          :nogood
                                          #{{:base :e11, :target :e1}
                                            {:base :Water, :target :Coffee}
                                            {:base :e13, :target :e3}
                                            {:base :e12, :target :e2}},
                                          :children #{{:base :e7, :target :e2} {:base :e6, :target :e1}}},
                                         {:base :Beaker, :target :Coffee}
                                         {:emaps    #{{:base :Beaker, :target :Coffee}},
                                          :nogood   #{{:base :Water, :target :Coffee}},
                                          :children #{}},
                                         {:base :Water, :target :Coffee}
                                         {:emaps    #{{:base :Water, :target :Coffee}},
                                          :nogood   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                          :children #{}},
                                         {:base :e9, :target :e0}
                                         {:emaps
                                          #{{:base :Water, :target :Heat}
                                            {:base :Beaker, :target :Coffee}
                                            {:base :Pipe, :target :Bar}
                                            {:base :Vial, :target :Icecube}},
                                          :nogood #{{:base :Water, :target :Coffee}},
                                          :children
                                          #{{:base :Water, :target :Heat}
                                            {:base :Beaker, :target :Coffee}
                                            {:base :Pipe, :target :Bar}
                                            {:base :Vial, :target :Icecube}}},
                                         {:base :e6, :target :e1}
                                         {:emaps    #{{:base :Beaker, :target :Coffee}},
                                          :nogood   #{{:base :e11, :target :e1} {:base :Water, :target :Coffee}},
                                          :children #{{:base :Beaker, :target :Coffee}}},
                                         {:base :Pipe, :target :Bar}
                                         {:emaps #{{:base :Pipe, :target :Bar}}, :nogood #{}, :children #{}},
                                         {:base :e16, :target :e5}
                                         {:emaps    #{{:base :Water, :target :Coffee}},
                                          :nogood   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                          :children #{{:base :Water, :target :Coffee}}},
                                         {:base :Vial, :target :Icecube}
                                         {:emaps #{{:base :Vial, :target :Icecube}}, :nogood #{}, :children #{}},
                                         {:base :e13, :target :e3}
                                         {:emaps    #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}},
                                          :nogood
                                          #{{:base :e7, :target :e2}
                                            {:base :e8, :target :e3}
                                            {:base :Water, :target :Coffee}
                                            {:base :e6, :target :e1}},
                                          :children #{{:base :e11, :target :e1} {:base :e12, :target :e2}}},
                                         {:base :e12, :target :e2}
                                         {:emaps    #{{:base :Vial, :target :Icecube}},
                                          :nogood   #{{:base :e7, :target :e2}},
                                          :children #{{:base :Vial, :target :Icecube}}}},
                                        :gmaps
                                        #{{:mhs
                                           #{{:base :Water, :target :Heat}
                                             {:base :Beaker, :target :Coffee}
                                             {:base :e9, :target :e0}
                                             {:base :Pipe, :target :Bar}
                                             {:base :Vial, :target :Icecube}},
                                           :structure
                                           {:roots  #{{:base :e9, :target :e0}},
                                            :nogood #{{:base :Water, :target :Coffee}},
                                            :emaps
                                            #{{:base :Water, :target :Heat}
                                              {:base :Beaker, :target :Coffee}
                                              {:base :Pipe, :target :Bar}
                                              {:base :Vial, :target :Icecube}}}}
                                          {:mhs #{{:base :e15, :target :e4} {:base :Water, :target :Coffee}},
                                           :structure
                                           {:roots #{{:base :e15, :target :e4}},
                                            :nogood
                                            #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                            :emaps #{{:base :Water, :target :Coffee}}}}
                                          {:mhs
                                           #{{:base :e11, :target :e1}
                                             {:base :Beaker, :target :Coffee}
                                             {:base :Vial, :target :Icecube}
                                             {:base :e13, :target :e3}
                                             {:base :e12, :target :e2}},
                                           :structure
                                           {:roots #{{:base :e13, :target :e3}},
                                            :nogood
                                            #{{:base :e7, :target :e2}
                                              {:base :e8, :target :e3}
                                              {:base :Water, :target :Coffee}
                                              {:base :e6, :target :e1}},
                                            :emaps
                                            #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}}}}
                                          {:mhs #{{:base :Water, :target :Coffee} {:base :e16, :target :e5}},
                                           :structure
                                           {:roots #{{:base :e16, :target :e5}},
                                            :nogood
                                            #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                            :emaps #{{:base :Water, :target :Coffee}}}}
                                          {:mhs
                                           #{{:base :e7, :target :e2}
                                             {:base :e8, :target :e3}
                                             {:base :Beaker, :target :Coffee}
                                             {:base :e6, :target :e1}
                                             {:base :Vial, :target :Icecube}},
                                           :structure
                                           {:roots #{{:base :e8, :target :e3}},
                                            :nogood
                                            #{{:base :e11, :target :e1}
                                              {:base :Water, :target :Coffee}
                                              {:base :e13, :target :e3}
                                              {:base :e12, :target :e2}},
                                            :emaps
                                            #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}}}}}}
                                       {{:base :e11, :target :e1}
                                        {:emaps    #{{:base :Beaker, :target :Coffee}},
                                         :nogood   #{{:base :Water, :target :Coffee} {:base :e6, :target :e1}},
                                         :children #{{:base :Beaker, :target :Coffee}}},
                                        {:base :e15, :target :e4}
                                        {:emaps    #{{:base :Water, :target :Coffee}},
                                         :nogood   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                         :children #{{:base :Water, :target :Coffee}}},
                                        {:base :Water, :target :Heat}
                                        {:emaps    #{{:base :Water, :target :Heat}},
                                         :nogood   #{{:base :Water, :target :Coffee}},
                                         :children #{}},
                                        {:base :e7, :target :e2}
                                        {:emaps    #{{:base :Vial, :target :Icecube}},
                                         :nogood   #{{:base :e12, :target :e2}},
                                         :children #{{:base :Vial, :target :Icecube}}},
                                        {:base :e8, :target :e3}
                                        {:emaps    #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}},
                                         :nogood
                                         #{{:base :e11, :target :e1}
                                           {:base :Water, :target :Coffee}
                                           {:base :e13, :target :e3}
                                           {:base :e12, :target :e2}},
                                         :children #{{:base :e7, :target :e2} {:base :e6, :target :e1}}},
                                        {:base :Beaker, :target :Coffee}
                                        {:emaps    #{{:base :Beaker, :target :Coffee}},
                                         :nogood   #{{:base :Water, :target :Coffee}},
                                         :children #{}},
                                        {:base :Water, :target :Coffee}
                                        {:emaps    #{{:base :Water, :target :Coffee}},
                                         :nogood   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                         :children #{}},
                                        {:base :e9, :target :e0}
                                        {:emaps
                                         #{{:base :Water, :target :Heat}
                                           {:base :Beaker, :target :Coffee}
                                           {:base :Pipe, :target :Bar}
                                           {:base :Vial, :target :Icecube}},
                                         :nogood #{{:base :Water, :target :Coffee}},
                                         :children
                                         #{{:base :Water, :target :Heat}
                                           {:base :Beaker, :target :Coffee}
                                           {:base :Pipe, :target :Bar}
                                           {:base :Vial, :target :Icecube}}},
                                        {:base :e6, :target :e1}
                                        {:emaps    #{{:base :Beaker, :target :Coffee}},
                                         :nogood   #{{:base :e11, :target :e1} {:base :Water, :target :Coffee}},
                                         :children #{{:base :Beaker, :target :Coffee}}},
                                        {:base :Pipe, :target :Bar}
                                        {:emaps #{{:base :Pipe, :target :Bar}}, :nogood #{}, :children #{}},
                                        {:base :e16, :target :e5}
                                        {:emaps    #{{:base :Water, :target :Coffee}},
                                         :nogood   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                         :children #{{:base :Water, :target :Coffee}}},
                                        {:base :Vial, :target :Icecube}
                                        {:emaps #{{:base :Vial, :target :Icecube}}, :nogood #{}, :children #{}},
                                        {:base :e13, :target :e3}
                                        {:emaps    #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}},
                                         :nogood
                                         #{{:base :e7, :target :e2}
                                           {:base :e8, :target :e3}
                                           {:base :Water, :target :Coffee}
                                           {:base :e6, :target :e1}},
                                         :children #{{:base :e11, :target :e1} {:base :e12, :target :e2}}},
                                        {:base :e12, :target :e2}
                                        {:emaps    #{{:base :Vial, :target :Icecube}},
                                         :nogood   #{{:base :e7, :target :e2}},
                                         :children #{{:base :Vial, :target :Icecube}}}}
                                       ))

(def expected-combined-gmaps (undiff expected-computed-initial-gmaps
                               {:gmaps
                                '(#{{:mhs
                                     #{{:base :Water, :target :Heat}
                                       {:base :Beaker, :target :Coffee}
                                       {:base :e9, :target :e0}
                                       {:base :Pipe, :target :Bar}
                                       {:base :Vial, :target :Icecube}},
                                     :structure
                                     {:roots  #{{:base :e9, :target :e0}},
                                      :nogood #{{:base :Water, :target :Coffee}},
                                      :emaps
                                      #{{:base :Water, :target :Heat}
                                        {:base :Beaker, :target :Coffee}
                                        {:base :Pipe, :target :Bar}
                                        {:base :Vial, :target :Icecube}}}}
                                    {:mhs
                                     #{{:base :e11, :target :e1}
                                       {:base :Beaker, :target :Coffee}
                                       {:base :Vial, :target :Icecube}
                                       {:base :e13, :target :e3}
                                       {:base :e12, :target :e2}},
                                     :structure
                                     {:roots #{{:base :e13, :target :e3}},
                                      :nogood
                                      #{{:base :e7, :target :e2}
                                        {:base :e8, :target :e3}
                                        {:base :Water, :target :Coffee}
                                        {:base :e6, :target :e1}},
                                      :emaps
                                      #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}}}}}
                                   #{{:mhs
                                      #{{:base :Water, :target :Heat}
                                        {:base :Beaker, :target :Coffee}
                                        {:base :e9, :target :e0}
                                        {:base :Pipe, :target :Bar}
                                        {:base :Vial, :target :Icecube}},
                                      :structure
                                      {:roots  #{{:base :e9, :target :e0}},
                                       :nogood #{{:base :Water, :target :Coffee}},
                                       :emaps
                                       #{{:base :Water, :target :Heat}
                                         {:base :Beaker, :target :Coffee}
                                         {:base :Pipe, :target :Bar}
                                         {:base :Vial, :target :Icecube}}}}
                                     {:mhs
                                      #{{:base :e7, :target :e2}
                                        {:base :e8, :target :e3}
                                        {:base :Beaker, :target :Coffee}
                                        {:base :e6, :target :e1}
                                        {:base :Vial, :target :Icecube}},
                                      :structure
                                      {:roots #{{:base :e8, :target :e3}},
                                       :nogood
                                       #{{:base :e11, :target :e1}
                                         {:base :Water, :target :Coffee}
                                         {:base :e13, :target :e3}
                                         {:base :e12, :target :e2}},
                                       :emaps
                                       #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}}}}}
                                   #{{:mhs #{{:base :e15, :target :e4} {:base :Water, :target :Coffee}},
                                      :structure
                                      {:roots #{{:base :e15, :target :e4}},
                                       :nogood
                                       #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                       :emaps #{{:base :Water, :target :Coffee}}}}
                                     {:mhs #{{:base :Water, :target :Coffee} {:base :e16, :target :e5}},
                                      :structure
                                      {:roots #{{:base :e16, :target :e5}},
                                       :nogood
                                       #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                       :emaps #{{:base :Water, :target :Coffee}}}}})}
                               {:gmaps
                                #{{:mhs
                                   #{{:base :Water, :target :Heat}
                                     {:base :Beaker, :target :Coffee}
                                     {:base :e9, :target :e0}
                                     {:base :Pipe, :target :Bar}
                                     {:base :Vial, :target :Icecube}},
                                   :structure
                                   {:roots  #{{:base :e9, :target :e0}},
                                    :nogood #{{:base :Water, :target :Coffee}},
                                    :emaps
                                    #{{:base :Water, :target :Heat}
                                      {:base :Beaker, :target :Coffee}
                                      {:base :Pipe, :target :Bar}
                                      {:base :Vial, :target :Icecube}}}}
                                  {:mhs #{{:base :e15, :target :e4} {:base :Water, :target :Coffee}},
                                   :structure
                                   {:roots #{{:base :e15, :target :e4}},
                                    :nogood
                                    #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                    :emaps #{{:base :Water, :target :Coffee}}}}
                                  {:mhs
                                   #{{:base :e11, :target :e1}
                                     {:base :Beaker, :target :Coffee}
                                     {:base :Vial, :target :Icecube}
                                     {:base :e13, :target :e3}
                                     {:base :e12, :target :e2}},
                                   :structure
                                   {:roots #{{:base :e13, :target :e3}},
                                    :nogood
                                    #{{:base :e7, :target :e2}
                                      {:base :e8, :target :e3}
                                      {:base :Water, :target :Coffee}
                                      {:base :e6, :target :e1}},
                                    :emaps
                                    #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}}}}
                                  {:mhs #{{:base :Water, :target :Coffee} {:base :e16, :target :e5}},
                                   :structure
                                   {:roots #{{:base :e16, :target :e5}},
                                    :nogood
                                    #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                    :emaps #{{:base :Water, :target :Coffee}}}}
                                  {:mhs
                                   #{{:base :e7, :target :e2}
                                     {:base :e8, :target :e3}
                                     {:base :Beaker, :target :Coffee}
                                     {:base :e6, :target :e1}
                                     {:base :Vial, :target :Icecube}},
                                   :structure
                                   {:roots #{{:base :e8, :target :e3}},
                                    :nogood
                                    #{{:base :e11, :target :e1}
                                      {:base :Water, :target :Coffee}
                                      {:base :e13, :target :e3}
                                      {:base :e12, :target :e2}},
                                    :emaps
                                    #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}}}}}}))

(def expected-merged-gmaps (undiff expected-combined-gmaps
                             {:gmaps
                              [{:mhs
                                #{{:base :e11, :target :e1}
                                  {:base :Water, :target :Heat}
                                  {:base :Beaker, :target :Coffee}
                                  {:base :e9, :target :e0}
                                  {:base :Pipe, :target :Bar}
                                  {:base :Vial, :target :Icecube}
                                  {:base :e13, :target :e3}
                                  {:base :e12, :target :e2}},
                                :structure
                                {:roots #{{:base :e9, :target :e0} {:base :e13, :target :e3}},
                                 :nogood
                                 #{{:base :e7, :target :e2}
                                   {:base :e8, :target :e3}
                                   {:base :Water, :target :Coffee}
                                   {:base :e6, :target :e1}},
                                 :emaps
                                 #{{:base :Water, :target :Heat}
                                   {:base :Beaker, :target :Coffee}
                                   {:base :Pipe, :target :Bar}
                                   {:base :Vial, :target :Icecube}}}}
                               {:mhs
                                #{{:base :Water, :target :Heat}
                                  {:base :e7, :target :e2}
                                  {:base :e8, :target :e3}
                                  {:base :Beaker, :target :Coffee}
                                  {:base :e9, :target :e0}
                                  {:base :e6, :target :e1}
                                  {:base :Pipe, :target :Bar}
                                  {:base :Vial, :target :Icecube}},
                                :structure
                                {:roots #{{:base :e8, :target :e3} {:base :e9, :target :e0}},
                                 :nogood
                                 #{{:base :e11, :target :e1}
                                   {:base :Water, :target :Coffee}
                                   {:base :e13, :target :e3}
                                   {:base :e12, :target :e2}},
                                 :emaps
                                 #{{:base :Water, :target :Heat}
                                   {:base :Beaker, :target :Coffee}
                                   {:base :Pipe, :target :Bar}
                                   {:base :Vial, :target :Icecube}}}}
                               {:mhs
                                #{{:base :e15, :target :e4}
                                  {:base :Water, :target :Coffee}
                                  {:base :e16, :target :e5}},
                                :structure
                                {:roots  #{{:base :e15, :target :e4} {:base :e16, :target :e5}},
                                 :nogood #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                 :emaps  #{{:base :Water, :target :Coffee}}}}]}
                             {:gmaps
                              [#{{:mhs
                                  #{{:base :Water, :target :Heat}
                                    {:base :Beaker, :target :Coffee}
                                    {:base :e9, :target :e0}
                                    {:base :Pipe, :target :Bar}
                                    {:base :Vial, :target :Icecube}},
                                  :structure
                                  {:roots  #{{:base :e9, :target :e0}},
                                   :nogood #{{:base :Water, :target :Coffee}},
                                   :emaps
                                   #{{:base :Water, :target :Heat}
                                     {:base :Beaker, :target :Coffee}
                                     {:base :Pipe, :target :Bar}
                                     {:base :Vial, :target :Icecube}}}}
                                 {:mhs
                                  #{{:base :e11, :target :e1}
                                    {:base :Beaker, :target :Coffee}
                                    {:base :Vial, :target :Icecube}
                                    {:base :e13, :target :e3}
                                    {:base :e12, :target :e2}},
                                  :structure
                                  {:roots #{{:base :e13, :target :e3}},
                                   :nogood
                                   #{{:base :e7, :target :e2}
                                     {:base :e8, :target :e3}
                                     {:base :Water, :target :Coffee}
                                     {:base :e6, :target :e1}},
                                   :emaps
                                   #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}}}}}
                               #{{:mhs
                                  #{{:base :Water, :target :Heat}
                                    {:base :Beaker, :target :Coffee}
                                    {:base :e9, :target :e0}
                                    {:base :Pipe, :target :Bar}
                                    {:base :Vial, :target :Icecube}},
                                  :structure
                                  {:roots  #{{:base :e9, :target :e0}},
                                   :nogood #{{:base :Water, :target :Coffee}},
                                   :emaps
                                   #{{:base :Water, :target :Heat}
                                     {:base :Beaker, :target :Coffee}
                                     {:base :Pipe, :target :Bar}
                                     {:base :Vial, :target :Icecube}}}}
                                 {:mhs
                                  #{{:base :e7, :target :e2}
                                    {:base :e8, :target :e3}
                                    {:base :Beaker, :target :Coffee}
                                    {:base :e6, :target :e1}
                                    {:base :Vial, :target :Icecube}},
                                  :structure
                                  {:roots #{{:base :e8, :target :e3}},
                                   :nogood
                                   #{{:base :e11, :target :e1}
                                     {:base :Water, :target :Coffee}
                                     {:base :e13, :target :e3}
                                     {:base :e12, :target :e2}},
                                   :emaps
                                   #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}}}}}
                               #{{:mhs #{{:base :e15, :target :e4} {:base :Water, :target :Coffee}},
                                  :structure
                                  {:roots #{{:base :e15, :target :e4}},
                                   :nogood
                                   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                   :emaps #{{:base :Water, :target :Coffee}}}}
                                 {:mhs #{{:base :Water, :target :Coffee} {:base :e16, :target :e5}},
                                  :structure
                                  {:roots #{{:base :e16, :target :e5}},
                                   :nogood
                                   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                   :emaps #{{:base :Water, :target :Coffee}}}}}]}))

(def expected-gmap-score (undiff expected-merged-gmaps
                           {:mhs          #{{:base :e11, :target :e1}
                                            {:base :Water, :target :Heat}
                                            {:base :Beaker, :target :Coffee}
                                            {:base :e9, :target :e0}
                                            {:base :Pipe, :target :Bar}
                                            {:base :Vial, :target :Icecube}
                                            {:base :e13, :target :e3}
                                            {:base :e12, :target :e2}},
                            :structure    {:roots #{{:base :e9, :target :e0} {:base :e13, :target :e3}},
                                           :nogood
                                           #{{:base :e7, :target :e2}
                                             {:base :e8, :target :e3}
                                             {:base :Water, :target :Coffee}
                                             {:base :e6, :target :e1}},
                                           :emaps
                                           #{{:base :Water, :target :Heat}
                                             {:base :Beaker, :target :Coffee}
                                             {:base :Pipe, :target :Bar}
                                             {:base :Vial, :target :Icecube}}},
                            :score        18,
                            :emap-matches 0}
                           {:mh-structure {{:base :e11, :target :e1}
                                           {:emaps    #{{:base :Beaker, :target :Coffee}},
                                            :nogood   #{{:base :Water, :target :Coffee} {:base :e6, :target :e1}},
                                            :children #{{:base :Beaker, :target :Coffee}}},
                                           {:base :e15, :target :e4}
                                           {:emaps    #{{:base :Water, :target :Coffee}},
                                            :nogood   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                            :children #{{:base :Water, :target :Coffee}}},
                                           {:base :Water, :target :Heat}
                                           {:emaps    #{{:base :Water, :target :Heat}},
                                            :nogood   #{{:base :Water, :target :Coffee}},
                                            :children #{}},
                                           {:base :e7, :target :e2}
                                           {:emaps    #{{:base :Vial, :target :Icecube}},
                                            :nogood   #{{:base :e12, :target :e2}},
                                            :children #{{:base :Vial, :target :Icecube}}},
                                           {:base :e8, :target :e3}
                                           {:emaps    #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}},
                                            :nogood
                                            #{{:base :e11, :target :e1}
                                              {:base :Water, :target :Coffee}
                                              {:base :e13, :target :e3}
                                              {:base :e12, :target :e2}},
                                            :children #{{:base :e7, :target :e2} {:base :e6, :target :e1}}},
                                           {:base :Beaker, :target :Coffee}
                                           {:emaps    #{{:base :Beaker, :target :Coffee}},
                                            :nogood   #{{:base :Water, :target :Coffee}},
                                            :children #{}},
                                           {:base :Water, :target :Coffee}
                                           {:emaps    #{{:base :Water, :target :Coffee}},
                                            :nogood   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                            :children #{}},
                                           {:base :e9, :target :e0}
                                           {:emaps
                                            #{{:base :Water, :target :Heat}
                                              {:base :Beaker, :target :Coffee}
                                              {:base :Pipe, :target :Bar}
                                              {:base :Vial, :target :Icecube}},
                                            :nogood #{{:base :Water, :target :Coffee}},
                                            :children
                                            #{{:base :Water, :target :Heat}
                                              {:base :Beaker, :target :Coffee}
                                              {:base :Pipe, :target :Bar}
                                              {:base :Vial, :target :Icecube}}},
                                           {:base :e6, :target :e1}
                                           {:emaps    #{{:base :Beaker, :target :Coffee}},
                                            :nogood   #{{:base :e11, :target :e1} {:base :Water, :target :Coffee}},
                                            :children #{{:base :Beaker, :target :Coffee}}},
                                           {:base :Pipe, :target :Bar}
                                           {:emaps #{{:base :Pipe, :target :Bar}}, :nogood #{}, :children #{}},
                                           {:base :e16, :target :e5}
                                           {:emaps    #{{:base :Water, :target :Coffee}},
                                            :nogood   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                            :children #{{:base :Water, :target :Coffee}}},
                                           {:base :Vial, :target :Icecube}
                                           {:emaps #{{:base :Vial, :target :Icecube}}, :nogood #{}, :children #{}},
                                           {:base :e13, :target :e3}
                                           {:emaps    #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}},
                                            :nogood
                                            #{{:base :e7, :target :e2}
                                              {:base :e8, :target :e3}
                                              {:base :Water, :target :Coffee}
                                              {:base :e6, :target :e1}},
                                            :children #{{:base :e11, :target :e1} {:base :e12, :target :e2}}},
                                           {:base :e12, :target :e2}
                                           {:emaps    #{{:base :Vial, :target :Icecube}},
                                            :nogood   #{{:base :e7, :target :e2}},
                                            :children #{{:base :Vial, :target :Icecube}}}},
                            :gmaps        '({:mhs
                                             #{{:base :e11, :target :e1}
                                               {:base :Water, :target :Heat}
                                               {:base :Beaker, :target :Coffee}
                                               {:base :e9, :target :e0}
                                               {:base :Pipe, :target :Bar}
                                               {:base :Vial, :target :Icecube}
                                               {:base :e13, :target :e3}
                                               {:base :e12, :target :e2}},
                                             :structure
                                             {:roots #{{:base :e9, :target :e0} {:base :e13, :target :e3}},
                                              :nogood
                                              #{{:base :e7, :target :e2}
                                                {:base :e8, :target :e3}
                                                {:base :Water, :target :Coffee}
                                                {:base :e6, :target :e1}},
                                              :emaps
                                              #{{:base :Water, :target :Heat}
                                                {:base :Beaker, :target :Coffee}
                                                {:base :Pipe, :target :Bar}
                                                {:base :Vial, :target :Icecube}}}}
                                            {:mhs
                                             #{{:base :Water, :target :Heat}
                                               {:base :e7, :target :e2}
                                               {:base :e8, :target :e3}
                                               {:base :Beaker, :target :Coffee}
                                               {:base :e9, :target :e0}
                                               {:base :e6, :target :e1}
                                               {:base :Pipe, :target :Bar}
                                               {:base :Vial, :target :Icecube}},
                                             :structure
                                             {:roots #{{:base :e8, :target :e3} {:base :e9, :target :e0}},
                                              :nogood
                                              #{{:base :e11, :target :e1}
                                                {:base :Water, :target :Coffee}
                                                {:base :e13, :target :e3}
                                                {:base :e12, :target :e2}},
                                              :emaps
                                              #{{:base :Water, :target :Heat}
                                                {:base :Beaker, :target :Coffee}
                                                {:base :Pipe, :target :Bar}
                                                {:base :Vial, :target :Icecube}}}}
                                            {:mhs
                                             #{{:base :e15, :target :e4}
                                               {:base :Water, :target :Coffee}
                                               {:base :e16, :target :e5}},
                                             :structure
                                             {:roots  #{{:base :e15, :target :e4} {:base :e16, :target :e5}},
                                              :nogood #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                              :emaps  #{{:base :Water, :target :Coffee}}}})}))


(def expected-finalized-gmaps (undiff expected-gmap-score
                                {:mh-structure {{:base :e11, :target :e1}
                                                {:emaps    #{{:base :Beaker, :target :Coffee}},
                                                 :nogood   #{{:base :Water, :target :Coffee} {:base :e6, :target :e1}},
                                                 :children #{{:base :Beaker, :target :Coffee}}},
                                                {:base :e15, :target :e4}
                                                {:emaps    #{{:base :Water, :target :Coffee}},
                                                 :nogood   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                                 :children #{{:base :Water, :target :Coffee}}},
                                                {:base :Water, :target :Heat}
                                                {:emaps    #{{:base :Water, :target :Heat}},
                                                 :nogood   #{{:base :Water, :target :Coffee}},
                                                 :children #{}},
                                                {:base :e7, :target :e2}
                                                {:emaps    #{{:base :Vial, :target :Icecube}},
                                                 :nogood   #{{:base :e12, :target :e2}},
                                                 :children #{{:base :Vial, :target :Icecube}}},
                                                {:base :e8, :target :e3}
                                                {:emaps    #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}},
                                                 :nogood
                                                 #{{:base :e11, :target :e1}
                                                   {:base :Water, :target :Coffee}
                                                   {:base :e13, :target :e3}
                                                   {:base :e12, :target :e2}},
                                                 :children #{{:base :e7, :target :e2} {:base :e6, :target :e1}}},
                                                {:base :Beaker, :target :Coffee}
                                                {:emaps    #{{:base :Beaker, :target :Coffee}},
                                                 :nogood   #{{:base :Water, :target :Coffee}},
                                                 :children #{}},
                                                {:base :Water, :target :Coffee}
                                                {:emaps    #{{:base :Water, :target :Coffee}},
                                                 :nogood   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                                 :children #{}},
                                                {:base :e9, :target :e0}
                                                {:emaps
                                                 #{{:base :Water, :target :Heat}
                                                   {:base :Beaker, :target :Coffee}
                                                   {:base :Pipe, :target :Bar}
                                                   {:base :Vial, :target :Icecube}},
                                                 :nogood #{{:base :Water, :target :Coffee}},
                                                 :children
                                                 #{{:base :Water, :target :Heat}
                                                   {:base :Beaker, :target :Coffee}
                                                   {:base :Pipe, :target :Bar}
                                                   {:base :Vial, :target :Icecube}}},
                                                {:base :e6, :target :e1}
                                                {:emaps    #{{:base :Beaker, :target :Coffee}},
                                                 :nogood   #{{:base :e11, :target :e1} {:base :Water, :target :Coffee}},
                                                 :children #{{:base :Beaker, :target :Coffee}}},
                                                {:base :Pipe, :target :Bar}
                                                {:emaps #{{:base :Pipe, :target :Bar}}, :nogood #{}, :children #{}},
                                                {:base :e16, :target :e5}
                                                {:emaps    #{{:base :Water, :target :Coffee}},
                                                 :nogood   #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                                 :children #{{:base :Water, :target :Coffee}}},
                                                {:base :Vial, :target :Icecube}
                                                {:emaps #{{:base :Vial, :target :Icecube}}, :nogood #{}, :children #{}},
                                                {:base :e13, :target :e3}
                                                {:emaps    #{{:base :Beaker, :target :Coffee} {:base :Vial, :target :Icecube}},
                                                 :nogood
                                                 #{{:base :e7, :target :e2}
                                                   {:base :e8, :target :e3}
                                                   {:base :Water, :target :Coffee}
                                                   {:base :e6, :target :e1}},
                                                 :children #{{:base :e11, :target :e1} {:base :e12, :target :e2}}},
                                                {:base :e12, :target :e2}
                                                {:emaps    #{{:base :Vial, :target :Icecube}},
                                                 :nogood   #{{:base :e7, :target :e2}},
                                                 :children #{{:base :Vial, :target :Icecube}}}},
                                 :gmaps        '({:mhs
                                                  #{{:base :e11, :target :e1}
                                                    {:base :Water, :target :Heat}
                                                    {:base :Beaker, :target :Coffee}
                                                    {:base :e9, :target :e0}
                                                    {:base :Pipe, :target :Bar}
                                                    {:base :Vial, :target :Icecube}
                                                    {:base :e13, :target :e3}
                                                    {:base :e12, :target :e2}},
                                                  :structure
                                                  {:roots #{{:base :e9, :target :e0} {:base :e13, :target :e3}},
                                                   :nogood
                                                   #{{:base :e7, :target :e2}
                                                     {:base :e8, :target :e3}
                                                     {:base :Water, :target :Coffee}
                                                     {:base :e6, :target :e1}},
                                                   :emaps
                                                   #{{:base :Water, :target :Heat}
                                                     {:base :Beaker, :target :Coffee}
                                                     {:base :Pipe, :target :Bar}
                                                     {:base :Vial, :target :Icecube}}},
                                                  :score        18,
                                                  :emap-matches 0,
                                                  :mapping
                                                  {:base
                                                   {:name :simple-water-flow,
                                                    :graph
                                                    {:e16 {:name :e16, :type :expression, :functor :liquid, :args (:Water)},
                                                     :e10 {:name :e10, :type :expression, :functor :cause, :args (:e8 :e9)},
                                                     :e8  {:name :e8, :type :expression, :functor :greater, :args (:e6 :e7)},
                                                     :e6  {:name :e6, :type :expression, :functor :pressure, :args (:Beaker)},
                                                     :e12 {:name :e12, :type :expression, :functor :diameter, :args (:Vial)},
                                                     :e9
                                                     {:name    :e9,
                                                      :type    :expression,
                                                      :functor :flow,
                                                      :args    (:Beaker :Vial :Water :Pipe)},
                                                     :e14 {:name :e14, :type :expression, :functor :clear, :args (:Beaker)},
                                                     :e11
                                                     {:name :e11, :type :expression, :functor :diameter, :args (:Beaker)},
                                                     :e13
                                                     {:name :e13, :type :expression, :functor :greater, :args (:e11 :e12)},
                                                     :e15
                                                     {:name :e15, :type :expression, :functor :flat-top, :args (:Water)},
                                                     :e7  {:name :e7, :type :expression, :functor :pressure, :args (:Vial)}},
                                                    :spec
                                                    ([:cause
                                                      [:greater [:pressure :Beaker] [:pressure :Vial]]
                                                      [:flow :Beaker :Vial :Water :Pipe]]
                                                     [:greater [:diameter :Beaker] [:diameter :Vial]]
                                                     [:clear :Beaker]
                                                     [:flat-top :Water]
                                                     [:liquid :Water])},
                                                   :target
                                                   {:name :simple-heat-flow,
                                                    :graph
                                                    {:e0
                                                     {:name    :e0,
                                                      :type    :expression,
                                                      :functor :flow,
                                                      :args    (:Coffee :Icecube :Heat :Bar)},
                                                     :e1
                                                     {:name :e1, :type :expression, :functor :temperature, :args (:Coffee)},
                                                     :e2
                                                     {:name :e2, :type :expression, :functor :temperature, :args (:Icecube)},
                                                     :e3 {:name :e3, :type :expression, :functor :greater, :args (:e1 :e2)},
                                                     :e4 {:name :e4, :type :expression, :functor :flat-top, :args (:Coffee)},
                                                     :e5 {:name :e5, :type :expression, :functor :liquid, :args (:Coffee)}},
                                                    :spec
                                                    ([:flow :Coffee :Icecube :Heat :Bar]
                                                     [:greater [:temperature :Coffee] [:temperature :Icecube]]
                                                     [:flat-top :Coffee]
                                                     [:liquid :Coffee])}}}
                                                 {:mhs
                                                  #{{:base :Water, :target :Heat}
                                                    {:base :e7, :target :e2}
                                                    {:base :e8, :target :e3}
                                                    {:base :Beaker, :target :Coffee}
                                                    {:base :e9, :target :e0}
                                                    {:base :e6, :target :e1}
                                                    {:base :Pipe, :target :Bar}
                                                    {:base :Vial, :target :Icecube}},
                                                  :structure
                                                  {:roots #{{:base :e8, :target :e3} {:base :e9, :target :e0}},
                                                   :nogood
                                                   #{{:base :e11, :target :e1}
                                                     {:base :Water, :target :Coffee}
                                                     {:base :e13, :target :e3}
                                                     {:base :e12, :target :e2}},
                                                   :emaps
                                                   #{{:base :Water, :target :Heat}
                                                     {:base :Beaker, :target :Coffee}
                                                     {:base :Pipe, :target :Bar}
                                                     {:base :Vial, :target :Icecube}}},
                                                  :score        18,
                                                  :emap-matches 0,
                                                  :mapping
                                                  {:base
                                                   {:name :simple-water-flow,
                                                    :graph
                                                    {:e16 {:name :e16, :type :expression, :functor :liquid, :args (:Water)},
                                                     :e10 {:name :e10, :type :expression, :functor :cause, :args (:e8 :e9)},
                                                     :e8  {:name :e8, :type :expression, :functor :greater, :args (:e6 :e7)},
                                                     :e6  {:name :e6, :type :expression, :functor :pressure, :args (:Beaker)},
                                                     :e12 {:name :e12, :type :expression, :functor :diameter, :args (:Vial)},
                                                     :e9
                                                     {:name    :e9,
                                                      :type    :expression,
                                                      :functor :flow,
                                                      :args    (:Beaker :Vial :Water :Pipe)},
                                                     :e14 {:name :e14, :type :expression, :functor :clear, :args (:Beaker)},
                                                     :e11
                                                     {:name :e11, :type :expression, :functor :diameter, :args (:Beaker)},
                                                     :e13
                                                     {:name :e13, :type :expression, :functor :greater, :args (:e11 :e12)},
                                                     :e15
                                                     {:name :e15, :type :expression, :functor :flat-top, :args (:Water)},
                                                     :e7  {:name :e7, :type :expression, :functor :pressure, :args (:Vial)}},
                                                    :spec
                                                    ([:cause
                                                      [:greater [:pressure :Beaker] [:pressure :Vial]]
                                                      [:flow :Beaker :Vial :Water :Pipe]]
                                                     [:greater [:diameter :Beaker] [:diameter :Vial]]
                                                     [:clear :Beaker]
                                                     [:flat-top :Water]
                                                     [:liquid :Water])},
                                                   :target
                                                   {:name :simple-heat-flow,
                                                    :graph
                                                    {:e0
                                                     {:name    :e0,
                                                      :type    :expression,
                                                      :functor :flow,
                                                      :args    (:Coffee :Icecube :Heat :Bar)},
                                                     :e1
                                                     {:name :e1, :type :expression, :functor :temperature, :args (:Coffee)},
                                                     :e2
                                                     {:name :e2, :type :expression, :functor :temperature, :args (:Icecube)},
                                                     :e3 {:name :e3, :type :expression, :functor :greater, :args (:e1 :e2)},
                                                     :e4 {:name :e4, :type :expression, :functor :flat-top, :args (:Coffee)},
                                                     :e5 {:name :e5, :type :expression, :functor :liquid, :args (:Coffee)}},
                                                    :spec
                                                    ([:flow :Coffee :Icecube :Heat :Bar]
                                                     [:greater [:temperature :Coffee] [:temperature :Icecube]]
                                                     [:flat-top :Coffee]
                                                     [:liquid :Coffee])}}}
                                                 {:mhs
                                                  #{{:base :e15, :target :e4}
                                                    {:base :Water, :target :Coffee}
                                                    {:base :e16, :target :e5}},
                                                  :structure
                                                  {:roots  #{{:base :e15, :target :e4} {:base :e16, :target :e5}},
                                                   :nogood #{{:base :Water, :target :Heat} {:base :Beaker, :target :Coffee}},
                                                   :emaps  #{{:base :Water, :target :Coffee}}},
                                                  :score        5,
                                                  :emap-matches 0,
                                                  :mapping
                                                  {:base
                                                   {:name :simple-water-flow,
                                                    :graph
                                                    {:e16 {:name :e16, :type :expression, :functor :liquid, :args (:Water)},
                                                     :e10 {:name :e10, :type :expression, :functor :cause, :args (:e8 :e9)},
                                                     :e8  {:name :e8, :type :expression, :functor :greater, :args (:e6 :e7)},
                                                     :e6  {:name :e6, :type :expression, :functor :pressure, :args (:Beaker)},
                                                     :e12 {:name :e12, :type :expression, :functor :diameter, :args (:Vial)},
                                                     :e9
                                                     {:name    :e9,
                                                      :type    :expression,
                                                      :functor :flow,
                                                      :args    (:Beaker :Vial :Water :Pipe)},
                                                     :e14 {:name :e14, :type :expression, :functor :clear, :args (:Beaker)},
                                                     :e11
                                                     {:name :e11, :type :expression, :functor :diameter, :args (:Beaker)},
                                                     :e13
                                                     {:name :e13, :type :expression, :functor :greater, :args (:e11 :e12)},
                                                     :e15
                                                     {:name :e15, :type :expression, :functor :flat-top, :args (:Water)},
                                                     :e7  {:name :e7, :type :expression, :functor :pressure, :args (:Vial)}},
                                                    :spec
                                                    ([:cause
                                                      [:greater [:pressure :Beaker] [:pressure :Vial]]
                                                      [:flow :Beaker :Vial :Water :Pipe]]
                                                     [:greater [:diameter :Beaker] [:diameter :Vial]]
                                                     [:clear :Beaker]
                                                     [:flat-top :Water]
                                                     [:liquid :Water])},
                                                   :target
                                                   {:name :simple-heat-flow,
                                                    :graph
                                                    {:e0
                                                     {:name    :e0,
                                                      :type    :expression,
                                                      :functor :flow,
                                                      :args    (:Coffee :Icecube :Heat :Bar)},
                                                     :e1
                                                     {:name :e1, :type :expression, :functor :temperature, :args (:Coffee)},
                                                     :e2
                                                     {:name :e2, :type :expression, :functor :temperature, :args (:Icecube)},
                                                     :e3 {:name :e3, :type :expression, :functor :greater, :args (:e1 :e2)},
                                                     :e4 {:name :e4, :type :expression, :functor :flat-top, :args (:Coffee)},
                                                     :e5 {:name :e5, :type :expression, :functor :liquid, :args (:Coffee)}},
                                                    :spec
                                                    ([:flow :Coffee :Icecube :Heat :Bar]
                                                     [:greater [:temperature :Coffee] [:temperature :Icecube]]
                                                     [:flat-top :Coffee]
                                                     [:liquid :Coffee])}}})}
                                {:mhs
                                 #{{:base :e11, :target :e1}
                                   {:base :Water, :target :Heat}
                                   {:base :Beaker, :target :Coffee}
                                   {:base :e9, :target :e0}
                                   {:base :Pipe, :target :Bar}
                                   {:base :Vial, :target :Icecube}
                                   {:base :e13, :target :e3}
                                   {:base :e12, :target :e2}},
                                 :structure
                                 {:roots #{{:base :e9, :target :e0} {:base :e13, :target :e3}},
                                  :nogood
                                  #{{:base :e7, :target :e2}
                                    {:base :e8, :target :e3}
                                    {:base :Water, :target :Coffee}
                                    {:base :e6, :target :e1}},
                                  :emaps
                                  #{{:base :Water, :target :Heat}
                                    {:base :Beaker, :target :Coffee}
                                    {:base :Pipe, :target :Bar}
                                    {:base :Vial, :target :Icecube}}},
                                 :score        18,
                                 :emap-matches 0}))

(def expected-generated-inferences #_::fail (undiff expected-finalized-gmaps {:gmaps [{:inferences #{}} {:inferences #{}} {:inferences #{}}]} nil))

(def expected-transferred-inferences #_::fail (undiff expected-generated-inferences
                                                {:gmaps [{:transferred ()} {:transferred ()} {:transferred ()}]}
                                                nil))

#_(data/diff expected-transferred-inferences (SUT/match kg rules/literal-similarity simple-water-flow simple-heat-flow))

(def expected-transferred ::fail)
(def expected-matches ::fail)

(defn remove-mapping [result]
  (update result :gmaps (partial map #(dissoc % :mapping))))

(deftest heat-water-test
  ;; Water flow is the base, heat flow the target

  (is (=
        expected-match-hypotheses
        (->> rules/literal-similarity
          (SUT/create-match-hypotheses kg simple-water-flow simple-heat-flow)
          (map #(into {} %))
          set)))

  (is (=
        expected-hypothesis-structure
        (SUT/build-hypothesis-structure kg expected-match-hypotheses)))

  (is (=
        expected-propagated-from-emaps
        (SUT/propagate-from-emaps expected-hypothesis-structure)))

  (is (=
        expected-computed-initial-gmaps
        (SUT/compute-initial-gmaps kg expected-propagated-from-emaps)))

  (is (=
        expected-combined-gmaps
        (SUT/combine-gmaps expected-computed-initial-gmaps)))

  (is (=
        expected-merged-gmaps
        (SUT/merge-gmaps expected-combined-gmaps)))


  (is (=
        '({:base :Water, :target :Water})
        (filter (fn [{:keys [base target]}] (SUT/emaps-equal?
                                             (get kg base)
                                             (get kg target)))
          '({:base :Water, :target :Water}
            {:base :Beaker, :target :Coffee}
            {:base :Pipe, :target :Bar}
            {:base :Vial, :target :Icecube}))))

  (is (=
        '({:base :Water, :target :Heat}
          {:base :Beaker, :target :Coffee}
          {:base :Pipe, :target :Bar}
          {:base :Vial, :target :Icecube})
        (filter (partial SUT/is-emap? kg) (-> expected-merged-gmaps :gmaps first :mhs))))

  ;;Fails for now
  #_(is (=
          ::fail
          (SUT/matching-emaps kg (-> expected-merged-gmaps :gmaps first))))

  (is (=
        expected-gmap-score
        (SUT/score-gmap kg expected-merged-gmaps (-> expected-merged-gmaps :gmaps first))))

  (is (=
        (remove-mapping expected-finalized-gmaps)
        (-> (SUT/finalize-gmaps kg simple-water-flow simple-heat-flow expected-merged-gmaps)
          remove-mapping)))

  (is (=
        expected-generated-inferences
        (SUT/generate-inferences kg simple-water-flow expected-finalized-gmaps)
        ))

  (is (=
        expected-transferred-inferences
        (SUT/transfer-inferences kg expected-generated-inferences)
        ))

  ;; Should show the cause relation between the greater temperature
  ;; relation and the heat flow relation. This relation has been inferred
  ;; based on the analogical cause relation in the water flow graph.
  #_(is (= expected-transferred (-> (SUT/match kg rules/literal-similarity simple-water-flow simple-heat-flow)
                                  :gmaps
                                  first
                                  :transferred)))

  (is (= expected-transferred-inferences (SUT/match kg rules/literal-similarity simple-water-flow simple-heat-flow)))
  (data/diff expected-transferred-inferences (SUT/match kg rules/literal-similarity simple-water-flow simple-heat-flow)))
