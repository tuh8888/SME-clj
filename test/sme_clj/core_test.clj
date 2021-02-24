(ns sme-clj.core-test
  (:require [clojure.data :as data]
            [clojure.set :as set]
            [clojure.test :refer [deftest is]]
            [mop-records :as mr]
            [mops :as mops]
            [sme-clj.core :as SUT]
            [sme-clj.ruledef :as rules]
            [sme-clj.typedef :as types]
            [sme-clj.util :as util]))

#_(= new-var (apply undiff old-var (take 2 (data/diff new-var old-var))))
(defn- seqzip
  "returns a sequence of [[ value-left] [value-right]....]  padding with nulls for shorter sequences "
  [left right]
  (loop [list [] a left b right]
    (if (or (seq a) (seq b))
      (recur (conj list [(first a) (first b)] ) (rest a) (rest b))
      list)))

(defn- recursive-diff-merge
  " Merge two structures recusively  taking non-nil values from sequences and maps and merging sets"
  [part-state original-state]
  (cond
    (sequential? part-state) (map (fn [[l r]] (recursive-diff-merge l r)) (seqzip part-state original-state))
    (map? part-state)        (merge-with recursive-diff-merge part-state original-state)
    (set? part-state)        (set/union part-state original-state)
    (nil? part-state )       original-state
    :default                 part-state))

(defn undiff
  "returns the state of x after reversing the changes described by a diff against
   an earlier state (where before and after are the first two elements of the diff)"
  [x before after]
  (let [[a _ _] (clojure.data/diff x after)]
    (recursive-diff-merge a before)))


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

(def simple-water-flow (types/make-concept-graph (keyword "simple-water-flow")
                         [:cause
                          [:greater [:pressure :Beaker] :Vial #_[:pressure :Vial]]
                          [:flow :Beaker :Vial :Water #_:Pipe]]
                         #_[:greater [:diameter :Beaker] [:diameter :Vial]]
                         #_[:clear :Beaker]
                         #_[:flat-top :Water]
                         #_[:liquid :Water]))

(def simple-heat-flow (types/make-concept-graph (keyword "simple-heat-flow")
                        [:flow :Coffee :Icecube :Heat #_:Bar]
                        [:greater [:temperature :Coffee] :Icecube #_[:temperature :Icecube]]
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


(def kg (merge-with
          (fn [v1 v2]
            {:help (vector v1 v2)})
          entity-map
          predicate-map
          (:graph simple-heat-flow)
          (:graph simple-water-flow)))

(def expected-match-hypotheses #{{:base   :Water
                                  :target :Heat}
                                 {:base   :flow-Beaker-Vial-Water-Pipe
                                  :target :flow-Coffee-Icecube-Heat-Bar}
                                 {:base   :greater-diameter-Beaker-diameter-Vial
                                  :target :greater-temperature-Coffee-temperature-Icecube}
                                 {:base   :diameter-Beaker
                                  :target :temperature-Coffee}
                                 {:base   :greater-pressure-Beaker-pressure-Vial
                                  :target :greater-temperature-Coffee-temperature-Icecube}
                                 {:base   :Beaker
                                  :target :Coffee}
                                 {:base   :Water
                                  :target :Coffee}
                                 {:base   :pressure-Vial
                                  :target :temperature-Icecube}
                                 {:base   :pressure-Beaker
                                  :target :temperature-Coffee}
                                 {:base   :diameter-Vial
                                  :target :temperature-Icecube}
                                 {:base   :flat-top-Water
                                  :target :flat-top-Coffee}
                                 {:base   :Pipe
                                  :target :Bar}
                                 {:base   :Vial
                                  :target :Icecube}
                                 {:base   :liquid-Water
                                  :target :liquid-Coffee}})

(def expected-hypothesis-structure {{:base :Water :target :Heat}
                                    {:emaps    #{{:base :Water :target :Heat}}
                                     :nogood   #{{:base :Water :target :Coffee}}
                                     :children #{}}
                                    {:base :flow-Beaker-Vial-Water-Pipe :target :flow-Coffee-Icecube-Heat-Bar}
                                    {:emaps  #{}
                                     :nogood #{}
                                     :children
                                     #{{:base :Water :target :Heat}
                                       {:base :Beaker :target :Coffee}
                                       {:base :Pipe :target :Bar}
                                       {:base :Vial :target :Icecube}}}
                                    {:base   :greater-diameter-Beaker-diameter-Vial
                                     :target :greater-temperature-Coffee-temperature-Icecube}
                                    {:emaps #{}
                                     :nogood
                                     #{{:base   :greater-pressure-Beaker-pressure-Vial
                                        :target :greater-temperature-Coffee-temperature-Icecube}}
                                     :children
                                     #{{:base :diameter-Beaker :target :temperature-Coffee}
                                       {:base :diameter-Vial :target :temperature-Icecube}}}
                                    {:base :diameter-Beaker :target :temperature-Coffee}
                                    {:emaps    #{}
                                     :nogood   #{{:base :pressure-Beaker :target :temperature-Coffee}}
                                     :children #{{:base :Beaker :target :Coffee}}}
                                    {:base   :greater-pressure-Beaker-pressure-Vial
                                     :target :greater-temperature-Coffee-temperature-Icecube}
                                    {:emaps #{}
                                     :nogood
                                     #{{:base   :greater-diameter-Beaker-diameter-Vial
                                        :target :greater-temperature-Coffee-temperature-Icecube}}
                                     :children
                                     #{{:base :pressure-Vial :target :temperature-Icecube}
                                       {:base :pressure-Beaker :target :temperature-Coffee}}}
                                    {:base :Beaker :target :Coffee}
                                    {:emaps    #{{:base :Beaker :target :Coffee}}
                                     :nogood   #{{:base :Water :target :Coffee}}
                                     :children #{}}
                                    {:base :Water :target :Coffee}
                                    {:emaps    #{{:base :Water :target :Coffee}}
                                     :nogood   #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                     :children #{}}
                                    {:base :pressure-Vial :target :temperature-Icecube}
                                    {:emaps    #{}
                                     :nogood   #{{:base :diameter-Vial :target :temperature-Icecube}}
                                     :children #{{:base :Vial :target :Icecube}}}
                                    {:base :pressure-Beaker :target :temperature-Coffee}
                                    {:emaps    #{}
                                     :nogood   #{{:base :diameter-Beaker :target :temperature-Coffee}}
                                     :children #{{:base :Beaker :target :Coffee}}}
                                    {:base :diameter-Vial :target :temperature-Icecube}
                                    {:emaps    #{}
                                     :nogood   #{{:base :pressure-Vial :target :temperature-Icecube}}
                                     :children #{{:base :Vial :target :Icecube}}}
                                    {:base :flat-top-Water :target :flat-top-Coffee}
                                    {:emaps #{} :nogood #{} :children #{{:base :Water :target :Coffee}}}
                                    {:base :Pipe :target :Bar}
                                    {:emaps #{{:base :Pipe :target :Bar}} :nogood #{} :children #{}}
                                    {:base :Vial :target :Icecube}
                                    {:emaps #{{:base :Vial :target :Icecube}} :nogood #{} :children #{}}
                                    {:base :liquid-Water :target :liquid-Coffee}
                                    {:emaps #{} :nogood #{} :children #{{:base :Water :target :Coffee}}}})


(def expected-propagated-from-emaps (undiff expected-hypothesis-structure
                                      {{:base :flow-Beaker-Vial-Water-Pipe :target :flow-Coffee-Icecube-Heat-Bar}
                                       {:nogood #{{:base :Water :target :Coffee}}
                                        :emaps
                                        #{{:base :Water :target :Heat}
                                          {:base :Beaker :target :Coffee}
                                          {:base :Pipe :target :Bar}
                                          {:base :Vial :target :Icecube}}}
                                       {:base   :greater-diameter-Beaker-diameter-Vial
                                        :target :greater-temperature-Coffee-temperature-Icecube}
                                       {:nogood
                                        #{{:base :Water :target :Coffee}
                                          {:base :pressure-Vial :target :temperature-Icecube}
                                          {:base :pressure-Beaker :target :temperature-Coffee}}
                                        :emaps #{{:base :Beaker :target :Coffee} {:base :Vial :target :Icecube}}}
                                       {:base :diameter-Beaker :target :temperature-Coffee}
                                       {:nogood #{{:base :Water :target :Coffee}}
                                        :emaps  #{{:base :Beaker :target :Coffee}}}
                                       {:base   :greater-pressure-Beaker-pressure-Vial
                                        :target :greater-temperature-Coffee-temperature-Icecube}
                                       {:nogood
                                        #{{:base :diameter-Beaker :target :temperature-Coffee}
                                          {:base :Water :target :Coffee}
                                          {:base :diameter-Vial :target :temperature-Icecube}}
                                        :emaps #{{:base :Beaker :target :Coffee} {:base :Vial :target :Icecube}}}
                                       {:base :pressure-Vial :target :temperature-Icecube}
                                       {:emaps #{{:base :Vial :target :Icecube}}}
                                       {:base :pressure-Beaker :target :temperature-Coffee}
                                       {:nogood #{{:base :Water :target :Coffee}}
                                        :emaps  #{{:base :Beaker :target :Coffee}}}
                                       {:base :diameter-Vial :target :temperature-Icecube}
                                       {:emaps #{{:base :Vial :target :Icecube}}}
                                       {:base :flat-top-Water :target :flat-top-Coffee}
                                       {:nogood #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                        :emaps  #{{:base :Water :target :Coffee}}}
                                       {:base :liquid-Water :target :liquid-Coffee}
                                       {:nogood #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                        :emaps  #{{:base :Water :target :Coffee}}}}
                                      {{:base :flow-Beaker-Vial-Water-Pipe :target :flow-Coffee-Icecube-Heat-Bar}
                                       {:nogood nil :emaps nil}
                                       {:base   :greater-diameter-Beaker-diameter-Vial
                                        :target :greater-temperature-Coffee-temperature-Icecube}
                                       {:emaps nil}
                                       {:base :diameter-Beaker :target :temperature-Coffee} {:emaps nil}
                                       {:base   :greater-pressure-Beaker-pressure-Vial
                                        :target :greater-temperature-Coffee-temperature-Icecube}
                                       {:emaps nil}
                                       {:base :pressure-Vial :target :temperature-Icecube}  {:emaps nil}
                                       {:base :pressure-Beaker :target :temperature-Coffee} {:emaps nil}
                                       {:base :diameter-Vial :target :temperature-Icecube}  {:emaps nil}
                                       {:base :flat-top-Water :target :flat-top-Coffee}     {:nogood nil :emaps nil}
                                       {:base :liquid-Water :target :liquid-Coffee}         {:nogood nil :emaps nil}}))


(take 2 (data/diff {:gmaps
                    #{{:mhs
                       #{{:base :Water :target :Heat}
                         {:base   :flow-Beaker-Vial-Water-Pipe
                          :target :flow-Coffee-Icecube-Heat-Bar}
                         {:base :Beaker :target :Coffee}
                         {:base :Pipe :target :Bar}
                         {:base :Vial :target :Icecube}}
                       :structure
                       {:roots
                        #{{:base   :flow-Beaker-Vial-Water-Pipe
                           :target :flow-Coffee-Icecube-Heat-Bar}}
                        :nogood #{{:base :Water :target :Coffee}}
                        :emaps
                        #{{:base :Water :target :Heat}
                          {:base :Beaker :target :Coffee}
                          {:base :Pipe :target :Bar}
                          {:base :Vial :target :Icecube}}}}
                      {:mhs
                       #{{:base   :greater-diameter-Beaker-diameter-Vial
                          :target :greater-temperature-Coffee-temperature-Icecube}
                         {:base :diameter-Beaker :target :temperature-Coffee}
                         {:base :Beaker :target :Coffee}
                         {:base :diameter-Vial :target :temperature-Icecube}
                         {:base :Vial :target :Icecube}}
                       :structure
                       {:roots
                        #{{:base   :greater-diameter-Beaker-diameter-Vial
                           :target :greater-temperature-Coffee-temperature-Icecube}}
                        :nogood
                        #{{:base   :greater-pressure-Beaker-pressure-Vial
                           :target :greater-temperature-Coffee-temperature-Icecube}
                          {:base :Water :target :Coffee}
                          {:base :pressure-Vial :target :temperature-Icecube}
                          {:base :pressure-Beaker :target :temperature-Coffee}}
                        :emaps
                        #{{:base :Beaker :target :Coffee} {:base :Vial :target :Icecube}}}}
                      {:mhs
                       #{{:base   :greater-pressure-Beaker-pressure-Vial
                          :target :greater-temperature-Coffee-temperature-Icecube}
                         {:base :Beaker :target :Coffee}
                         {:base :pressure-Vial :target :temperature-Icecube}
                         {:base :pressure-Beaker :target :temperature-Coffee}
                         {:base :Vial :target :Icecube}}
                       :structure
                       {:roots
                        #{{:base   :greater-pressure-Beaker-pressure-Vial
                           :target :greater-temperature-Coffee-temperature-Icecube}}
                        :nogood
                        #{{:base   :greater-diameter-Beaker-diameter-Vial
                           :target :greater-temperature-Coffee-temperature-Icecube}
                          {:base :diameter-Beaker :target :temperature-Coffee}
                          {:base :Water :target :Coffee}
                          {:base :diameter-Vial :target :temperature-Icecube}}
                        :emaps
                        #{{:base :Beaker :target :Coffee} {:base :Vial :target :Icecube}}}}
                      {:mhs
                       #{{:base :Water :target :Coffee}
                         {:base :flat-top-Water :target :flat-top-Coffee}}
                       :structure
                       {:roots  #{{:base :flat-top-Water :target :flat-top-Coffee}}
                        :nogood #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                        :emaps  #{{:base :Water :target :Coffee}}}}
                      {:mhs
                       #{{:base :Water :target :Coffee}
                         {:base :liquid-Water :target :liquid-Coffee}}
                       :structure
                       {:roots  #{{:base :liquid-Water :target :liquid-Coffee}}
                        :nogood #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                        :emaps  #{{:base :Water :target :Coffee}}}}}
                    :mh-structure
                    {{:base :Water :target :Heat}
                     {:emaps    #{{:base :Water :target :Heat}}
                      :nogood   #{{:base :Water :target :Coffee}}
                      :children #{}}
                     {:base :flow-Beaker-Vial-Water-Pipe :target :flow-Coffee-Icecube-Heat-Bar}
                     {:emaps
                      #{{:base :Water :target :Heat}
                        {:base :Beaker :target :Coffee}
                        {:base :Pipe :target :Bar}
                        {:base :Vial :target :Icecube}}
                      :nogood #{{:base :Water :target :Coffee}}
                      :children
                      #{{:base :Water :target :Heat}
                        {:base :Beaker :target :Coffee}
                        {:base :Pipe :target :Bar}
                        {:base :Vial :target :Icecube}}}
                     {:base   :greater-diameter-Beaker-diameter-Vial
                      :target :greater-temperature-Coffee-temperature-Icecube}
                     {:emaps #{{:base :Beaker :target :Coffee} {:base :Vial :target :Icecube}}
                      :nogood
                      #{{:base   :greater-pressure-Beaker-pressure-Vial
                         :target :greater-temperature-Coffee-temperature-Icecube}
                        {:base :Water :target :Coffee}
                        {:base :pressure-Vial :target :temperature-Icecube}
                        {:base :pressure-Beaker :target :temperature-Coffee}}
                      :children
                      #{{:base :diameter-Beaker :target :temperature-Coffee}
                        {:base :diameter-Vial :target :temperature-Icecube}}}
                     {:base :diameter-Beaker :target :temperature-Coffee}
                     {:emaps    #{{:base :Beaker :target :Coffee}}
                      :nogood
                      #{{:base :Water :target :Coffee}
                        {:base :pressure-Beaker :target :temperature-Coffee}}
                      :children #{{:base :Beaker :target :Coffee}}}
                     {:base   :greater-pressure-Beaker-pressure-Vial
                      :target :greater-temperature-Coffee-temperature-Icecube}
                     {:emaps #{{:base :Beaker :target :Coffee} {:base :Vial :target :Icecube}}
                      :nogood
                      #{{:base   :greater-diameter-Beaker-diameter-Vial
                         :target :greater-temperature-Coffee-temperature-Icecube}
                        {:base :diameter-Beaker :target :temperature-Coffee}
                        {:base :Water :target :Coffee}
                        {:base :diameter-Vial :target :temperature-Icecube}}
                      :children
                      #{{:base :pressure-Vial :target :temperature-Icecube}
                        {:base :pressure-Beaker :target :temperature-Coffee}}}
                     {:base :Beaker :target :Coffee}
                     {:emaps    #{{:base :Beaker :target :Coffee}}
                      :nogood   #{{:base :Water :target :Coffee}}
                      :children #{}}
                     {:base :Water :target :Coffee}
                     {:emaps    #{{:base :Water :target :Coffee}}
                      :nogood   #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                      :children #{}}
                     {:base :pressure-Vial :target :temperature-Icecube}
                     {:emaps    #{{:base :Vial :target :Icecube}}
                      :nogood   #{{:base :diameter-Vial :target :temperature-Icecube}}
                      :children #{{:base :Vial :target :Icecube}}}
                     {:base :pressure-Beaker :target :temperature-Coffee}
                     {:emaps    #{{:base :Beaker :target :Coffee}}
                      :nogood
                      #{{:base :diameter-Beaker :target :temperature-Coffee}
                        {:base :Water :target :Coffee}}
                      :children #{{:base :Beaker :target :Coffee}}}
                     {:base :diameter-Vial :target :temperature-Icecube}
                     {:emaps    #{{:base :Vial :target :Icecube}}
                      :nogood   #{{:base :pressure-Vial :target :temperature-Icecube}}
                      :children #{{:base :Vial :target :Icecube}}}
                     {:base :flat-top-Water :target :flat-top-Coffee}
                     {:emaps    #{{:base :Water :target :Coffee}}
                      :nogood   #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                      :children #{{:base :Water :target :Coffee}}}
                     {:base :Pipe :target :Bar}
                     {:emaps #{{:base :Pipe :target :Bar}} :nogood #{} :children #{}}
                     {:base :Vial :target :Icecube}
                     {:emaps #{{:base :Vial :target :Icecube}} :nogood #{} :children #{}}
                     {:base :liquid-Water :target :liquid-Coffee}
                     {:emaps    #{{:base :Water :target :Coffee}}
                      :nogood   #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                      :children #{{:base :Water :target :Coffee}}}}}
          expected-propagated-from-emaps))
(def expected-computed-initial-gmaps (undiff expected-propagated-from-emaps
                                       {:mh-structure
                                        {{:base :Water :target :Heat}
                                         {:emaps    #{{:base :Water :target :Heat}}
                                          :nogood   #{{:base :Water :target :Coffee}}
                                          :children #{}}
                                         {:base :flow-Beaker-Vial-Water-Pipe :target :flow-Coffee-Icecube-Heat-Bar}
                                         {:emaps
                                          #{{:base :Water :target :Heat}
                                            {:base :Beaker :target :Coffee}
                                            {:base :Pipe :target :Bar}
                                            {:base :Vial :target :Icecube}}
                                          :nogood #{{:base :Water :target :Coffee}}
                                          :children
                                          #{{:base :Water :target :Heat}
                                            {:base :Beaker :target :Coffee}
                                            {:base :Pipe :target :Bar}
                                            {:base :Vial :target :Icecube}}}
                                         {:base   :greater-diameter-Beaker-diameter-Vial
                                          :target :greater-temperature-Coffee-temperature-Icecube}
                                         {:emaps #{{:base :Beaker :target :Coffee} {:base :Vial :target :Icecube}}
                                          :nogood
                                          #{{:base   :greater-pressure-Beaker-pressure-Vial
                                             :target :greater-temperature-Coffee-temperature-Icecube}
                                            {:base :Water :target :Coffee}
                                            {:base :pressure-Vial :target :temperature-Icecube}
                                            {:base :pressure-Beaker :target :temperature-Coffee}}
                                          :children
                                          #{{:base :diameter-Beaker :target :temperature-Coffee}
                                            {:base :diameter-Vial :target :temperature-Icecube}}}
                                         {:base :diameter-Beaker :target :temperature-Coffee}
                                         {:emaps    #{{:base :Beaker :target :Coffee}}
                                          :nogood
                                          #{{:base :Water :target :Coffee}
                                            {:base :pressure-Beaker :target :temperature-Coffee}}
                                          :children #{{:base :Beaker :target :Coffee}}}
                                         {:base   :greater-pressure-Beaker-pressure-Vial
                                          :target :greater-temperature-Coffee-temperature-Icecube}
                                         {:emaps #{{:base :Beaker :target :Coffee} {:base :Vial :target :Icecube}}
                                          :nogood
                                          #{{:base   :greater-diameter-Beaker-diameter-Vial
                                             :target :greater-temperature-Coffee-temperature-Icecube}
                                            {:base :diameter-Beaker :target :temperature-Coffee}
                                            {:base :Water :target :Coffee}
                                            {:base :diameter-Vial :target :temperature-Icecube}}
                                          :children
                                          #{{:base :pressure-Vial :target :temperature-Icecube}
                                            {:base :pressure-Beaker :target :temperature-Coffee}}}
                                         {:base :Beaker :target :Coffee}
                                         {:emaps    #{{:base :Beaker :target :Coffee}}
                                          :nogood   #{{:base :Water :target :Coffee}}
                                          :children #{}}
                                         {:base :Water :target :Coffee}
                                         {:emaps    #{{:base :Water :target :Coffee}}
                                          :nogood   #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                          :children #{}}
                                         {:base :pressure-Vial :target :temperature-Icecube}
                                         {:emaps    #{{:base :Vial :target :Icecube}}
                                          :nogood   #{{:base :diameter-Vial :target :temperature-Icecube}}
                                          :children #{{:base :Vial :target :Icecube}}}
                                         {:base :pressure-Beaker :target :temperature-Coffee}
                                         {:emaps    #{{:base :Beaker :target :Coffee}}
                                          :nogood
                                          #{{:base :diameter-Beaker :target :temperature-Coffee}
                                            {:base :Water :target :Coffee}}
                                          :children #{{:base :Beaker :target :Coffee}}}
                                         {:base :diameter-Vial :target :temperature-Icecube}
                                         {:emaps    #{{:base :Vial :target :Icecube}}
                                          :nogood   #{{:base :pressure-Vial :target :temperature-Icecube}}
                                          :children #{{:base :Vial :target :Icecube}}}
                                         {:base :flat-top-Water :target :flat-top-Coffee}
                                         {:emaps    #{{:base :Water :target :Coffee}}
                                          :nogood   #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                          :children #{{:base :Water :target :Coffee}}}
                                         {:base :Pipe :target :Bar}
                                         {:emaps #{{:base :Pipe :target :Bar}} :nogood #{} :children #{}}
                                         {:base :Vial :target :Icecube}
                                         {:emaps #{{:base :Vial :target :Icecube}} :nogood #{} :children #{}}
                                         {:base :liquid-Water :target :liquid-Coffee}
                                         {:emaps    #{{:base :Water :target :Coffee}}
                                          :nogood   #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                          :children #{{:base :Water :target :Coffee}}}}
                                        :gmaps
                                        #{{:mhs
                                           #{{:base :Water :target :Heat}
                                             {:base   :flow-Beaker-Vial-Water-Pipe
                                              :target :flow-Coffee-Icecube-Heat-Bar}
                                             {:base :Beaker :target :Coffee}
                                             {:base :Pipe :target :Bar}
                                             {:base :Vial :target :Icecube}}
                                           :structure
                                           {:roots
                                            #{{:base   :flow-Beaker-Vial-Water-Pipe
                                               :target :flow-Coffee-Icecube-Heat-Bar}}
                                            :nogood #{{:base :Water :target :Coffee}}
                                            :emaps
                                            #{{:base :Water :target :Heat}
                                              {:base :Beaker :target :Coffee}
                                              {:base :Pipe :target :Bar}
                                              {:base :Vial :target :Icecube}}}}
                                          {:mhs
                                           #{{:base   :greater-diameter-Beaker-diameter-Vial
                                              :target :greater-temperature-Coffee-temperature-Icecube}
                                             {:base :diameter-Beaker :target :temperature-Coffee}
                                             {:base :Beaker :target :Coffee}
                                             {:base :diameter-Vial :target :temperature-Icecube}
                                             {:base :Vial :target :Icecube}}
                                           :structure
                                           {:roots
                                            #{{:base   :greater-diameter-Beaker-diameter-Vial
                                               :target :greater-temperature-Coffee-temperature-Icecube}}
                                            :nogood
                                            #{{:base   :greater-pressure-Beaker-pressure-Vial
                                               :target :greater-temperature-Coffee-temperature-Icecube}
                                              {:base :Water :target :Coffee}
                                              {:base :pressure-Vial :target :temperature-Icecube}
                                              {:base :pressure-Beaker :target :temperature-Coffee}}
                                            :emaps
                                            #{{:base :Beaker :target :Coffee} {:base :Vial :target :Icecube}}}}
                                          {:mhs
                                           #{{:base   :greater-pressure-Beaker-pressure-Vial
                                              :target :greater-temperature-Coffee-temperature-Icecube}
                                             {:base :Beaker :target :Coffee}
                                             {:base :pressure-Vial :target :temperature-Icecube}
                                             {:base :pressure-Beaker :target :temperature-Coffee}
                                             {:base :Vial :target :Icecube}}
                                           :structure
                                           {:roots
                                            #{{:base   :greater-pressure-Beaker-pressure-Vial
                                               :target :greater-temperature-Coffee-temperature-Icecube}}
                                            :nogood
                                            #{{:base   :greater-diameter-Beaker-diameter-Vial
                                               :target :greater-temperature-Coffee-temperature-Icecube}
                                              {:base :diameter-Beaker :target :temperature-Coffee}
                                              {:base :Water :target :Coffee}
                                              {:base :diameter-Vial :target :temperature-Icecube}}
                                            :emaps
                                            #{{:base :Beaker :target :Coffee} {:base :Vial :target :Icecube}}}}
                                          {:mhs
                                           #{{:base :Water :target :Coffee}
                                             {:base :flat-top-Water :target :flat-top-Coffee}}
                                           :structure
                                           {:roots #{{:base :flat-top-Water :target :flat-top-Coffee}}
                                            :nogood
                                            #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                            :emaps #{{:base :Water :target :Coffee}}}}
                                          {:mhs
                                           #{{:base :Water :target :Coffee}
                                             {:base :liquid-Water :target :liquid-Coffee}}
                                           :structure
                                           {:roots #{{:base :liquid-Water :target :liquid-Coffee}}
                                            :nogood
                                            #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                            :emaps #{{:base :Water :target :Coffee}}}}}}
                                       {{:base :Water :target :Heat}
                                        {:emaps    #{{:base :Water :target :Heat}}
                                         :nogood   #{{:base :Water :target :Coffee}}
                                         :children #{}}
                                        {:base :flow-Beaker-Vial-Water-Pipe :target :flow-Coffee-Icecube-Heat-Bar}
                                        {:emaps
                                         #{{:base :Water :target :Heat}
                                           {:base :Beaker :target :Coffee}
                                           {:base :Pipe :target :Bar}
                                           {:base :Vial :target :Icecube}}
                                         :nogood #{{:base :Water :target :Coffee}}
                                         :children
                                         #{{:base :Water :target :Heat}
                                           {:base :Beaker :target :Coffee}
                                           {:base :Pipe :target :Bar}
                                           {:base :Vial :target :Icecube}}}
                                        {:base   :greater-diameter-Beaker-diameter-Vial
                                         :target :greater-temperature-Coffee-temperature-Icecube}
                                        {:emaps #{{:base :Beaker :target :Coffee} {:base :Vial :target :Icecube}}
                                         :nogood
                                         #{{:base   :greater-pressure-Beaker-pressure-Vial
                                            :target :greater-temperature-Coffee-temperature-Icecube}
                                           {:base :Water :target :Coffee}
                                           {:base :pressure-Vial :target :temperature-Icecube}
                                           {:base :pressure-Beaker :target :temperature-Coffee}}
                                         :children
                                         #{{:base :diameter-Beaker :target :temperature-Coffee}
                                           {:base :diameter-Vial :target :temperature-Icecube}}}
                                        {:base :diameter-Beaker :target :temperature-Coffee}
                                        {:emaps    #{{:base :Beaker :target :Coffee}}
                                         :nogood
                                         #{{:base :Water :target :Coffee}
                                           {:base :pressure-Beaker :target :temperature-Coffee}}
                                         :children #{{:base :Beaker :target :Coffee}}}
                                        {:base   :greater-pressure-Beaker-pressure-Vial
                                         :target :greater-temperature-Coffee-temperature-Icecube}
                                        {:emaps #{{:base :Beaker :target :Coffee} {:base :Vial :target :Icecube}}
                                         :nogood
                                         #{{:base   :greater-diameter-Beaker-diameter-Vial
                                            :target :greater-temperature-Coffee-temperature-Icecube}
                                           {:base :diameter-Beaker :target :temperature-Coffee}
                                           {:base :Water :target :Coffee}
                                           {:base :diameter-Vial :target :temperature-Icecube}}
                                         :children
                                         #{{:base :pressure-Vial :target :temperature-Icecube}
                                           {:base :pressure-Beaker :target :temperature-Coffee}}}
                                        {:base :Beaker :target :Coffee}
                                        {:emaps    #{{:base :Beaker :target :Coffee}}
                                         :nogood   #{{:base :Water :target :Coffee}}
                                         :children #{}}
                                        {:base :Water :target :Coffee}
                                        {:emaps    #{{:base :Water :target :Coffee}}
                                         :nogood   #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                         :children #{}}
                                        {:base :pressure-Vial :target :temperature-Icecube}
                                        {:emaps    #{{:base :Vial :target :Icecube}}
                                         :nogood   #{{:base :diameter-Vial :target :temperature-Icecube}}
                                         :children #{{:base :Vial :target :Icecube}}}
                                        {:base :pressure-Beaker :target :temperature-Coffee}
                                        {:emaps    #{{:base :Beaker :target :Coffee}}
                                         :nogood
                                         #{{:base :diameter-Beaker :target :temperature-Coffee}
                                           {:base :Water :target :Coffee}}
                                         :children #{{:base :Beaker :target :Coffee}}}
                                        {:base :diameter-Vial :target :temperature-Icecube}
                                        {:emaps    #{{:base :Vial :target :Icecube}}
                                         :nogood   #{{:base :pressure-Vial :target :temperature-Icecube}}
                                         :children #{{:base :Vial :target :Icecube}}}
                                        {:base :flat-top-Water :target :flat-top-Coffee}
                                        {:emaps    #{{:base :Water :target :Coffee}}
                                         :nogood   #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                         :children #{{:base :Water :target :Coffee}}}
                                        {:base :Pipe :target :Bar}
                                        {:emaps #{{:base :Pipe :target :Bar}} :nogood #{} :children #{}}
                                        {:base :Vial :target :Icecube}
                                        {:emaps #{{:base :Vial :target :Icecube}} :nogood #{} :children #{}}
                                        {:base :liquid-Water :target :liquid-Coffee}
                                        {:emaps    #{{:base :Water :target :Coffee}}
                                         :nogood   #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                         :children #{{:base :Water :target :Coffee}}}}))

(take 2 (data/diff {:mh-structure
                    {{:base :Water :target :Heat}
                     {:emaps    #{{:base :Water :target :Heat}}
                      :nogood   #{{:base :Water :target :Coffee}}
                      :children #{}}
                     {:base :flow-Beaker-Vial-Water-Pipe :target :flow-Coffee-Icecube-Heat-Bar}
                     {:emaps
                      #{{:base :Water :target :Heat}
                        {:base :Beaker :target :Coffee}
                        {:base :Pipe :target :Bar}
                        {:base :Vial :target :Icecube}}
                      :nogood #{{:base :Water :target :Coffee}}
                      :children
                      #{{:base :Water :target :Heat}
                        {:base :Beaker :target :Coffee}
                        {:base :Pipe :target :Bar}
                        {:base :Vial :target :Icecube}}}
                     {:base   :greater-diameter-Beaker-diameter-Vial
                      :target :greater-temperature-Coffee-temperature-Icecube}
                     {:emaps #{{:base :Beaker :target :Coffee} {:base :Vial :target :Icecube}}
                      :nogood
                      #{{:base   :greater-pressure-Beaker-pressure-Vial
                         :target :greater-temperature-Coffee-temperature-Icecube}
                        {:base :Water :target :Coffee}
                        {:base :pressure-Vial :target :temperature-Icecube}
                        {:base :pressure-Beaker :target :temperature-Coffee}}
                      :children
                      #{{:base :diameter-Beaker :target :temperature-Coffee}
                        {:base :diameter-Vial :target :temperature-Icecube}}}
                     {:base :diameter-Beaker :target :temperature-Coffee}
                     {:emaps    #{{:base :Beaker :target :Coffee}}
                      :nogood
                      #{{:base :Water :target :Coffee}
                        {:base :pressure-Beaker :target :temperature-Coffee}}
                      :children #{{:base :Beaker :target :Coffee}}}
                     {:base   :greater-pressure-Beaker-pressure-Vial
                      :target :greater-temperature-Coffee-temperature-Icecube}
                     {:emaps #{{:base :Beaker :target :Coffee} {:base :Vial :target :Icecube}}
                      :nogood
                      #{{:base   :greater-diameter-Beaker-diameter-Vial
                         :target :greater-temperature-Coffee-temperature-Icecube}
                        {:base :diameter-Beaker :target :temperature-Coffee}
                        {:base :Water :target :Coffee}
                        {:base :diameter-Vial :target :temperature-Icecube}}
                      :children
                      #{{:base :pressure-Vial :target :temperature-Icecube}
                        {:base :pressure-Beaker :target :temperature-Coffee}}}
                     {:base :Beaker :target :Coffee}
                     {:emaps    #{{:base :Beaker :target :Coffee}}
                      :nogood   #{{:base :Water :target :Coffee}}
                      :children #{}}
                     {:base :Water :target :Coffee}
                     {:emaps    #{{:base :Water :target :Coffee}}
                      :nogood   #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                      :children #{}}
                     {:base :pressure-Vial :target :temperature-Icecube}
                     {:emaps    #{{:base :Vial :target :Icecube}}
                      :nogood   #{{:base :diameter-Vial :target :temperature-Icecube}}
                      :children #{{:base :Vial :target :Icecube}}}
                     {:base :pressure-Beaker :target :temperature-Coffee}
                     {:emaps    #{{:base :Beaker :target :Coffee}}
                      :nogood
                      #{{:base :diameter-Beaker :target :temperature-Coffee}
                        {:base :Water :target :Coffee}}
                      :children #{{:base :Beaker :target :Coffee}}}
                     {:base :diameter-Vial :target :temperature-Icecube}
                     {:emaps    #{{:base :Vial :target :Icecube}}
                      :nogood   #{{:base :pressure-Vial :target :temperature-Icecube}}
                      :children #{{:base :Vial :target :Icecube}}}
                     {:base :flat-top-Water :target :flat-top-Coffee}
                     {:emaps    #{{:base :Water :target :Coffee}}
                      :nogood   #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                      :children #{{:base :Water :target :Coffee}}}
                     {:base :Pipe :target :Bar}
                     {:emaps #{{:base :Pipe :target :Bar}} :nogood #{} :children #{}}
                     {:base :Vial :target :Icecube}
                     {:emaps #{{:base :Vial :target :Icecube}} :nogood #{} :children #{}}
                     {:base :liquid-Water :target :liquid-Coffee}
                     {:emaps    #{{:base :Water :target :Coffee}}
                      :nogood   #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                      :children #{{:base :Water :target :Coffee}}}}
                    :gmaps #{#{{:mhs
                                #{{:base :Water :target :Heat}
                                  {:base   :flow-Beaker-Vial-Water-Pipe
                                   :target :flow-Coffee-Icecube-Heat-Bar}
                                  {:base :Beaker :target :Coffee}
                                  {:base :Pipe :target :Bar}
                                  {:base :Vial :target :Icecube}}
                                :structure
                                {:roots
                                 #{{:base   :flow-Beaker-Vial-Water-Pipe
                                    :target :flow-Coffee-Icecube-Heat-Bar}}
                                 :nogood #{{:base :Water :target :Coffee}}
                                 :emaps
                                 #{{:base :Water :target :Heat}
                                   {:base :Beaker :target :Coffee}
                                   {:base :Pipe :target :Bar}
                                   {:base :Vial :target :Icecube}}}}
                               {:mhs
                                #{{:base   :greater-diameter-Beaker-diameter-Vial
                                   :target :greater-temperature-Coffee-temperature-Icecube}
                                  {:base :diameter-Beaker :target :temperature-Coffee}
                                  {:base :Beaker :target :Coffee}
                                  {:base :diameter-Vial :target :temperature-Icecube}
                                  {:base :Vial :target :Icecube}}
                                :structure
                                {:roots
                                 #{{:base   :greater-diameter-Beaker-diameter-Vial
                                    :target :greater-temperature-Coffee-temperature-Icecube}}
                                 :nogood
                                 #{{:base   :greater-pressure-Beaker-pressure-Vial
                                    :target :greater-temperature-Coffee-temperature-Icecube}
                                   {:base :Water :target :Coffee}
                                   {:base :pressure-Vial :target :temperature-Icecube}
                                   {:base :pressure-Beaker :target :temperature-Coffee}}
                                 :emaps
                                 #{{:base :Beaker :target :Coffee} {:base :Vial :target :Icecube}}}}}
                             #{{:mhs
                                #{{:base :Water :target :Heat}
                                  {:base   :flow-Beaker-Vial-Water-Pipe
                                   :target :flow-Coffee-Icecube-Heat-Bar}
                                  {:base :Beaker :target :Coffee}
                                  {:base :Pipe :target :Bar}
                                  {:base :Vial :target :Icecube}}
                                :structure
                                {:roots
                                 #{{:base   :flow-Beaker-Vial-Water-Pipe
                                    :target :flow-Coffee-Icecube-Heat-Bar}}
                                 :nogood #{{:base :Water :target :Coffee}}
                                 :emaps
                                 #{{:base :Water :target :Heat}
                                   {:base :Beaker :target :Coffee}
                                   {:base :Pipe :target :Bar}
                                   {:base :Vial :target :Icecube}}}}
                               {:mhs
                                #{{:base   :greater-pressure-Beaker-pressure-Vial
                                   :target :greater-temperature-Coffee-temperature-Icecube}
                                  {:base :Beaker :target :Coffee}
                                  {:base :pressure-Vial :target :temperature-Icecube}
                                  {:base :pressure-Beaker :target :temperature-Coffee}
                                  {:base :Vial :target :Icecube}}
                                :structure
                                {:roots
                                 #{{:base   :greater-pressure-Beaker-pressure-Vial
                                    :target :greater-temperature-Coffee-temperature-Icecube}}
                                 :nogood
                                 #{{:base   :greater-diameter-Beaker-diameter-Vial
                                    :target :greater-temperature-Coffee-temperature-Icecube}
                                   {:base :diameter-Beaker :target :temperature-Coffee}
                                   {:base :Water :target :Coffee}
                                   {:base :diameter-Vial :target :temperature-Icecube}}
                                 :emaps
                                 #{{:base :Beaker :target :Coffee} {:base :Vial :target :Icecube}}}}}
                             #{{:mhs
                                #{{:base :Water :target :Coffee}
                                  {:base :flat-top-Water :target :flat-top-Coffee}}
                                :structure
                                {:roots #{{:base :flat-top-Water :target :flat-top-Coffee}}
                                 :nogood
                                 #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                 :emaps #{{:base :Water :target :Coffee}}}}
                               {:mhs
                                #{{:base :Water :target :Coffee}
                                  {:base :liquid-Water :target :liquid-Coffee}}
                                :structure
                                {:roots #{{:base :liquid-Water :target :liquid-Coffee}}
                                 :nogood
                                 #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                 :emaps #{{:base :Water :target :Coffee}}}}}}} expected-computed-initial-gmaps))

(def expected-combined-gmaps (undiff expected-computed-initial-gmaps
                               {:gmaps #{#{{:mhs
                                            #{{:base :Water :target :Heat}
                                              {:base   :flow-Beaker-Vial-Water-Pipe
                                               :target :flow-Coffee-Icecube-Heat-Bar}
                                              {:base :Beaker :target :Coffee}
                                              {:base :Pipe :target :Bar}
                                              {:base :Vial :target :Icecube}}
                                            :structure
                                            {:roots
                                             #{{:base   :flow-Beaker-Vial-Water-Pipe
                                                :target :flow-Coffee-Icecube-Heat-Bar}}
                                             :nogood #{{:base :Water :target :Coffee}}
                                             :emaps
                                             #{{:base :Water :target :Heat}
                                               {:base :Beaker :target :Coffee}
                                               {:base :Pipe :target :Bar}
                                               {:base :Vial :target :Icecube}}}}
                                           {:mhs
                                            #{{:base   :greater-diameter-Beaker-diameter-Vial
                                               :target :greater-temperature-Coffee-temperature-Icecube}
                                              {:base :diameter-Beaker :target :temperature-Coffee}
                                              {:base :Beaker :target :Coffee}
                                              {:base :diameter-Vial :target :temperature-Icecube}
                                              {:base :Vial :target :Icecube}}
                                            :structure
                                            {:roots
                                             #{{:base   :greater-diameter-Beaker-diameter-Vial
                                                :target :greater-temperature-Coffee-temperature-Icecube}}
                                             :nogood
                                             #{{:base   :greater-pressure-Beaker-pressure-Vial
                                                :target :greater-temperature-Coffee-temperature-Icecube}
                                               {:base :Water :target :Coffee}
                                               {:base :pressure-Vial :target :temperature-Icecube}
                                               {:base :pressure-Beaker :target :temperature-Coffee}}
                                             :emaps
                                             #{{:base :Beaker :target :Coffee} {:base :Vial :target :Icecube}}}}}
                                         #{{:mhs
                                            #{{:base :Water :target :Heat}
                                              {:base   :flow-Beaker-Vial-Water-Pipe
                                               :target :flow-Coffee-Icecube-Heat-Bar}
                                              {:base :Beaker :target :Coffee}
                                              {:base :Pipe :target :Bar}
                                              {:base :Vial :target :Icecube}}
                                            :structure
                                            {:roots
                                             #{{:base   :flow-Beaker-Vial-Water-Pipe
                                                :target :flow-Coffee-Icecube-Heat-Bar}}
                                             :nogood #{{:base :Water :target :Coffee}}
                                             :emaps
                                             #{{:base :Water :target :Heat}
                                               {:base :Beaker :target :Coffee}
                                               {:base :Pipe :target :Bar}
                                               {:base :Vial :target :Icecube}}}}
                                           {:mhs
                                            #{{:base   :greater-pressure-Beaker-pressure-Vial
                                               :target :greater-temperature-Coffee-temperature-Icecube}
                                              {:base :Beaker :target :Coffee}
                                              {:base :pressure-Vial :target :temperature-Icecube}
                                              {:base :pressure-Beaker :target :temperature-Coffee}
                                              {:base :Vial :target :Icecube}}
                                            :structure
                                            {:roots
                                             #{{:base   :greater-pressure-Beaker-pressure-Vial
                                                :target :greater-temperature-Coffee-temperature-Icecube}}
                                             :nogood
                                             #{{:base   :greater-diameter-Beaker-diameter-Vial
                                                :target :greater-temperature-Coffee-temperature-Icecube}
                                               {:base :diameter-Beaker :target :temperature-Coffee}
                                               {:base :Water :target :Coffee}
                                               {:base :diameter-Vial :target :temperature-Icecube}}
                                             :emaps
                                             #{{:base :Beaker :target :Coffee} {:base :Vial :target :Icecube}}}}}
                                         #{{:mhs
                                            #{{:base :Water :target :Coffee}
                                              {:base :flat-top-Water :target :flat-top-Coffee}}
                                            :structure
                                            {:roots #{{:base :flat-top-Water :target :flat-top-Coffee}}
                                             :nogood
                                             #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                             :emaps #{{:base :Water :target :Coffee}}}}
                                           {:mhs
                                            #{{:base :Water :target :Coffee}
                                              {:base :liquid-Water :target :liquid-Coffee}}
                                            :structure
                                            {:roots #{{:base :liquid-Water :target :liquid-Coffee}}
                                             :nogood
                                             #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                             :emaps #{{:base :Water :target :Coffee}}}}}}}
                               {:gmaps #{{:mhs
                                          #{{:base :Water :target :Heat}
                                            {:base   :flow-Beaker-Vial-Water-Pipe
                                             :target :flow-Coffee-Icecube-Heat-Bar}
                                            {:base :Beaker :target :Coffee}
                                            {:base :Pipe :target :Bar}
                                            {:base :Vial :target :Icecube}}
                                          :structure
                                          {:roots
                                           #{{:base   :flow-Beaker-Vial-Water-Pipe
                                              :target :flow-Coffee-Icecube-Heat-Bar}}
                                           :nogood #{{:base :Water :target :Coffee}}
                                           :emaps
                                           #{{:base :Water :target :Heat}
                                             {:base :Beaker :target :Coffee}
                                             {:base :Pipe :target :Bar}
                                             {:base :Vial :target :Icecube}}}}
                                         {:mhs
                                          #{{:base   :greater-diameter-Beaker-diameter-Vial
                                             :target :greater-temperature-Coffee-temperature-Icecube}
                                            {:base :diameter-Beaker :target :temperature-Coffee}
                                            {:base :Beaker :target :Coffee}
                                            {:base :diameter-Vial :target :temperature-Icecube}
                                            {:base :Vial :target :Icecube}}
                                          :structure
                                          {:roots
                                           #{{:base   :greater-diameter-Beaker-diameter-Vial
                                              :target :greater-temperature-Coffee-temperature-Icecube}}
                                           :nogood
                                           #{{:base   :greater-pressure-Beaker-pressure-Vial
                                              :target :greater-temperature-Coffee-temperature-Icecube}
                                             {:base :Water :target :Coffee}
                                             {:base :pressure-Vial :target :temperature-Icecube}
                                             {:base :pressure-Beaker :target :temperature-Coffee}}
                                           :emaps
                                           #{{:base :Beaker :target :Coffee} {:base :Vial :target :Icecube}}}}
                                         {:mhs
                                          #{{:base   :greater-pressure-Beaker-pressure-Vial
                                             :target :greater-temperature-Coffee-temperature-Icecube}
                                            {:base :Beaker :target :Coffee}
                                            {:base :pressure-Vial :target :temperature-Icecube}
                                            {:base :pressure-Beaker :target :temperature-Coffee}
                                            {:base :Vial :target :Icecube}}
                                          :structure
                                          {:roots
                                           #{{:base   :greater-pressure-Beaker-pressure-Vial
                                              :target :greater-temperature-Coffee-temperature-Icecube}}
                                           :nogood
                                           #{{:base   :greater-diameter-Beaker-diameter-Vial
                                              :target :greater-temperature-Coffee-temperature-Icecube}
                                             {:base :diameter-Beaker :target :temperature-Coffee}
                                             {:base :Water :target :Coffee}
                                             {:base :diameter-Vial :target :temperature-Icecube}}
                                           :emaps
                                           #{{:base :Beaker :target :Coffee} {:base :Vial :target :Icecube}}}}
                                         {:mhs
                                          #{{:base :Water :target :Coffee}
                                            {:base :flat-top-Water :target :flat-top-Coffee}}
                                          :structure
                                          {:roots #{{:base :flat-top-Water :target :flat-top-Coffee}}
                                           :nogood
                                           #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                           :emaps #{{:base :Water :target :Coffee}}}}
                                         {:mhs
                                          #{{:base :Water :target :Coffee}
                                            {:base :liquid-Water :target :liquid-Coffee}}
                                          :structure
                                          {:roots #{{:base :liquid-Water :target :liquid-Coffee}}
                                           :nogood
                                           #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                           :emaps #{{:base :Water :target :Coffee}}}}}}))

(def expected-merged-gmaps (undiff expected-combined-gmaps
                             {:gmaps {:mhs
                                      #{{:base :Water :target :Coffee}
                                        {:base :flat-top-Water :target :flat-top-Coffee}
                                        {:base :liquid-Water :target :liquid-Coffee}}
                                      :structure
                                      {:roots
                                       #{{:base :flat-top-Water :target :flat-top-Coffee}
                                         {:base :liquid-Water :target :liquid-Coffee}}
                                       :nogood #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                       :emaps  #{{:base :Water :target :Coffee}}}}}
                             {:gmaps #{#{{:mhs
                                          #{{:base :Water :target :Heat}
                                            {:base   :flow-Beaker-Vial-Water-Pipe
                                             :target :flow-Coffee-Icecube-Heat-Bar}
                                            {:base :Beaker :target :Coffee}
                                            {:base :Pipe :target :Bar}
                                            {:base :Vial :target :Icecube}}
                                          :structure
                                          {:roots
                                           #{{:base   :flow-Beaker-Vial-Water-Pipe
                                              :target :flow-Coffee-Icecube-Heat-Bar}}
                                           :nogood #{{:base :Water :target :Coffee}}
                                           :emaps
                                           #{{:base :Water :target :Heat}
                                             {:base :Beaker :target :Coffee}
                                             {:base :Pipe :target :Bar}
                                             {:base :Vial :target :Icecube}}}}
                                         {:mhs
                                          #{{:base   :greater-diameter-Beaker-diameter-Vial
                                             :target :greater-temperature-Coffee-temperature-Icecube}
                                            {:base :diameter-Beaker :target :temperature-Coffee}
                                            {:base :Beaker :target :Coffee}
                                            {:base :diameter-Vial :target :temperature-Icecube}
                                            {:base :Vial :target :Icecube}}
                                          :structure
                                          {:roots
                                           #{{:base   :greater-diameter-Beaker-diameter-Vial
                                              :target :greater-temperature-Coffee-temperature-Icecube}}
                                           :nogood
                                           #{{:base   :greater-pressure-Beaker-pressure-Vial
                                              :target :greater-temperature-Coffee-temperature-Icecube}
                                             {:base :Water :target :Coffee}
                                             {:base :pressure-Vial :target :temperature-Icecube}
                                             {:base :pressure-Beaker :target :temperature-Coffee}}
                                           :emaps
                                           #{{:base :Beaker :target :Coffee} {:base :Vial :target :Icecube}}}}}
                                       #{{:mhs
                                          #{{:base :Water :target :Heat}
                                            {:base   :flow-Beaker-Vial-Water-Pipe
                                             :target :flow-Coffee-Icecube-Heat-Bar}
                                            {:base :Beaker :target :Coffee}
                                            {:base :Pipe :target :Bar}
                                            {:base :Vial :target :Icecube}}
                                          :structure
                                          {:roots
                                           #{{:base   :flow-Beaker-Vial-Water-Pipe
                                              :target :flow-Coffee-Icecube-Heat-Bar}}
                                           :nogood #{{:base :Water :target :Coffee}}
                                           :emaps
                                           #{{:base :Water :target :Heat}
                                             {:base :Beaker :target :Coffee}
                                             {:base :Pipe :target :Bar}
                                             {:base :Vial :target :Icecube}}}}
                                         {:mhs
                                          #{{:base   :greater-pressure-Beaker-pressure-Vial
                                             :target :greater-temperature-Coffee-temperature-Icecube}
                                            {:base :Beaker :target :Coffee}
                                            {:base :pressure-Vial :target :temperature-Icecube}
                                            {:base :pressure-Beaker :target :temperature-Coffee}
                                            {:base :Vial :target :Icecube}}
                                          :structure
                                          {:roots
                                           #{{:base   :greater-pressure-Beaker-pressure-Vial
                                              :target :greater-temperature-Coffee-temperature-Icecube}}
                                           :nogood
                                           #{{:base   :greater-diameter-Beaker-diameter-Vial
                                              :target :greater-temperature-Coffee-temperature-Icecube}
                                             {:base :diameter-Beaker :target :temperature-Coffee}
                                             {:base :Water :target :Coffee}
                                             {:base :diameter-Vial :target :temperature-Icecube}}
                                           :emaps
                                           #{{:base :Beaker :target :Coffee} {:base :Vial :target :Icecube}}}}}
                                       #{{:mhs
                                          #{{:base :Water :target :Coffee}
                                            {:base :flat-top-Water :target :flat-top-Coffee}}
                                          :structure
                                          {:roots #{{:base :flat-top-Water :target :flat-top-Coffee}}
                                           :nogood
                                           #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                           :emaps #{{:base :Water :target :Coffee}}}}
                                         {:mhs
                                          #{{:base :Water :target :Coffee}
                                            {:base :liquid-Water :target :liquid-Coffee}}
                                          :structure
                                          {:roots #{{:base :liquid-Water :target :liquid-Coffee}}
                                           :nogood
                                           #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                           :emaps #{{:base :Water :target :Coffee}}}}}}}))

(def expected-gmap-score (undiff expected-merged-gmaps
                           {:mhs          #{{:base :e11 :target :e1}
                                            {:base :Water :target :Heat}
                                            {:base :Beaker :target :Coffee}
                                            {:base :e9 :target :e0}
                                            {:base :Pipe :target :Bar}
                                            {:base :Vial :target :Icecube}
                                            {:base :e13 :target :e3}
                                            {:base :e12 :target :e2}}
                            :structure    {:roots #{{:base :e9 :target :e0} {:base :e13 :target :e3}}
                                           :nogood
                                           #{{:base :e7 :target :e2}
                                             {:base :e8 :target :e3}
                                             {:base :Water :target :Coffee}
                                             {:base :e6 :target :e1}}
                                           :emaps
                                           #{{:base :Water :target :Heat}
                                             {:base :Beaker :target :Coffee}
                                             {:base :Pipe :target :Bar}
                                             {:base :Vial :target :Icecube}}}
                            :score        18
                            :emap-matches 0}
                           {:mh-structure {{:base :e11 :target :e1}
                                           {:emaps    #{{:base :Beaker :target :Coffee}}
                                            :nogood   #{{:base :Water :target :Coffee} {:base :e6 :target :e1}}
                                            :children #{{:base :Beaker :target :Coffee}}}
                                           {:base :e15 :target :e4}
                                           {:emaps    #{{:base :Water :target :Coffee}}
                                            :nogood   #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                            :children #{{:base :Water :target :Coffee}}}
                                           {:base :Water :target :Heat}
                                           {:emaps    #{{:base :Water :target :Heat}}
                                            :nogood   #{{:base :Water :target :Coffee}}
                                            :children #{}}
                                           {:base :e7 :target :e2}
                                           {:emaps    #{{:base :Vial :target :Icecube}}
                                            :nogood   #{{:base :e12 :target :e2}}
                                            :children #{{:base :Vial :target :Icecube}}}
                                           {:base :e8 :target :e3}
                                           {:emaps    #{{:base :Beaker :target :Coffee} {:base :Vial :target :Icecube}}
                                            :nogood
                                            #{{:base :e11 :target :e1}
                                              {:base :Water :target :Coffee}
                                              {:base :e13 :target :e3}
                                              {:base :e12 :target :e2}}
                                            :children #{{:base :e7 :target :e2} {:base :e6 :target :e1}}}
                                           {:base :Beaker :target :Coffee}
                                           {:emaps    #{{:base :Beaker :target :Coffee}}
                                            :nogood   #{{:base :Water :target :Coffee}}
                                            :children #{}}
                                           {:base :Water :target :Coffee}
                                           {:emaps    #{{:base :Water :target :Coffee}}
                                            :nogood   #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                            :children #{}}
                                           {:base :e9 :target :e0}
                                           {:emaps
                                            #{{:base :Water :target :Heat}
                                              {:base :Beaker :target :Coffee}
                                              {:base :Pipe :target :Bar}
                                              {:base :Vial :target :Icecube}}
                                            :nogood #{{:base :Water :target :Coffee}}
                                            :children
                                            #{{:base :Water :target :Heat}
                                              {:base :Beaker :target :Coffee}
                                              {:base :Pipe :target :Bar}
                                              {:base :Vial :target :Icecube}}}
                                           {:base :e6 :target :e1}
                                           {:emaps    #{{:base :Beaker :target :Coffee}}
                                            :nogood   #{{:base :e11 :target :e1} {:base :Water :target :Coffee}}
                                            :children #{{:base :Beaker :target :Coffee}}}
                                           {:base :Pipe :target :Bar}
                                           {:emaps #{{:base :Pipe :target :Bar}} :nogood #{} :children #{}}
                                           {:base :e16 :target :e5}
                                           {:emaps    #{{:base :Water :target :Coffee}}
                                            :nogood   #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                            :children #{{:base :Water :target :Coffee}}}
                                           {:base :Vial :target :Icecube}
                                           {:emaps #{{:base :Vial :target :Icecube}} :nogood #{} :children #{}}
                                           {:base :e13 :target :e3}
                                           {:emaps    #{{:base :Beaker :target :Coffee} {:base :Vial :target :Icecube}}
                                            :nogood
                                            #{{:base :e7 :target :e2}
                                              {:base :e8 :target :e3}
                                              {:base :Water :target :Coffee}
                                              {:base :e6 :target :e1}}
                                            :children #{{:base :e11 :target :e1} {:base :e12 :target :e2}}}
                                           {:base :e12 :target :e2}
                                           {:emaps    #{{:base :Vial :target :Icecube}}
                                            :nogood   #{{:base :e7 :target :e2}}
                                            :children #{{:base :Vial :target :Icecube}}}}
                            :gmaps        #{{:mhs
                                             #{{:base :e11 :target :e1}
                                               {:base :Water :target :Heat}
                                               {:base :Beaker :target :Coffee}
                                               {:base :e9 :target :e0}
                                               {:base :Pipe :target :Bar}
                                               {:base :Vial :target :Icecube}
                                               {:base :e13 :target :e3}
                                               {:base :e12 :target :e2}}
                                             :structure
                                             {:roots #{{:base :e9 :target :e0} {:base :e13 :target :e3}}
                                              :nogood
                                              #{{:base :e7 :target :e2}
                                                {:base :e8 :target :e3}
                                                {:base :Water :target :Coffee}
                                                {:base :e6 :target :e1}}
                                              :emaps
                                              #{{:base :Water :target :Heat}
                                                {:base :Beaker :target :Coffee}
                                                {:base :Pipe :target :Bar}
                                                {:base :Vial :target :Icecube}}}}
                                            {:mhs
                                             #{{:base :Water :target :Heat}
                                               {:base :e7 :target :e2}
                                               {:base :e8 :target :e3}
                                               {:base :Beaker :target :Coffee}
                                               {:base :e9 :target :e0}
                                               {:base :e6 :target :e1}
                                               {:base :Pipe :target :Bar}
                                               {:base :Vial :target :Icecube}}
                                             :structure
                                             {:roots #{{:base :e8 :target :e3} {:base :e9 :target :e0}}
                                              :nogood
                                              #{{:base :e11 :target :e1}
                                                {:base :Water :target :Coffee}
                                                {:base :e13 :target :e3}
                                                {:base :e12 :target :e2}}
                                              :emaps
                                              #{{:base :Water :target :Heat}
                                                {:base :Beaker :target :Coffee}
                                                {:base :Pipe :target :Bar}
                                                {:base :Vial :target :Icecube}}}}
                                            {:mhs
                                             #{{:base :e15 :target :e4}
                                               {:base :Water :target :Coffee}
                                               {:base :e16 :target :e5}}
                                             :structure
                                             {:roots  #{{:base :e15 :target :e4} {:base :e16 :target :e5}}
                                              :nogood #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                              :emaps  #{{:base :Water :target :Coffee}}}}}}))


#_(def expected-finalized-gmaps (undiff expected-gmap-score
                                  {:mh-structure {{:base :e11 :target :e1}
                                                  {:emaps    #{{:base :Beaker :target :Coffee}}
                                                   :nogood   #{{:base :Water :target :Coffee} {:base :e6 :target :e1}}
                                                   :children #{{:base :Beaker :target :Coffee}}}
                                                  {:base :e15 :target :e4}
                                                  {:emaps    #{{:base :Water :target :Coffee}}
                                                   :nogood   #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                                   :children #{{:base :Water :target :Coffee}}}
                                                  {:base :Water :target :Heat}
                                                  {:emaps    #{{:base :Water :target :Heat}}
                                                   :nogood   #{{:base :Water :target :Coffee}}
                                                   :children #{}}
                                                  {:base :e7 :target :e2}
                                                  {:emaps    #{{:base :Vial :target :Icecube}}
                                                   :nogood   #{{:base :e12 :target :e2}}
                                                   :children #{{:base :Vial :target :Icecube}}}
                                                  {:base :e8 :target :e3}
                                                  {:emaps    #{{:base :Beaker :target :Coffee} {:base :Vial :target :Icecube}}
                                                   :nogood
                                                   #{{:base :e11 :target :e1}
                                                     {:base :Water :target :Coffee}
                                                     {:base :e13 :target :e3}
                                                     {:base :e12 :target :e2}}
                                                   :children #{{:base :e7 :target :e2} {:base :e6 :target :e1}}}
                                                  {:base :Beaker :target :Coffee}
                                                  {:emaps    #{{:base :Beaker :target :Coffee}}
                                                   :nogood   #{{:base :Water :target :Coffee}}
                                                   :children #{}}
                                                  {:base :Water :target :Coffee}
                                                  {:emaps    #{{:base :Water :target :Coffee}}
                                                   :nogood   #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                                   :children #{}}
                                                  {:base :e9 :target :e0}
                                                  {:emaps
                                                   #{{:base :Water :target :Heat}
                                                     {:base :Beaker :target :Coffee}
                                                     {:base :Pipe :target :Bar}
                                                     {:base :Vial :target :Icecube}}
                                                   :nogood #{{:base :Water :target :Coffee}}
                                                   :children
                                                   #{{:base :Water :target :Heat}
                                                     {:base :Beaker :target :Coffee}
                                                     {:base :Pipe :target :Bar}
                                                     {:base :Vial :target :Icecube}}}
                                                  {:base :e6 :target :e1}
                                                  {:emaps    #{{:base :Beaker :target :Coffee}}
                                                   :nogood   #{{:base :e11 :target :e1} {:base :Water :target :Coffee}}
                                                   :children #{{:base :Beaker :target :Coffee}}}
                                                  {:base :Pipe :target :Bar}
                                                  {:emaps #{{:base :Pipe :target :Bar}} :nogood #{} :children #{}}
                                                  {:base :e16 :target :e5}
                                                  {:emaps    #{{:base :Water :target :Coffee}}
                                                   :nogood   #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                                   :children #{{:base :Water :target :Coffee}}}
                                                  {:base :Vial :target :Icecube}
                                                  {:emaps #{{:base :Vial :target :Icecube}} :nogood #{} :children #{}}
                                                  {:base :e13 :target :e3}
                                                  {:emaps    #{{:base :Beaker :target :Coffee} {:base :Vial :target :Icecube}}
                                                   :nogood
                                                   #{{:base :e7 :target :e2}
                                                     {:base :e8 :target :e3}
                                                     {:base :Water :target :Coffee}
                                                     {:base :e6 :target :e1}}
                                                   :children #{{:base :e11 :target :e1} {:base :e12 :target :e2}}}
                                                  {:base :e12 :target :e2}
                                                  {:emaps    #{{:base :Vial :target :Icecube}}
                                                   :nogood   #{{:base :e7 :target :e2}}
                                                   :children #{{:base :Vial :target :Icecube}}}}
                                   :gmaps        '({:mhs
                                                    #{{:base :e11 :target :e1}
                                                      {:base :Water :target :Heat}
                                                      {:base :Beaker :target :Coffee}
                                                      {:base :e9 :target :e0}
                                                      {:base :Pipe :target :Bar}
                                                      {:base :Vial :target :Icecube}
                                                      {:base :e13 :target :e3}
                                                      {:base :e12 :target :e2}}
                                                    :structure
                                                    {:roots #{{:base :e9 :target :e0} {:base :e13 :target :e3}}
                                                     :nogood
                                                     #{{:base :e7 :target :e2}
                                                       {:base :e8 :target :e3}
                                                       {:base :Water :target :Coffee}
                                                       {:base :e6 :target :e1}}
                                                     :emaps
                                                     #{{:base :Water :target :Heat}
                                                       {:base :Beaker :target :Coffee}
                                                       {:base :Pipe :target :Bar}
                                                       {:base :Vial :target :Icecube}}}
                                                    :score        18
                                                    :emap-matches 0
                                                    :mapping
                                                    {:base
                                                     {:name :simple-water-flow
                                                      :graph
                                                      {:e16 {:name :e16 :type :expression :functor :liquid :args (:Water)}
                                                       :e10 {:name :e10 :type :expression :functor :cause :args (:e8 :e9)}
                                                       :e8  {:name :e8 :type :expression :functor :greater :args (:e6 :e7)}
                                                       :e6  {:name :e6 :type :expression :functor :pressure :args (:Beaker)}
                                                       :e12 {:name :e12 :type :expression :functor :diameter :args (:Vial)}
                                                       :e9
                                                       {:name    :e9
                                                        :type    :expression
                                                        :functor :flow
                                                        :args    (:Beaker :Vial :Water :Pipe)}
                                                       :e14 {:name :e14 :type :expression :functor :clear :args (:Beaker)}
                                                       :e11
                                                       {:name :e11 :type :expression :functor :diameter :args (:Beaker)}
                                                       :e13
                                                       {:name :e13 :type :expression :functor :greater :args (:e11 :e12)}
                                                       :e15
                                                       {:name :e15 :type :expression :functor :flat-top :args (:Water)}
                                                       :e7  {:name :e7 :type :expression :functor :pressure :args (:Vial)}}
                                                      :spec
                                                      ([:cause
                                                        [:greater [:pressure :Beaker] [:pressure :Vial]]
                                                        [:flow :Beaker :Vial :Water :Pipe]]
                                                       [:greater [:diameter :Beaker] [:diameter :Vial]]
                                                       [:clear :Beaker]
                                                       [:flat-top :Water]
                                                       [:liquid :Water])}
                                                     :target
                                                     {:name :simple-heat-flow
                                                      :graph
                                                      {:e0
                                                       {:name    :e0
                                                        :type    :expression
                                                        :functor :flow
                                                        :args    (:Coffee :Icecube :Heat :Bar)}
                                                       :e1
                                                       {:name :e1 :type :expression :functor :temperature :args (:Coffee)}
                                                       :e2
                                                       {:name :e2 :type :expression :functor :temperature :args (:Icecube)}
                                                       :e3 {:name :e3 :type :expression :functor :greater :args (:e1 :e2)}
                                                       :e4 {:name :e4 :type :expression :functor :flat-top :args (:Coffee)}
                                                       :e5 {:name :e5 :type :expression :functor :liquid :args (:Coffee)}}
                                                      :spec
                                                      ([:flow :Coffee :Icecube :Heat :Bar]
                                                       [:greater [:temperature :Coffee] [:temperature :Icecube]]
                                                       [:flat-top :Coffee]
                                                       [:liquid :Coffee])}}}
                                                   {:mhs
                                                    #{{:base :Water :target :Heat}
                                                      {:base :e7 :target :e2}
                                                      {:base :e8 :target :e3}
                                                      {:base :Beaker :target :Coffee}
                                                      {:base :e9 :target :e0}
                                                      {:base :e6 :target :e1}
                                                      {:base :Pipe :target :Bar}
                                                      {:base :Vial :target :Icecube}}
                                                    :structure
                                                    {:roots #{{:base :e8 :target :e3} {:base :e9 :target :e0}}
                                                     :nogood
                                                     #{{:base :e11 :target :e1}
                                                       {:base :Water :target :Coffee}
                                                       {:base :e13 :target :e3}
                                                       {:base :e12 :target :e2}}
                                                     :emaps
                                                     #{{:base :Water :target :Heat}
                                                       {:base :Beaker :target :Coffee}
                                                       {:base :Pipe :target :Bar}
                                                       {:base :Vial :target :Icecube}}}
                                                    :score        18
                                                    :emap-matches 0
                                                    :mapping
                                                    {:base
                                                     {:name :simple-water-flow
                                                      :graph
                                                      {:e16 {:name :e16 :type :expression :functor :liquid :args (:Water)}
                                                       :e10 {:name :e10 :type :expression :functor :cause :args (:e8 :e9)}
                                                       :e8  {:name :e8 :type :expression :functor :greater :args (:e6 :e7)}
                                                       :e6  {:name :e6 :type :expression :functor :pressure :args (:Beaker)}
                                                       :e12 {:name :e12 :type :expression :functor :diameter :args (:Vial)}
                                                       :e9
                                                       {:name    :e9
                                                        :type    :expression
                                                        :functor :flow
                                                        :args    (:Beaker :Vial :Water :Pipe)}
                                                       :e14 {:name :e14 :type :expression :functor :clear :args (:Beaker)}
                                                       :e11
                                                       {:name :e11 :type :expression :functor :diameter :args (:Beaker)}
                                                       :e13
                                                       {:name :e13 :type :expression :functor :greater :args (:e11 :e12)}
                                                       :e15
                                                       {:name :e15 :type :expression :functor :flat-top :args (:Water)}
                                                       :e7  {:name :e7 :type :expression :functor :pressure :args (:Vial)}}
                                                      :spec
                                                      ([:cause
                                                        [:greater [:pressure :Beaker] [:pressure :Vial]]
                                                        [:flow :Beaker :Vial :Water :Pipe]]
                                                       [:greater [:diameter :Beaker] [:diameter :Vial]]
                                                       [:clear :Beaker]
                                                       [:flat-top :Water]
                                                       [:liquid :Water])}
                                                     :target
                                                     {:name :simple-heat-flow
                                                      :graph
                                                      {:e0
                                                       {:name    :e0
                                                        :type    :expression
                                                        :functor :flow
                                                        :args    (:Coffee :Icecube :Heat :Bar)}
                                                       :e1
                                                       {:name :e1 :type :expression :functor :temperature :args (:Coffee)}
                                                       :e2
                                                       {:name :e2 :type :expression :functor :temperature :args (:Icecube)}
                                                       :e3 {:name :e3 :type :expression :functor :greater :args (:e1 :e2)}
                                                       :e4 {:name :e4 :type :expression :functor :flat-top :args (:Coffee)}
                                                       :e5 {:name :e5 :type :expression :functor :liquid :args (:Coffee)}}
                                                      :spec
                                                      ([:flow :Coffee :Icecube :Heat :Bar]
                                                       [:greater [:temperature :Coffee] [:temperature :Icecube]]
                                                       [:flat-top :Coffee]
                                                       [:liquid :Coffee])}}}
                                                   {:mhs
                                                    #{{:base :e15 :target :e4}
                                                      {:base :Water :target :Coffee}
                                                      {:base :e16 :target :e5}}
                                                    :structure
                                                    {:roots  #{{:base :e15 :target :e4} {:base :e16 :target :e5}}
                                                     :nogood #{{:base :Water :target :Heat} {:base :Beaker :target :Coffee}}
                                                     :emaps  #{{:base :Water :target :Coffee}}}
                                                    :score        5
                                                    :emap-matches 0
                                                    :mapping
                                                    {:base
                                                     {:name :simple-water-flow
                                                      :graph
                                                      {:e16 {:name :e16 :type :expression :functor :liquid :args (:Water)}
                                                       :e10 {:name :e10 :type :expression :functor :cause :args (:e8 :e9)}
                                                       :e8  {:name :e8 :type :expression :functor :greater :args (:e6 :e7)}
                                                       :e6  {:name :e6 :type :expression :functor :pressure :args (:Beaker)}
                                                       :e12 {:name :e12 :type :expression :functor :diameter :args (:Vial)}
                                                       :e9
                                                       {:name    :e9
                                                        :type    :expression
                                                        :functor :flow
                                                        :args    (:Beaker :Vial :Water :Pipe)}
                                                       :e14 {:name :e14 :type :expression :functor :clear :args (:Beaker)}
                                                       :e11
                                                       {:name :e11 :type :expression :functor :diameter :args (:Beaker)}
                                                       :e13
                                                       {:name :e13 :type :expression :functor :greater :args (:e11 :e12)}
                                                       :e15
                                                       {:name :e15 :type :expression :functor :flat-top :args (:Water)}
                                                       :e7  {:name :e7 :type :expression :functor :pressure :args (:Vial)}}
                                                      :spec
                                                      ([:cause
                                                        [:greater [:pressure :Beaker] [:pressure :Vial]]
                                                        [:flow :Beaker :Vial :Water :Pipe]]
                                                       [:greater [:diameter :Beaker] [:diameter :Vial]]
                                                       [:clear :Beaker]
                                                       [:flat-top :Water]
                                                       [:liquid :Water])}
                                                     :target
                                                     {:name :simple-heat-flow
                                                      :graph
                                                      {:e0
                                                       {:name    :e0
                                                        :type    :expression
                                                        :functor :flow
                                                        :args    (:Coffee :Icecube :Heat :Bar)}
                                                       :e1
                                                       {:name :e1 :type :expression :functor :temperature :args (:Coffee)}
                                                       :e2
                                                       {:name :e2 :type :expression :functor :temperature :args (:Icecube)}
                                                       :e3 {:name :e3 :type :expression :functor :greater :args (:e1 :e2)}
                                                       :e4 {:name :e4 :type :expression :functor :flat-top :args (:Coffee)}
                                                       :e5 {:name :e5 :type :expression :functor :liquid :args (:Coffee)}}
                                                      :spec
                                                      ([:flow :Coffee :Icecube :Heat :Bar]
                                                       [:greater [:temperature :Coffee] [:temperature :Icecube]]
                                                       [:flat-top :Coffee]
                                                       [:liquid :Coffee])}}})}
                                  {:mhs
                                   #{{:base :e11 :target :e1}
                                     {:base :Water :target :Heat}
                                     {:base :Beaker :target :Coffee}
                                     {:base :e9 :target :e0}
                                     {:base :Pipe :target :Bar}
                                     {:base :Vial :target :Icecube}
                                     {:base :e13 :target :e3}
                                     {:base :e12 :target :e2}}
                                   :structure
                                   {:roots #{{:base :e9 :target :e0} {:base :e13 :target :e3}}
                                    :nogood
                                    #{{:base :e7 :target :e2}
                                      {:base :e8 :target :e3}
                                      {:base :Water :target :Coffee}
                                      {:base :e6 :target :e1}}
                                    :emaps
                                    #{{:base :Water :target :Heat}
                                      {:base :Beaker :target :Coffee}
                                      {:base :Pipe :target :Bar}
                                      {:base :Vial :target :Icecube}}}
                                   :score        18
                                   :emap-matches 0}))

(def expected-generated-inferences {:gmaps        [{:mhs          [{:base :Beaker :target :Coffee}
                                                                   {:base :Vial :target :Icecube}
                                                                   {:base :Water :target :Heat}
                                                                   {:base :flow-Beaker-Vial-Water :target :flow-Coffee-Icecube-Heat}
                                                                   {:base :pressure-Beaker :target :temperature-Coffee}
                                                                   {:base :greater-pressure-Beaker-Vial :target :greater-temperature-Coffee-Icecube}]
                                                    :structure    {:roots  #{{:base   :flow-Beaker-Vial-Water
                                                                              :target :flow-Coffee-Icecube-Heat}
                                                                             {:base   :greater-pressure-Beaker-Vial
                                                                              :target :greater-temperature-Coffee-Icecube}}
                                                                   :nogood #{}
                                                                   :emaps  #{{:base   :Beaker
                                                                              :target :Coffee}
                                                                             {:base   :Vial
                                                                              :target :Icecube}
                                                                             {:base   :Water
                                                                              :target :Heat}}}
                                                    :score        13
                                                    :emap-matches 0
                                                    :mapping      {:base   {:name       "simple water flow"
                                                                            :graph      [:cause-greater-pressure-Beaker-Vial-flow-Beaker-Vial-Water
                                                                                         :greater-pressure-Beaker-Vial
                                                                                         :pressure-Beaker
                                                                                         :flow-Beaker-Vial-Water]
                                                                            :id-counter 3}
                                                                   :target {:name       "simple heat flow"
                                                                            :graph      [:flow-Coffee-Icecube-Heat
                                                                                         :greater-temperature-Coffee-Icecube
                                                                                         :temperature-Coffee]
                                                                            :id-counter 2}}
                                                    :inferences   #{:cause-greater-pressure-Beaker-Vial-flow-Beaker-Vial-Water}}]
                                    :mh-structure {{:base :Beaker :target :Coffee}                                                   {:emaps    #{{:base   :Beaker
                                                                                                                                                   :target :Coffee}}
                                                                                                                                      :nogood   #{}
                                                                                                                                      :children #{}}
                                                   {:base :Vial :target :Icecube}                                                    {:emaps    #{{:base   :Vial
                                                                                                                                                   :target :Icecube}}
                                                                                                                                      :nogood   #{}
                                                                                                                                      :children #{}}
                                                   {:base :Water :target :Heat}                                                      {:emaps    #{{:base   :Water
                                                                                                                                                   :target :Heat}}
                                                                                                                                      :nogood   #{}
                                                                                                                                      :children #{}}
                                                   {:base :flow-Beaker-Vial-Water :target :flow-Coffee-Icecube-Heat}                 {:emaps    #{{:base   :Beaker
                                                                                                                                                   :target :Coffee}
                                                                                                                                                  {:base   :Vial
                                                                                                                                                   :target :Icecube}
                                                                                                                                                  {:base   :Water
                                                                                                                                                   :target :Heat}}
                                                                                                                                      :nogood   #{}
                                                                                                                                      :children #{{:base   :Beaker
                                                                                                                                                   :target :Coffee}
                                                                                                                                                  {:base   :Vial
                                                                                                                                                   :target :Icecube}
                                                                                                                                                  {:base   :Water
                                                                                                                                                   :target :Heat}}}
                                                   {:base :pressure-Beaker :target :temperature-Coffee}                              {:emaps    #{{:base   :Beaker
                                                                                                                                                   :target :Coffee}}
                                                                                                                                      :nogood   #{}
                                                                                                                                      :children #{{:base   :Beaker
                                                                                                                                                   :target :Coffee}}}
                                                   {:base :greater-pressure-Beaker-Vial :target :greater-temperature-Coffee-Icecube} {:emaps    #{{:base   :Beaker
                                                                                                                                                   :target :Coffee}
                                                                                                                                                  {:base   :Vial
                                                                                                                                                   :target :Icecube}}
                                                                                                                                      :nogood   #{}
                                                                                                                                      :children #{{:base   :Vial
                                                                                                                                                   :target :Icecube}
                                                                                                                                                  {:base   :pressure-Beaker
                                                                                                                                                   :target :temperature-Coffee}}}}})


(def expected-transferred-inferences  {:gmaps [{:mhs          [{:base :Beaker :target :Coffee}
                                                               {:base :Vial :target :Icecube}
                                                               {:base :Water :target :Heat}
                                                               {:base :flow-Beaker-Vial-Water :target :flow-Coffee-Icecube-Heat}
                                                               {:base :pressure-Beaker :target :temperature-Coffee}
                                                               {:base :greater-pressure-Beaker-Vial :target :greater-temperature-Coffee-Icecube}]
                                                :structure    {:roots  #{{:base   :flow-Beaker-Vial-Water
                                                                          :target :flow-Coffee-Icecube-Heat}
                                                                         {:base   :greater-pressure-Beaker-Vial
                                                                          :target :greater-temperature-Coffee-Icecube}}
                                                               :nogood #{}
                                                               :emaps  #{{:base   :Beaker
                                                                          :target :Coffee}
                                                                         {:base   :Vial
                                                                          :target :Icecube}
                                                                         {:base   :Water
                                                                          :target :Heat}}}
                                                :score        13
                                                :emap-matches 0
                                                :mapping      {:base   {:name       "simple water flow"
                                                                        :graph      [:cause-greater-pressure-Beaker-Vial-flow-Beaker-Vial-Water
                                                                                     :greater-pressure-Beaker-Vial
                                                                                     :pressure-Beaker
                                                                                     :flow-Beaker-Vial-Water]
                                                                        :id-counter 3}
                                                               :target {:name       "simple heat flow"
                                                                        :graph      [:flow-Coffee-Icecube-Heat
                                                                                     :greater-temperature-Coffee-Icecube
                                                                                     :temperature-Coffee]
                                                                        :id-counter 2}}
                                                :inferences   #{:cause-greater-pressure-Beaker-Vial-flow-Beaker-Vial-Water}

                                                :transferred [[:cause
                                                               :greater-temperature-Coffee-Icecube
                                                               :flow-Coffee-Icecube-Heat]]}]
                                       :mh-structure {{:base :Beaker :target :Coffee}                                                   {:emaps    #{{:base   :Beaker
                                                                                                                                                      :target :Coffee}}
                                                                                                                                         :nogood   #{}
                                                                                                                                         :children #{}}
                                                      {:base :Vial :target :Icecube}                                                    {:emaps    #{{:base   :Vial
                                                                                                                                                      :target :Icecube}}
                                                                                                                                         :nogood   #{}
                                                                                                                                         :children #{}}
                                                      {:base :Water :target :Heat}                                                      {:emaps    #{{:base   :Water
                                                                                                                                                      :target :Heat}}
                                                                                                                                         :nogood   #{}
                                                                                                                                         :children #{}}
                                                      {:base :flow-Beaker-Vial-Water :target :flow-Coffee-Icecube-Heat}                 {:emaps    #{{:base   :Beaker
                                                                                                                                                      :target :Coffee}
                                                                                                                                                     {:base   :Vial
                                                                                                                                                      :target :Icecube}
                                                                                                                                                     {:base   :Water
                                                                                                                                                      :target :Heat}}
                                                                                                                                         :nogood   #{}
                                                                                                                                         :children #{{:base   :Beaker
                                                                                                                                                      :target :Coffee}
                                                                                                                                                     {:base   :Vial
                                                                                                                                                      :target :Icecube}
                                                                                                                                                     {:base   :Water
                                                                                                                                                      :target :Heat}}}
                                                      {:base :pressure-Beaker :target :temperature-Coffee}                              {:emaps    #{{:base   :Beaker
                                                                                                                                                      :target :Coffee}}
                                                                                                                                         :nogood   #{}
                                                                                                                                         :children #{{:base   :Beaker
                                                                                                                                                      :target :Coffee}}}
                                                      {:base :greater-pressure-Beaker-Vial :target :greater-temperature-Coffee-Icecube} {:emaps    #{{:base   :Beaker
                                                                                                                                                      :target :Coffee}
                                                                                                                                                     {:base   :Vial
                                                                                                                                                      :target :Icecube}}
                                                                                                                                         :nogood   #{}
                                                                                                                                         :children #{{:base   :Vial
                                                                                                                                                      :target :Icecube}
                                                                                                                                                     {:base   :pressure-Beaker
                                                                                                                                                      :target :temperature-Coffee}}}}})

#_(data/diff expected-transferred-inferences (SUT/match kg rules/literal-similarity simple-water-flow simple-heat-flow))

(def expected-transferred ::fail)
(def expected-matches ::fail)

(defn remove-mapping [result]
  (update result :gmaps (partial map #(dissoc % :mapping))))

(deftest heat-water-test
  ;; Water flow is the base heat flow the target

  #_(is (=
          expected-match-hypotheses
          (->> rules/literal-similarity
            (SUT/create-match-hypotheses kg simple-water-flow simple-heat-flow)
            (map #(into {} %))
            set)
          ))

  #_(is (=
          expected-hypothesis-structure
          (SUT/build-hypothesis-structure kg expected-match-hypotheses)))

  #_(is (=
          expected-propagated-from-emaps
          (SUT/propagate-from-emaps expected-hypothesis-structure)))

  #_(is (=
          expected-computed-initial-gmaps
          (SUT/compute-initial-gmaps kg expected-propagated-from-emaps)))

  #_(is (=
          expected-combined-gmaps
          (SUT/combine-gmaps expected-computed-initial-gmaps)))

  #_(is (=
          expected-merged-gmaps
          (SUT/merge-gmaps expected-combined-gmaps)))


  #_(is (=
          '({:base :Water :target :Water})
          (filter (fn [{:keys [base target]}] (SUT/emaps-equal?
                                               (get kg base)
                                               (get kg target)))
            '({:base :Water :target :Water}
              {:base :Beaker :target :Coffee}
              {:base :Pipe :target :Bar}
              {:base :Vial :target :Icecube}))))

  #_(is (=
          '({:base :Water :target :Heat}
            {:base :Beaker :target :Coffee}
            {:base :Pipe :target :Bar}
            {:base :Vial :target :Icecube})
          (filter (partial SUT/is-emap? kg) (-> expected-merged-gmaps :gmaps first :mhs))))

  ;;Fails for now
  #_(is (=
          ::fail
          (SUT/matching-emaps kg (-> expected-merged-gmaps :gmaps first))))

  #_(is (=
          expected-gmap-score
          (SUT/score-gmap kg expected-merged-gmaps (-> expected-merged-gmaps :gmaps first))))

  #_(is (=
          (remove-mapping expected-finalized-gmaps)
          (-> (SUT/finalize-gmaps kg simple-water-flow simple-heat-flow expected-merged-gmaps)
            remove-mapping)))

  #_(is (=
          expected-generated-inferences
          (SUT/generate-inferences kg simple-water-flow expected-finalized-gmaps)
          ))

  (is (=
        expected-transferred-inferences
        (SUT/transfer-inferences kg expected-generated-inferences)))

  ;; Should show the cause relation between the greater temperature
  ;; relation and the heat flow relation. This relation has been inferred
  ;; based on the analogical cause relation in the water flow graph.
  #_(is (= expected-transferred (-> (SUT/match kg rules/literal-similarity simple-water-flow simple-heat-flow)
                                  :gmaps
                                  first
                                  :transferred)))

  #_(is (= expected-transferred-inferences (SUT/match kg rules/literal-similarity simple-water-flow simple-heat-flow)))
  #_(data/diff expected-transferred-inferences (SUT/match kg rules/literal-similarity simple-water-flow simple-heat-flow)))
;; => #'sme-clj.core-test/heat-water-test;; => #'sme-clj.core-test/heat-water-test
