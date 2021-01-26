(ns sme-clj.example.simple-heat-water
  "Example adapted from SME literature [1], of an analogy between the flow of
  water from a large beaker through a pipe to a small vial, and the flow of
  heat from a cup of coffee through a bar into an ice cube.

  This is the running example Falkenhainer et al. use in their description of
  the SME algorithm.

    [1] Falkenhainer, Forbus & Gentner (1989). The structure-mapping engine:
          algorithm and examples. Artificial Intelligence, 41, 1-62.
  "
  (:require [clojure.data :as data]
            [clojure.pprint :as pp]
            [clojure.test :as test]
            [clojure.walk :as walk]
            [sme-clj.core :as sme]
            [sme-clj.ruledef :as rules]
            [sme-clj.typedef :refer :all]
            [sme-clj.util :refer [vals-as-keys]]))

;; Predicate definitions
(def predicate-map (vals-as-keys :name [(make-predicate :flow :type :relation :arity 4)
                                        (make-predicate :greater :type :relation :arity 2)
                                        (make-predicate :cause :type :relation :arity 2)
                                        (make-predicate :temperature :type :function)
                                        (make-predicate :flat-top :type :function)
                                        (make-predicate :pressure :type :function)
                                        (make-predicate :diameter :type :function)
                                        (make-predicate :liquid :type :attribute)
                                        (make-predicate :clear :type :attribute)]))


;; Entities
(def entity-map (vals-as-keys :name [(make-entity :Coffee)
                                     (make-entity :Icecube)
                                     (make-entity :Bar)
                                     (make-entity :Heat)

                                     (make-entity :Water)
                                     (make-entity :Beaker)
                                     (make-entity :Vial)
                                     (make-entity :Pipe)]))

;; Concept graph definitions
(def simple-heat-flow (make-concept-graph (keyword "simple-heat-flow")
                        [:flow :Coffee :Icecube :Heat :Bar]
                        [:greater [:temperature :Coffee] [:temperature :Icecube]]
                        [:flat-top :Coffee]
                        [:liquid :Coffee]))

(def simple-water-flow (make-concept-graph (keyword "simple water flow")
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

(comment
  (count (sme/create-match-hypotheses kg simple-water-flow simple-heat-flow rules/literal-similarity))
  (count mhs)

  ;; Water flow is the base, heat flow the target
  (def result (sme/match simple-water-flow simple-heat-flow))
  (def gmaps (:gmaps result))

  ;; Should show the cause relation between the greater temperature
  ;; relation and the heat flow relation. This relation has been inferred
  ;; based on the analogical cause relation in the water flow graph.
  (pp/write (:transferred (first gmaps)) :suppress-namespaces true))
