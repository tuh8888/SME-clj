(ns sme-clj.mop-helpers
  (:require [mop-records :as mr]
            [sme-clj.typedef :as types]))

(defn make-mop
  [m id parent & [slots]]
  (let [mop (mops/->mop id slots)]
    (-> m
      (mops/add-mop mop)
      (mr/-add-slot-to-mop id :parents #{parent}))))

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
