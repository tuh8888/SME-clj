(ns sme-clj.test-util
  (:require [clojure.data :as data]
            [clojure.set :as set]))

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
  (let [[a _ _] (data/diff x after)]
    (recursive-diff-merge a before)))
