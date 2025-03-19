(ns space-invaders.invaders-spec
  (:require [clojure.spec.alpha :as s]))

(s/def ::radar-line
  (and string?
       #(re-matches #"^[-o]+$" %)))

(s/def ::radar-snapshot
  (s/coll-of ::radar-line))

(s/def ::invader-position
  (and vector?
       (s/coll-of int? :count 2)))

(s/def ::invader-positions
  (and vector?
       (s/coll-of ::invader-position :count 2)))
