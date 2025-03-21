(ns space-invaders.invaders-spec
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as str]))

(s/def ::radar-line
  (s/with-gen
    (and string?
         #(re-matches #"^[-o]+$" %))
    #(gen/fmap
       (fn [x] (str/join x))
       (gen/vector
         (gen/elements #{\- \o}) 5 10))))

(s/def ::radar-snapshot
  (s/coll-of ::radar-line))

(s/def ::invader-position
  (and vector?
       (s/coll-of int? :count 2)))

(s/def ::invader-positions
  (and vector?
       (s/coll-of ::invader-position :count 2)))

(s/def ::tolerance
  (and nat-int?
       #(< % 4)))

(s/def ::match-fn
  (s/fspec :args (s/cat :part ::radar-line
                        :line ::radar-line)
           :ret boolean?))

(s/def ::detect-invaders-args
  (s/keys :req-un [::radar-lines]
          :opt-un [::tolerance ::match-fn]))

(s/def ::starting-positions
  (s/coll-of (s/tuple nat-int?
                      (and vector?
                           (s/coll-of ::invader-position)))))

(s/def ::lines ::radar-snapshot)

(s/def ::invader ::radar-snapshot)

(s/def ::match-rest-args
  (s/keys :req-un [::lines ::invader ::starting-positions ::tolerance ::match-fn]))

