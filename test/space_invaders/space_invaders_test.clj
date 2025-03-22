(ns space-invaders.space-invaders-test
  (:require [clojure.test :refer :all]
            [clojure.spec.test.alpha :as stest]
            [clojure.string :as str]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [space-invaders.space-invaders :refer :all]))

(stest/instrument `detect-invaders)
(stest/instrument `match-rest)

(deftest detect-invaders-test
  (testing "a single type 1 invader"
    (let [invader-type-1
"--o-o--o--o-o-oooo-
-----o-----o--ooooo
------o---o--------
-----ooooooo----ooo
----oo-ooo-oo----oo
---ooooooooooo-o-o-
---o-ooooooo-o-----
---o-o-----o-o-----
------oo-oo-----o--"]
    (is (= [[[1 3] [8 14]]]
           (detect-invaders
             {:radar-lines (str/split invader-type-1 #"\n")} )))))

  (testing "a partial type 1 invader coming over the right edge"
    (let [invader-type-1
"--o-o--o--o
-----o-----
------o---o
-----oooooo
----oo-ooo-
---oooooooo
---o-oooooo
---o-o-----
------oo-oo"]
      (is (= [[[1 3] [8 11]]]
             (detect-invaders {:radar-lines (str/split invader-type-1 #"\n")} )))))

  (testing "a partial type 1 invader coming over the left edge"
    (let [invader-type-1
"--o--o-o-oooo-
o-----o--ooooo
-o---o--------
ooooooo----ooo
o-ooo-oo----oo
ooooooooo-o-o-
ooooooo-o-----
o-----o-o-----
-oo-oo-----o--"]
      (is (= [[[1 0] [8 9]]]
             (detect-invaders {:radar-lines (str/split invader-type-1 #"\n")} )))))

  (testing "a single type 2 invader"
    (let [invader-type-2
"-----oo---oo-
----oooo----o
---oooooo-o-o
--oo-oo-oo---
--ooooooooooo
----o--o--o-o
---o-oo-o--o-
--o-o--o-o---"]
      (is (= [[[0 2] [7 10]]]
             (detect-invaders {:radar-lines (str/split invader-type-2 #"\n")} )))))

  (testing "a single type 2 invader coming over the left edge"
    (let [invader-type-2
"-oo---oo-
oooo----o
ooooo-o-o
-oo-oo---
ooooooooo
o--o--o-o
-oo-o--o-
o--o-o---"]
      (is (= [[[0 0] [7 6]]]
             (detect-invaders {:radar-lines (str/split invader-type-2 #"\n")} )))))

  (testing "a single type 2 invader coming over the right edge"
    (let [invader-type-2
"-----oo
----ooo
---oooo
--oo-oo
--ooooo
----o--
---o-oo
--o-o--"]
      (is (= [[[0 2] [7 7]]]
             (detect-invaders {:radar-lines (str/split invader-type-2 #"\n")} )))))


  (testing "multiple invaders"
    (let [radar
"-----o-----o--ooooo-----o-----o--ooooo
------o---o--------------o---o--------
-----ooooooo----ooo-----ooooooo----ooo
----oo-ooo-oo----oo----oo-ooo-oo----oo
---ooooooooooo-o-o----ooooooooooo-o-o-
---o-ooooooo-o--------o-ooooooo-o-----
---o-o-----o-o--------o-o-----o-o-----
------oo-oo-----o--------oo-oo-----o--
-o-o-o-o-o-o---oo----o-o-o-o-o-o-o-o--
-o-o-o-o-o-o--oooo---o-o-o-o-o-o-o-o--
-o-o-o-o-o-o-oooooo--o-o-o-o-o-o-o-o--
-o-o-o-o-o-ooo-oo-oo-o-o-o-o-o-o-o-o--
-o-o-o-o-o-ooooooooo-o-o-o-o-o-o-o-o--
-o-o-o-o-o-o--o--o---o-o-o-o-o-o-o-o--
-o-o-o-o-o-o-o-oo-o--o-o-o-o-o-o-o-o--
-o-o-o-o-o-oo-o--o-o-o-o-o-o-o-o-o-o--"]
      (is (= [[[0 3] [7 14]] [[0 22] [7 33]] [[8 12] [15 20]]]
             (detect-invaders {:radar-lines (str/split radar #"\n")} )))))

  (testing "shifted lines"
    (let [shifted-invader
"--o-o--o--o-o-oooo-
-----o-----o--ooooo
--------o---o------
---ooooooo----ooo--
----oo-ooo-oo----oo
------ooooooooooo-o
---o-ooooooo-o-----
---o-o-----o-o-----
------oo-oo-----o--"]
    (is (= [[[1 3] [8 14]]]
           (detect-invaders
             {:radar-lines (str/split shifted-invader #"\n")} ))))))

(deftest fuzzy-matcher-test
  (testing "true when 8 out of 10 elements match with 80% accuracy"
    (is (true?
          (fuzzy-matcher 80
                         "----------"
                         "--------oo"))))

  (testing "false when 7 out of 10 match with 80% accuracy"
    (is (false?
          (fuzzy-matcher 80
                         "----------"
                         "-------ooo")))))

(def line-generator
  (gen/fmap
    str/join
    (gen/vector
      (gen/elements #{\- \o}) 10)))

(defn flip-char
  [c]
  (if (= \o c)
    \-
    \o))

(defn update-string
  [s idxs]
  (str/join
    (reduce (fn [acc idx]
              (update acc idx flip-char))
            (vec (seq s))
            idxs)))


(defspec fuzzy-matcher-same-prop 100
  (prop/for-all [inp line-generator]
                (true? (fuzzy-matcher 100 inp inp))))

(defspec fuzzy-matcher-90-pc-prop 100
  ;; when you change a single char in 10 character string - there will be a 90% match
  (prop/for-all [inp line-generator]
                (= [true false]
                   (let [inp2 (update-string inp [0])]
                     [(fuzzy-matcher 90 inp inp2)
                      (fuzzy-matcher 91 inp inp2)]))))

(defspec fuzzy-matcher-80-pc-prop 100
  ;; when you change a single char in 10 character string - there will be a 90% match
  (prop/for-all [inp line-generator]

                (= [true false]
                   (let [inp2 (update-string inp [0 1])]
                     [(fuzzy-matcher 80
                                     inp
                                     inp2)
                      (fuzzy-matcher 81
                                     inp
                                     inp2)]))))
