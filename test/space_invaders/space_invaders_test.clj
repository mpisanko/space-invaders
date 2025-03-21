(ns space-invaders.space-invaders-test
  (:require [clojure.test :refer :all]
            [clojure.spec.test.alpha :as stest]
            [clojure.string :as str]
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
             (str/split invader-type-1 #"\n") 0 :eq)))))

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
             (detect-invaders (str/split invader-type-1 #"\n") 0 :eq)))))

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
             (detect-invaders (str/split invader-type-1 #"\n") 0 :eq)))))

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
             (detect-invaders (str/split invader-type-2 #"\n") 0 :eq)))))

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
             (detect-invaders (str/split invader-type-2 #"\n") 0 :eq)))))

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
             (detect-invaders (str/split invader-type-2 #"\n") 0 :eq)))))


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
      (is (= [[[0 22] [7 33]] [[0 3] [7 14]] [[8 12] [15 20]]]
             (detect-invaders (str/split radar #"\n") 0 :eq)))))

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
             (str/split shifted-invader #"\n") 3 :eq)))))
  )

(deftest fuzzy-matcher-test
  (testing "true when 8 out of 10 elements match"
    (is (true?
          (fuzzy-matcher "----------"
                         "--------oo"))))

  (testing "false when 7 out of 10 match"
    (is (false?
          (fuzzy-matcher "----------"
                         "-------ooo")))))
