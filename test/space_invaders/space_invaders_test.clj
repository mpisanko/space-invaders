(ns space-invaders.space-invaders-test
  (:require [clojure.test :refer :all]
            [clojure.spec.test.alpha :as stest]
            [clojure.string :as str]
            [space-invaders.space-invaders :refer :all]))

(stest/instrument `detect-invaders)

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
             (str/split invader-type-1 #"\n") 0)))))

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
             (detect-invaders (str/split invader-type-1 #"\n") 0)))))

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
             (detect-invaders (str/split invader-type-1 #"\n") 0)))))

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
             (detect-invaders (str/split invader-type-2 #"\n") 0)))))

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
             (detect-invaders (str/split invader-type-2 #"\n") 0)))))

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
             (detect-invaders (str/split invader-type-2 #"\n") 0)))))
  )
