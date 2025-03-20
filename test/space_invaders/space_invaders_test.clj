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
             (detect-invaders (str/split invader-type-1 #"\n") 0))))))
