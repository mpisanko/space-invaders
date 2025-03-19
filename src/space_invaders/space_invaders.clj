(ns space-invaders.space-invaders
  (:gen-class)
  (:require [clojure.tools.logging :as log]
            [clojure.spec.alpha :as s]
            [space-invaders.invaders-spec :as sis]))

(def invader-1
  ["--o-----o--"
   "---o---o---"
   "--ooooooo--"
   "-oo-ooo-oo-"
   "ooooooooooo"
   "o-ooooooo-o"
   "o-o-----o-o"
   "---oo-oo---"])

(def invader-2
  ["---oo---"
   "--oooo--"
   "-oooooo-"
   "oo-oo-oo"
   "oooooooo"
   "--o--o--"
   "-o-oo-o-"
   "o-o--o-o"])

(s/fdef detect-invaders
  :args (s/cat :radar-lines ::sis/radar-lines)
  :ret ::sis/invader-positions)

(defn detect-invaders
  "The main function taking the radar reading and determining positions of known invaders."
  [radar-lines])

(defn -main
  "Entrypoint to the application."
  [& args]
  (log/infof "Space Invaders Radar Interpreter. Got %s" args)
  )
