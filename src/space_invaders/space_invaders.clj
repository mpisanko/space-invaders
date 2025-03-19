(ns space-invaders.space-invaders
  (:gen-class)
  (:require [clojure.tools.logging :as log]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
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


(defn match-part
  "Looks for a match in a line. Comparison can be defined by :compare-fn.
   Returns a vector of match start and its length"
  [{:keys [part line match-fn]
    :or {match-fn =}}]
  (let [part (seq part)
        len (count part)]
    (seq
      (remove
        nil?
        (for [start (range (- (count line)
                              (int (/ len 2))))
              :let  [chunk (take len (drop start line))
                     chunk-len (count chunk)]]
          (when (match-fn
                  chunk
                  (if (> len chunk-len)
                    (take chunk-len part)
                    part))
            [start chunk-len]))))))

(s/fdef detect-invaders
  :args (s/cat :radar-lines ::sis/radar-lines
               :tolerance :sis/tolerance)
  :ret ::sis/invader-positions)

(defn detect-invaders
  "The main function taking the radar reading and determining positions of known invaders."
  [radar-lines tolerance]
  (let [allowed-range (take (- (count radar-lines) tolerance) radar-lines)
        find-starting-points-for (fn [top-line]
                                   (remove
                                     nil?
                                     (map-indexed
                                       (fn [idx line]
                                         (when-let [matches (match-part
                                                              {:part top-line
                                                               :line line})]
                                           [idx matches]))
                                       allowed-range)))
        starting-positions-1 (find-starting-points-for (first invader-1))
        starting-positions-2 (find-starting-points-for (first invader-2))]
    [starting-positions-1 starting-positions-2]
    ;; TODO handle the rest
    ))

(defn -main
  "Entrypoint to the application."
  [& args]
  (log/infof "Space Invaders Radar Interpreter. Got %s" args)
  (let [radar-lines (-> args
                        first
                        slurp
                        (str/split #"\n"))
        tolerance (or (Integer/parseInt (second args))
                      5)
        invaders (detect-invaders radar-lines tolerance)]
    (log/infof "top rows %s" invaders))
  )
