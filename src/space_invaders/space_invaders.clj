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
        ;; TODO loop - recur might save some cycles
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

(defn get-region
  [{:keys [lines line-no start height length tolerance]}]
  (->> lines
       (drop (inc line-no))
       (take height)
       (map (fn [line]
              (->> line
                   (drop (- start tolerance))
                   (take (+ length tolerance)))))))

(defn match-invader
  [{:keys [invader line-no start length] :as args}]
  (let [region (get-region args)
        matching-lines (keep
                         (fn [[template line]]
                           (match-part
                             {:part template
                              :line line}))
                         (zipmap invader region))]
    (when (= (count region)
             (count matching-lines))
      [[line-no start]
       [(+ line-no (count region)) (+ start length)]])))

(s/fdef match-rest
  :args (s/cat :argz ::sis/match-rest-args)
  :ret ::sis/invader-positions)
(defn match-rest
  [{:keys [invader _lines starting-positions _tolerance] :as args}]
  (let [height (count invader)]
    (log/tracef "Found starting positions %s" (first starting-positions))
    (mapcat
      (fn [[line-no positions]]
        (keep
          (fn [[start length]]
            (match-invader (assoc args
                                  :line-no   line-no
                                  :start     start
                                  :height    height
                                  :length    length)))
          positions))
      starting-positions)))

(s/fdef detect-invaders
  :args (s/cat :radar-lines ::sis/radar-snapshot
               :tolerance ::sis/tolerance)
  :ret ::sis/invader-positions)

(defn detect-invaders
  "The main function taking the radar reading and determining positions of known invaders."
  [radar-lines tolerance]
  (let [allowed-range (take (- (count radar-lines) tolerance) radar-lines)
        find-starting-points-for (fn [top-line]
                                   (keep-indexed
                                     (fn [idx line]
                                       (when-let [matches (match-part
                                                            {:part top-line
                                                             :line line})]
                                         [idx matches]))
                                     allowed-range))
        starting-positions-1 (find-starting-points-for (first invader-1))
        starting-positions-2 (find-starting-points-for (first invader-2))
        find-invaders (fn [invader starting-positions]
                        (filterv (comp not empty?)
                              (match-rest {:invader            invader
                                           :lines              radar-lines
                                           :starting-positions starting-positions
                                           :tolerance          tolerance})))
        invaders-1 (vec (find-invaders (rest invader-1) starting-positions-1))
        invaders-2 (find-invaders (rest invader-2) starting-positions-2)]
    (log/tracef "INV-1 %s INV-2 %s together %s" invaders-1 invaders-2 (into invaders-1 invaders-2))
    (into invaders-1 invaders-2)))

(defn -main
  "Entrypoint to the application."
  [& args]
  (log/infof "Space Invaders Radar Interpreter. Got %s" args)
  (if (seq args)
    (let [radar-lines (-> args
                          first
                          slurp
                          (str/split #"\n"))
          tolerance   (try
                        (Integer/parseInt (or (second args) "5"))
                        (catch Exception _
                          5))
          invaders    (detect-invaders radar-lines tolerance)]

      (if (seq invaders)
        (log/infof "Detected the following Invaders:\n%s" (str/join "\n" invaders))
        (log/info "No Space Invaders found.")))
    (log/infof "No input provided, exiting. Please provide path to file with radar snapshot to analyse (and optionally tolerance of line shift - zero or more).")))

(comment
  (def lines (-> "./invaders.txt" slurp (str/split #"\n")))
  (def invaders (detect-invaders lines 0))

  #_1)
