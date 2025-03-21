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

(defn fuzzy-matcher
  [part line]
  (let [{:keys [eq cnt]} (reduce (fn [acc [k v]]
                                        (-> acc
                                            (update :cnt inc)
                                            (update :eq
                                                    (if (= k v)
                                                      inc
                                                      identity))))
                                   {:eq  0
                                    :cnt 0}
                                   (map vector part line))]
    (<= 0.8
        (/ eq
           cnt))))

(def MATCHERS
  {:eq =
   :fuz fuzzy-matcher})

(defn match-part
  "Looks for a match in a line. Comparison can be defined by :compare-fn.
   Returns a vector of match start and its length"
  [{:keys [part line match-fn]}]
  (let [match-fn (get MATCHERS match-fn =)
        part (seq part)
        len (count part)
        half-len (int (/ len 2))]
    (when-let [positions
               (seq
                 (remove
                   nil?
                   ;; TODO loop - recur might save some cycles
                   (into
                     ;; handle invader partially visible on the left
                     (for [prefix (range half-len len)
                           :let   [chunk (take prefix line)
                                   part (drop (- len prefix) part)]]
                       (when (match-fn
                               chunk
                               part)
                         [0 prefix]))
                     (for [start (range (- (count line)
                                           half-len))
                           :let  [chunk (take len (drop start line))
                                  chunk-len (count chunk)]]
                       (when (match-fn
                               chunk
                               (if (> len chunk-len)
                                 (take chunk-len part)
                                 part))
                         [start chunk-len])))))]
      (vec positions))))

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
  [{:keys [invader line-no start length match-fn] :as args}]
  (let [region (get-region args)
        matching-lines (keep
                         (fn [[template line]]
                           (match-part
                             {:part template
                              :line line
                              :match-fn match-fn}))
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
    (mapcat
      (fn [[line-no positions]]
        (keep
          (fn [[start length]]
            (match-invader (assoc args
                                  :line-no line-no
                                  :start   start
                                  :height  height
                                  :length  length)))
          positions))
      starting-positions)))

(s/fdef detect-invaders
  :args (s/cat :radar-lines ::sis/radar-snapshot
               :tolerance ::sis/tolerance
               :match-fn ::sis/match-fn)
  :ret ::sis/invader-positions)

(defn detect-invaders
  "The main function taking the radar reading and determining positions of known invaders."
  [radar-lines tolerance match-fn]
  (let [allowed-range (take (- (count radar-lines) tolerance) radar-lines)
        find-starting-points-for (fn [top-line]
                                   (keep-indexed
                                     (fn [idx line]
                                       (when-let [matches (match-part
                                                            {:part top-line
                                                             :line line
                                                             :match-fn match-fn})]
                                         [idx matches]))
                                     allowed-range))
        starting-positions-1 (find-starting-points-for (first invader-1))
        starting-positions-2 (find-starting-points-for (first invader-2))
        find-invaders (fn [invader starting-positions]
                        (filterv (comp not empty?)
                              (match-rest {:invader            invader
                                           :lines              radar-lines
                                           :starting-positions starting-positions
                                           :tolerance          tolerance
                                           :match-fn           match-fn})))
        invaders-1 (vec (find-invaders (rest invader-1) starting-positions-1))
        invaders-2 (find-invaders (rest invader-2) starting-positions-2)]
    (into invaders-1 invaders-2)))

(defn -main
  "Entrypoint to the application."
  [& args]
  (log/infof "Space Invaders Radar Interpreter. Got %s" args)
  (if (seq args)
    (let [[file-name tolerance match-fn] args
          radar-lines (-> file-name
                          slurp
                          (str/split #"\n"))
          tolerance   (try
                        (Integer/parseInt (or tolerance "0"))
                        (catch Exception _
                          0))
          match-fn    (keyword (or match-fn :eq))
          invaders    (detect-invaders radar-lines tolerance match-fn)]

      (if (seq invaders)
        (log/infof "Detected the following Invaders:\n%s" (str/join "\n" invaders))
        (log/info "No Space Invaders found.")))
    (log/infof "No input provided, exiting.
Please provide:
 - [Required] path to file with radar snapshot to analyse
 - [Optional] tolerance: line shift - 0 upto 3 (default: 0)
 - [Optional] matcher-fn: :eq or :fuz - equality or fuzzy match (default :eq)")))

(comment
  (def lines (-> "./invaders.txt" slurp (str/split #"\n")))
  (def invaders (detect-invaders lines 0 :eq))

  #_1)
