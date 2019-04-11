(ns homework.core
  (:require [clojure.string :as str])
  (:gen-class))

(def str->numchar
  "Mapping of joined string to number character"
  {" ||_ _ ||" \0
   "       ||" \1
   "  |___ | " \2
   "   ___ ||" \3
   " |  _  ||" \4
   " | ___  |" \5
   " ||___  |" \6
   "   _   ||" \7
   " ||___ ||" \8
   " | ___ ||" \9})

(defn read-file
  "Reads text from file and forms a list of tuples"
  [filename]
  (->> (slurp filename)
       str/split-lines
       (take 56)
       (partition-all 4)))

(defn parse
  "Takes a tuple representing a number and returns the string representation of
  that number.

  Each tuple is a string of the top line of the number, the middle line, and the
  bottom line"
  [[top middle bottom]]
  (let [slices (map #(str %1 %2 %3) top middle bottom)
        strings (map #(str/join "" %) (partition 3 slices))
        str-num (str/join (map #(get str->numchar % \?) strings))]
    str-num))

(defn valid-checksum?
  "Validates a given account number via checksum"
  [n]
  (let [product (->> n
                     (map #(Character/digit % 10))
                     (map + [0 9 8 7 6 5 4 3 2])
                     (reduce *))]
    (= 0 (mod product 11))))

(defn validate
  "Appends an error string to an account number if it fails validation. Otherwise
  returns the original number"
  [n]
  (if-let [err-code (cond
                      (str/includes? n "?") "ILL"
                      (not (valid-checksum? n)) "ERR")]
    (str n " " err-code)
    n))

(defn -main
  "Read in filename and print out numbers"
  [filename]
  (let [content (read-file filename)
        acc-numbers (map parse content)
        validated (map validate acc-numbers)]
    (println (str/join "\n" validated))))
