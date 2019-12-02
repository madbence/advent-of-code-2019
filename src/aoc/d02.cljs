(ns aoc.d02
  (:require [aoc.utils :refer [->int]]))

(defn step-intcode [pos code]
  (case (get code pos)
    99 nil
    1 (assoc code (get code (+ pos 3))
                  (+ (get code (get code (+ pos 1)))
                     (get code (get code (+ pos 2)))))
    2 (assoc code (get code (+ pos 3))
                  (* (get code (get code (+ pos 1)))
                     (get code (get code (+ pos 2)))))))

(defn run-intcode [noun verb code]
  (loop [code (-> code (assoc 1 noun) (assoc 2 verb))
         pos 0]
    (let [next (step-intcode pos code)]
      (if (nil? next)
        code
        (recur next (+ pos 4))))))

(defn find-intcode [target code]
  (->> (for [noun (range 0 100)
             verb (range 0 100)] [noun verb])
       (some (fn [[noun verb]]
               (if (= target (-> (run-intcode noun verb code) first))
                 [noun verb]
                 nil)))))

(defn a [input]
  (->> (-> input (.split ","))
       (map ->int)
       (into [])
       (run-intcode 12 2)
       first))

(defn b [input]
  (->> (-> input (.split ","))
       (map ->int)
       (into [])
       (find-intcode 19690720)
       ((fn [[noun verb]] (+ (* noun 100) verb)))))
