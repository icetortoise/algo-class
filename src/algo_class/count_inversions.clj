(ns algo-class.count-inversions
  (:use [clojure.core.match :only [match]]))

(defn- count-splits-and-merge [l-sorted r-sorted]
  (loop [c 0 merged []

         ls l-sorted rs r-sorted]
    (match [(first ls) (first rs)]
           [nil _] [c (vec (concat merged rs))]
           [_ nil] [c (vec (concat merged ls))]
           [l r] (if (<= l r)
                   (recur c (vec (conj merged l))
                          (next ls) rs)
                   (recur (+ c (count ls))
                          (vec (conj merged r))
                          ls (next rs))))))

(defn count-inversions [xs]
  (if (= 1 (count xs)) [0 xs]
      (let [[ls rs] (split-at (/ (count xs) 2) xs)
            [l-count l-sorted] (count-inversions (vec ls))
            [r-count r-sorted] (count-inversions (vec rs))
            [s-count merged] (count-splits-and-merge l-sorted r-sorted)]
        [(+ l-count r-count s-count) merged])))



(comment
  (def ls (slurp "/Users/andywu/projects/algo-class/src/algo_class/count-inversions.input"))
  (import '(java.io BufferedReader StringReader))
  (def lss (line-seq (BufferedReader. (StringReader. ls))))
  (def its (map #(Integer/parseInt %) lss))
  (time (first (count-inversions its))))