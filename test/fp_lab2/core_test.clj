(ns fp-lab2.core-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [fp-lab2.bag :refer [create-prefix-tree count-occurrences
                                 remove-one-from-bag entries
                                 filter-bag map-entries entries-with-mapped-keys
                                 merge-bags compare-bags fold-left-trie fold-right-trie]]))

(deftest test-insert-and-count
  (let [tree (create-prefix-tree ["apple" "banana" "apple"])]
    (is (= 2 (count-occurrences tree "apple")))
    (is (= 1 (count-occurrences tree "banana")))
    (is (= 0 (count-occurrences tree "grape")))
    (is (= (set (entries tree)) #{["apple" 2] ["banana" 1]}))))

(deftest test-remove-one
  (let [tree (-> (create-prefix-tree ["apple" "apple" "banana"])
                 (remove-one-from-bag "apple")
                 (remove-one-from-bag "banana"))]
    (is (= 1 (count-occurrences tree "apple")))
    (is (= 0 (count-occurrences tree "banana")))
    (is (= 0 (count-occurrences tree "pear")))
    (is (= (set (entries tree)) #{["apple" 1]}))))

(deftest test-filter
  (let [tree (create-prefix-tree ["apple" "banana" "apricot"])
        out (filter-bag tree #(str/starts-with? % "a"))]
    (is (= (set (entries out)) #{["apple" 1] ["apricot" 1]}))))

(deftest test-map-entries
  (let [tree (create-prefix-tree ["apple" "banana" "apple"])
        mapped (map-entries tree (fn [k cnt] [(str k "-mapped") (* cnt 2)]))]
    (is (= (set (entries mapped)) #{["apple-mapped" 4] ["banana-mapped" 2]}))
    (is (= (set (entries-with-mapped-keys mapped)) (set (entries mapped))))))

(deftest test-merge-and-compare
  (let [a (create-prefix-tree ["apple" "banana"])
        b (create-prefix-tree ["apple" "apricot"])
        merged (merge-bags a b)]
    (is (= (set (entries merged)) #{["apple" 2] ["apricot" 1] ["banana" 1]}))
    (is (compare-bags a (create-prefix-tree ["apple" "banana"])))
    (is (not (compare-bags a b)))))

(deftest test-folds
  (let [bag (create-prefix-tree ["apple" "banana" "banana" "cherry"])]
    (is (= (fold-left-trie bag (fn [acc _ cnt] (+ acc cnt)) 0) 4))
    (is (= (fold-right-trie bag (fn [acc _ cnt] (+ acc cnt)) 0) 4))
    (is (= (fold-left-trie bag (fn [acc key _] (conj acc key)) []) ["apple" "banana" "cherry"]))))

(deftest test-seq-functions
  (let [bag (create-prefix-tree ["apple" "banana" "apple" "cherry"])]
    ;; map
    (is (= {"APPLE" 2 "BANANA" 1 "CHERRY" 1}
           (frequencies (map clojure.string/upper-case (seq bag)))))
    ;; reduce
    (is (= 4 (reduce (fn [acc _] (inc acc)) 0 bag)))
    ;; filter
    (is (= {"apple" 2}
           (frequencies (filter #(= "apple" %) (seq bag)))))
    ;; count
    (is (= 4 (count bag)))
    ;; distinct
    (is (= #{"apple" "banana" "cherry"}
           (set (distinct (seq bag)))))
    ;; sort
    (is (= ["apple" "apple" "banana" "cherry"]
           (sort (seq bag))))
    ;; group-by
    (let [bag2 (create-prefix-tree ["apple" "banana" "apple" "apricot"])]
      (is (= {"a" ["apple" "apple" "apricot"]
              "b" ["banana"]}
             (group-by #(str (first %)) (seq bag2)))))))
