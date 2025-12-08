(ns fp-lab2.core-prop-test
  (:require [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.clojure-test :refer [defspec]]
            [fp-lab2.bag :refer [create-prefix-tree insert count-occurrences remove-one-from-bag
                                 merge-bags compare-bags empty-bag]]))

(declare prop-insert-count
         prop-remove-one
         prop-merge-commutative
         prop-merge-associative
         prop-neutral-element)

(def string-gen
  (gen/fmap #(apply str %) (gen/vector gen/char-alpha 1 10)))

(def bag-gen
  (gen/fmap (fn [keys] (create-prefix-tree keys))
            (gen/vector string-gen 0 30)))

(defspec prop-insert-count 200
  (prop/for-all [bag bag-gen
                 key string-gen]
                (= (inc (count-occurrences bag key))
                   (count-occurrences (insert bag key) key))))

(defspec prop-remove-one 200
  (prop/for-all [bag bag-gen
                 key string-gen]
                (let [b2 (remove-one-from-bag (insert bag key) key)]
                  (<= (count-occurrences b2 key) (count-occurrences (insert bag key) key)))))

(defspec prop-merge-commutative 200
  (prop/for-all [a bag-gen b bag-gen]
                (compare-bags (merge-bags a b) (merge-bags b a))))

(defspec prop-merge-associative 200
  (prop/for-all [a bag-gen b bag-gen c bag-gen]
                (compare-bags (merge-bags a (merge-bags b c))
                              (merge-bags (merge-bags a b) c))))

(defspec prop-neutral-element 200
  (prop/for-all [a bag-gen]
                (compare-bags (merge-bags a empty-bag) a)))
