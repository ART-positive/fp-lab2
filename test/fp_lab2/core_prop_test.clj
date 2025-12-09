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

(def vec-chars-gen
  (gen/fmap (fn [chars] (vec chars)) (gen/vector gen/char-alpha 1 6)))

(def vec-ints-gen
  (gen/fmap (fn [xs] (vec xs)) (gen/vector (gen/choose 0 9) 1 6)))

(def key-gen
  (gen/one-of [string-gen vec-chars-gen vec-ints-gen]))

(def pair-gen
  (gen/tuple key-gen (gen/choose 1 6)))

(def bag-gen
  (gen/fmap (fn [pairs]
              (let [expanded (mapcat (fn [[k n]] (repeat n k)) pairs)]
                (create-prefix-tree expanded)))
            (gen/vector pair-gen 0 20)))

(defspec prop-insert-count 200
  (prop/for-all [bag bag-gen
                 key key-gen]
                (= (inc (count-occurrences bag key))
                   (count-occurrences (insert bag key) key))))

(defspec prop-remove-one 200
  (prop/for-all [bag bag-gen
                 key key-gen]
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
                (and (compare-bags (merge-bags a empty-bag) a)
                     (compare-bags (merge-bags empty-bag a) a))))
