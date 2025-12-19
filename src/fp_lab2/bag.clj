(ns fp-lab2.bag
  (:import (clojure.lang IReduceInit Seqable Counted)))

(declare entries insert count-occurrences remove-one-from-bag
         merge-bags compare-bags)

(defn- empty-trie
  []
  {:count 0 :children {}})

(defn- sort-child-keys
  [m]
  (sort-by pr-str (keys m)))

(deftype ^:private TrieBag [trie]
  Seqable
  (seq [this]
    (let [es (entries this)]
      (when (seq es)
        (mapcat (fn [[k n]] (repeat n k)) es))))

  java.lang.Iterable
  (iterator [this]
    (.iterator ^java.lang.Iterable (seq this)))

  IReduceInit
  (reduce [this f init]
    (let [es (entries this)]
      (loop [s (seq es) acc init]
        (if (seq s)
          (let [[k n] (first s)
                acc2 (loop [i n a acc]
                       (if (zero? i) a (recur (dec i) (f a k))))]
            (recur (next s) acc2))
          acc))))

  Counted
  (count [this]
    (reduce (fn [acc [_ n]] (+ acc n)) 0 (entries this)))

  Object
  (equals [this other]
    (and (instance? TrieBag other)
         (compare-bags this other)))
  (hashCode [this]
    (hash (into {} (entries this)))))

(defn- get-trie
  [^TrieBag bag]
  (.trie bag))

(defn create-prefix-tree
  [keys]
  (reduce (fn [bag k] (insert bag k))
          (TrieBag. (empty-trie))
          keys))

(defn- insert-seq
  [trie elems n orig-key]
  (letfn [(ins [node s]
            (if (empty? s)
              (let [updated-count (update node :count (fn [c] (+ (or c 0) n)))
                    updated-node  (if (and (nil? (:orig-key updated-count)) (some? orig-key))
                                    (assoc updated-count :orig-key orig-key)
                                    updated-count)]
                updated-node)
              (let [e (first s)
                    next-node (get-in node [:children e] (empty-trie))
                    updated-next (ins next-node (rest s))]
                (assoc-in node [:children e] updated-next))))]
    (ins trie elems)))

(defn insert
  [^TrieBag bag key]
  (let [elems (seq key)]
    (TrieBag. (insert-seq (get-trie bag) elems 1 key))))

(defn- insert-n
  [^TrieBag bag key n]
  (if (<= n 0)
    bag
    (let [elems (seq key)]
      (TrieBag. (insert-seq (get-trie bag) elems n key)))))

(defn count-occurrences
  [^TrieBag bag key]
  (letfn [(lookup [node s]
            (if (empty? s)
              (or (:count node) 0)
              (let [next (get-in node [:children (first s)])]
                (if next (lookup next (rest s)) 0))))]
    (lookup (get-trie bag) (seq key))))

(defn- remove-seq
  [trie elems n]
  (letfn [(rem-1 [node s]
            (if (empty? s)
              (let [newnode (update node :count (fn [c] (max 0 (- (or c 0) n))))]
                ;; if count dropped to zero, remove stored orig-key (keep children if any)
                (if (zero? (or (:count newnode) 0))
                  (dissoc newnode :orig-key)
                  newnode))
              (let [e (first s)
                    next-node (get-in node [:children e])]
                (if (nil? next-node)
                  node
                  (let [updated-next (rem-1 next-node (rest s))
                        node-with-child (if (and (zero? (or (:count updated-next) 0))
                                                 (empty? (:children updated-next)))
                                          (update node :children dissoc e)
                                          (assoc-in node [:children e] updated-next))]
                    node-with-child)))))]
    (rem-1 trie elems)))

(defn remove-one-from-bag
  [^TrieBag bag key]
  (TrieBag. (remove-seq (get-trie bag) (seq key) 1)))

(defn trie-keys
  [^TrieBag bag]
  (map first (entries bag)))

(defn entries
  [^TrieBag bag]
  (letfn [(collect [node prefix]
            (let [cur (when (> (or (:count node) 0) 0)
                        (if (contains? node :orig-key)
                          [(:orig-key node) (:count node)]
                          [(apply str prefix) (:count node)]))
                  children (mapcat (fn [k]
                                     (collect (get-in node [:children k])
                                              (conj prefix k)))
                                   (sort-child-keys (:children node)))]
              (if cur
                (cons cur children)
                children)))]
    (collect (get-trie bag) [])))

(defn entries-with-mapped-keys
  [bag]
  (entries bag))

(defn compare-tries
  [t1 t2]
  (letfn [(cmp [n1 n2]
            (and (= (or (:count n1) 0) (or (:count n2) 0))
                 (= (set (keys (:children n1))) (set (keys (:children n2))))
                 (every? (fn [k]
                           (cmp (get-in n1 [:children k]) (get-in n2 [:children k])))
                         (keys (:children n1)))))]
    (cmp t1 t2)))

(defn compare-bags
  [^TrieBag a ^TrieBag b]
  (and (instance? TrieBag a)
       (instance? TrieBag b)
       (compare-tries (get-trie a) (get-trie b))))

(defn merge-bags
  [^TrieBag a ^TrieBag b]
  (reduce (fn [acc [k n]]
            (insert-n acc k n))
          a
          (entries b)))

(defn filter-bag
  [^TrieBag bag pred]
  (let [ke (filter (fn [[k _]] (pred k)) (entries bag))]
    (reduce (fn [acc [k n]] (insert-n acc k n))
            (TrieBag. (empty-trie))
            ke)))

(defn map-entries
  [^TrieBag bag f]
  (let [mapped (map (fn [[k n]] (f k n)) (entries bag))]
    (reduce (fn [acc [k n]] (insert-n acc k n))
            (TrieBag. (empty-trie))
            mapped)))

(defn fold-left-trie
  [^TrieBag bag f initial]
  (reduce (fn [acc [k n]] (f acc k n)) initial (entries bag)))

(defn fold-right-trie
  [^TrieBag bag f initial]
  (reduce (fn [acc [k n]] (f acc k n)) initial (reverse (vec (entries bag)))))

(def empty-bag (TrieBag. (empty-trie)))
