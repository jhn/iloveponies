(defn singleton? [col]
  (if (empty? col) false
    (if (empty? (rest col)) true false)))

(defn my-last [col]
  (if (empty? col) false
    (if (singleton? col) (first col)
      (my-last (rest col)))))

(my-last [1 2 3])

(defn max-element [col]
  (if (empty? col) nil
    (if (singleton? col) (first col)
      (max (first col) (max-element (rest col))))))

(max-element [2 4 1 4])
(max-element [2])

(defn seq-max [f s]
  (if (> (count f) (count s)) f s))

(seq-max [1] [1 2])
(seq-max [1 2] [3 4])
(seq-max [] [1 2 3])

(defn longest-sequence [col]
  (if (empty? col) nil
    (if (singleton? col) (first col)
      (seq-max (first col) (longest-sequence(rest col))))))

(longest-sequence [[1 2] [] [1 2 3]])
(longest-sequence [[1 2]])
(longest-sequence [])

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(my-filter odd? [1 2 3 4])
(my-filter (fn [x] (> x 9000)) [12 49 90 9001])
(my-filter even? [1 3 5 7])

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= (first a-seq) elem) true
    :else (sequence-contains? elem (rest a-seq))))

(sequence-contains? 3 [1 2 3]) ;=> true
(sequence-contains? 3 [4 7 9]) ;=> false
(sequence-contains? :pony [])  ;=> false

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons
        (first a-seq)
        (my-take-while pred? (rest a-seq)))
      '())))

(my-take-while odd? [1 2 3 4])  ;=> (1)
(my-take-while odd? [1 3 4 5])  ;=> (1 3)
(my-take-while even? [1 3 4 5]) ;=> ()
(my-take-while odd? [])         ;=> ()

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(my-drop-while odd? [1 2 3 4])  ;=> (2 3 4)
(my-drop-while odd? [1 3 4 5])  ;=> (4 5)
(my-drop-while even? [1 3 4 5]) ;=> (1 3 4 5)
(my-drop-while odd? [])         ;=> ()

(defn seq= [seq-1 seq-2]
  (cond
    (and
      (empty? seq-1)
      (empty? seq-2))
    true
    (or
      (empty? seq-1)
      (empty? seq-2))
    false
    :else (let [[x & xs] seq-1
                [y & ys] seq-2]
            (if (not (= x y))
              false
              (recur xs ys)))))

(seq= [1 2 4] '(1 2 4))  ;=> true
(seq= [1 2 3] [1 2 3 4]) ;=> false
(seq= [1 3 5] [])        ;=> false

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (let [[x & xs] seq-1
          [y & ys] seq-2]
      (cons (f x y) (my-map f xs ys)))))

(my-map + [1 2 3] [4 4 4])   ;=> (5 6 7)
(my-map + [1 2 3 4] [0 0 0]) ;=> (1 2 3)
(my-map + [1 2 3] [])        ;=> ()

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(power 2 2)  ;=> 4
(power 5 3)  ;=> 125
(power 7 0)  ;=> 1
(power 0 10) ;=> 0

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib(- n 1))
       (fib(- n 2)))))

(fib 0) ;=> 0
(fib 1) ;=> 1
(fib 2) ;=> 1
(fib 3) ;=> 2
(fib 10) ;=> 55

(defn my-repeat [how-many what-to]
  (if (<= how-many 0)
    '()
    (cons what-to
          (my-repeat (dec how-many) what-to))))

(my-repeat 2 :a)    ;=> (:a :a)
(my-repeat 3 "lol") ;=> ("lol" "lol" "lol")
(my-repeat -1 :a)   ;=> ()

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to)
          (my-range (dec up-to)))))

(my-range 0)  ;=> ()
(my-range 1)  ;=> (0)
(my-range 2)  ;=> (1 0)
(my-range 3)  ;=> (2 1 0)

(defn tails [col]
  (if (empty? col)
    '(())
    (cons (seq col) (tails (rest col)))))

(tails [1 2 3 4]) ;=> ((1 2 3 4) (2 3 4) (3 4) (4) ())
(inits [1 2 3 4]) ;=> (() (1) (1 2) (1 2 3) (1 2 3 4))

(defn inits [col]
  (if (empty? col)
    '(())
    (let [coll (reverse col)]
      (reverse (map reverse (tails coll))))))

(inits [1 2 3 4]) ;=> ((1 2) () (1 2 3) (1) (1 2 3 4))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (inits a-seq)))))

(rotations [])        ;=> (())
(rotations [1 2 3])   ;=> ((1 2 3) (2 3 1) (3 1 2))
(rotations [:a :b])   ;=> ((:a :b) (:b :a))
; The order of rotations does not matter.
(rotations [:a :b])   ;=> ((:b :a) (:a :b))
(rotations [1 5 9 2]) ;=> ((1 5 9 2) (2 1 5 9) (9 2 1 5) (5 9 2 1))
(count (rotations [6 5 8 9 2])) ;=> 5

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [k (first a-seq)
          new-freqs (if (contains? freqs k)
                        (assoc freqs k (inc (freqs k)))
                        (assoc freqs k 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(my-frequencies []) ;=> {}
(my-frequencies [:a "moi" :a "moi" "moi" :a 1]) ;=> {:a 3, "moi" 3, 1 1}

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[k v] (first a-map)]
      (concat (repeat v k)) (un-frequencies (rest a-map)))))

(un-frequencies {:a 3 :b 2 "^_^" 1})             ;=> (:a :a :a "^_^" :b :b)
(un-frequencies (my-frequencies [:a :b :c :a]))  ;=> (:a :a :b :c)
(my-frequencies (un-frequencies {:a 100 :b 10})) ;=> {:a 100 :b 10}

(defn my-take [n coll]
  (if (or (empty? coll) (zero? n))
    '()
    (cons (first coll)
          (my-take (dec n) (rest coll)))))

(my-take 2 [1 2 3 4]) ;=> (1 2)
(my-take 4 [:a :b])   ;=> (:a :b)

(defn my-drop [n coll]
  (if (zero? n)
    coll
    (if (empty? coll)
      '()
      (my-drop (dec n) (rest coll)))))

(my-drop 2 [1 2 3 4]) ;=> (3 4)
(my-drop 4 [:a :b])   ;=> ()

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    (vector (my-take half a-seq) (my-drop half a-seq))))

(halve [1 2 3 4])   ;=> [(1 2) (3 4)]
(halve [1 2 3 4 5]) ;=> [(1 2) (3 4 5)]
(halve [1])         ;=> [() (1)]
(halve [])          ;=> [() (1)]

(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq)
    b-seq
    (if (empty? b-seq)
      a-seq
      (let [[x & xs] a-seq
            [y & ys] b-seq]
        (if (< x y)
          (cons x (seq-merge xs b-seq))
          (cons y (seq-merge a-seq ys)))))))

(seq-merge [4] [1 2 6 7])        ;=> (1 2 4 6 7)
(seq-merge [1 5 7 9] [2 2 8 10]) ;=> (1 2 2 5 7 8 9 10)

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))

(merge-sort [])                 ;=> ()
(merge-sort [1 2 3])            ;=> (1 2 3)
(merge-sort [5 3 4 17 2 100 1]) ;=> (1 2 3 4 5 17 100)

