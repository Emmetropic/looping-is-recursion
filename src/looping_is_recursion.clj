(ns looping-is-recursion)

(defn power [base exp]
  (let [helper
        (fn [a n]
          (if (> n 2)
            (recur (* a base) (dec n))
            (* a base)))]
    (cond
     (zero? exp) 1
     (= exp 1) base
     :else (helper base exp))))

(defn last-element [a-seq]
  (let [helper
        (fn [a n]
          (if (> n 1)
            (recur (rest a) (dec n))
            (first a)))]
    (helper a-seq (count a-seq))))

(defn seq= [seq1 seq2]
  (let
    [helper
     (fn [seq1 seq2 n]
       (cond
        (> n 1)
        (if (= (first seq1) (first seq2))
          (recur (rest seq1) (rest seq2) (dec n))
          false)
        :else (= (first seq1) (first seq2))))]
  (if (= (count seq1) (count seq2))
    (helper seq1 seq2 (count seq1))
    false)))

(defn find-first-index [pred a-seq]
  (loop
    [a a-seq n (count a-seq)]
    (cond
     (> n 0)
     (if (pred (first a))
        (- (count a-seq) n)
        (recur (rest a) (dec n)))
     :else nil)))

(defn avg [a-seq]
  (loop
    [sum 0 n 0 a a-seq]
    (cond
     (empty? a-seq) 0
     (< n (count a-seq)) (recur (+ sum (first a)) (inc n) (rest a))
     :else (/ sum n))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))


;;kaytettava togglen sisaltaman contains?-metodin takia #{}, silla
;;muuten muuten metodi ei toimi
(defn parity [a-seq]
  (loop [parity-seq #{} old-seq a-seq]
    (if (empty? old-seq)
      parity-seq
      (recur (toggle parity-seq (first old-seq)) (rest old-seq)))))

(defn fast-fibo [n]
  (loop [w n f0 0 f1 1]
    (cond
     (= w 0) 0
     (= w 1) 1
     (> w 2) (recur (dec w) f1 (+ f0 f1))
     :else (+ f0 f1))))

(defn cut-at-repetition [a-seq]
  (loop [check-list #{} new-seq [] old-seq a-seq]
    (cond
     (empty? old-seq) new-seq
     (contains? check-list (first old-seq)) new-seq
     :else (recur (conj check-list (first old-seq)) (conj new-seq (first old-seq)) (rest old-seq)))))

