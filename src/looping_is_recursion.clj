(ns looping-is-recursion)

(defn power [base exp]
  (loop [acc 1
         n exp]
    (if (zero? n)
      acc
      (recur (* acc base) (- n 1)))))

(defn last-element [a-seq]
  (loop [cur nil
         rem a-seq]
    (if (empty? rem)
      cur
      (recur (first rem) (rest rem)))))

(defn seq= [seq1 seq2]
  (loop [a seq1
         b seq2]
    (cond 
      (and (empty? a) (empty? b)) true
      (or (empty? a) (empty? b)) false
      (not (= (first a) (first b))) false
      :else (recur (rest a) (rest b)))))

(defn find-first-index [pred a-seq]
  (loop [idx 0
         coll a-seq]
    (cond
      (empty? coll) nil
      (pred (first coll)) idx
      :else (recur (+ 1 idx) (rest coll)))))

(defn avg [a-seq]
  (loop [count 0
         total 0
         coll a-seq]
    (if (empty? coll)
      (/ total count)
      (recur (+ 1 count) (+ total (first coll)) (rest coll)))))

(defn toggle [a-set e]
  (if (contains? a-set e)
    (disj a-set e)
    (conj a-set e)))

(defn parity [a-seq]
  (loop [odd #{}
         coll a-seq]
    (if (empty? coll)
      odd
      (recur (toggle odd (first coll)) (rest coll)))))

(defn fast-fibo [n]
  (loop [cur 0
         next 1
         cnt n]
    (if (zero? cnt)
      cur
      (recur next (+ next cur) (- cnt 1)))))

(defn cut-at-repetition [a-seq]
  (loop [seen #{}
         orig []
         coll a-seq]
    (cond
      (empty? a-seq) orig
      (nil? (first coll)) orig
      (contains? seen (first coll)) orig
      :else (recur (conj seen (first coll)) (conj orig (first coll)) (rest coll)))))
