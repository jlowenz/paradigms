(ns paradigms.core
  (:gen-class))

;; names are defined as a list of strings
(defn make-name [& parts]
  (apply list parts))


;; ---=== Exercise 1.1 ===---
;; - Norvig's representation
(def name-suffixes #{"Jr." "MD" "PhD" "I" "II" "III"})
(defn last-name [name]
  "Return the last name, ignoring common suffixes"
  (let [maybe-last (last name)]
    (if (name-suffixes maybe-last)
      (last-name (butlast name))
      maybe-last)))

;; ---=== Exercise 1.2 ===---
(defn power [n p]
  "Compute the n^p"
  (loop [p p
         acc 1]
    (if (> p 0)
      (recur (dec p) (* n acc))
      acc)))

;; ---=== Exercise 1.3 ===---
(defn count-atoms [expr]
  "Count the number of atoms (non-collections)"
  (if (coll? expr)
    (apply + (map count-atoms expr))    
    1))

;; ---=== Exercise 1.4 ===---
(defn count-anywhere [item expr]
  "Count the occurences of item in expr"
  (if (coll? expr)
    (apply + (map #(count-anywhere item %) expr))
    (if (= expr item) 1 0)))

;; ---=== Exercise 1.5 ===---
(defn dot [a b]
  "Compute the dot product of two equal-sized collections of numbers"
  (assert (= (count a) (count b)) "Unequal collections")
  (loop [a a
         b b
         acc 0]
    (if (not-empty a)
      (recur (rest a) (rest b) (+ acc (* (first a) (first b))))
      acc)))
