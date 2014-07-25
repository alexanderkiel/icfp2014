(ns icfp2014.core
  (:require [clojure.core.match :refer (match)]))

(def src '((fn [x] (+ x x)) 21))

(type src)

(let [f (first src)]
  f)

(defn ldc
  "LDC - load constant

  load an immediate literal;
  push it onto the data stack"
  [n]
  [:ldc n])

(defn ld
  "LD - load from environment

  load a value from the environment;
  push it onto the data stack"
  [n i]
  [:ld n i])

(def add
  "ADD - integer addition

  pop two integers off the data stack;
  push their sum"
  [:add])

(defn compile-closure [[_ args & body]]
  )

(compile-closure '(fn [x] x))

(defn compile-code [[f & args]]
  (match [f]
    ['+] (conj (mapv ldc args) add)))

(compile-code '(+ 1 2))

