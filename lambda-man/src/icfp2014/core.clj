(ns icfp2014.core
  (:import (clojure.lang ArityException))
  (:use plumbing.core)
  (:require [clojure.string :as str]
            [clojure.pprint :refer (pprint)]
            [clojure.core.match :refer (match)])
  (:import [clojure.lang ArityException])
  (:refer-clojure :exclude (compile)))

(def src '((fn [x] (+ x x)) 21))

(defn check-arity [args arity name]
  (when-not (= (count args) arity)
    (throw (ArityException. (count args) name))))

(defn op [x]
  (case x
    + :add
    - :sub
    * :mul
    / :div
    = :ceq
    > :cgt
    >= :cgte
    cons :cons
    first :car
    second :cdr))

(defn single-instr [x]
  {:ins [x]})

(defn merge-compiles [compiles]
  (apply merge-with into compiles))

(defn resolve-symbols
  "Takes args like [x] and instructions like [:ld x] and returns instructions
  like [:ld 0 0] were symbols are replaced with there index in args."
  [args body]
  (let [arg-map (into {} (map-indexed #(vector %2 %1) args))]
    (mapv
      (fn [[cmd sym :as instr]]
        (if (= :ld cmd)
          (if-let [idx (arg-map sym)]
            [:ld 0 idx]
            (throw (RuntimeException. (str "Unable to resolve symbol: " sym
                                           " in this context"))))
          instr))
      body)))

(declare compile)

(defn compile-op [hd tl arity]
  (check-arity tl arity (name hd))
  (-> (mapv compile tl)
      (conj (single-instr [(op hd)]))
      (merge-compiles)))

(def fn-cnt (atom 0))

(defn compile-closure [sigs]
  (let [name (when (symbol? (first sigs)) (first sigs))
        sigs (if name (next sigs) sigs)
        name (or name (symbol (str "fn-" (swap! fn-cnt inc))))
        args (first sigs)
        body (compile (second sigs))
        _ (println "body" body)
        fn {name (resolve-symbols args (conj (:ins body) [:rtn]))}]
    {:ins [[:ldf name]]
     :fns (into fn (:fns body))}))

(defn compile-form [[hd & tl :as form]]
  (println "compile" form)
  (case hd
    (+ - * / = > >= cons)
    (compile-op hd tl 2)

    (first second)
    (compile-op hd tl 1)

    fn
    (compile-closure tl)

    (let [compiled-hd (compile hd)
          compiled-bindings (merge-compiles (map compile tl))]
      (merge-compiles [compiled-bindings compiled-hd
                       (single-instr [:ap (count tl)])]))))

(defn compile [x]
  (cond
    (list? x) (compile-form x)
    (symbol? x) (single-instr [:ld x])
    (integer? x) (single-instr [:ldc x])
    :else (throw (IllegalArgumentException. (str "Can't compile: " x)))))

(defn ldf-with-sym? [[cmd x]]
  (and (= :ldf cmd) (symbol? x)))

(defn assemble [{:keys [ins fns]}]
  (loop [ins (conj ins [:rtn])]
    (println ins)
    (let [next-addr (count ins)
          [part-1 part-2] (split-with (complement ldf-with-sym?) ins)
          [[_ sym] & part-2] part-2]
      (if (seq part-2)
        (-> (vec part-1)
            (conj [:ldf next-addr])
            (into part-2)
            (into (fns sym))
            (recur))
        ins))))

(defn emit [ins]
  (doseq [i ins]
    (let [[cmd & rest] i]
      (apply println (str/upper-case (name cmd)) rest))))

(comment
  (compile 'x)
  (compile '(+ x y))
  (compile '(fn [x] (+ x x)))
  (compile '((fn [op x] (op x x)) + 21))
  (compile '((fn [x] (+ x x)) 21))
  (compile '((fn [x] ((fn [x] (+ x x)) x)) 21))
  (compile '((fn main [go] (go 1)) (fn go [n] (* n 2))))
  (pprint *1)
  (assemble *1)
  (emit *1)
  )

