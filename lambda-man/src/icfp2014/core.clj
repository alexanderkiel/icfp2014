(ns icfp2014.core
  (:import (clojure.lang ArityException))
  (:use plumbing.core)
  (:require [clojure.string :as str]
            [clojure.pprint :refer (pprint)]
            [clojure.repl :refer (pst)]
            [clojure.core.match :refer (match)])
  (:import [clojure.lang ArityException])
  (:refer-clojure :exclude (compile)))

(def src '((fn [x] (+ x x)) 21))

(defn check-arity [args arity name]
  (when-not (= (count args) arity)
    (throw (ArityException. (count args) name))))

(defn op [x]
  (case x
    + 'ADD
    - 'SUB
    * 'MUL
    / 'DIV
    = 'CEQ
    > 'CGT
    >= 'CGTE
    cons 'CONS
    first 'CAR
    rest 'CDR))

(defn single-instr [x]
  {:ins [x]})

(defn merge-compiles [compiles]
  (apply merge-with into compiles))

(defn resolve-symbols
  "Takes a seq of execution contexts like ([x]), a level like 0 and
  a instruction like ['LD x] and returns a instruction like ['LD 0 0] were
  symbols are replaced with there index in the first execution context.

  Pops an execution context if a sumbol was not found and calls itself with the
  rest of the execution contexts and the level incremented."
  [[cxt & rest] level [cmd sym :as instr]]
  (println "resolve-symbols - cxt" cxt "instr" instr)
  (let [cxt-map (into {} (map-indexed #(vector %2 %1) cxt))]
    (if (= 'LD cmd)
      (if-let [idx (cxt-map sym)]
        ['LD level idx]
        (if rest
          (resolve-symbols rest (inc level) instr)
          (throw (RuntimeException. (str "Unable to resolve symbol: " sym
                                         " in this context")))))
      instr)))

(declare compile)

(defn compile-op [cxts hd tl arity]
  (check-arity tl arity (name hd))
  (-> (mapv #(compile cxts %) tl)
      (conj (single-instr [(op hd)]))
      (merge-compiles)))

(defn precompile-let [[bindings & body]]
  (when-not (even? (count bindings))
    (throw (IllegalArgumentException. "Expected an even number of forms in
    binding vector.")))
  (let [[arg form & rest] bindings]
    (if rest
      (list (list 'fn [arg] (precompile-let (cons rest body))) form)
      (list (apply list 'fn [arg] body) form))))

(comment
  (precompile-let '([lms (first (rest world))
                     loc (first (rest lms))]
                    (trace loc)
                    (cons state 1)))
  (compile nil *1)
  (pst)

  ((fn [lms]
     (fn [loc]
       (trace loc)
       (cons state 1))
     (first (rest lms))) (first (rest world)))
  )

(defn compile-trace [cxts [hd & tl :as form]]
  (when-not (= 'trace hd)
    (throw (IllegalArgumentException. (str "Expected trace but got " form))))
  (check-arity tl 1 "trace")
  (-> (mapv #(compile cxts %) tl)
      (conj (single-instr ['DBUG]))
      (merge-compiles)))

(def fn-cnt (atom 0))

(defn compile-closure [cxts sigs]
  (let [name (when (symbol? (first sigs)) (first sigs))
        sigs (if name (next sigs) sigs)
        name (or name (symbol (str "fn-" (swap! fn-cnt inc))))
        args (first sigs)
        cxts (cons args cxts)
        body (merge-compiles (conj (mapv #(compile-trace cxts %)
                                         (butlast (rest sigs)))
                                   (compile cxts (last sigs))))
        resolved-body-ins (mapv #(resolve-symbols cxts 0 %) (:ins body))
        fn {name (conj resolved-body-ins ['RTN])}]
    {:ins [['LDF name]]
     :fns (into fn (:fns body))}))

(defn compile-main [cxts [args body]]
  (let [{:keys [ins] :as body} (compile cxts body)
        ins (mapv #(resolve-symbols (cons args cxts) 0 %) ins)]
    (assoc-in body [:ins] (conj ins ['RTN]))))

(defn compile-form [cxts [hd & tl]]
  (case hd
    (+ - * / = > >= cons)
    (compile-op cxts hd tl 2)

    (first rest)
    (compile-op cxts hd tl 1)

    let
    (compile cxts (precompile-let tl))

    fn
    (compile-closure cxts tl)

    main
    (compile-main cxts tl)

    (let [compiled-hd (compile cxts hd)
          compiled-bindings (merge-compiles (map #(compile cxts %) tl))]
      (merge-compiles [compiled-bindings compiled-hd
                       (single-instr ['AP (count tl)])]))))

(defn compile [cxts x]
  (cond
    (sequential? x) (compile-form cxts x)
    (symbol? x) (single-instr ['LD x])
    (integer? x) (single-instr ['LDC x])
    :else (throw (IllegalArgumentException. (str "Can't compile: " x)))))

(defn ldf-with-sym? [[cmd x]]
  (and (= 'LDF cmd) (symbol? x)))

(defn line-comment? [instr]
  (every? string? instr))

(defn assemble [{:keys [ins fns] :or {fns {}}}]
  (loop [ins ins]
    (let [next-addr (count (remove line-comment? ins))
          [part-1 part-2] (split-with (complement ldf-with-sym?) ins)
          [[_ sym] & part-2] part-2]
      (if (seq part-2)
        (-> (vec part-1)
            (conj ['LDF next-addr (name sym)])
            (into part-2)
            (into [[(name sym)]])
            (into (fns sym))
            (recur))
        ins))))

(defn emit-instr [instr]
  {:pre [(vector? instr)]}
  (let [last (peek instr)]
    (if (string? last)
      (if (= 1 (count instr))
        (println ";" last)
        (println (format "%-10s; %s" (str/join " " (butlast instr)) last)))
      (apply println instr))))

(defn emit [ins]
  (doseq [instr ins]
    (emit-instr instr)))

(defmacro defprog [name args body]
  `(def ~name '(~'main ~args ~body)))

;; ---- Lambda-Man Interface --------------------------------------------------

(defprog always-right-ai [world _]
  (cons
    world
    (fn step [state world]
      (cons state 1))))

(defprog ai [world _]
  (cons
    0
    (fn step [state world]
      (let [lms (first (rest world))
            loc (first (rest lms))]
        (trace loc)
        (cons state 1)))))

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

  (-> always-right-ai compile assemble emit)
  (->> ai (compile nil) assemble emit)
  (pst)
  )

