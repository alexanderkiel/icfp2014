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

(defn load-instr
  "Takes a seq of execution contexts like ({x 0}), a level like 0 and
  a symbol like x and returns a instruction like ['LD 0 0] were
  symbols are replaced with there index in the first execution context.

  Pops an execution context if a sumbol was not found and calls itself with the
  rest of the execution contexts and the level incremented."
  [[cxt & rest] level sym]
  (if-let [idx (cxt sym)]
    ['LD level idx]
    (if rest
      (load-instr rest (inc level) sym)
      (throw (RuntimeException. (str "Unable to resolve symbol: " sym
                                     " in this context"))))))

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

(defn precompile-second [tl]
  (check-arity tl 1 "second")
  (list 'first (list 'rest (first tl))))

(def branch-cnt (atom 0))

(defn gen-branch-sym! []
  (symbol (str "branch-" (swap! branch-cnt inc))))

(defn compile-branch [cxts body]
  (let [compiled-body (compile cxts body)
        sym (gen-branch-sym!)
        fn {sym (conj (:ins compiled-body) ['JOIN])}]
    {:sym sym :prog {:fns (merge (:fns compiled-body) fn)}}))

(defn compile-if [cxts [test then else :as tl]]
  (check-arity tl 3 "if")
  (let [{then-sym :sym compiled-then :prog} (compile-branch cxts then)
        {else-sym :sym compiled-else :prog} (compile-branch cxts else)]
    (merge-compiles
      [(compile cxts test)
       (single-instr ['SEL then-sym else-sym])
       compiled-then
       compiled-else])))

(defn compile-trace [cxts [hd & tl :as form]]
  (when-not (= 'trace hd)
    (throw (IllegalArgumentException. (str "Expected trace but got " form))))
  (check-arity tl 1 "trace")
  (-> (mapv #(compile cxts %) tl)
      (conj (single-instr ['DBUG]))
      (merge-compiles)))

(def fn-cnt (atom 0))

(defn gen-fn-sym! []
  (symbol (str "fn-" (swap! fn-cnt inc))))

(defn build-cxt [args]
  (into {} (map-indexed #(vector %2 %1) args)))

(defn compile-closure [cxts sigs]
  (let [name (when (symbol? (first sigs)) (first sigs))
        sigs (if name (next sigs) sigs)
        name (or name (gen-fn-sym!))
        args (first sigs)
        cxts (cons (build-cxt args) cxts)
        body (merge-compiles (conj (mapv #(compile-trace cxts %)
                                         (butlast (rest sigs)))
                                   (compile cxts (last sigs))))
        fn {name (conj (:ins body) ['RTN])}]
    {:ins [['LDF name]]
     :fns (merge (:fns body) fn)}))

(defn compile-main [cxts [args body]]
  {:pre [(nil? cxts)]}
  (let [{:keys [ins] :as body} (compile (list (build-cxt args)) body)]
    (assoc-in body [:ins] (conj ins ['RTN]))))

(defn compile-form [cxts [hd & tl]]
  (case hd
    (+ - * / = > >= cons)
    (compile-op cxts hd tl 2)

    (first rest)
    (compile-op cxts hd tl 1)

    let
    (compile cxts (precompile-let tl))

    second
    (compile cxts (precompile-second tl))

    if
    (compile-if cxts tl)

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
    (symbol? x) (single-instr (load-instr cxts 0 x))
    (integer? x) (single-instr ['LDC x])
    :else (throw (IllegalArgumentException. (str "Can't compile: " x)))))

(defn cmd-with-sym? [[cmd & args]]
  (and (#{'LDF 'SEL} cmd) (every? symbol? args)))

(defn line-comment? [instr]
  (every? string? instr))

(defn assemble-ldf [part-1 [cmd sym] part-2 fns next-addr]
  (-> (vec part-1)
      (conj [cmd next-addr (name sym)])
      (into part-2)
      (into [[(name sym)]])
      (into (fns sym))))

(defn assemble-sel [part-1 [cmd then-sym else-sym] part-2 fns next-addr]
  (let [then-branch (fns then-sym)
        else-branch (fns else-sym)]
    (-> (vec part-1)
        (conj [cmd next-addr (+ next-addr (count then-branch))
               (str then-sym " " else-sym)])
        (into part-2)
        (into [[(name then-sym)]])
        (into then-branch)
        (into [[(name else-sym)]])
        (into else-branch))))

(defn assemble [{:keys [ins fns] :or {fns {}}}]
  (loop [ins ins]
    (let [next-addr (count (remove line-comment? ins))
          [part-1 part-2] (split-with (complement cmd-with-sym?) ins)
          [[cmd :as instr] & part-2] part-2]
      (if (seq part-2)
        (case cmd
          LDF (recur (assemble-ldf part-1 instr part-2 fns next-addr))
          SEL (recur (assemble-sel part-1 instr part-2 fns next-addr)))
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
  (let [and (fn [a b] (= 2 (+ a b)))
        pair= (fn [a b] (and (= (first a) (first b))
                             (= (rest a) (rest b))))
        adv-dir (fn [dir] (if (= dir 3) 0 (+ dir 1)))]
    (cons
      (let [status (second world)] (second status))
      (fn step [old-loc world]
        (let [status (second world)
              cur-loc (second status)
              cur-dir (second (rest status))]
          (trace old-loc)
          (trace cur-loc)
          (trace cur-dir)
          (cons
            cur-loc
            (if (pair= old-loc cur-loc) (adv-dir cur-dir) cur-dir)))))))

(comment

  (->> ai (compile nil) assemble emit)
  (pst)

  )

