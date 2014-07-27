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
    rest 'CDR
    empty? 'ATOM))

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
  (-> (mapv #(compile cxts % false) tl)
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

(def branch-cnt (atom 0))

(defn gen-branch-sym! []
  (symbol (str "branch-" (swap! branch-cnt inc))))

(defn append-rtn [ins]
  {:pre [(vector? ins)]}
  (case (first (last ins))
    (TSEL TAP) ins
    (conj ins ['RTN])))

(defn append-join [prog]
  (update-in prog [:ins] conj ['JOIN]))

(defn compile-branch [cxts body tail-pos]
  (let [compiled-body (compile cxts body tail-pos)
        compiled-body (if tail-pos
                        (update-in compiled-body [:ins] append-rtn)
                        (append-join compiled-body))
        sym (gen-branch-sym!)
        fn {sym (:ins compiled-body)}]
    {:sym sym :prog {:fns (merge (:fns compiled-body) fn)}}))

(defn compile-if [cxts [test then else :as tl] tail-pos]
  (check-arity tl 3 "if")
  (let [{then-sym :sym compiled-then :prog} (compile-branch cxts then tail-pos)
        {else-sym :sym compiled-else :prog} (compile-branch cxts else tail-pos)]
    (merge-compiles
      [(compile cxts test)
       (single-instr [(if tail-pos 'TSEL 'SEL) then-sym else-sym])
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
                                   (compile cxts (last sigs) true)))
        fn {name (append-rtn (:ins body))}]
    {:ins [['LDF name]]
     :fns (merge (:fns body) fn)}))

(defn compile-recur [cxts tl tail-pos]
  (when-not tail-pos
    (throw (IllegalArgumentException. "recur not on tail position")))
  (let [load-closure-instr (single-instr ['LD 0 (count (first cxts))])]
    (-> (mapv #(compile cxts %) tl)
        (conj load-closure-instr)
        (conj load-closure-instr)
        (conj (single-instr ['TAP (inc (count (first cxts)))]))
        (merge-compiles))))

(defn compile-main [cxts [args body]]
  {:pre [(nil? cxts)]}
  (let [{:keys [ins] :as body} (compile (list (build-cxt args)) body)]
    (assoc-in body [:ins] (conj ins ['RTN]))))

(defn compile-form [cxts [hd & tl] tail-pos]
  (case hd
    (+ - * / = > >= cons)
    (compile-op cxts hd tl 2)

    (first rest empty?)
    (compile-op cxts hd tl 1)

    let
    (compile cxts (precompile-let tl) tail-pos)

    if
    (compile-if cxts tl tail-pos)

    fn
    (compile-closure cxts tl)

    recur
    (compile-recur cxts tl tail-pos)

    main
    (compile-main cxts tl)

    (let [compiled-hd (compile cxts hd)
          compiled-bindings (merge-compiles (map #(compile cxts %) tl))]
      (-> [compiled-bindings
           compiled-hd
           {:ins [(last (:ins compiled-hd))]}
           (single-instr ['AP (inc (count tl))])]
          (merge-compiles)))))

(defn compile-symbol [cxts x tail-pos]
  (single-instr (load-instr cxts 0 x)))

(defn compile
  ([cxts x]
   (compile cxts x false))
  ([cxts x tail-pos]
   (cond
     (sequential? x) (compile-form cxts x tail-pos)
     (symbol? x) (compile-symbol cxts x tail-pos)
     (integer? x) (single-instr ['LDC x])
     (nil? x) (compile cxts 0 tail-pos)
     :else (throw (IllegalArgumentException. (str "Can't compile: " x))))))

(defn cmd-with-sym? [[cmd & args]]
  (and (#{'LDF 'SEL 'TSEL} cmd) (every? symbol? args)))

(defn line-comment? [instr]
  (every? string? instr))

(defn assemble-ldf [part-1 [cmd sym] part-2 fns next-addr]
  (-> (vec part-1)
      (conj [cmd next-addr (name sym)])
      (into part-2)
      ;(into [[(name sym)]])
      (into (safe-get fns sym))))

(defn assemble-ldf-only [part-1 [cmd sym] part-2 addr]
  (-> (vec part-1)
      (conj [cmd addr (name sym)])
      (into part-2)))

(defn assemble-sel [part-1 [cmd then-sym else-sym] part-2 fns next-addr]
  (let [then-branch (safe-get fns then-sym)
        else-branch (safe-get fns else-sym)]
    (-> (vec part-1)
        (conj [cmd next-addr (+ next-addr (count then-branch))
               (str then-sym " " else-sym)])
        (into part-2)
        ;(into [[(name then-sym)]])
        (into then-branch)
        ;(into [[(name else-sym)]])
        (into else-branch))))

(defn assemble [{:keys [ins fns] :or {fns {}}}]
  (loop [ins ins
         allocated-fns {}]
    (let [next-addr (count (remove line-comment? ins))
          [part-1 part-2] (split-with (complement cmd-with-sym?) ins)
          [[cmd :as instr] & part-2] part-2]
      (if instr
        (case cmd
          LDF
          (let [[_ sym] instr]
            (if-let [addr (allocated-fns sym)]
              (recur (assemble-ldf-only part-1 instr part-2 addr)
                     allocated-fns)
              (recur (assemble-ldf part-1 instr part-2 fns next-addr)
                     (assoc allocated-fns sym next-addr))))
          (SEL TSEL)
          (recur (assemble-sel part-1 instr part-2 fns next-addr) allocated-fns))
        ins))))

(defn emit-instr [instr]
  {:pre [(vector? instr)]}
  (let [last (peek instr)]
    (if (string? last)
      (if (= 1 (count instr))
        (println ";" last)
        (println (format "%-12s; %s" (str/join " " (butlast instr)) last)))
      (apply println instr))))

(defn emit [ins]
  (doseq [instr ins]
    (emit-instr instr)))

(defmacro defprog [name args body]
  `(def ~name '(~'main ~args ~body)))

;; ---- Lambda-Man Interface --------------------------------------------------

(def trace println)

(defprog ai [world _]
  (let [up 0 right 1 down 2 left 3
        wall 0 empty 1 pill 2 power-pill 3
        and (fn [a b] (= 2 (+ a b)))
        not (fn [a] (if a 0 1))
        pair= (fn [a b] (and (= (first a) (first b))
                             (= (rest a) (rest b))))
        inc (fn [x] (+ x 1))
        dec (fn [x] (- x 1))
        inc-dir (fn [dir]
                  (if (= dir 3) 0 (inc dir)))
        second (fn [coll] (first (rest coll)))
        drop (fn [n coll]
               (if (and (> n 0) (not (empty? coll)))
                 (recur (- n 1) (rest coll))
                 coll))
        r-reduce (fn [f start coll]
                   (if (empty? coll)
                     start
                     (recur f (f (first coll) start) (rest coll))))
        map (fn [f coll]
              (r-reduce (fn [v ret] (cons v ret)) nil
                        (r-reduce (fn [v ret] (cons (f v) ret)) nil coll)))
        x (fn [dir] (first dir))
        y (fn [dir] (rest dir))
        go (fn [loc dir]
             (let [x (first loc) y (rest loc)]
               (if (= dir up)
                 (cons x (dec y))
                 (if (= dir right)
                   (cons (inc x) y)
                   (if (= dir down)
                     (cons x (inc y))
                     (cons (dec x) y))))))
        field-at-loc (fn [map loc]
                       (let [row (first (drop (rest loc) map))]
                         (first (drop (first loc) row))))
        look-ahead (fn [map loc dir]
                     (field-at-loc map (go loc dir)))
        look-for (fn [map loc start-dir things num-tries]
                   (if (= 4 num-tries)
                     (recur map loc start-dir (rest things) 0)
                     (if (= (first things) (look-ahead map loc start-dir))
                       start-dir
                       (recur map loc (inc-dir start-dir) things (inc num-tries)))))
        pill-query (cons power-pill (cons pill (cons empty nil)))
        abs (fn [x] (if (>= x 0) x (- 0 x)))
        dist-vec (fn [l1 l2]
                   (cons (- (first l2) (first l1))
                         (- (rest l2) (rest l1))))
        dist (fn [dist-vec] (+ (abs (first dist-vec)) (abs (rest dist-vec))))
        min (fn [cmp coll]
              (r-reduce (fn [v ret] (if (> (cmp ret v) 0) v ret)) (first coll) coll))
        cmp-of (fn [f] (fn [a b] (- (f a) (f b))))
        can-go (fn [map loc dir] (> (look-ahead map loc dir) wall))
        escape-dir (fn [map loc dist-vec]
                     (if (> (x dist-vec) (y dist-vec))
                       (if (> 0 (y dist-vec))
                         (if (can-go map loc down)
                           down
                           (if (> 0 (x dist-vec))
                             right
                             left))
                         (if (can-go map loc up)
                           up
                           (if (> 0 (x dist-vec))
                             right
                             left)))
                       (if (> 0 (x dist-vec))
                         (if (can-go map loc right)
                           right
                           (if (> 0 (y dist-vec))
                             down
                             up))
                         (if (can-go map loc left)
                           left
                           (if (> 0 (y dist-vec))
                             down
                             up)))))]
    (cons
      0
      (fn step [state world]
        (let [the-map (first world)
              my-status (second world)
              cur-loc (second my-status)
              cur-dir (second (rest my-status))
              best-pill-dir (look-for the-map cur-loc cur-dir pill-query 0)
              ghost-statuses (first (drop 2 world))
              ghost-locs (map second ghost-statuses)
              ghost-dist-vecs (map (fn [loc] (dist-vec cur-loc loc)) ghost-locs)
              min-ghost-dist-vec (min (cmp-of dist) ghost-dist-vecs)
              new-dir (if (> 5 (dist min-ghost-dist-vec))
                        (escape-dir the-map cur-loc min-ghost-dist-vec)
                        best-pill-dir)]
          (trace min-ghost-dist-vec)
          (trace new-dir)
          (cons 0 new-dir))))))

(defprog map-test []
  (let [map (fn [f coll]
              ((fn [acc f coll]
                 (if (empty? coll)
                   acc
                   (recur (cons (f (first coll)) acc) f (rest coll))))
               nil f coll))]
    (map (fn inc [x] (+ x 1)) (cons 1 nil))))

(defprog let-test []
  (let [x 1 y 2] (+ x y)))

(defprog drop-test []
  (let [and (fn [a b] (= 2 (+ a b)))
        not (fn [a] (if a 0 1))
        drop (fn [n coll]
               (if (and (> n 0) (not (nil? coll)))
                 (recur (- n 1) (rest coll))
                 coll))]
    (drop 2 nil)))

(defprog abs-test []
  (let [abs (fn [x] (if (>= x 0) x (- 0 x)))]
    (abs -1)))

(defprog reduce-test []
  (let [r-reduce (fn [f start coll]
                     (if (empty? coll)
                       start
                       (recur f (f (first coll) start) (rest coll))))
          map (fn [f coll]
                (r-reduce (fn [v ret] (cons v ret)) nil
                          (r-reduce (fn [v ret] (cons (f v) ret)) nil coll)))]
    (map (fn [x] (+ x 1)) (cons 1 (cons 2 nil)))))

(comment

  (->> ai (compile nil) assemble emit)

  (->> tail-call-test (compile nil) assemble emit)
  (->> let-test (compile nil) assemble emit)
  (->> drop-test (compile nil) assemble emit)
  (->> reduce-test (compile nil) assemble emit)
  (map)
  (pst)

  (drop 2 nil)

  )

