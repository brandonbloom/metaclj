(ns metaclj.core
  (:require [backtick :refer [template]]))

; A hacky port of http://okmij.org/ftp/meta-programming/#meta-scheme
; Not particularly useful because defmacro doesn't compose like syntax-rules.
; And the transliterated pattern matching isn't particularly pretty either.

(declare apply-k)

(defn bracket-fn
  ([expr] (bracket-fn expr #{} [[:up]] 1))
  ([expr env stack level]
   (cond

     (and (seq? expr) (= (first expr) 'quote))
     (apply-k expr stack level)

     ;XXX does not handle varargs, implicit do, etc
     (and (seq? expr) (= (first expr) 'fn*))
     (let [[_ [:as args] body] expr]
       (bracket-fn body (into env args) (conj stack [:fn args]) level))

     (and (seq? expr) (= (first expr) 'bracket))
     (bracket-fn (next expr) env (conj stack [:up]) (inc level))

;    XXX Cross-Stage Persistence is not implemented
;    ((bracket (% e) env stack level)
;      (bracket (unquote (lift e)) env stack level))

     (and (seq? expr) (= (first expr) 'unquote) (pos? level))
     (bracket-fn (second expr) env (conj stack [:down]) (dec level))

     ;XXX does not handle multi-lets, implicit do, etc
     (and (seq? expr) (= (first expr) 'let*))
     (let [[_ [x e] body] expr]
       (bracket-fn (list (list 'fn* [x] body) e) env stack level))

     (seq? expr)
     (bracket-fn (first expr)
                 env
                 (conj stack [:app [] (vec (next expr)) env])
                 level)

     (zero? level)
     (apply-k expr stack 0)

     (symbol? expr)
     (apply-k (if (contains? env expr)
                (list `unquote expr)
                expr)
              stack
              level)

     :else
     (apply-k expr stack level)

     )))

(defn apply-k [expr stack level]
  (cond

    (empty? stack)
    expr

    (and (= (peek stack) [:up]) (pos? level))
    (apply-k `(template ~expr) (pop stack) (dec level))

    (= (peek stack) [:down])
    (apply-k (list `unquote expr) (pop stack) (inc level))

     (= (first (peek stack)) :fn)
     (let [args (second (peek stack))]
       (apply-k (if (zero? level)
                  (list 'fn* args expr)
                  (list `unquote
                        (list 'let* (vec (interleave args (map #(list 'quote %) (repeatedly gensym))))
                              (list `list ''fn* `(template [~@(map #(list `unquote %) args)]) `(template ~expr)))))
                (pop stack)
                level))

     (= (first (peek stack)) :app)
     (let [[_ done todo env] (peek stack)]
       (if (seq todo)
         (let [stack* (conj (pop stack) [:app (conj done expr) (next todo) env])]
           (bracket-fn (first todo) env stack* level))
         (apply-k (seq (conj done expr)) (pop stack) level)))

  ))

(defmacro bracket [expr]
  (bracket-fn expr))


(comment

  (bracket 5)
  (bracket '5)
  (bracket x)
  (bracket (fn* [x] x))
  (bracket (let* [x 1] x))
  (bracket ((fn* [x y] (+ x y)) 5 10))

  (require '[clojure.walk :refer [macroexpand-all]])

  (macroexpand-all '(fn* [f] (bracket (fn* [x] ~(f (bracket x))))))
  ((fn* [f] (bracket (fn* [x] ~(f (bracket x))))) identity)

)
