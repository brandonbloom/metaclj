(ns metaclj.core
  (:require [metaclj.impl.env :refer [->Env]]
            [metaclj.impl.parse :refer [parse expr?]]
            [metaclj.impl.syntax :refer [transform]]))

(defmacro syntax-call [f & args]
  `(~f ~@(map #(list `syntax %) args)))

(defn syntax-eval [x]
  (eval (transform x)))

(defmacro syntax [expr]
  (let [free (keys &env) ;TODO compute minimal set from
        locals (into {} (for [sym free] [(list 'quote sym) sym]))]
    `(parse '~expr (->Env ~*ns* ~locals))))

(defmacro defmeta [name & fn-tail]
  (let [f (eval (list* 'fn name fn-tail))]
    `(defmacro ~name [~'& args#]
       `(let [~'~'x (syntax-call ~~f ~@args#)]
          (syntax-eval ~'~'x)))))

(comment

  (require 'fipp.edn)
  (require 'fipp.clojure)

  (syntax-call prn 1 2)

  (fipp.clojure/pprint (macroexpand-1 '
  (defmeta blahblah
    ([foo] [foo])
    ([foo & bar] [foo bar]))
  ))

  (fipp.clojure/pprint (macroexpand-1 '
  (defmeta dotwice [expr]
    (syntax (do expr expr)))
  ))

  (fipp.clojure/pprint (macroexpand-1 '
    (dotwice (prn 1))
  ))

  (dotwice (prn 1))

  (let [x 1
        y [1 2 3]]
    (syntax 1))

  (let [x 1
        y [1 2 3]]
    (syntax x))

  (parse 1 :an-env)

)
