(ns metaclj.core
  (:require [metaclj.impl.env :refer [->Env]]
            [metaclj.impl.syntax :refer [->Syntax transform]]))

(defmacro syntax-call [f & args]
  `(~f ~@(map #(list `syntax %) args)))

(defn syntax-eval [x]
  ;(fipp.edn/pprint (list 'run! 'eval (list 'quote (transform x))))
  (run! eval (transform x)))

(defmacro syntax [& forms]
  (let [locals (into {} (for [sym (keys &env)]
                          [(list 'quote sym) sym]))]
    `(->Syntax '~(vec forms)
               (->Env ~*ns* ~locals))))

(defmacro defmeta [name & fn-tail]
  (let [f (eval (list* 'fn fn-tail))]
    `(defmacro ~(vary-meta name assoc :meta-macro f) [~'& args#]
       `(let [~'~'x (syntax-call ~~f ~@args#)]
          (syntax-eval ~'~'x)))))

(defmacro defbn [name & fn-tail]
  (let [methods (if (vector? (first fn-tail)) (list fn-tail) fn-tail)]
    `(defmeta ~name ~@(for [[args & body] methods]
                        (list args (list* `syntax body))))))

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
    (blahblah 1 2 3 4)
  ))

  (fipp.clojure/pprint (macroexpand-1 '
  (defmeta dotwice [expr]
    (syntax (do expr expr)))
  ))

  (fipp.clojure/pprint (macroexpand-1 '
    (dotwice (prn 1))
  ))

  (defmeta dotwice [expr]
    (syntax (do expr expr)))

  (dotwice (prn 1))

  (fipp.edn/pprint
  (let [x 1] (syntax x))
  )

  (let [x 1
        y [1 2 3]]
    (syntax 0))

  (let [x 1
        y [1 2 3]]
    (syntax x y))

  (defbn my-if [test then else]
    (if test then else))

  (my-if true 1 2)
  (my-if false 1 2)

  (defbn my-and
    ([] true)
    ([x] x)
    ([x next] ;XXX variadic
     (let [y x]
       (if y (my-and (my-and next)) y))))

  (my-and)
  (my-and 1)
  (my-and 1 2)
  (my-and 1 2 3)

)
