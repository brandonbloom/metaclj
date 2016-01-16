(ns metaclj.core
  (:require [metaclj.impl.env :refer [->Env]]
            [metaclj.impl.syntax :refer [->Syntax transform]]))

(defmacro syntax-call [f & args]
  `(~f ~@(map #(list `syntax %) args)))

(defn syntax-eval [x]
  (run! eval (transform x)))

(defmacro syntax [& forms]
  (let [locals (into {} (for [sym (keys &env)]
                          [(list 'quote sym) sym]))]
    `(->Syntax '~(vec forms)
               (->Env ~*ns* ~locals))))

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

  (let [x 1
        y [1 2 3]]
    (syntax 0))

  (let [x 1
        y [1 2 3]]
    (syntax x y))

  ;(defmeta my-and
  ;  ([] true)
  ;  ([x] x)
  ;  ([x & next]
  ;   (syntax (let [y x]
  ;             (if y (my-and (my-and next)) y)))))

  ;(my-and 1 2 3)

)
