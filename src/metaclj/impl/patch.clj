(ns metaclj.impl.patch
  ;; This seems to confuse vim-fireplace. Use :Require to reload file.
  (:refer-clojure :exclude [case]))

(def vars {#'clojure.core/case 'metaclj.impl.patch/case
           ;;XXX find and fix occurrences of "eclj".
           #'clojure.core/ns 'eclj.core/ns
           #'clojure.core/deftype 'eclj.core/deftype
           #'clojure.core/defrecord 'eclj.core/defrecord
           #'clojure.core/defprotocol 'eclj.core/defprotocol})

(declare case*)

(defmacro case
  [e & clauses]
  (let [default? (odd? (count clauses))]
    ;;TODO: Handle documented list behavior.
    `(metaclj.impl.patch/case* ~e
       ~(->> (if default? (butlast clauses) clauses)
             (partition 2) (map vec) (into {}))
       ~(if default?
          (last clauses)
          `(throw (ex-info (str "No clause matching")
                           {:error :no-matching-clause}))))))
