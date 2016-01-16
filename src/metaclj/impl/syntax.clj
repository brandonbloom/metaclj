(ns metaclj.impl.syntax
  (:require [metaclj.impl.env :as env]
            [metaclj.impl.parse :refer [parse]]))

(defrecord Syntax [forms env])

(defn syntax? [x]
  (instance? Syntax x))

(defn head [x]
  (if (syntax? x)
    :syntax
    (:head x)))

(defmulti transform #'head)

(defn transform-in [env x]
  (if (syntax? x)
    (transform x)
    (transform (parse x env))))

(defmethod transform :syntax [{:keys [forms env]}]
  (mapcat #(transform-in env %) forms))

(defmethod transform :constant [{:keys [value]}]
  [value])

(defmethod transform :name [{:keys [env sym]}]
  (let [{:keys [origin value]} (env/-resolve env sym)]
    ;XXX check origin, see eval-head :name in meta.eclj
    (transform-in env value)
    ))

(defmethod transform :do [{:keys [env statements ret]}]
  [(concat ['do]
           (mapcat #(transform-in env %) statements)
           (transform-in env ret))])

(defmethod transform :invoke [{:keys [env f args]}]
  [(concat (transform-in env f)
           (mapcat #(transform-in env %) args))])

(defmethod transform :var [{:keys [sym]}]
  [sym])
