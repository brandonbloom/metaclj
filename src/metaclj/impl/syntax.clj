(ns metaclj.impl.syntax
  (:require [metaclj.impl.env :as env]
            [metaclj.impl.parse :refer [parse expr?]]))

(defmulti transform :head)

(defn transform-in [env x]
  (transform (parse x env)))

(defmethod transform :constant [{:keys [value]}]
  value)

(defmethod transform :name [{:keys [env sym]}]
  (let [{:keys [origin value]} (env/-resolve env sym)]
    ;XXX check origin, see eval-head :name in meta.eclj
    (if (expr? value)
      (transform value)
      value)))

(defmethod transform :do [{:keys [env statements ret]}]
  (concat ['do]
          (map #(transform-in env %) statements)
          [(transform-in env ret)]))

(defmethod transform :invoke [{:keys [env f args]}]
  (list* (transform-in env f)
         (map #(transform-in env %) args)))
