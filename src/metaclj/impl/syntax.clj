(ns metaclj.impl.syntax
  (:require [metaclj.impl.env :as env]
            [metaclj.impl.parse :refer [parse]]))

(defmulti transform :head)

(defmethod transform :constant [{:keys [value]}] value)

(defmethod transform :name [{:keys [env sym]}]
  (env/-resolve env sym))

(defn syntax* [form env]
  (transform (parse form env)))
