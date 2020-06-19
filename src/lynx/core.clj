(ns lynx.core
  (:require
   [lynx.expressions]
   [lynx.interpreter :as i]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defmacro f [kw args body & rest]
  `(do
     (swap! i/env assoc-in [::i/lambdas ~kw]
            {:name ~kw
             :args (quote ~args)
             :body (quote ~body)
             :f (fn ~args ~body)})
     (get-in @i/env [::i/lambdas ~kw])))

(defn prepare-expr [expr]
  (cond
    (list? expr) expr
    (keyword? expr)
    (if-let [namespc
             (-> (str/replace (name expr) "." "/")
                 (str/replace  "-" "_")
                 (#(str %1 ".lnx"))
                 io/resource)]
      (read-string (slurp namespc))
      (throw (Exception. (str "namespace " expr " not found"))))))

(defn evaluate*
  ([expr] (i/eval-expr! (merge @i/env {::time 0}) (prepare-expr expr)))
  ([expr env & rest]
   (i/eval-expr! (merge @i/env {::time 0} env) (concat (prepare-expr expr) (map prepare-expr rest)))))

(defn evaluate [& args] (second (apply evaluate* args)))

(defn reset-env! [& args]
  (reset! i/env {}))

(comment
  (reset-env!)

  )
