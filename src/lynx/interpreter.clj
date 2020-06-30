(ns lynx.interpreter
  (:require
   [lynx.utils :as utils]))

(defonce env (atom {}))

(defn expr [& exprs]
  (let [expr-id (gensym)]
    (doseq [[pattern fun] (partition 2 exprs)]
      (swap! env assoc-in [::exprs pattern]
             {:pattern pattern
              :fun fun
              :id expr-id}))))

(defn combine [weight result]
  (when weight 
    (when-let [n (first result)]
      (+ weight n))))

;; TODO make all symbols except head ones match by type
;; easier for grammar-reading
(defn match-expr [pattern lst]
  (let [weight
        (cond
          (and (nil? lst) (not-empty pattern)
               (or (= (first pattern) '&)
                   (= (first pattern) '(quote &))))
          0

          (and (nil? pattern) (nil? lst)) 0
          (and (symbol? pattern) (symbol? lst) (= pattern lst)) 0
          (and (seq? pattern) (seq? lst)
               (not-empty pattern) (not-empty lst))
          (let [[phead & ptail] pattern
                [lhead & ltail] lst]
            (cond
              (or (= '& phead) (= '(quote &) phead)) 10

              (and (or (= '* phead) (= '(quote *) phead)))
              (combine 1 (match-expr ptail ltail))

              (= phead 'quote) (combine 0 (match-expr (first ptail) lst))

              (and (seq? phead) (seq? lhead))
              (combine
               (when-let [weight* (first (match-expr phead lhead))]
                 (/ weight* 10))
               (match-expr ptail ltail))

              (= phead lhead)
              (combine 0 (match-expr ptail ltail)))))]
    [weight pattern lst]))

(defn implementation [env expr]
  (when-let [[_ pattern-name _]
             (->> (::exprs env)
                  (map (fn [[k v]]
                         (match-expr k expr)))
                  (filter first)
                  (sort-by first)
                  first)]
    (get-in env [::exprs pattern-name])))

(defn validate! [expr arg]
  (if (and (vector? arg)
           (= (count arg) 2))
    arg
    (let [err {:context [expr :evaluation]
               :type :expr-iface-mismatch}]
      (throw (Exception. (pr-str err))))))

(defn leaf-log [env expr]
  (let [log {;;:start-time (:lynx.core/time env)
             ;;:end-time (:lynx.core/time env)
             :expression expr
             :result expr}]
    (assoc env :lynx.core/log log)))

(defn find-term [env term]
  (->> (::terms env)
       (filter (fn [[k v]] (= k term)))
       first
       second))

(defn resolve-noun [env noun]
  (or (get env (utils/->keyword noun))
      (get-in env [::terms noun :value])))

(defn ->log [result]
  (cond
    (fn? result) (pr-str result)
    :else result))

(defn eval-expr! [env expr]
  (let [{:keys [pattern fun] :as impl} (implementation env expr)]
    (cond
      (not-empty impl) 
      (let [eval-subexp
            (fn [acc [idx sub-exp]]
              (let [ex (get (vec pattern) idx)
                    quoted-arg? (and (seq? ex) (= (first ex) 'quote))
                    quoted-and? (contains? (set (take (+ idx 1) pattern)) '(quote &))]
                (if (or quoted-arg? quoted-and?)
                  (-> acc
                      (update :args conj sub-exp)
                      (update :lynx.core/log conj {:expression sub-exp :quoted? true}))
                  (let [new-env (assoc (:env acc) ::last (last (:args acc)))
                        [{log :lynx.core/log :as env*} res] (validate! sub-exp (eval-expr! new-env sub-exp))]
                    (-> acc
                        (assoc :env env*)
                        (update :lynx.core/log conj log)
                        (update :args conj res))))))

            {env* :env args :args subexp-log :lynx.core/log}
            (->> (rest expr)
                 (map-indexed (fn [idx v] [(+ idx 1) v]))
                 (reduce eval-subexp {:env env :args [] :lynx.core/log []}))

            [{impl-log :lynx.core/log :as env**} res]
            (apply fun (dissoc env* :lynx.core/log) args)

            expr-log 
            (cond-> {;;:start-time (:lynx.core/time env**)
                     ;;:end-time (:lynx.core/time env)
                     :expression expr
                     :result (->log res)
                     :subexp subexp-log}
              (not-empty impl-log) (update :subexp conj impl-log))]
        [(assoc env** :lynx.core/log expr-log) res])

      (seq? expr)
      (let [term (resolve-noun env expr)
            {fterm-type :type} (find-term env (first expr))
            {sterm-type :type} (find-term env (second expr))]
        (cond
          (not (nil? term)) [env term]
          (= fterm-type :noun) (eval-expr! env (concat '(is) expr))
          (= fterm-type :verb) (eval-expr! env (concat '(do) expr))
          (and (= 2 (count expr)) (= sterm-type :noun)) (eval-expr! env (concat '(do) expr))
          :else [(leaf-log env expr) expr]))

      (not (nil? (resolve-noun env expr)))
      [env (resolve-noun env expr)]

      :else
      [(leaf-log env expr) expr])))


