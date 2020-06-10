(ns lynx.interpreter)

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
  (let [log {:start-time (:lynx.core/time env)
             :end-time (:lynx.core/time env)
             :expression expr
             :result expr}]
    (assoc env :lynx.core/log log)))

(defn eval-expr! [env expr]
  (if-let [{:keys [pattern fun]} (implementation env expr)]
    (let [eval-subexp
          (fn [acc [idx sub-exp]]
            (let [ex (get (vec pattern) idx)
                  quoted-arg? (and (seq? ex) (= (first ex) 'quote))
                  quoted-and? (contains? (set (take (+ idx 1) pattern)) '(quote &))]
              (if (or quoted-arg? quoted-and?)
                (update acc :args conj sub-exp)
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

          [env** res] (apply fun env* args)

          expr-log 
          {:start-time (:lynx.core/time env**)
           :end-time (:lynx.core/time env)
           :expression expr
           :result res
           :subexp subexp-log}]
      [(assoc env** :lynx.core/log expr-log) res])
    (if (seq? expr)
      (if-let [{term-type :type}
               (->> (::terms env)
                    (filter (fn [[k v]] (= k (first expr))))
                    first
                    second)]
        (cond
          (= term-type :noun) (eval-expr! env (concat '(is) expr))
          (= term-type :verb) (eval-expr! env (concat '(do) expr))
          :else [(leaf-log env expr) expr])
        [(leaf-log env expr) expr])
      [(leaf-log env expr)  expr])))



