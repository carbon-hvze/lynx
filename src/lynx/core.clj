(ns lynx.core
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defonce lambdas (atom {}))

(defmacro f [kw args body & rest]
  `(do
     (swap! lambdas assoc ~kw
            {:name ~kw
             :args (quote ~args)
             :body (quote ~body)
             :f (fn ~args ~body)})
     (get @lambdas ~kw)))

(ns-unmap *ns* '*eval)

(defmulti *eval
  (fn [op env & args]
    (if (::stop env)
      :terminate
      op)))

(defn eval! [expr env]
  (apply *eval (first expr) env (rest expr)))

(defn resolve! [env fun]
  ;; TODO finish expr application
  (if (symbol? fun)
    (if-let [lambda-cfg (get @lambdas (keyword fun))]
      (fn [& args]
        (let [f-arg (first (:args lambda-cfg))
              args* (cond->> args
                      (= f-arg 'env) (cons env)
                      (and (map? f-arg) (= (:as f-arg) 'env)) (cons env))]
          (apply (get lambda-cfg :f) args*)))
      (fn [& args] (apply (eval fun) args)))
    (fn [& args] (apply (eval fun) args))))

(defmethod *eval 'to
  [_ env [op noun adj] [tail fun] & rest]
  (cond
    (= tail 'use)
    (let [[tp val]
          (cond
            (= op 'know-if) [:adj adj]
            :else [:verb op])]
      [val 
       (update-in env [::terms val]
                  #(-> (merge % {:type tp :symbol val})
                       (assoc-in [:applies-to noun]
                                 {:with (resolve! env fun)})))])

    (= tail 'makes)
    [noun
     (update-in env [::terms fun]
                #(merge % {:type :adjective :symbol fun
                           :cause op}))]))

(defmethod *eval 'eval-list
  [op env & body]
  (reduce (fn [[previous env*] expr]
            (eval! expr (assoc env* ::last previous)))
          [nil env]
          body))

(defmethod *eval 'expl-when
  [_ env conds body & rest]
  [(str "If "
        (first (eval! (list 'expl-expr conds) env))
        " we do "
        (first (eval! (list 'expl-expr body) env)))
   env])

(defmethod *eval 'when
  [_ env conds body & rest]
  (if (first (eval! conds env))
    (eval! body env)
    [nil env]))

(defmethod *eval 'and
  [_ env & ops]
  [(->> ops
        (map #(eval! % env))
        (every? first))
   env])

(defmethod *eval 'expl-is
  [_ env noun adj]
  [(str noun " is " adj)
   env])

(defmethod *eval 'is
  [_ env noun adj]
  (let [[noun-value env*] (eval! (list 'noun noun) env)
        pred (get-in env* [::terms adj :applies-to noun :with])]
    [(pred noun-value) env*]))

(defmethod *eval 'expl-do
  [_ env [verb noun]]
  [(str verb " a " noun)
   env])

(defmethod *eval 'do
  [_ env [verb noun & params]]
  (let [[noun-value env*] (eval! (list 'noun noun) env)
        op (get-in env* [::terms verb :applies-to noun :with])
        args (cons noun-value params)]
    [(apply op args)
     (-> env*
         (update-in [::terms verb :applied] conj {:time (::time env*) :noun noun})
         (update ::time + 1))]))

(defn ->keyword [term]
  (cond 
    (list? term) (keyword (str/join "-" term))
    :else (keyword term)))

(defn term [tp env term & args]
  (if-let [*term (get-in env [::terms term])]
    [(:value *term) env]
    (let [value (get env (->keyword term))
          term
          (cond-> {:type tp :symbol term}
            (first args) (assoc :definition (first args))
            value (assoc :value value))]
      [value (update-in env [::terms term] #(merge % term))])))

(defmethod *eval 'noun
  [& args] (apply term args))

(defmethod *eval 'verb
  [& args] (apply term args))

(defmethod *eval 'adj
  [& args] (apply term args))

(defmethod *eval 'expl-verb
  [_ env verb & args]
  [(str "To " verb " means to " (first args))
   env])

(defmethod *eval 'expl-adj
  [_ env adj & args]
  [(str "If something is " adj " it means it " (first args))
   env])

(defmethod *eval 'expl-noun
  [_ env noun & args]
  [(str "A " noun " is " (first args))
   env])

(defmethod *eval 'define
  [_ env term]
  [(->> (vals (::terms env))
        (filter #(= (:symbol %) term))
        first
        :definition)
   env])

(defmethod *eval 'expl-expr
  [_ env [op & body]]
  (let [op* (symbol (str "expl-" (str op)))]
    (cons op* body)
    (eval! (cons op* body) env)))

(defmethod *eval 'expl-list
  [_ env & args]
  (reduce (fn [[prev env*] expr]
            (let [[cur env**] (eval! (list 'expl-expr expr) env*)]
              [(cond->> cur prev (str prev ". "))
               env**]))
          [nil env]
          args))

(defmethod *eval 'should
  [op env noun adj]
  (let [op-name (get-in env [::terms adj :cause])
        ops-applied
        (->> (::terms env)
             vals
             (filter (fn [{:keys [applied]}]
                       (some (fn [{applied-to :noun}]
                               (= noun applied-to))
                             applied)))
             (map :symbol)
             set)]
    (if (contains? ops-applied op-name)
      [true env]
      [(list op noun adj) (assoc env ::stop true)])))

(defmethod *eval :default
  [op env & body]
  [(cons op body) env])

(defmethod *eval :terminate
  [_ env & args]
  [(::last env) env])

(defn ->symbol [v]
  (cond
    (keyword? v) (symbol (name v))
    :else (symbol v)))

(defn pre-process [env]
  (merge {::time 0} env))

(defn evaluate*
  ([expr]
   (apply *eval (first expr) (pre-process {}) (rest expr)))
  ([expr env & exprs]
   (apply *eval (first expr) (pre-process env) (concat (rest expr) exprs))))

(defn evaluate
  ([expr]
   (first (evaluate* expr)))
  ([expr env & args]
   (first (apply evaluate* expr env args))))

