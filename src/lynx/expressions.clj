(ns lynx.expressions
  (:require
   [lynx.utils :as utils]
   [lynx.interpreter :as i]))

(defn apply-lambda [env cfg]
  (fn [& args]
    (let [f-arg (first (:args cfg))
          args* (cond->> args
                  (= f-arg 'env) (cons env)
                  (and (map? f-arg) (= (:as f-arg) 'env)) (cons env))]
      ;; TODO think about data error vs exception
      (when (not= (count args*) (count (:args cfg)))
          (let [err {:context [(:name cfg) :invokation]
                     :error :arguments-mismatch
                     :expected (count (:args cfg))
                     :received (count args*)
                     :lucky-guess "maybe you haven't aliased env as first argument?"}]
            (throw (Exception. (pr-str err)))))
      (apply (get cfg :f) args*))))

(i/expr
 '(use *)
 (fn [env fun]
   (let [apply-list (fn [& args] (apply (eval fun) args))
         lambda
         (if (symbol? fun)
           (if-let [cfg (get-in env [::i/lambdas (keyword fun)])]
             (apply-lambda env cfg)
             apply-list)
           apply-list)]
     [env lambda])))

(i/expr
 '(to (know-if * *) (use *))
 (fn [env [_ noun adj] f & args]
   [(-> env
        (update-in [::i/terms noun] merge {:type :noun :symbol noun})
        (update-in [::i/terms adj]
                   #(-> (merge % {:type :adj :symbol adj})
                        (assoc-in [:applies-to noun] {:with f}))))
    adj])
 
 '(to * * (use *))
 (fn [env verb noun f & args]
   [(-> env
        (update-in [::i/terms noun] merge {:type :noun :symbol noun})
        (update-in [::i/terms verb]
                   #(-> (merge % {:type :verb :symbol verb})
                        (assoc-in [:applies-to noun] {:with f}))))
    verb]))

(i/expr
 '(eval-list &)
 (fn [env & args]
   [env (last args)]))

(i/expr
 '(do &)
 (fn [env verb noun & params]
   (let [op (get-in env [::i/terms verb :applies-to noun :with])
         args (cons (get env (utils/->keyword noun)) params)]
     [(-> env
          (update-in [::i/terms verb :applied] conj {:time (:lynx.core/time env) :noun noun})
          (update :lynx.core/time + 1))
      (apply op args)])))

(i/expr
 '(is * *)
 (fn [env noun adj]
   (let [pred (get-in env [::i/terms adj :applies-to noun :with])]
     (when (nil? pred)
       (let [error
             {:context [:is noun adj]
              :error :adjective-not-defined
              :lucky-guess "Have you renamed something recently?"}]
         (throw (Exception. (pr-str error)))))
     (let [noun-value
           (if-let [generic (get-in env [::i/terms noun :property-of])]
             (get env (utils/->keyword generic))
             (get env (utils/->keyword noun)))]
       [env (pred noun-value)]))))

(i/expr
 '(when * '&)
 (fn [env cond-value & body]
   (if cond-value
     (i/eval-expr! env (cons 'eval-list body))
     [env nil])))

(defn define-term [env tp term definition]
  (let [term
        {:type tp :symbol term :definition definition}]
    (update-in env [::i/terms term] merge term)))

(i/expr
 '(of *)
 (fn [env sym]
   [env sym]))

(i/expr
 '(noun * *)
 (fn [env noun def*]
   [(define-term env :noun noun def*)
    noun])

 '(noun * (of *))
 (fn [env current generic]
   [(update-in env [::i/terms current] merge {:property-of generic})
    current])

 '(noun * (of *) &)
 (fn [env current generic & args]
   [(cond-> (update-in env [::i/terms current] merge {:property-of generic})
      (not-empty args) (define-term :noun current (first args)))
    current]))

(i/expr
 '(define *)
 (fn [env term]
   [env
    (->> (vals (::i/terms env))
         (filter #(= (:symbol %) term))
         first
         :definition)]))

(i/expr
 '(and &)
 (fn [env & conds]
   [env
    (every? identity conds)]))

