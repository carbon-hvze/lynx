(ns lynx.expressions
  (:require
   [lynx.utils :as utils]
   [lynx.interpreter :as i]))

;; IMPORTANT
;; do not forget to reload after changing expression signature

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
 '(to (know-if '* *) (use *))
 (fn [env [_ noun adj] f & args]
   [(-> env
        (update-in [::i/terms noun] merge {:type :noun :symbol noun})
        (update-in [::i/terms adj]
                   #(-> (merge % {:type :adj :symbol adj})
                        (assoc-in [:applies-to noun] {:with f}))))
    adj])
 
 '(to * '* (use *))
 (fn [env verb noun f & args]
   [(-> env
        (update-in [::i/terms noun] merge {:type :noun :symbol noun})
        (update-in [::i/terms verb]
                   #(-> (merge % {:type :verb :symbol verb})
                        (assoc-in [:applies-to noun] {:with f}))))
    verb])

 '(to '(get *) (use *))
 (fn [env [_ noun] f]
   (let [value (f)]
     [(-> env
          (update-in [::i/terms noun] merge {:type :noun :symbol noun :value value}))
      value])))

(i/expr
 '(eval-list &)
 (fn [env & args]
   [env (last args)])

 '(group '* &)
 (fn [env group-name & args]
   [env (last args)]))

(i/expr
 '(do * '* &)
 (fn [env verb noun & params]
   (if-let [op (get-in env [::i/terms verb :applies-to noun :with])]
     (let [args (cons (utils/get-noun env noun) params)]
       [(-> env
            (update-in [::i/terms verb :applied] conj {:time (:lynx.core/time env) :noun noun})
            (update :lynx.core/time + 1))
        (apply op args)])
     (let [error
           {:context [:do verb noun]
            :error :verb-not-defined
            :lucky-guess "Have you renamed something recently?"}]
       (throw (Exception. (pr-str error)))))))

(i/expr
 '(can-be '* &)
 (fn [env noun & args]
   [(update-in env
               [::i/terms noun :props]
               (fn [props]
                 (->> args
                      (map (fn [adj] [adj {}]))
                      (into {})
                      (merge props))))
    noun]))

(i/expr
 '(is '* *)
 (fn [env noun adj]
   (let [pred (get-in env [::i/terms adj :applies-to noun :with])
         noun-value (utils/get-noun env noun)
         props (get-in env [::i/terms noun :props])

         error
         {:context [:is noun adj]
          :error :adjective-not-defined
          :lucky-guess "Have you renamed something recently?"}]

     (cond
       (nil? noun-value) [env nil]

       (and (not (nil? pred)) (not (nil? noun-value)))
       (let [pred-res (pred noun-value)]
         [(assoc-in env [::i/terms noun :props adj :is] (boolean pred-res))
          pred-res])

       (not-empty props)
       (let [other-props
             (->> (dissoc props adj)
                  (map #(:is (second %))))]
         (cond
           (and (get props adj) (every? false? other-props))
           [(update-in env [::i/terms noun :props adj]
                       merge {:is true :deduced true})
            true]

           (and (get props adj) (some true? other-props))
           [(update-in env [::i/terms noun :props adj]
                       merge {:is false :deduced true})
            false]

           :else (throw (Exception. (pr-str error)))))

       :else (throw (Exception. (pr-str error)))))))

(i/expr
 '(when * '&)
 (fn [env cond-value & body]
   (if cond-value
     (i/eval-expr! env (cons 'eval-list body))
     [env nil])))

(defn define-term [env tp term definition]
  (update-in env [::i/terms term]
             merge {:type tp :symbol term :definition definition}))

(i/expr
 '(of *)
 (fn [env sym]
   [env sym]))

(i/expr
 '(noun '* *)
 (fn [env noun def*]
   [(define-term env :noun noun def*)
    noun]))

(i/expr
 '(define '*)
 (fn [env term]
   [env
    (->> (vals (::i/terms env))
         (filter #(= (:symbol %) term))
         first
         :definition)]))

(i/expr
 '(and &)
 (fn [env & conds]
   [env (every? identity conds)]))

(i/expr
 '(not *)
 (fn [env term]
   [env (not term)]))

(i/expr
 '(no '*)
 (fn [env term]
   [env (not (utils/get-noun env term))]))

(i/expr
 '(then '&)
 (fn [env & body]
   (loop [cur-env env
          [fexp & tail] body]
     (let [[env* res] (i/eval-expr! cur-env fexp)]
       (cond
         (seq? fexp)
         (let [{exp-type :type nouns :applies-to} (i/find-term env* (first fexp))
               {arg-type :type} (i/find-term env* (second fexp))]
           (cond
             (empty? tail) [env* res]
             (and (= :verb exp-type) (= :noun arg-type))
             (recur (assoc env* (keyword (second fexp)) res) tail)
             :else (recur env* tail))))))))

