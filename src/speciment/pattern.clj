(ns speciment.pattern
  (:require [clojure.string :as string]
            [clojure.test :as t]
            [clojure.walk :as walk]))

(defrecord Match [matches expected actual])
(defrecord Missing [matches expected])
(defrecord Mismatch [matches expected actual])

(defn binding? [s]
  (and (symbol? s) (string/starts-with? (name s) "?")))

(declare matcher)

(defn map-matcher [p]
  (let [p' (update-vals p matcher)]
    (fn match-map
      ([] p)
      ([data] (match-map {} data))
      ([matches data]
       (if (map? data)
         (reduce-kv
          (fn [{:keys [matches actual expected] :as match} k match-fn]
            (if (contains? data k)
              (let [data (get data k)
                    {:keys [matches] :as res} (match-fn matches data)]
                (if (instance? Match res)
                  (-> match
                      (assoc-in [:actual k] data)
                      (assoc :matches matches))
                  (->Mismatch matches (assoc expected k (:expected res)) (assoc actual k res))))
              (->Mismatch matches
                          (assoc expected k (:expected (match-fn matches ::na)))
                          (assoc actual k (->Missing matches (:expected (match-fn matches ::na)))))))
          (->Match matches {} {})
          p')
         (->Mismatch matches p data))))))

(defn binding-matcher [p]
  (let [bind (symbol (namespace p) (subs (name p) 1))]
    (fn match-binding
      ([] p)
      ([data] (match-binding {} data))
      ([matches data]
       (cond
         (or (= bind '_)
             (and (contains? matches bind)
                  (= data (get matches bind))))
         (->Match matches data data)

         (not (contains? matches bind))
         (->Match (assoc matches bind data) data data)

         :else
         (->Mismatch matches (get matches bind) data))))))

(defn zip [xs ys filler]
  (let [xs (concat xs (repeat filler))
        ys (concat ys (repeat filler))]
    (letfn [(step [xs ys]
              (lazy-seq
               (let [x (first xs)
                     y (first ys)]
                 (when-not (= filler x y)
                   (cons [x y] (step (rest xs) (rest ys)))))))]
      (step xs ys))))

(defmulti directive first)

(defmethod directive :default [p]
  (let [p' (mapv matcher p)]
    (fn match-sequence
      ([] p)
      ([data] (match-sequence {} data))
      ([matches data]
       (if (sequential? data)
         (loop [{:keys [matches expected actual] :as res} (->Match matches [] [])
                pairs (zip p' data ::na)]
           (if-let [[[match-fn data] & pairs] pairs]
             (cond
               (= data ::na)
               (let [res' (match-fn matches ::na)]
                 (recur (->Mismatch matches
                                    (conj expected (:expected res'))
                                    (conj actual (->Missing matches (:expected res'))))
                        pairs))

               (= match-fn ::na)
               res

               :else
               (let [{expected' :expected actual' :actual matches' :matches :as res'} (match-fn matches data)]
                 (if (instance? Match res')
                   (recur (-> res
                              (update :expected conj expected')
                              (update :actual conj actual')
                              (assoc :matches matches'))
                          pairs)
                   (recur (->Mismatch matches' (conj expected expected') (conj actual res')) pairs))))
             res))
         (->Mismatch matches (type p) (type data)))))))

(defn matcher [p]
  (cond
    (map? p)
    (map-matcher p)

    (sequential? p)
    (directive p)

    (binding? p)
    (binding-matcher p)

    :else
    (fn scalar-matcher
      ([] p)
      ([data] (scalar-matcher {} data))
      ([matches data]
       (if (= data p)
         (->Match matches data data)
         (->Mismatch matches p data))))))

(defn match? [p data]
  (instance? Match ((matcher p) data)))

(defn- expected [match]
  (walk/prewalk
   (fn [node]
     (if (or (instance? Match node)
             (instance? Mismatch node)
             (instance? Missing node))
       (:expected node)
       node))
   match))

(defn- actual [match]
  (walk/prewalk
   (fn [node]
     (cond
       (instance? Mismatch node)
       (let [{:keys [expected actual]} node]
         (if (and (coll? expected)
                  (coll? actual)
                  (= (empty expected)
                     (empty actual)))
           (cond
             (map? expected)
             (reduce-kv
              (fn [acc k v]
                (if (or (instance? Missing v)
                        (not (contains? expected k)))
                  acc
                  (assoc acc k v)))
              {}
              actual)

             (vector? expected)
             (reduce
              (fn [acc el]
                (if (instance? Missing el)
                  acc
                  (conj acc el)))
              []
              actual))
           actual))

       (instance? Match node)
       (:actual node)

       :else
       node))
   match))

(defn- match?-body [msg form]
  `(let [args# (list ~@(rest form))
         [pattern# data#] args#
         matcher# (matcher pattern#)]
     (if (= 2 (count args#))
       (t/do-report (let [match# (matcher# data#)]
                      (if (instance? Match match#)
                        {:type :pass
                         :message ~msg}
                        (let [expected# (expected match#)
                              actual# (actual match#)]
                          {:type :fail
                           :message ~msg
                           :expected expected#
                           :actual actual#}))))
       (t/do-report {:type :fail
                     :message ~msg
                     :expected (symbol "match? expects 2 arguments")
                     :actual (symbol (str (count args#) " were provided: " '~form))}))))

(defmethod t/assert-expr 'match? [msg form]
  (match?-body msg form))

(defmethod t/assert-expr (symbol (str *ns*) "match?") [msg form]
  (match?-body msg form))
