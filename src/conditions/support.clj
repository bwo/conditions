(ns conditions.support
  (:require [clojure.walk :as walk]
            [conditions.free :as free]
            [slingshot.support :as s]))

(defn uninteresting-clause? [env f v]
  (or (not (seq? f))
      (let [ff (first f)]
        (or
         (not (symbol? ff))
         (contains? env ff)
         (not= v (resolve ff))))))

(defn- exn-pred [env arg-var matcher exn-symbol]
  (cond
   (and (symbol? matcher) (class? (resolve matcher))) `(instance? ~matcher ~exn-symbol)
   (vector? matcher) (if (even? (count matcher))
                       `(and (map? ~exn-symbol)
                             ~@(map (fn [[k v]] `(= ~v (get ~exn-symbol ~k)))
                                    (partition 2 matcher)))
                       (throw (Exception.
                               (format "Bad format: %s is not in format [key value & kvs]"
                                       matcher))))
   (free/contains-reference? env arg-var matcher)
   (let [g (gensym)]
     `(let [~g ~exn-symbol]
        ~(free/replace-all-reference env arg-var g matcher)))
   ;; optimistically assume that this is a fn, even though it might not be
   (or (seq? matcher)
       (symbol? matcher))  `(~matcher ~exn-symbol)
   :else (throw (Exception. (format "Do not know how to create a selector from %s" matcher)))))

(defn exn-match-clause [env arg-var catch-form exn-symbol]
  (let [[matcher binding & body] (rest catch-form)]
    [(exn-pred env arg-var matcher exn-symbol) `(let [~binding ~exn-symbol] ~@body)]))

(defn maybe-unwrap [exn-symbol]
  `(if-let [unwrapped# (s/unwrap ~exn-symbol)]
     (:object unwrapped#)
     ~exn-symbol))

(defn exn-match-form [env arg-var exn catch-forms else-form]
  (let [exn-symbol (gensym "exn-")]
    `(let [~exn-symbol ~exn
           ~exn-symbol ~(maybe-unwrap exn-symbol)]
       (cond ~@(mapcat #(exn-match-clause env arg-var % exn-symbol) catch-forms)
             :else ~else-form))))
