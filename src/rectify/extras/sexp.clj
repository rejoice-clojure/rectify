(ns rectify.extras.sexp
  "Transform s-expression output.")

(defn transform-all
  "Transforms all exprs with given fn."
  [f exprs]
  (doall (for [x exprs] (f x))))

(defn transform-last
  "Transforms only last expr with given fn."
  [f exprs]
  (concat (butlast exprs) [(f (last exprs))]))

(defn transform-clauses
  "Transform return value of clause-like exprs."
  [f clauses]
  (doall (mapcat
          (fn [[check ret]] [check (f ret)])
          (partition-all 2 clauses))))

(def parsers
  [;; == clojure core
   ['do
    (fn [args f] `(do ~@(transform-last f args)))]

   ['let
    (fn [[bindings & body] f] `(let ~bindings ~@(transform-last f body)))]

   ['for
    (fn [[bindings body] f] `(for ~bindings ~(f body)))]

   ['if
    (fn [[condition a b] f] `(if ~condition ~(f a) ~(f b)))]

   ['when
    (fn [[bindings & body] f] `(when ~bindings ~@(transform-last body f)))]

   ['when-some
    (fn [[bindings & body] f] `(when-some ~bindings ~@(transform-last f body)))]

  ['when-let
   (fn [[bindings & body] f] `(when-let ~bindings ~@(transform-last f body)))]

   ['when-first
    (fn [[bindings & body] f] `(when-first ~bindings ~@(transform-last f body)))]

   ['when-not
    (fn [[bindings & body] f] `(when-not ~bindings ~@(transform-last f body)))]

   ['if-not
    (fn [[bindings & body] f] `(if-not ~bindings ~@(transform-all f body)))]

   ['if-some
    (fn [[bindings & body] f] `(if-some ~bindings ~@(transform-all f body)))]

   ['if-let
    (fn [[bindings & body] f] `(if-let ~bindings ~@(transform-all f body)))]

   ['case
    (fn [[expr & clauses] f] `(case ~expr ~@(transform-clauses f clauses)))]

   ['condp
    (fn [[pred expr & clauses] f] `(condp ~pred ~expr ~@(transform-clauses f clauses)))]

   ['cond
    (fn [[& clauses] f] `(cond ~@(transform-clauses f clauses)))]

   ;;  == dev
   ['println
    (fn  [args f] `(println ~@(transform-last f args)))]])
