(ns cloap.core.sexp
  "Transform s-expression output.
   NOTE: This code was forked from: weavejester/hiccup -> r0man/sablono -> rauhs/hicada.")

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

(defmulti transform-children
  "Transform s-expr `forms` output with `transform-fn`.
   Dispatches based on the expr operator's name."
  (fn [forms _transform-fn]
    (name (first forms))))

(defmethod transform-children :default [form _]
  ;; todo if expr is not in this list maybe we throw an error 
  form)

;;  == logging

(defmethod transform-children "println"
  [[_ & forms] f]
  `(println ~@(transform-last f forms)))

;; == clojure variants

(defmethod transform-children "do"
  [[_ & forms] f]
  `(do ~@(transform-last f forms)))

(defmethod transform-children "let"
  [[_ bindings & body] f]
  `(let ~bindings ~@(transform-last f body)))

(defmethod transform-children "let*"
  [[_ bindings & body] f]
  `(let* ~bindings ~@(transform-last f body)))

(defmethod transform-children "letfn*"
  [[_ bindings & body] f]
  `(letfn* ~bindings ~@(transform-last f body)))

(defmethod transform-children "for"
  [[_ bindings body] f]
  `(for ~bindings ~(f body)))

(defmethod transform-children "if"
  [[_ condition a b] f]
  `(if ~condition ~(f a) ~(f b)))

(defmethod transform-children "when"
  [[_ bindings & body] f]
  `(when ~bindings ~@(transform-last body f)))

(defmethod transform-children "when-some"
  [[_ bindings & body] f]
  `(when-some ~bindings ~@(transform-last f body)))

(defmethod transform-children "when-let"
  [[_ bindings & body] f]
  `(when-let ~bindings ~@(transform-last f body)))

(defmethod transform-children "when-first"
  [[_ bindings & body] f]
  `(when-first ~bindings ~@(transform-last f body)))

(defmethod transform-children "when-not"
  [[_ bindings & body] f]
  `(when-not ~bindings ~@(transform-last f body)))

(defmethod transform-children "if-not"
  [[_ bindings & body] f]
  `(if-not ~bindings ~@(transform-all f body)))

(defmethod transform-children "if-some"
  [[_ bindings & body] f]
  `(if-some ~bindings ~@(transform-all f body)))

(defmethod transform-children "if-let"
  [[_ bindings & body] f]
  `(if-let ~bindings ~@(transform-all f body)))

(defmethod transform-children "case"
  [[_ expr & clauses] f]
  `(case ~expr ~@(transform-clauses f clauses)))

(defmethod transform-children "condp"
  [[_ pred expr & clauses] f]
  `(condp ~pred ~expr ~@(transform-clauses f clauses)))

(defmethod transform-children "cond"
  [[_ & clauses] f]
  `(cond ~@(transform-clauses f clauses)))
