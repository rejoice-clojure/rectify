(ns rectify.core.hiccup
  (:require [rectify.core.spec :as spec])
  (:refer-clojure :exclude [compile]))

(defn- maybe-hiccup?
  [x]
  (vector? x))

;;  == Argument Parser

(defn seq-parse
  "Parse normalized args using `parsers`. 
   Matched parsers are ran sequentially, each passing its output to the next one.

   Parsers are {clause parser} pairs. 
   A clause is a predicate fn or keyword to that declares the match.
   A parser is a fn (fn [...args]) or map applied according to `apply-parser-map`.
   
   Options:
   - check-clause (fn [clause x] bool)
   - apply-parser (fn [x parsers])
   - terminate-early? (fn [] bool)"
  [nargs {:keys [parsers
                 check-clause
                 apply-parser
                 terminate-early?]
          :or {parsers []
               terminate-early? (constantly false)}}]
  (reduce (fn [acc [clause parser]]
            (if (check-clause clause acc)
              (let [parsed-args (apply-parser acc [clause parser])]
                (if (terminate-early? parsed-args)
                  (reduced parsed-args)
                  parsed-args))
              acc))
          nargs
          parsers))

(defn- check-props-clause [clause-key props]
  (contains? props clause-key))

(defn- check-args-clause [clause args]
  (let [tag (first args)]
    (if (fn? clause) (clause tag) (= clause tag))))

(defn apply-props-parser [props [clause-key parse-fn]]
  (update props clause-key parse-fn))

(defn- apply-args-parser
  "Apply parser to normalized args.
   Parser is a map that accepts :tag and :props keys.
   A parser value can either be a transform fn (fn [x] x) or hardcoded value.
   For props key, a map of keyvals to parse is also allowed."
  [args [_clause {:as parser
                  :or {tag identity props identity}}]]
  (letfn [(fn-or-x [fx x] (if (fn? fx) (fx x) (or fx x)))
          (run-props-parsers
            [prop-parsers props]
            (seq-parse props {:parsers (into [] prop-parsers)
                              :check-clause check-props-clause
                              :apply-parser apply-props-parser}))
          (apply-parser-map
            [[tag props slots] parser]
            (let [par-tag (:tag parser) par-props (:props parser) par-slots (:slots parser)]
              [(fn-or-x par-tag tag)
               (when props ((if (map? par-props) run-props-parsers fn-or-x) par-props props))
               (fn-or-x par-slots slots)]))]
    (if (fn? parser)
      (apply parser args)
      (apply-parser-map args parser))))


(defn run-hiccup-parser [args parsers]
  (seq-parse args {:parsers parsers
                   :check-clause check-args-clause
                   :apply-parser  apply-args-parser
                   :terminate-early?  #(not (maybe-hiccup? %))}))

(comment
  ;; tag parsing
  (run-hiccup-parser
   [:a]
   [[keyword? {:tag #(-> % name str)}]])

  ;; props parsing
  (run-hiccup-parser
   [:a {:b "c"}]
   [[keyword? {:props {:b keyword}}]]))

(defn run-expr-parser [expr parsers compile-fn]
  (seq-parse
   expr
   {:parsers parsers

    :check-clause (fn [tag expr] (= tag (first expr)))

    :apply-parser (fn [[_ & args] [_ f]]
                    (f args compile-fn))

    :terminate-early? (constantly false)}))

(comment
  (run-expr-parser
   '(for [x items] x)
   [['for (fn [args f] `(for ~@(butlast args) ~(f (last args))))]]
   (fn [x] `(emit ~x))))


;; == Hiccup Compiler

(declare compile)

(defn- normalize [{:keys [tag props slots]}]
  [tag]
  [tag props (or slots [])])

(defn compile
  "Compile hiccup body. 
   Body can be hiccup data or list expr to parse; anything else is returned directly.
   Hiccup must be parsed into a callable expr either by `parsers` or optional `emitter`.
   If `strict` is true then only expr in parser list are accepted."
  [body {:keys [strict emitter args-parsers sexpr-parsers]
         :or {strict true
              args-parsers []
              sexpr-parsers []}
         :as opts}]
  (cond
     ;; => Hiccup
    (vector? body)
    (let [parsers (spec/conform args-parsers :rec/parsers)
          nargs (normalize (spec/conform body :rec/hiccup))
          parsed (run-hiccup-parser nargs parsers)]
      (cond
        (vector? parsed)
        (if emitter
          (let [[tag props slots] parsed] (emitter tag props (mapv #(compile % opts) slots)))
          (throw (ex-info "Hiccup must be parsed to callable expr if emitter is not passed." {:expr parsed})))
        (sequential? parsed) parsed
        :else (throw (ex-info "Invalid hiccup parsed." {:args parsed}))))

     ;; => S-Expression
    (list? body)
    (let [expr-parsers (spec/conform sexpr-parsers :rec/parsers)
          allowed-tags (reduce #(conj %1 (first %2)) #{} expr-parsers)]
      (cond
        (contains? allowed-tags (first body))
        (run-expr-parser body expr-parsers #(compile % opts))
        (not strict) body 
        :else (throw (ex-info "Expr is not registered in parser list." {:expr body}))))

    :else body))

(comment
  ;; Testing compile options below 
  ;; for testing how hiccup or  parser arguments are conformed, see spec.

  (defn emitter [tag props slots] `(emit ~tag ~props ~@slots))
  (def opts {:emitter emitter
             :args-parsers [[any? {:tag #(-> % name symbol)}]]})

  ;; tag + child
  (compile [:a "b"] opts)

  ;; tag + props + child
  (compile [:a {:b "c"} "d"] opts)

  ;; tag + props + children
  (compile [:a {:b "c"} "d" "e"] opts)

 ;; nested child is compiled
  (compile [:a {:b "c"} [:d]] opts)

  ;; if parser returns expr it is returned directly
  (let [opts {:args-parsers [[:a (fn [_ _ _] `(A))]
                        [:a (fn [_ _ _] :not-here)]]}]
    (compile [:a] opts))

  ;; handles expr
  (let [opts {:expr-parsers [['a (fn [args _] `(A ~@args))]]}]
    (compile '(a :b) opts)))
