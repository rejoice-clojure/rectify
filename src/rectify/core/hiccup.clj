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
   A parser is a fn (fn [...args]) or map applied according to `apply-parser-map`."
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
  (letfn [(fn-or-x [f x] (if (fn? f) (f x) (or f x)))
          (run-props-parsers
            [props prop-parsers]
            (seq-parse props {:parsers (into [] prop-parsers)
                              :check-clause check-props-clause
                              :apply-parser apply-props-parser}))
          (apply-parser-map
            [[tag props slots] parser]
            [(fn-or-x (:tag parser) tag)
             (when props
               (if (map? props)
                 (run-props-parsers props (:props parser))
                 (fn-or-x (:props parser) props)))
             (fn-or-x (:slots parser) slots)])]
    (if (fn? parser)
      (apply parser args)
      (apply-parser-map args parser))))


(defn run-args-parsers [args parsers]
  (seq-parse args {:parsers parsers
                   :check-clause check-args-clause
                   :apply-parser  apply-args-parser
                   :terminate-early?  #(not (maybe-hiccup? %))}))

(comment
  ;; tag parsing
  (run-args-parsers
   [:a]
   [[keyword? {:tag #(-> % name str)}]])

  ;; props parsing
  (run-args-parsers
   [:a {:b "c"}]
   [[keyword? {:props {:b keyword}}]]))


;; == Hiccup Compiler

(declare compile)

(defn- normalize [{:keys [tag props slots]}]
  [tag]
  [tag props (or slots [])])

(defn compile
  "Compile hiccup body. 
   Body can be hiccup data or list expr to parse; anything else is returned directly.
   Accepts optional `emitter` and `parsers` as options. 
   Hiccup must be parsed into a callable expr either by parsers or emitter."
  [body {:keys [emitter parsers]
         :or {parsers []}
         :as opts}]
  (cond
     ;; => Hiccup
    (vector? body)
    (let [parsers (spec/conform parsers :rec/parsers)
          normalized-args (normalize (spec/conform body :rec/hiccup))
          parsed (run-args-parsers normalized-args parsers)]
      (cond
        ;; => Emit args
        (vector? parsed) 
        (if emitter
         (let [[tag props slots] parsed] (emitter tag props (mapv #(compile % opts) slots)))
          (throw (ex-info "Hiccup must be parsed to callable expr if emitter is not passed." {:expr parsed})))

        ;; => Call parsed expr
        (sequential? parsed) parsed

        :else (throw (ex-info "Invalid hiccup parsed." {:args parsed}))))

     ;; => S-Expression
    ;; (list? body)
    ;; (sexp/transform-children body #(compile % opts))

    :else body))

(comment
  ;; Testing compile options below 
  ;; for testing how Hiccup arguments are conformed, see spec.

  (defn emitter [tag props slots] `(emit ~tag ~props ~@slots))
  (def opts {:emitter emitter
             :parsers [[any? {:tag #(-> % name symbol)}]]})

  ;; tag + child
  (compile [:a "b"] opts)

  ;; tag + props + child
  (compile [:a {:b "c"} "d"] opts)

  ;; tag + props + children
  (compile [:a {:b "c"} "d" "e"] opts)

 ;; nested child is compiled
  (compile [:a {:b "c"} [:d]] opts)

  (let [opts {:emitter emitter
              :parsers [[:a (fn [_ _ _] `(A))]]}]
    (compile [:a] opts)))
