(ns cloap.core.hiccup
  (:require [clojure.spec.alpha :as s]
            [cloap.core.spec :as spec])
  (:refer-clojure :exclude [compile]))

(defn- maybe-hiccup?
  [x]
  (vector? x))

;;  == Argument Parser

(defn seq-parse
  "Parse normalized `args` using `parsers`. 
   Matched parsers are ran sequentially, each passing its output to the next one.

   Parsers are {check parser} pairs. 
   A check is a predicate fn or keyword to that declares the match.
   A parser is a fn (fn [...args]) or map applied according to `apply-parser-map`."
  [initial {:keys [parsers
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
          initial
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
            [[tag props] parser]
            [(fn-or-x (:tag parser) tag)
             (when props
               (if (map? props)
                 (run-props-parsers props (:props parser))
                 (fn-or-x (:props parser) props)))])]
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

(defn- normalize [{:keys [tag props slot-prop]}]
  [tag]
  (if-let [[k v] slot-prop]
    (if-not (= v :clojure.spec.alpha/nil)
      [tag (assoc props k v)]
      [tag props])
    [tag props]))

(defn create-default-parsers [compile-fn]
  [[any? {:props {:child compile-fn
                  :children #(mapv compile-fn %)}}]])

(defn compile
  [body {:keys [emitter parsers]
         :or {parsers []}
         :as opts}]
  (cond
     ;; => Hiccup
    (vector? body)
    (let [parsers (spec/conform parsers :cloap/parsers)
          normalized-args (normalize (spec/conform body :cloap/hiccup))
          parsers  (into parsers (create-default-parsers #(compile % opts)))
          args (run-args-parsers normalized-args parsers)]      
      (cond
        ;; => Emit args.
        ;; Any vector args returned from parser will be normalized, so we apply them to emitter diretly.
        (vector? args) (apply emitter args)

        ;; => Call expr. 
        ;; Parsers can directly return self-callable exprs.
        (sequential? args) args

        :else (throw (ex-info "Invalid component argument." {:args args}))))

     ;; => S-Expression
    ;; (list? body)
    ;; (sexp/transform-children body #(compile % opts))

    :else body
    )
    )

(comment
  (defn emitter [tag props] `(emit ~tag ~props))
  (def opts {:emitter emitter
             :parsers [[any? {:tag #(-> % name symbol)}]]})

  ;; tag + child
  (compile [:a "b"] opts)

  ;; tag + props + child
  (compile [:a {:b "c"} "d"] opts)

  ;; tag + props + children
  (compile [:a {:b "c"} "d" "e"] opts)

 ;; nested child
  (compile [:a {:b "c"} [:d]] opts)
  
  (let [opts {:emitter emitter
              :parsers [[:a (fn [_ _] `(A))]]}]
    (compile [:a] opts))
  
  )
