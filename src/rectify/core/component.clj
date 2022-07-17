(ns rectify.core.component
  (:require
   [rectify.core.spec :as spec]
   [rectify.core.hiccup :as hic]))

(defn widget-fn*
  "Generate widget as a fn expr."
  [component-map compile-opts]
  (let [{:keys [name params opts body]} component-map
        {:keys [evals hiccups]} body]
    (when (> (count hiccups) 1)
      (println "(Warning) Extra hiccup expressions in component" (str "'" name "'") "are ignored."))
    ;; todo may have to gensym params
    `(fn ~name ~params
       ~@(let [exprs (cond-> []
                       (seq evals) (into evals)
                       (seq hiccups)  (conj (last hiccups)))]
          ;;  make sure last return value is compiled, even if it's a list expr
           (update exprs (- (count exprs) 1) #(hic/compile % compile-opts))))))


(defn generate* [decls compile-opts]
  (let [{:keys [name docstr] :as component-map} (spec/conform decls :rec/component)]
    `(def ~@(if docstr [name docstr] [name])
       ~(widget-fn* component-map compile-opts))))

(comment
  (defn emitter [tag props [& children]] `(~tag ~props ~@children))

  (def opts {:parsers [[keyword? {:tag #(-> % name symbol)}]]
             :emitter emitter})

  ;; string
  (generate* '(name [] "string") opts)

  ;; tag + child
  (generate* '(name [] [:a "b"]) opts)

  ;;  without any hiccup, last eval expr acts as child
  ;; note: will throw since not registered in allowed sexp list
  (generate* '(name [] (component)) opts)

  ;; extra child prints warning
  (generate* '(name [] [:a "b"] [:a "b"]) opts)
  )

