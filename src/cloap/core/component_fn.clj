(ns cloap.core.component-fn
  (:require
   [cloap.core.spec :as spec]
   [cloap.core.hiccup :as hiccup]))

(defn widget-fn*
  "Generate widget as a fn expr."
  [component-map compile-opts]
  (let [{:keys [name params opts body]} component-map
        {:keys [evals hiccups]} body]
    (when (> (count hiccups) 1)
      (println "(Warning) Extra hiccup expressions in component" (str "'" name "'") "are ignored."))
    ;; todo may have to gensym params
    `(fn ~name ~params
       ~@(when (seq evals) evals)
       ~(hiccup/compile (last hiccups) compile-opts))))

(defn generate* [decls compile-opts]
  (let [{:keys [name docstr] :as component-map} (spec/conform decls :cloap/component)]
    `(def ~@(if docstr [name docstr] [name])
       ~(widget-fn* component-map compile-opts))))

(comment
  (defn emitter [tag props] `(~tag ~props))

  (def opts {:parsers [[keyword? {:tag #(-> % name symbol)}]]
             :emitter `emitter})

  ;; string
  (generate* '(name [] "string") opts)

  ;; tag + child
  (generate* '(name [] [:a "b"]) opts)

  ;; extra child prints warning
  (generate* '(name [] [:a "b"] [:a "b"]) opts))

