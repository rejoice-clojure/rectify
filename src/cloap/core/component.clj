(ns cloap.core.component
  (:require [cloap.core.spec :as spec]
            [cloap.core.hiccup :as hiccup]))

(defn generate* [input compile-opts]
  (let [{:keys [name params opts body]} (spec/conform input ::spec/component)
        {:keys [evals hiccups]} body]
    (when (> (count hiccups) 1)
      (println "(Warning) Extra hiccup expressions in component" (str "'" name "'") "are ignored."))

    `(fn ~name ~params
       ~@(when (seq evals) evals)
       ~(hiccup/compile (last hiccups) compile-opts))))

(comment
  (defn emitter [tag props] `(~tag ~props))
  (def opts {:parsers [[keyword? {:tag #(-> % name symbol)}]]
             :emitter emitter})

  ;; tag + child
  (generate* '(name [] [:a "b"]) opts)

  ;; extra child prints warning
  (generate* '(name [] [:a "b"] [:a "b"]) opts)
  )


