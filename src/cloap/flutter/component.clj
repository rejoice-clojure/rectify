(ns cloap.flutter.component
  (:require [cloap.core.component :as co]))

(def parsers
  [[:app (fn [_ {:keys [child]}]
           `(f/widget (fm/MaterialApp :home ~(co/hiccup child))))]

   [:txt (fn [_ {:keys [child]}]
           `(fm/Text ~child))]])

(defn- emitter [tag props]
  `(~tag ~props))


(def options
  {:parsers parsers
   :emitter emitter})

(defn define-component [decls]
  (try
    (co/define-component decls options)
    (catch Exception e
      (println "------ ERROR -------------------------------------------------------------------")
      ;; (println e)
      (println "Error compilng component.")
      (println "--------------------------------------------------------------------------------")
      ;; todo: we're throwing error so defcs don't fail, but then cljd prints non-runtime error.
      (throw e))))
