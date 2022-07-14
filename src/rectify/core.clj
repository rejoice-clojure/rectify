(ns rectify.core
  (:require
   [rectify.core.component :as co]
   [rectify.core.hiccup :as hic]))


(def ^:dynamic *component-context* nil)

(defn with-bound-component [f]
  (if-let [ctx *component-context*]
    (f ctx)
    (throw (Exception. "Component context not bound."))))


;; == Stateful fns 
;; These fns use the component context directly.

(defn hiccup [expr]
  (with-bound-component
    (fn [{:keys [compile-opts]}]
      (hic/compile expr compile-opts))))

(defn component [decls]
  (with-bound-component
    (fn [{:keys [compile-opts]}]
      (co/generate* decls compile-opts))))

;;  == Component factory
;; Each component binds its own context to delegate nested compilation.
;; Note: we use `with-bindings` rather than `binding` since the former is a fn run at macroexpand time.

(defn define-component [decls opts]
  (let [name (first decls)
        comp-id (str *ns* "/" name)]
    (with-bindings*
      {(var *component-context*) {:comp-id comp-id
                                  :compile-opts opts}}
      #(component decls))))

