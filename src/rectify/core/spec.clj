(ns rectify.core.spec
  (:require [clojure.spec.alpha :as s]))

(defn conform
  [input spec-key]
  (let [ret (s/conform spec-key input)]
    (if (= ret :clojure.spec.alpha/invalid)
      (throw (ex-info "Cannot conform hiccup declaration." (s/explain-data spec-key input)))
      ret)))

;; == Parsers

(s/def :rec/parsers
  (s/coll-of
   (s/tuple
    some?
    #(or (fn? %) (map? %)))
   :kind vector?))

(comment
  ;;  empty parser
  (conform [] :rec/parsers)

 	;; map parser
  (conform [[keyword? {}]] :rec/parsers)

 	;; fn parser
  (conform [[keyword? (fn [])]] :rec/parsers)

 	;; invalids
  (conform [nil] :rec/parsers)
  (conform [{}] :rec/parsers)
  (conform [[]] :rec/parsers))


;; == Hiccup

(s/def :rec/hiccup
  (s/cat
   :tag    #(or (keyword? %) (symbol? %))
   :props  (s/? (s/map-of keyword? any?))
  ;;  accumulate rest of children arguments as slots
   :slots  (s/* some?)
   :catch  (s/* nil?)))

(comment
  (conform '[widget]  :rec/hiccup)
  (conform '[widget nil] :rec/hiccup)
  (conform '[widget {} nil] :rec/hiccup)
  (conform '[widget {} [:text]] :rec/hiccup)
  (conform '[widget {} [:text] [:text]] :rec/hiccup))

;;  == Component

(s/def :rec/component
  (s/cat
   :name   symbol?
   :docstr (s/? string?)
   :params (s/coll-of any? :kind vector?)
   :opts  (s/? (s/map-of keyword? any?))
   :body  (s/? (s/cat
                :evals (s/* #(not (or (vector? %) (nil? %))))
                ;; Only one child should be returned but we match for many to throw more precise error elsewhere.
                :hiccups (s/* vector?)))
   :catch (s/* nil?)))

(comment
  ;;  name + param
  (conform '(name []) :rec/component)

  ;;  name + doc + param
  (conform '(name "doc" []) :rec/component)

  ;;  name + param + opts
  (conform '(name [] {}) :rec/component)

  ;; name + param + hiccup child
  (conform '(name [] [:a b]) :rec/component)

  ;;  name + param + opts + hiccup child 
  (conform '(name [] {} [:a b]) :rec/component)

  ;; name + param + opts + hiccup children
  (conform '(name [] {} [:a b] [:a b]) :rec/component)


  ;;  name + param + opts + evals + child
  (conform '(name [] {} (expr) [:a b]) :rec/component)

  ;; nil body
  (conform '(name [] nil) :rec/component))
