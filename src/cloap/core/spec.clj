(ns cloap.core.spec
  (:require [clojure.spec.alpha :as s]))

(defn conform
  [input spec-key]
  (let [ret (s/conform spec-key input)]
    (if (= ret :clojure.spec.alpha/invalid)
      (throw (ex-info "Cannot conform hiccup declaration." (s/explain-data spec-key input)))
      ret)))

;; == Parsers

(s/def :cloap/parsers
  (s/coll-of
   (s/tuple
    some?
    #(or (fn? %) (map? %)))
   :kind vector?))

(comment
  ;;  empty parser
  (conform [] ::parsers)

 	;; map parser
  (conform [[keyword? {}]] ::parsers)

 	;; fn parser
  (conform [[keyword? (fn [])]] ::parsers)

 	;; invalids
  (conform [nil] ::parsers)
  (conform [{}] ::parsers)
  (conform [[]] ::parsers))


;; == Hiccup

(s/def :cloap/hiccup
  (s/cat
   :tag   #(or (keyword? %) (symbol? %))
   :props (s/? (s/map-of keyword? any?))
   :slot-prop  (s/alt
                :child (s/? some?)
                :children (s/+ some?))
   :catch (s/* nil?)))

(comment
  (conform '[widget]  ::hiccup)
  (conform '[widget nil] ::hiccup)
  (conform '[widget {} nil] ::hiccup)
  (conform '[widget {} [:text]] ::hiccup)
  (conform '[widget {} [:text] [:text]] ::hiccup))



;;  == Component

(s/def :cloap/component
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
  (conform '(name []) ::component)

  ;;  name + doc + param
  (conform '(name "doc" []) ::component)

  ;;  name + param + opts
  (conform '(name [] {}) ::component)

  ;; name + param + hiccup child
  (conform '(name [] [:a b]) ::component)

  ;;  name + param + opts + hiccup child 
  (conform '(name [] {} [:a b]) ::component)

  ;; name + param + opts + hiccup children
  (conform '(name [] {} [:a b] [:a b]) ::component)


  ;;  name + param + opts + evals + child
  (conform '(name [] {} (expr) [:a b]) ::component)

  ;; nil body
  (conform '(name [] nil) ::component))
