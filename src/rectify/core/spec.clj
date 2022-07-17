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
  ;; standalone tag
  (conform '[:a]  :rec/hiccup)
  
  ;; nil props
  (conform '[:a nil] :rec/hiccup)
  
  ;; rops + nil children
  (conform '[:a {} nil] :rec/hiccup)
  
  ;; single child
  (conform '[:a {} [:b]] :rec/hiccup)

  ;; multiple children
  (conform '[:a {} [:b] [:c]] :rec/hiccup)

  ;; custom formation
  (conform '[:a symbol {:b c} [:d]] :rec/hiccup)
  )
