(ns rectify.core
  (:require [rectify.core.hiccup :as hic]))

(defn hiccup 
  "Compile hiccup `body`.

  Hiccup data is considered to be a vector where the first item is the tag, second is optional props map,
  and the rest are children. 
   e.g. [:example {:a b} child]
   
  To interop with frameworks like Flutter that have widgets with both .child and .children params,
  we treat any argument after props map as a list of slots.
  Hence why parser functions have a slot vector as the third argument: (fn [tag props slots] ..) 
  This can turn out to be a single child (fn [_ _ [child]]) or multiple chidlren (fn [_ _ [& children]])
  It's up to the parser fn to parse the allowed input appropriately.
 
  Accepts following `opts`:
  - :strict :: boolean
    Whether sexpr expr that are not declared in parse list are allowed.
    Not recommended for mainstream development since may lead to uncompiled hiccup.
    But useful for intial experimentation. 

  - :parsers :: vector of [matching-clause arg-parser] tuples
    Hiccup argument parsers are sequential, each passing off its values to the next with a matching clause.
    A parser may terminate early by returning non-hiccup.

  - :expr-parsers :: vector of [matching-symbol expr-parser] tuples
    Sexp parsers are not sequential, first matching result is returned.
    When running in strict mode, only matched exprs from these parsers are allowed to be used.

  - :emitter :: fn on  ([tag props [& children]] ..)
    When parsers do not return an emittable expr, the emitter opt is required."
  [body opts]
  (hic/compile body opts))

