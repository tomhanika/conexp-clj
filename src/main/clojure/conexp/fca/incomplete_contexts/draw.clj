(ns conexp.fca.incomplete-contexts.draw
  (:gen-class)
  (:require [clojure.set :refer :all]
            [conexp.fca.contexts :as cxts  :refer [Context make-context]]
            [conexp.io.layouts :refer :all]
            [conexp.fca.lattices :refer [concept-lattice]]
            [conexp.gui.draw :as draw]
            [conexp.layouts.dim-draw :refer :all]
            [conexp.fca.incomplete-contexts.incomplete-contexts :refer :all]
            [conexp.fca.incomplete-contexts.conexp-interop :refer :all]
))


(defn draw-concept-lattice
  "draw concept lattice of a formal or complete incomplete context"
  [cxt]
  (cond
    (satisfies? Context cxt)
    (draw-concept-lattice cxt)
    (and (satisfies? Incomplete-Context-Protocol cxt)
         (is-complete-incomplete-context? cxt))
    (draw/draw-concept-lattice (incomplete-context->formal-context cxt))
    true
    (throw (Exception. "input is neither a formal context nor a complete incomplete context"))
    ))

(defn draw-certain-concept-lattice
   "draw the concept lattice of an incomplete context where all '?' are replaced by '.'"
  [cxt]
  (cond
    (satisfies? Context cxt)
    (draw-concept-lattice cxt)
    (satisfies? Incomplete-Context-Protocol cxt)
    (draw/draw-concept-lattice (incomplete-context->certain-incidences-context cxt))
    true
    (throw (Exception. "input is neither a formal context nor an incomplete context"))
    ))


(defn draw-possible-concept-lattice
  "draw the concept lattice of an incomplete context where all '?' are replaced by 'x'"
  [cxt]
  (cond
    (satisfies? Context cxt)
    (draw-concept-lattice cxt)
    (satisfies? Incomplete-Context-Protocol cxt)
    (draw/draw-concept-lattice (incomplete-context->possible-incidences-context cxt))
    true
    (throw (Exception. "input is neither a formal context nor an incomplete context"))
    ))
