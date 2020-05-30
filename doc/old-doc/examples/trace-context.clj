;;; Daniel Borchmann, 2011
;;; This file is in the public domain.

(require 'conexp.main)
(in-ns 'conexp.main)

(use 'conexp.layouts.base)

;;;

(defn trace-context
  "Returns the trace context of ctx-S in ctx-K."
  [ctx-S ctx-K]
  (let [H    (set-of [h 1] | h (objects ctx-S)),
        N    (set-of [n 1] | n (attributes ctx-S)),
        G    (set-of [g 2] | g (objects ctx-K)),
        M    (set-of [m 2] | m (attributes ctx-K)),
        I_HN (set-of [[g 1] [m 1]] | [g m] (incidence ctx-K)
                                     :when (and (contains? H [g 1])
                                                (contains? N [m 1]))),
        I_GN (set-of [[g 2] [m 1]] | [g m] (incidence ctx-K)
                                     :when (and (contains? G [g 2])
                                                (contains? N [m 1]))),
        I_HM (set-of [[g 1] [m 2]] | [g m] (incidence ctx-K)
                                     :when (and (contains? H [g 1])
                                                (contains? M [m 2]))),
        I_GM (incidence
              (smallest-bond (make-context-nc G N I_GN)
                             (make-context-nc H M I_HM)
                             (set-of [[g 2] [m 2]] | [g m] (incidence ctx-K))))]
    (make-context-nc (union H G)
                     (union N M)
                     (union I_HN I_GN I_HM I_GM))))

(defn trace-layout
  "Returns a layout of the concept lattice of the trace context of ctx-S in ctx-K, with a reduced
  annotation."
  [ctx-S ctx-K]
  (let [trace-ctx (trace-context ctx-S ctx-K)
        layout    (standard-layout (concept-lattice trace-ctx)),
        ann       (concept-lattice-annotation layout),
        label     #(apply str (interpose ", " %))]
    (make-layout (lattice layout)
                 (positions layout)
                 (connections layout)
                 (fn [x]
                   [(label (set-of g | [g n] (first (ann x)) :when (= n 1))),
                    nil])
                 (fn [x]
                   [(label (set-of m | [m n] (second (ann x)) :when (= n 1))),
                    nil]))))

(defn interval-context
  "Returns the formal context representing the interval induced by the pair [E F] in the concept
  lattice of ctx."
  [ctx [E F]]
  (make-context (aprime ctx F) (oprime ctx E) (incidence ctx)))

;;;

nil
