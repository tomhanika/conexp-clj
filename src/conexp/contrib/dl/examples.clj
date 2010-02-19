(ns conexp.contrib.dl.examples
  (:use conexp
	conexp.contrib.dl))

(update-ns-meta! conexp.contrib.dl.examples
  :doc "Examples of some description logics.")

;;;

(define-dl Simple-DL [Mother Father Male Female] [hasChild] [and exists]
  and (fn [model C D]
	(intersection (interpret model C)
		      (interpret model D))),
  exists (fn [model r C]
	   (set-of x [x (model-base-set model),
		      :when (exists [y (interpret model C)]
			      (contains? (interpret model r) [x y]))])))

(def some-model (make-model Simple-DL '#{John Marry Peter}
			    {'Mother   '#{Marry},
			     'Father   '#{John},
			     'Male     '#{John, Peter},
			     'Female   '#{Marry},
			     'hasChild '#{[John Peter], [Marry Peter]}}))

(def dl-exp-1 (make-dl-expression Simple-DL '(exists hasChild Male)))
(def dl-exp-2 (make-dl-expression Simple-DL 'Father))

;;;

nil
