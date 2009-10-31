(ns conexp.io.util)

(defn get-line []
  (read-line))

(defn get-lines [n]
  (doall (take n (repeatedly #(get-line)))))

(defmacro with-in-reader [file & body]
  `(with-open [input# (reader ~file)]
     (binding [*in* input#]
       ~@body)))
