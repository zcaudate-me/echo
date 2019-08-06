(ns echo.core)

(defn -main []
  (loop []
    (let [input (do (print "\n=>")
                    (flush)
                    (read-line))]
      (prn input)
      (if (not= "q" input)
        (recur)))))

(comment
  (-main)
  )
