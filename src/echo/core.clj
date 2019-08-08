(ns echo.core)

(defn rps-compare [a b]
  (cond (= a b) 0
        (= [:s :r] [a b]) -1
        (= [:r :s] [a b]) 1
        (= [:p :s] [a b]) -1
        (= [:s :p] [a b]) 1
        (= [:r :p] [a b]) -1
        (= [:p :r] [a b]) 1))

(defn init-game []
  (let [cards (take 20 (repeatedly #(rand-nth [:s :r :p])))
        player-a (frequencies (take 3 cards))
        player-b (frequencies (take 3 (drop 3 cards)))]
    {:a player-a :b player-b}))


(defn read-input [state key]
  (let [valid (get @state key)]
    (loop []
      (let [input (do (println (format "\nPLAYER %s OPTIONS %s" key (keys valid)))
                            (print "=> ")
                      (flush)
                      (read-line))
            choice (try (let [choice (read-string input)]
                          (when (get valid choice)
                            (println (format "\nPLAYER %s CHOOSES %s" key choice))
                            choice))
                        (catch Throwable t))]
        (if choice
          (do (swap! state (fn [state]
                             (let [nstate (update-in state [key choice] dec)]
                               (if (zero? (-> nstate key choice))
                                 (update-in state [key] dissoc choice)
                                 nstate))))
              (println (format "\nPLAYER %s now has %s" key (get @state key)))
              [choice state])
          (do (println "WRONG INPUT, PLEASE TRY AGAIN")
              (recur)))))))

(defn round [state]
  (let [input-a (read-input state :a)
        input-b (read-input state :b)
        result (rps-compare input-a input-b)]
    (case result
      -1 :a
      1  :b
      0  nil)))

(defn game []
  (let [state (atom (init-game))
        _     (prn "GAME STATE" @state)]
    (or (round state)
        (recur))))

(comment
  (rps-compare :s :r) -1
  (rps-compare :p :p) 0
  (rps-compare :p :r) 1

  (frequencies [:s :s :p])
  {:s 2, :p 1}
  {:s 1, :r 1, :p 1}
  )

(comment
  (game)
  
  (init-game)

  (atom (init-game))
  
  (read-input (atom (init-game)) :a)
  
  (rps-compare :s :r)
  (rps-compare :s :s)

  (def -state- (atom (init-game)))

  (round (atom (init-game)))

  
  
  )

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
