(ns sudoku-solver.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn get-board
  "Get a board from a source as a vector of vectors"
  [] ; default
  [[:u :u :u  2  6 :u  7 :u  1]
   [ 6  8 :u :u  7 :u :u  9 :u]
   [ 1  9 :u :u :u  4  5 :u :u]
   [ 8  2 :u  1 :u :u :u  4 :u]
   [:u :u  4  6 :u  2  9 :u :u]
   [:u  5 :u :u :u  3 :u  2  8]
   [:u :u  9  3 :u :u :u  7  4]
   [:u  4 :u :u  5 :u :u  3  6]
   [ 7 :u  3 :u  1  8 :u :u :u]])

(defn get-rows
  "Get a seq of rows as a seq of numbers"
  [board]
  board)

(defn get-cols
  "Get a seq of columns as a seq of numbers"
  [board]
  (apply map list board))

(defn drop-x-y
  "Drop x cols and y rows"
  [coll x y]
  (map #(nthrest % x) (nthrest coll y)))

(defn take-cell
  [rows]
  (apply concat (map #(take 3 %) (take 3 rows))))

(defn get-cells
  "Get a seq of of the cells as a seq of numbers"
  [board]
  (for [col [0 3 6] row [0 3 6]
        :let [cell (take-cell (drop-x-y board col row))]]
    cell))

(defn valid?
  "Returns false if there are any duplicates in a row, col or cell"
  [board]
  (let [regions (concat (get-rows board)
                        (get-cols board)
                        (get-cells board))
        reduced-regions (map #(filter (complement keyword?) %) regions) ;remove :u
        valid-regions (map #(apply distinct? %) reduced-regions)]
    (when (every? identity valid-regions) board)))

(defn solve
  ([board]
   (solve board 0))
  ([board level]
   (let [row (quot level 9) col (mod level 9)]
     (cond
      (>= level 81) board
      (not= :u (nth (nth board row) col)) (recur board (inc level))
      :else (loop [try-now 1]
              (when (<= try-now 9)
                (if-let [tentative (valid? (assoc-in board [row col] try-now))]
                  (if-let [result (solve tentative (inc level))]
                    result
                    (recur (inc try-now)))
                  (recur (inc try-now)))))))))
