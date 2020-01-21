(ns two-zero-four-eight.main)
(require '[clojure.string :as str])

(defn remove-zero [col]
  (filter (complement zero?) col))

(defn partition-by-identity [col]
  (partition-by identity col))

(defn add-identical [col]
  (map (partial reduce +) col))

(defn fill-with-zero [size col]
  (if (= size (count col))
    col
    (recur size (conj col 0))))

(defn swap-and-add-identical [col]
  (fill-with-zero 4 (add-identical (partition-by-identity (remove-zero col)))))

(defn swap-right [cols]
  (map swap-and-add-identical cols))

(defn swap-left [cols]
  (map reverse (map swap-and-add-identical (map reverse cols))))

(defn rotate [cols] (apply map vector cols))

(defn add-down [cols]
  (rotate (map swap-and-add-identical (rotate cols))))

(defn add-up [cols]
  (map reverse (rotate (map swap-and-add-identical (map reverse (rotate cols))))))

(defn draw [grid action]
  (println grid action)
  (->> grid
       (mapv vec)
       (mapv #(str/join "|" %))
       (str/join "\n------\n")
       (println)))

(defn play-move [move grid]
  (case move
    1 (do (println "move all to right") (swap-right grid))
    2 (do (println "move all to left") (swap-left grid))
    3 (do (println "move all to down") (add-down grid))
    (do (println "move all to up") (add-up grid))))


(defn read-promt [msg]
  (do (println msg) (read)))


(defn ask-action []
  (read-promt "Enter some command\n"))

(defn play [grid]
  (let [action (ask-action) next-grid (play-move action grid)]
    (draw (mapv vec next-grid) action)
    (recur next-grid)))

(def grid1 [[0 0 2 2] [0 2 0 2] [4 2 8 0] [0 2 2 4]])
