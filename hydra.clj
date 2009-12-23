(import '(java.util.concurrent LinkedBlockingQueue)
        '(clojure.lang IDeref IFn))

(defn hydra
  "returns a BlockingQueue, will return a new infinite lazy-seq wrapped in a delay
  evertime it is deref'ed. all items put in the LinkedBlockingQueue will be added to
  all the lazy-seqs producded by deref'ing"
  []
  (let [consumers (atom nil)
        producer (proxy [LinkedBlockingQueue IDeref IFn] []
                   (invoke [& x]
                           (doseq [y x] (.put this y)))
                   (deref []
                          (let [x (LinkedBlockingQueue.)]
                            (swap! consumers conj x)
                            (delay (repeatedly #(.take x))))))]
    (future
      (while true
        (let [x (.take producer)]
          (doseq [y @consumers]
            (.put y x)))))
    producer))


(defmacro monitor* [[Q _ X _ Y & r] & body]
  (let [x (if (first r) r identity)]
    `(future
      (doseq [~Y (map ~x (filter ~X (deref ~Q)))]
	~@body))))

(defmacro monitor
  "Takes a hydra, Q filters for events of type X, executes Body
   with the payload of each event bound to Y"
  [[Q _ X _ Y] & body]
  `(monitor* [~Q (fn [x#] (= ~X (:type x#))) ~Y :payload] ~@body))

(defstruct <event> :type :payload)

(defn event [type payload]
  (struct <event> type payload))

(let [tiles (atom clojure.lang.IPersistantQueue/EMPTY)]
  (monitor [Q :for ::render :as x]
    (let [jc  (if (instance? JComponent x) x (x panel))]
      (EDT (.add panel jc)
	   (.grabFocus jc))
      (swap! tiles conj jc)
      (when (> (count @tiles) 10)
	(.remove panel (peek @tiles))
	(swap! tiles pop)))))