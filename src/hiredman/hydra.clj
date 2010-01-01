(ns hiredman.hydra
  (:import (java.util.concurrent LinkedBlockingQueue)
	   (clojure.lang IDeref IFn IReference)))

(defn hydra
  "returns a BlockingQueue, will return a new infinite lazy-seq wrapped in a
   delay evertime it is deref'ed. all items put in the LinkedBlockingQueue
   will be added to all the lazy-seqs producded by deref'ing"
  []
  (let [consumers (atom nil)
	m (atom nil)
        producer (proxy [LinkedBlockingQueue IDeref IFn IReference] []
                   (withMeta [x] (reset! m x) this)
		   (meta [] @m)
		   (alterMeta [fun args] (reset! m (apply fun @m args)) @m)
		   (resetMeta [x] (reset! m x) @m)
		   (invoke [& x]
                           (doseq [y x] (.put this y)))
                   (deref []
                          (let [x (LinkedBlockingQueue.)]
                            (swap! consumers conj x)
                            (repeatedly #(.take x)))))]
    (future
     (while true
       (let [x (.take producer)]
	 (doseq [y @consumers]
	   (.put y x)))))
    producer))



(defstruct <event> :type :payload)

(defn event [type payload]
  (struct <event> type payload))

(defmacro react-on
  "Takes a hydra, and on another thread, filters the body for events
   with (:type event) == predicate. and for each event that matches,
   the body is executed with the payload of the event bound as x"
  [[q to predicate as x] & body]
  `(future
    (doseq [~x (map :payload
		    (filter (fn [x#] (= ~predicate (:type x#)))
			    (deref ~q)))]
      ~@body)))