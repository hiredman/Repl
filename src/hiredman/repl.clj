;these imports are not all needed, needs to be weeded
(ns hiredman.repl
  (:gen-class)
  (require [clojure.main :as r])
  (:import (javax.swing JFrame JTextArea JPanel UIManager
			SwingUtilities JScrollPane BoxLayout
			Box JComponent JLabel InputMap ActionMap
			KeyStroke Action JButton)
	  (javax.swing.text DocumentFilter PlainDocument)
	  (javax.swing.event MouseInputAdapter)
	  (java.awt.event ActionListener KeyAdapter)
	  (java.awt Dimension Image Font)
	  (java.io StringReader PrintWriter PushbackReader 
		    Writer StringReader OutputStreamWriter BufferedReader)
	  (java.awt GridLayout FlowLayout Rectangle Point)
	  (javax.imageio ImageIO)
	  (java.net URL)
	  (java.util.logging Logger Level)
	  (java.util.concurrent LinkedBlockingQueue ArrayBlockingQueue)
	  (clojure.lang IDeref Associative LineNumberingPushbackReader IFn
			IReference)))

;*Q* is the shared queue used for wiring everythhing together
;$ is bound to call (partial render *Q*)
(declare *Q* *font* $)

(defn fn->kl [fun]
  (proxy [KeyAdapter] []
    (keyTyped [event] (fun event))))

(defn fn->mia [fun]
  (proxy [MouseInputAdapter] []
    (mouseClicked [e] (fun e))))

(defn fn->kl-pressed [fun]
  (proxy [KeyAdapter] []
    (keyPressed [event] (fun event))))

(defn fn->al [fun]
  (proxy [ActionListener] []
    (actionPerformed [event] (fun event))))

(defn doc-filter [func]
  (proxy [DocumentFilter] []
    (insertString [& x] (apply func x))))            
                    
(defmacro EDT
  "runs body on the Event-Dispatch-Thread (Swing)"
  [& body]
  `(SwingUtilities/invokeLater (fn [] ~@body)))                                

(defn image-component
  "paints an Image on a JComponent"
  [x]
  (doto (proxy [JComponent] []
	  (paint [g]
		 (. g (drawImage x 0 0 nil))))
    (.setPreferredSize (Dimension. (.getWidth x) (.getHeight x)))))

(defn scale
  "scale and image to fit in a box"
  [img box]
  (loop [p (Point. (.getWidth img) (.getHeight img))]
    (if (.contains box p)
      (.getScaledInstance img (.getX p) (.getY p) Image/SCALE_DEFAULT)
      (recur (Point. (double (* 0.7 (.getX p)))
		     (double (* 0.7 (.getY p))))))))

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

(defmacro log [string]
  `(.info (Logger/getLogger "global") ~string))

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

(def scroll-back 20)

(defn window [Q]
  (let [frame (JFrame. "Clojure")
	here (JPanel.)
	scroll (doto (JScrollPane.
		      (doto here
			(.setLayout (BoxLayout. here (. BoxLayout Y_AXIS)))))
		 (.setVerticalScrollBarPolicy
		  JScrollPane/VERTICAL_SCROLLBAR_ALWAYS))]
    (EDT
     (doto frame
       (.add scroll)
       (.setSize 480 360)
       (.setVisible true)))
    (let [tiles (atom clojure.lang.PersistentQueue/EMPTY)]
      (react-on [Q to ::focus-last as _] (EDT (.requestFocus (last @tiles))))
      (react-on [Q to ::render as X]
		(let [jc (if (fn? X) (X here) X)]
		  (EDT
		   (.add here jc)
		   (.validate frame)
		   (.repaint frame)
		   (.scrollRectToVisible
		    (.getViewport scroll)
		    (.getBounds jc))
		   (.requestFocus jc))
		  (swap! tiles conj jc)
		  (when (> (count @tiles) scroll-back)
		    (let [x (peek @tiles)]
		      (swap! tiles pop)
		      (EDT (.remove here x)))))))))

(defmulti render (fn [a b] (type b)))

(defmethod render String [Q string]
  (Q (event ::render
	    (doto (JTextArea. string)
	      (.setEditable false)
	      #_(.addMouseListener
	       (fn->mia
		(Q (event ::text-input-focus nil))))))))

(defmethod render JComponent [Q jcomp]
  (Q (event ::render jcomp)))

(defmethod render Image [Q image]
  (Q (event ::render
	    #(image-component (scale image (.getBounds %))))))

(def last-prompt (atom nil))

(defn prompt [Q]
  (let [parens (atom ())
	jta (JTextArea.)]
    (doto jta
      (-> (#(swap! last-prompt (constantly %))))
      (.setDocument
       (proxy [PlainDocument] []
	 (insertString [off str att]
	   (log (format "%s to %s"
			(pr-str [off str att])
			(pr-str (.getText jta))))
	   (condp = str
	     "(" (swap! parens conj str)
	     ")" (swap! parens pop)
	     nil)
	   (if (and (= str "\n")
		    #_(empty? @parens)
		    (not (.isEmpty (.getText jta))))
	     (future
	      (log ":go web go:")
	      (Q (event ::read
			(try
			 (-> jta
			     .getText
			     StringReader.
			     PushbackReader.)
			 (catch Exception e
			   (-> "" StringReader. PushbackReader.)))))
	      (EDT (.setEditable jta false)
		   (.addMouseListener
		    jta
		    (fn->mia
		     (fn [e]
		       (Q (event ::text-input-focus nil)))))))
	     (proxy-super insertString off str att))))))))

(defn to-read [Q string]
  (Q (event ::read
	    (-> string
		StringReader.
		PushbackReader.))))

(defn prompt [Q]
  (let [jta (JTextArea.)]
    (doto jta
      (.setFont (:font (meta Q)))
      (.setLineWrap true)
      (.setPreferredSize (Dimension. 100 0))
      (.setRows 1)
      (.addKeyListener
       (fn->kl-pressed
	(fn [e] (when (= 38 (.getKeyCode e))
		  (let [{:keys [place items]} (:history (meta Q))]
		    (.setText jta (get items place))
		    (alter-meta! Q update-in [:history :place] dec))))))
      (.setDocument
       (proxy [PlainDocument] []
	 (insertString [off str att]
           (let [txt (.getText jta)]
	     (if (and (= str "\n")
		      (re-find #"\S" txt))
	       (do
		 (future
		  (to-read Q txt)
		  (alter-meta! Q update-in [:history :items] conj txt)
		  (alter-meta! Q update-in [:history :place]
		    (fn [_] (-> Q meta :history :items count dec)))
		  (EDT (.setEditable jta false)))
		 (proxy-super insertString off "" att))
	       (proxy-super insertString off str att)))))))))

(defn bq->ops [jta]
  (let [buffer (StringBuffer.)]
    (proxy [java.io.OutputStream IDeref] []
      (deref [] buffer)
      (flush []
             (when (< 0 (.length buffer))
              (let [sb (.toString buffer)]
		(render jta sb)
                (.setLength buffer 0))))
      (close [] (.flush this))
      (write
        ([i] (.append buffer (char i)))
        ([buf off len]
         (doseq [i (take len (drop off buf))]
           (.append buffer (char i))))))))

(defn start-repl-thread [Q]
  (future
   (try 
    (binding [*out* (-> Q bq->ops OutputStreamWriter. PrintWriter.)
	      *Q* Q $ (partial render Q)
	      *font* (Font. "Monospaced" Font/PLAIN 12)]
      (let [read-q (LinkedBlockingQueue.)]
	(react-on [Q to ::read as rdr] (.put read-q rdr))
	(clojure.main/repl
	 :caught (fn [x] (.printStackTrace x *out*) (.flush *out*) (println ""))
	 :need-prompt (constantly true)
	 :prompt #(do (alter-meta! *Q* assoc :font *font*)
		      (render *Q* (str "#" (ns-name *ns*) "#"))
		      (render *Q* (prompt *Q*)))
	 :read (fn [a b]
		 (binding [*in* (.take read-q)]
		   (r/repl-read a b))))))
    (catch Exception e
      (log (pr-str e))))))

(defn repl []
  (. UIManager (setLookAndFeel (. UIManager (getSystemLookAndFeelClassName)))) 
  (let [Q (hydra)]
    (alter-meta! Q assoc :history {:place 0 :items ["(in-ns 'user)"]})
    (react-on [Q to ::log as x] (log x))
    (react-on [Q to ::text-input-focus as X]
	      (let [n @last-prompt]
		(when n
		  (EDT (.requestFocus n)))))
    (window Q)
    (start-repl-thread Q)
    Q))

(defn -main [] (repl))