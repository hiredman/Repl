; TODO: use hydra for events
(import '(javax.swing JFrame JTextArea JPanel UIManager
		      SwingUtilities JScrollPane BoxLayout
		      Box JComponent JLabel InputMap ActionMap
		      KeyStroke Action)
	'(javax.swing.text DocumentFilter)
        '(java.awt.event ActionListener KeyAdapter)
	'(java.awt Dimension Image)
        '(java.io StringReader PrintWriter PushbackReader 
		  Writer StringReader OutputStreamWriter BufferedReader)
        '(java.awt GridLayout FlowLayout Rectangle Point)
	'(javax.imageio ImageIO)
	'(java.net URL)
        '(java.util.concurrent LinkedBlockingQueue ArrayBlockingQueue)
        '(clojure.lang IDeref Associative LineNumberingPushbackReader))

(declare *gout*)

(. UIManager (setLookAndFeel (. UIManager (getSystemLookAndFeelClassName))))

(defn fn->kl [fun]
  (proxy [KeyAdapter] []
    (keyTyped [event] (fun event))))

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

(def history (atom {:place 0 :lines ["(in-ns 'user)"]}))

(defn window [in out]
  (let [here (JPanel.)
	window (JFrame. "Clojure")
	scroll (doto (JScrollPane.
		      (doto here
			(.setLayout (BoxLayout. here (. BoxLayout Y_AXIS)))))
		 (.setVerticalScrollBarPolicy
		  JScrollPane/VERTICAL_SCROLLBAR_ALWAYS))]
    (EDT
     (doto window
       (.setSize 480 360)
       (.add scroll)
       (.setVisible true)))
    (future
     (while true
	    (let [x (.take out)]
	      (EDT
	       (.add here
		     (if (fn? x) (x here) x))
	       (.setVisible window true))
	      (EDT
	       (let [y (.getBounds x)]
		 (-> scroll .getViewport
		     (.scrollRectToVisible y)))
	       (.grabFocus x)))))))

(defmulti display (fn [x y] (type y)))

(defmethod display String [out string]
  (.put out
	#(doto (JTextArea. string)
	   (.setEditable false)
	   (.setLineWrap true)
	   ((fn [ta]
	      (.setMaximumSize
	       ta
	       (let [x (.getPreferredSize ta)]
		 (Dimension. (- (.getWidth %) 10) (.getHeight x)))))))))

(defn scale
  "scale and image to fit in a box"
  [img box]
  (loop [p (Point. (.getWidth img) (.getHeight img))]
    (if (.contains box p)
      (.getScaledInstance img (.getX p) (.getY p) Image/SCALE_DEFAULT)
      (recur (Point. (double (* 0.7 (.getX p)))
		     (double (* 0.7 (.getY p))))))))

(defmethod display Image [out image]
  (.put out
	#(image-component
	  (scale image (.getBounds %)))))

(defmethod display javax.swing.JComponent [out comp]
  (.put out comp))

(defmethod display clojure.lang.IFn [out comp]
  (.put out comp))

(declare $)

(defn paren-count [x]
  (count (filter #{\( \)} x)))

(defn prompt []
  (let [oot (JTextArea. "  ")
	oot (doto oot
	      (.addKeyListener
	       (fn->kl-pressed
		(fn [event]
		  (when (and (= \newline (-> event bean :keyChar))
			     (-> event .getSource .getText
				 paren-count (mod 2) (= 0)))
		    (-> event .getSource
			(.setText
			 (-> event .getSource
			     .getText drop-last
			     ((partial apply str)))))
		    (future
		     (-> event .getSource .getText (#(.put IN %)))
		     (swap! history
			    (fn [{:keys [place lines]}]
			      {:lines (conj lines
					    (-> event .getSource .getText))
			       :place (count lines)}))
		     (EDT (.setEditable (.getSource event) false)))))))
	      (.addKeyListener
	       (fn->kl-pressed
		(fn [e]
		  (when (= 38 (-> e bean :keyCode))
		    (let [h @history]
		      (EDT
		       (-> e .getSource
			   (.setText ((:lines h) (:place h))))
		       (-> e .getSource
			   (.moveCaretPosition 
			    (dec (count ((:lines h) (:place h)))))2))
		      (swap! history update-in [:place] dec))))))
	      (.setColumns 15)
	      (.setRows 2))
	x (LinkedBlockingQueue. 1)
	p (doto (JTextArea. (str (ns-name *ns*) "=>"))
	    (.setEditable false))]
    (display OUT
	     (doto (JPanel.)
	       (.setLayout (FlowLayout. FlowLayout/LEADING))
	       (.add p)
	       (EDT (.put x :boo))
	       (.add oot)))
    (.take x)
    (EDT (.grabFocus oot))))

(defn bq->ops [jta]
  (let [buffer (StringBuffer.)]
    (proxy [java.io.OutputStream IDeref] []
      (deref [] buffer)
      (flush []
             (when (< 0 (.length buffer))
              (let [sb (.toString buffer)]
                (display jta sb)
                (.setLength buffer 0))))
      (close [] (.flush this))
      (write
        ([i] (.append buffer (char i)))
        ([buf off len]
         (doseq [i (take len (drop off buf))]
           (.append buffer (char i))))))))


(defn start-repl-thread [in out]
  (future
   (binding [*out* (-> out bq->ops OutputStreamWriter. PrintWriter.)
	     *gout* out $ (partial display out)]
     (clojure.main/repl
	:caught (fn [x] (.printStackTrace x *out*) (.flush *out*) (println ""))
	:need-prompt (constantly true)
	:prompt prompt
	:read (fn [a b]
		(binding [*in* (-> in .take StringReader. PushbackReader.)]
		  (clojure.main/repl-read a b)))))))




(doc-filter
 (fn [bp offset string attr]
   (.insertString bp offset string attr)))

(def IN (LinkedBlockingQueue.))

(def OUT (LinkedBlockingQueue.))

(window IN OUT)

(start-repl-thread IN OUT)
