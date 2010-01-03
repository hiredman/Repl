;; Repl runs basically by message passing over a shared bus/queue
;; the queue it uses is a hydra, see hydra.clj

;these imports are not all needed, needs to be weeded
(ns hiredman.repl
  (:gen-class)
  (:use [hiredman.hydra :only (react-on event hydra)]
	[clojure.stacktrace :only (e)])
  (:require [clojure.main :as r])
  (:import (javax.swing JFrame JTextArea JPanel UIManager
			SwingUtilities JScrollPane BoxLayout
			Box JComponent JLabel InputMap ActionMap
			KeyStroke Action JButton JTextPane)
	  (javax.swing.text DocumentFilter PlainDocument
			    DefaultStyledDocument StyleContext
			    StyleConstants)
	  (javax.swing.event MouseInputAdapter)
	  (java.awt.event ActionListener KeyAdapter)
	  (java.awt Dimension Image Font)
	  (java.io StringReader PrintWriter PushbackReader 
		    Writer StringReader OutputStreamWriter BufferedReader)
	  (java.awt GridLayout FlowLayout Rectangle Point Color)
	  (javax.imageio ImageIO)
	  (java.net URL)
	  (java.util.logging Logger Level)
	  (java.util.concurrent LinkedBlockingQueue ArrayBlockingQueue)
	  (clojure.lang IDeref Associative LineNumberingPushbackReader IFn
			IReference)))

(defmacro log [string]
  `(.info (Logger/getLogger "global") ~string))

;*Q* is the shared queue used for wiring everythhing together
;$ is bound to call (partial render *Q*)
(declare *Q* *font* $ *window*)

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
  (log "image-component")
  (doto (proxy [JPanel] []
	  (getSize [] (Dimension. (.getWidth x) (.getHeight x)))
	  (paint [g]
		 (proxy-super paint g)
		 (. g (drawImage x 0 0 nil))))
    (.setBackground Color/WHITE)
    (.setSize (Dimension. (.getWidth x) (.getHeight x)))
    (.setPreferredSize (Dimension. (.getWidth x) (.getHeight x)))))

(defn scale
  "scale and image to fit in a box"
  [img box]
  #_(log "scale")
  (loop [p (Point. (.getWidth img) (.getHeight img))]
    (if (.contains box p)
      (.getScaledInstance img (.getX p) (.getY p) Image/SCALE_DEFAULT)
      (recur (Point. (double (* 0.6 (.getX p)))
		     (double (* 0.6 (.getY p))))))))

(def scroll-back 20)

(defn- scroll-pane
  "this is not a function for reusability, but modularity.
   the window function was getting to honking long"
  [here]
  (doto (JScrollPane.
	 (doto here
	   (.setLayout (BoxLayout. here (. BoxLayout Y_AXIS)))))
    (.setVerticalScrollBarPolicy
     JScrollPane/VERTICAL_SCROLLBAR_ALWAYS)
    (.setHorizontalScrollBarPolicy
     JScrollPane/HORIZONTAL_SCROLLBAR_AS_NEEDED)))

(defn window
  "this function creates the JFrame, and sets up the gui"
  [Q]
  (let [frame (JFrame. "Clojure")
	here (JPanel.)
	scroll (scroll-pane here)]
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
		   #_(.repaint frame)
		   (.scrollRectToVisible
		    (.getViewport scroll)
		    (let [x (.getBounds jc)]
		      (Rectangle.
		       0
		       (.getY x)
		       (.getWidth x)
		       (.getHeight x))))
		   (.requestFocus jc))
		  (swap! tiles conj jc)
		  (when (> (count @tiles) scroll-back)
		    (let [x (peek @tiles)]
		      (swap! tiles pop)
		      (EDT (.remove here x)))))))
    frame))


(defmulti #^{:doc "used to render something to the repl"}
  render (fn [a b] (type b)))

(defmethod render String [Q string]
  (Q (event ::render
	    #(doto (JTextArea. string)
	       (.setFont (-> Q meta :font))
	       (.setEditable false)
	       (.setLineWrap true)
	       (.setSize (.getSize (.getParent %)))
	       (.addMouseListener
		(fn->mia
		 (Q (event ::text-input-focus nil))))))))

(defn- align-component [x]
  (doto (JPanel.)
    (.setLayout (FlowLayout. FlowLayout/LEADING))
    (.add x)
    (.setSize (.getSize x))))

(defmethod render JComponent [Q jcomp]
  (log "render JComponent")
  (Q (event ::render (align-component jcomp))))

(defmethod render JTextPane [Q jtp]
  (Q (event ::render jtp)))

(defmethod render Image [Q image]
  #_(log "render Image")
  (Q (event ::render
	    #(try 
	      (image-component
	       (scale image
		      (let [x (-> % .getParent .getSize)]
			(Rectangle. 0 0 (.getWidth x) (.getHeight x)))))
		  (catch Exception e
		    (JTextArea. (pr-str e)))))))

;unused? intent is for keeping track of last rendered thing, so all
;mouse listeners can redirect focus to last rendered thing. this is
;global state, other solutions more desirable
(def last-prompt (atom nil))

(defn to-read
  "takes a string and a Q and preps the string for binding to *in* and dumps
   into Q"
  [Q string]
  (Q (event ::read
	    (-> string
		StringReader.
		PushbackReader.))))

(def #^{:doc "balanced pairs"}
     pairs '((\( \))
	     (\[ \])
	     (\" \")
	     (\{ \})))

(defn balanced?
  "are all the pairs balanced in this string?"
  [string]
  ((comp not some)
   false?
   (map
    (fn [pair] (-> pair set (filter string) count (mod 2) zero?))
    pairs)))

(defn- fire [Q jta txt off att this]
  (EDT (.setEditable jta false))
  (.addMouseListener jta
   (fn->mia
    (fn [m]
      (Q (event ::focus-last nil)))))
  (future
   (to-read Q txt)
   (alter-meta! Q update-in [:history :items] conj txt)
   (alter-meta! Q update-in [:history :place]
		(fn [_]
		  (-> Q meta :history :items count dec))))
  (proxy-super insertString off "" att))

(defn- prompt-document [Q jta]
  (log "prompt-document")
  (let [sc (StyleContext.)]
    (proxy [DefaultStyledDocument] [sc]
      (insertString [off string att]
		    (log "insertString")
		    (let [txt (.getText jta)]
		      (if (and (= string "\n")
			       (re-find #"\S" txt)
			       (balanced? txt))
			(fire Q jta txt off att this)
			(do
			  (proxy-super insertString off string att)
			  (when-let [x ((comp first filter)
					#(= string (str (first %)))
					pairs)]
			    (proxy-super
			     insertString (inc off) (str (second x)) att)
			    (.setDot (.getCaret jta) (inc off))))))))))

(defn prompt [Q]
  (log "prompt")
  (let [jta (JTextPane.)]
    (doto jta
      (.setFont (:font (meta Q)))
      #_(.setLineWrap true)
      #_(.setPreferredSize (Dimension. 100 0))
      #_(.setRows 1)
      (.addKeyListener
       (fn->kl-pressed
	(fn [e] (when (= 38 (.getKeyCode e))
		  (let [{:keys [place items]} (:history (meta Q))]
		    (.setText jta (get items place))
		    (alter-meta! Q update-in [:history :place] dec))))))
      (.setDocument (prompt-document Q jta)))))

(defn bq->ops [jta]
  (let [buffer (StringBuffer.)]
    (proxy [java.io.OutputStream IDeref] []
      (deref [] buffer)
      (flush []
	     (log "bq->ops flush")
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
  (let [window *window*]
    (future
     (try
      (log "TWO")
      (binding [*out* (-> Q bq->ops OutputStreamWriter. PrintWriter.)
		*Q* Q $ (partial render Q)
		*font* (Font. "Monospaced" Font/PLAIN 12)
		*window* window]
	(log "ONE")
	(let [read-q (LinkedBlockingQueue.)]
	  (react-on [Q to ::read as rdr] (.put read-q rdr))
	  (clojure.main/repl
	   :caught (fn [x]
		     (binding [*e x] (e))
		     (.flush *out*)
		     (println ""))
	   :need-prompt (constantly true)
	   :prompt #(do (alter-meta! *Q* assoc :font *font*)
			(render *Q* (str "#" (ns-name *ns*) "#"))
			(render *Q* (prompt *Q*)))
	   :read (fn [a b]
		   (binding [*in* (.take read-q)]
		     (r/repl-read a b))))))
      (catch Exception e
	(log (pr-str e)))))))

(defn repl []
  (let [Q (hydra)]
    (alter-meta! Q assoc :history {:place 0 :items ["(in-ns 'user)"
						    "(in-ns 'hiredman.repl)"]})
    (react-on [Q to ::log as x] (log x))
    (react-on [Q to ::text-input-focus as X]
	      (let [n @last-prompt]
		(when n
		  (EDT (.requestFocus n)))))
    (binding [*window* (window Q)]
      (start-repl-thread Q)
      (alter-meta! Q assoc :window *window*))
    Q))

(defn write-history [name]
  (with-open [o (-> name java.io.File. java.io.FileWriter.)]
    (binding [*out* o]
      (doseq [i (-> *Q* meta :history :items)] (println i)))))

(defn load-history [name]
  (with-open [i (-> name java.io.File. java.io.FileReader.
		    java.io.PushbackReader.)]
    (let [o (Object.)]
      (loop [r (read i false o)]
	(when (not= r o)
	  (alter-meta! *Q* update-in [:history :items] conj (pr-str r))))))
  (alter-meta! *Q* update-in [:history :place]
	       (constantly (-> *Q* meta :history :items count))))

(defn -main []
  (. UIManager (setLookAndFeel (. UIManager (getSystemLookAndFeelClassName))))
  (let [Q (repl)]
    (.setDefaultCloseOperation (-> Q meta :window) JFrame/EXIT_ON_CLOSE))
  nil)