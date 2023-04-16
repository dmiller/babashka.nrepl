(ns babashka.nrepl.impl.utils
  {:author "Michiel Borkent"
   :no-doc true}
  (:refer-clojure :exclude [send])
  (:require [bencode.core :refer [write-bencode]]
            [clojure.tools.reader.reader-types :as rt]
			[clojure.tools.reader :as ctr])                     ;;; [sci.core :as sci]
  (:import [System.IO Stream StringWriter]))                                ;;; [java.io Writer PrintWriter StringWriter OutputStream BufferedWriter]

(set! *warn-on-reflection* true)

(defn response-for [old-msg msg]
  (let [session (get old-msg :session "none")
        id (get old-msg :id "unknown")]
    (assoc msg "session" session "id" id)))

(defn send [^Stream os msg {:keys [debug-send]}]                            ;;; ^OutputStream
  (when debug-send (prn "Sending" msg))
  (write-bencode os msg)
  (.Flush os))                                                              ;;; .flush

(defn send-exception [os msg ^Exception ex {:keys [debug] :as opts}]        ;;; ^Throwable
  (let [d (ex-data ex)
        sci-error? (isa? (:type d) :sci/error)
        ex-name (when sci-error?
                  (some-> ^Object (ex-cause ex)                             ;;; ^Throwable
                           .GetType .FullName))                             ;;;  .getClass .getName
        ex-map (Throwable->map ex)
        cause (:cause ex-map)
        {:keys [:file :line :column]} d
        ns *ns*                                                             ;;; @sci/ns  -- NO IDEA what this should be
        loc-str (str ns " "
                     (when line
                       (str (str (or file "REPL") ":")
                            line ":" column"")))
        _strace (.StackTrace ex)]                                           ;;; sci/stacktrace
    (when debug (prn "sending exception" ex-map))
    (send os (response-for msg {"err" (str ex-name
                                           (when cause (str ": " cause))
                                           " " loc-str "\n")}) opts)
    (send os (response-for msg {"ex" (str "class " ex-name)
                                "root-ex" (str "class " ex-name)
                                "status" #{"eval-error"}}) opts)
    (send os (response-for msg {"status" #{"done"}}) opts)))
	
	
(defn reader 
  [x]
  (rt/indexing-push-back-reader (rt/push-back-reader x)))	

;; A couple of ideas here take from borkdude/edamame::edamame.impl.parser


(defn whitespace? [c]
  (and c (or (identical? c \,)
              (Char/IsWhiteSpace (char c)))))

(def eof (Object.))

(defn skip-whitespace
  "Skips whitespace. Returns :none or :some depending on whitespace
  read. If end of stream is reached, returns nil."
  [reader]
  (loop [read :none]
    (when-let [c (rt/read-char reader)]
      (if (whitespace? c)
        (recur :some)
        (do (rt/unread reader c)
            read)))))


(defn parse-next
  [ reader]
  (if-let [c (and (skip-whitespace reader)
                     (rt/peek-char reader))]
	(ctr/read reader)
	eof))


;; this is taken from sci.impl.interpreter/eval-string*

(defn eval-string* [s]
  (with-bindings {*ns* *ns*} 
    (let [reader (reader s)]
      (loop [ret nil]
        (let [expr (parse-next reader)]
          (if (= eof expr)
            ret
            (let [ret (eval expr)]
               (recur ret))))))))