(ns babashka.nrepl.impl.utils
  {:author "Michiel Borkent"
   :no-doc true}
  (:refer-clojure :exclude [send])
  (:require [bencode.core :refer [write-bencode]]
            )                                      ;;; [sci.core :as sci]
  (:import [System.IO Stream StringWriter]))                  ;;; [java.io Writer PrintWriter StringWriter OutputStream BufferedWriter]

(set! *warn-on-reflection* true)

(defn response-for [old-msg msg]
  (let [session (get old-msg :session "none")
        id (get old-msg :id "unknown")]
    (assoc msg "session" session "id" id)))

(defn send [^Stream os msg {:keys [debug-send]}]                        ;;; ^OutputStream
  (when debug-send (prn "Sending" msg))
  (write-bencode os msg)
  (.Flush os))                                                          ;;; .flush

(defn send-exception [os msg ^Exception ex {:keys [debug] :as opts}]    ;;; ^Throwable
  (let [d (ex-data ex)
        sci-error? (isa? (:type d) :sci/error)
        ex-name (when sci-error?
                  (some-> ^Object (ex-cause ex)                      ;;; ^Throwable
                           .GetType .FullName))                              ;;;  .getClass .getName
        ex-map (Throwable->map ex)
        cause (:cause ex-map)
        {:keys [:file :line :column]} d
        ns ""                                                           ;;; @sci/ns  -- NO IDEA what this should be
        loc-str (str ns " "
                     (when line
                       (str (str (or file "REPL") ":")
                            line ":" column"")))
        _strace (.StackTrace ex)]                                       ;;; sci/stacktrace
    (when debug (prn "sending exception" ex-map))
    (send os (response-for msg {"err" (str ex-name
                                           (when cause (str ": " cause))
                                           " " loc-str "\n")}) opts)
    (send os (response-for msg {"ex" (str "class " ex-name)
                                "root-ex" (str "class " ex-name)
                                "status" #{"eval-error"}}) opts)
    (send os (response-for msg {"status" #{"done"}}) opts)))

