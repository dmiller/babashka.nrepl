(ns babashka.nrepl.test-utils
  {:author "Michiel Borkent"}
  (:require [clojure.clr.io :as io])
  (:import [System.Net Dns IPEndPoint IPAddress]
           [System.Net.Sockets TcpListener SocketException] ))

(set! *warn-on-reflection* true)

(defn current-time-millis []
   (.ToUnixTimeMilliseconds DateTimeOffset/UtcNow))
   

(defn wait-for-port
  "Waits for TCP connection to be available on host and port. Options map
  supports `:timeout` and `:pause`. If `:timeout` is provided and reached,
  `:default`'s value (if any) is returned. The `:pause` option determines
  the time waited between retries."
  ([host port]
   (wait-for-port host port nil))
  ([^String host ^long port {:keys [:default :timeout :pause] :as opts}]
   (let [opts (merge {:host host
                      :port port}
                     opts)
         t0 (current-time-millis)
		 host-entry (Dns/GetHostEntry ^String host)
         ip-address (first (.AddressList host-entry))
		 ip-endpoint (IPEndPoint. ^IPAddress ip-address (int port))]
     (loop []
       (let [v (try (.Stop (TcpListener. ip-endpoint))
                    (- (current-time-millis) t0)
                    (catch SocketException _e
                      (let [took (- (current-time-millis) t0)]
                        (if (and timeout (>= took timeout))
                          :wait-for-port.impl/timed-out
                          :wait-for-port.impl/try-again))))]
         (cond (identical? :wait-for-port.impl/try-again v)
               (do (System.Threading.Thread/Sleep (int (or pause 100)))
                   (recur))
               (identical? :wait-for-port.impl/timed-out v)
               default
               :else
               (assoc opts :took v)))))))

(defn wait-for-path
  "Waits for file path to be available. Options map supports `:default`,
  `:timeout` and `:pause`. If `:timeout` is provided and reached, `:default`'s
  value (if any) is returned. The `:pause` option determines the time waited
  between retries."
  ([path]
   (wait-for-path path nil))
  ([^String path {:keys [:default :timeout :pause] :as opts}]
   (let [opts (merge {:path path}
                     opts)
         t0 (current-time-millis)]
     (loop []
       (let [v (when (not (.Exists (io/file-info path)))
                 (let [took (- (current-time-millis) t0)]
                   (if (and timeout (>= took timeout))
                     :wait-for-path.impl/timed-out
                     :wait-for-path.impl/try-again)))]
         (cond (identical? :wait-for-path.impl/try-again v)
               (do (System.Threading.Thread/Sleep (int (or pause 100)))
                   (recur))
               (identical? :wait-for-path.impl/timed-out v)
               default
               :else
               (assoc opts :took
                 (- (current-time-millis) t0))))))))

(comment
  (wait-for-port "localhost" 80)
  (wait-for-port "localhost" 80 {:timeout 1000})
  (wait-for-port "google.com" 80)

  (wait-for-path "/tmp/hi")
  (wait-for-path "/tmp/there" {:timeout 1000})

  )
