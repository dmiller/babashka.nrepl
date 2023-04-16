(ns babashka.nrepl.impl.sci-utils
  (:require [clojure.tools.reader.reader-types :as rt]))

;; stuff taken from various places:
;;    babashka/sci
;;    borkdude/edamame


;; reader

;; from babahska/sci
#_(defn reader
  "Coerces x into indexing pushback-reader to be used with
  parse-next. Accepts: string or java.io.Reader."
  [x]
  (parser/reader x))
  
;; this is parser/reader
#_(defn reader [x]
  (edamame/reader x))  
  
;; from borkdude/edamame
;; r = clojure.tools.reader.reader-types
#_(defn reader
  [x]
  #?(:clj (r/indexing-push-back-reader (r/push-back-reader x))
     :cljs (let [string-reader (r/string-reader x)
                 buf-len 1
                 pushback-reader (r/PushbackReader. string-reader
                                                    (object-array buf-len)
                                                    buf-len buf-len)]
             (r/indexing-push-back-reader pushback-reader))))

;; our version

(defn reader 
  [x]
  (rt/indexing-push-back-reader (rt/push-back-reader x))


;; parse-next

;; from babashka/sci
#_(defn parse-next
  "Parses next form from reader"
  ([ctx reader] (parse-next ctx reader {}))
  ([ctx reader opts]
   (let [v (parser/parse-next ctx reader opts)]
     (if (utils/kw-identical? parser/eof v)
       (or (get opts :eof)
           ::eof)
       v))))
	   
;; this is parser/parse-next
#_(defn parse-next
  ([ctx r]
   (parse-next ctx r nil))
  ([ctx r opts]
   (let [features (:features ctx)
         readers (:readers ctx)
         readers (if (utils/var? readers) @readers readers)
         auto-resolve (auto-resolve ctx opts)
         parse-opts (cond-> (assoc default-opts
                                   :features features
                                   :auto-resolve auto-resolve
                                   :syntax-quote {:resolve-symbol #(fully-qualify ctx %)}
                                   :readers (fn [t]
                                              (or (and readers (readers t))
                                                  (@data-readers t)
                                                  (some-> (@utils/eval-resolve-state ctx {} t)
                                                          meta
                                                          :sci.impl.record/map-constructor)
                                                  (when-let [f @default-data-reader-fn]
                                                    (fn [form]
                                                      (f t form)))))
                                   :read-eval (if @read-eval
                                                (fn [x]
                                                  (utils/eval ctx x))
                                                throw-eval-read))
                      opts (merge opts))
         ret (try (let [v (edamame/parse-next r parse-opts)]
                    (if (utils/kw-identical? v :edamame.core/eof)
                      eof
                      (if (and (symbol? v)
                               (rt/indexing-reader? r))
                        (vary-meta v assoc
                                   :line (get-line-number r)
                                   :column (- (get-column-number r)
                                              #?(:clj (.length (str v))
                                                 :cljs (.-length (str v)))))
                        v)))
                  (catch #?(:clj clojure.lang.ExceptionInfo
                            :cljs cljs.core/ExceptionInfo) e
                    (throw (ex-info #?(:clj (.getMessage e)
                                       :cljs (.-message e))
                                    (assoc (ex-data e)
                                           :type :sci.error/parse
                                           :phase "parse"
                                           :file @utils/current-file)
                                    e))))]
     ret)))
	   
	   
;; edamame/parse-next

#_(defn parse-next
  "Parses next form from reader. Accepts same opts as `parse-string`,
  but must be normalized with `normalize-opts` first."
  ([reader] (parse-next reader (p/normalize-opts {})))
  ([reader normalized-opts]
   (when (rt/source-logging-reader? reader)
     (let [^StringBuilder buf (p/buf reader)]
       #?(:clj (.setLength buf 0)
          :cljs (.clear buf))))
   (let [v (p/parse-next normalized-opts reader)]
     (if (identical? p/eof v)
       (or (get normalized-opts :eof)
           ::eof)
       v))))	   
;; p/normalize-opts
;;   can we ignore?  we have no opts to pass

#_(defn parse-next
  ([ctx reader] (parse-next ctx reader nil))
  ([ctx reader desugar]
   (let [ir? (r/indexing-reader? reader)]
     (if-let [c (and (skip-whitespace ctx reader)
                     (r/peek-char reader))]
       (let [loc (when ir? (location reader))
             log? (:source ctx)
             ^StringBuilder buf (when log? (buf reader))
             offset (when log? #?(:clj (.length buf)
                                  :cljs (.getLength buf)))
             obj (if log?
                   #?(:clj (r/log-source reader (dispatch ctx reader c))
                      :cljs (r/log-source* reader #(dispatch ctx reader c)))
                   (dispatch ctx reader c))]
         (if (identical? reader obj)
           (recur ctx reader desugar)
           (if (identical? expected-delimiter obj)
             obj
             (let [auto-resolve-ns (:auto-resolve-ns ctx)
                   _ (when auto-resolve-ns
                       (when-let [ns-parsed (when (and (seq? obj)
                                                       (= 'ns (first obj)))
                                              (try (ns-parser/parse-ns-form obj)
                                                   (catch Exception _ nil)))]
                         (when-let [ns-state (:ns-state ctx)]
                           (reset! ns-state (assoc (:aliases ns-parsed) :current (:name ns-parsed))))))
                   postprocess (:postprocess ctx)
                   location? (:location? ctx)
                   end-loc? (:end-location ctx)
                   iobj?? (iobj? obj)
                   src (when log?
                         (.trim (subs (str buf) offset)))
                   loc? (and ir? (or (and iobj??
                                          (or (not location?)
                                              (location? obj)))
                                     postprocess))
                   end-loc (when (and ir? loc? end-loc?)
                             (location reader))
                   row (when loc? (:row loc))
                   end-row (when end-loc? (:row end-loc))
                   col (when loc? (:col loc))
                   end-col (when end-loc? (:col end-loc))
                   postprocess-fn (when postprocess
                                    #(postprocess
                                      (cond->
                                          {:obj %}
                                        loc? (assoc :loc (cond-> {(:row-key ctx) row
                                                                  (:col-key ctx) col}
                                                           end-loc? (-> (assoc (:end-row-key ctx) end-row
                                                                               (:end-col-key ctx) end-col))))
                                        src (assoc (or (:source-key ctx)
                                                       :source)
                                                   src))))
                   obj (if desugar
                         (if postprocess-fn
                           (desugar-meta obj postprocess-fn)
                           (desugar-meta obj)) obj)
                   obj (cond postprocess (postprocess-fn obj)
                             loc? (vary-meta obj
                                             #(cond->
                                                  (-> %
                                                      (assoc (:row-key ctx) row)
                                                      (assoc (:col-key ctx) col))
                                                end-loc? (-> (assoc (:end-row-key ctx) end-row)
                                                             (assoc (:end-col-key ctx) end-col))
                                                src (assoc (:source-key ctx) src)))
                             :else obj)]
               obj))))
       eof))))

;; which appears to require almost the entirety of edamame
	   