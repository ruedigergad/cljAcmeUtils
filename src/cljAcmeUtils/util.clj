;;;
;;;   Copyright 2012, Ruediger Gad
;;;
;;;   This software is released under the terms of the Eclipse Public License 
;;;   (EPL) 1.0. You can find a copy of the EPL at: 
;;;   http://opensource.org/licenses/eclipse-1.0.php
;;;

(ns
  ^{:author "Ruediger Gad",
    :doc "Utility and helper functions"}
  cljAcmeUtils.util
  (:use clojure.java.io)
  (:use clojure.walk)
  (:use clojure.xml)
  (:require (clojure [string :as str])))


(defn sleep [ms]
  (Thread/sleep ms))

(defn exec [cmd]
  (-> (Runtime/getRuntime) (.exec cmd)))
(defn exec-blocking [cmd]
  (-> (exec cmd) (.waitFor)))

(defn exec-with-out [cmd stdout-fn]
  (let [proc (exec cmd)
        stdout-reader (reader (.getInputStream proc))
        stdout-thread (Thread. (fn []
                                 (try 
                                   (while (not (nil? (stdout-fn (.readLine stdout-reader))))))
                                 (.destroy proc)))]
    (.start stdout-thread)))



; Helper functions for handling system properties.
(defn get-system-property [p]
  (System/getProperty p))
(def get-arch (partial get-system-property "os.arch"))
(def get-os (partial get-system-property "os.name"))

(defn is-os? [os]
  (-> (get-os) (.toLowerCase) (.startsWith os)))



; Helper functions for handling files and directories accepting names as 
; Strings or URI (Essentially everything clojure.java.io/file can handle.).
; Well actually these had only been tested using Strings... o_X
(defn exists? [f]
  (.exists (file f)))

(defn is-file? [f]
  (.isFile (file f)))

(defn file-exists? [f]
  (and 
    (exists? f)
    (is-file? f)))

(defn is-dir? [d]
  (.isDirectory (file d)))

(defn dir-exists? [d]
  (and
    (exists? d)
    (is-dir? d)))

(defn mkdir [d]
  (.mkdirs (file d)))

(defn rm [f]
  (delete-file (file f)))

(defn rmdir [d]
  (if (is-dir? d) (rm d)))

(defn touch [f]
  (.createNewFile (file f)))



(defn classname [o]
  "Get classname without leading package names of the given object o."
  (-> (type o) (str) (str/split #"\.") (last)))


(defmacro delay-eval [d & body]
  "Evaluates the supplied body with the given delay 'd' ([ms])."
  `(doto (Thread. 
           (fn [] 
             (do 
               (Thread/sleep ~d) 
               ~@body))) 
     (.start)))



(defn prepare-flag []
  (ref {:flag false}))

(defn set-flag [f]
  (dosync
    (alter f assoc :flag true)))

(defn flag-set? [f]
  (:flag @f))



(defn prepare-counter []
  (ref 0))

(defn inc-counter [c]
  (dosync (alter c inc)))


(defn fn-name [f]
  (-> 
    (.getClass f) 
    (.getName) 
    (str/split #"\$") 
    (last) 
    (str/replace "_" "-")))



(defn byte-seq-to-int [byte-seq]
  (loop [s byte-seq acc 0 shift 0]
    (if (empty? s)
      acc
      (recur (rest s) 
             (+ acc 
                (bit-shift-left 
                  (first s) 
                  (* 8 shift))) 
             (inc shift)))))

(defn get-int-from-byte-vector [v n] 
  (byte-seq-to-int 
    (subvec v n (+ n 4))))

(defn int-to-byte-vector [val]
  (loop [acc [] shift 0] 
    (if (= 8 shift)
      acc
      (recur 
        (conj acc (bit-and (bit-shift-right val (* 8 shift)) 0xff))
        (inc shift)))))

(defn vec-replace [v n delta]
  (loop [i 0 acc v]
    (if (= i (count delta))
      acc
      (recur (inc i) (assoc acc (+ n i) (delta i))))))

(defn change-int-in-byte-vector [v n f]
  (let [int-val (get-int-from-byte-vector v n)
        new-int (f int-val)
        byte-vec (subvec (int-to-byte-vector new-int) 0 4)]
    (vec-replace v n byte-vec)))


(defn xml-string-to-map [xml-str]
  "Takes an XML definition in form of a string and outputs the corresponding map."
  (with-open [xml-in (clojure.java.io/input-stream 
                       (.getBytes xml-str "UTF-8"))] 
    (clojure.xml/parse xml-in)))

(defn stringify-keyword [k]
  "If a keyword is passed returns the name of the keyword.
   Else the input is left unchanged."
  (if (keyword? k)
    (name k)
    k))

(defn stringify-map [m]
  "Convert _all_ keywords in a map to their respective names.
   This, essentially, is an extended version of clojure.walk/stringify-keys,
   which only converts the keys to strings."
  (let [map-fn (fn [[k v]] [(stringify-keyword k) (stringify-keyword v)])
        walk-fn (fn [m] 
                  (if (map? m)
                    (into {} (map map-fn m))
                    m))]
  (clojure.walk/postwalk walk-fn m)))

(defn xml-string-to-map-stringified [xml-str]
  (let [xml-map (xml-string-to-map xml-str)]
    (stringify-map xml-map)))



(defn print-err [s]
  (binding [*out* *err*]
    (print s)))

(defn print-err-ln [s]
  (binding [*out* *err*]
    (println s)))

