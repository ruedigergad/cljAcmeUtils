;;;
;;;   Copyright 2012, Ruediger Gad
;;;
;;;   This software is released under the terms of the Eclipse Public License 
;;;   (EPL) 1.0. You can find a copy of the EPL at: 
;;;   http://opensource.org/licenses/eclipse-1.0.php
;;;

(ns
  ^{:author "Ruediger Gad",
    :doc "Unit tests for utility and helper functions"}
  cljAcmeUtils.test.util
  (:use clojure.test
        cljAcmeUtils.util))


;;;
;;; Tests for executing commands.
;;;
(deftest test-exec-with-out
  (let [command "ls /etc/passwd"
        stdout-run (prepare-flag)
        stdout-fn (fn [_] (set-flag stdout-run) nil)]
    (exec-with-out command stdout-fn)
    (sleep 100)
    (is (flag-set? stdout-run))))
    
(deftest test-exec-with-out-process-output
  (let [command "ls /etc/passwd"
        stdout-run (prepare-flag)
        stdout-fn (fn [out] (if (= out "/etc/passwd") (set-flag stdout-run)))]
    (exec-with-out command stdout-fn)
    (sleep 100)
    (is (flag-set? stdout-run))))



;;;
;;; Tests for handling files and directories.
;;;
(def test-dirname "test-dir-foo")
(def test-filename "test-file-bar")

(deftest test-exists
  (is (exists? "test/cljAcmeUtils/test/util.clj"))
  (is (exists? "test")))

(deftest test-is-file
  (is (is-file? "test/cljAcmeUtils/test/util.clj"))
  (is (not (is-file? "test"))))

(deftest test-file-exists
  (is (file-exists? "test/cljAcmeUtils/test/util.clj"))
  (is (not (file-exists? "test")))
  (is (not (file-exists? test-filename))))

(deftest test-is-dir
  (is (is-dir? "test"))
  (is (not (is-dir? "test/cljAcmeUtils/test/util.clj"))))

(deftest test-dir-exists
  (is (dir-exists? "test"))
  (is (not (dir-exists? "test/cljAcmeUtils/test/util.clj")))
  (is (not (dir-exists? test-dirname))))

(deftest mkdir-rmdir
  (let [dirname test-dirname]
    (is (not (dir-exists? dirname)))
    (mkdir dirname)
    (is (dir-exists? dirname))
    (rmdir dirname)
    (is (not (dir-exists? dirname)))))

(deftest touch-rm
  (let [filename test-filename]
    (is (not (file-exists? filename)))
    (touch filename)
    (is (file-exists? filename))
    (rm filename)
    (is (not (file-exists? filename)))))



;;;
;;; Tests for setting and querying flags and a simple counter.
;;;
(deftest flag-not-set
  (let [flag (prepare-flag)]
    (is (not (flag-set? flag)))))

(deftest flag-set
  (let [flag (prepare-flag)]
    (set-flag flag)
    (is (flag-set? flag))))

(deftest counter-test
  (let [my-counter (prepare-counter)]
    (dotimes [_ 1000] (inc-counter my-counter))
    (is (= 1000 @my-counter))))



;;;
;;; Tests for getting class and fn names.
;;;
(deftest get-classname
  (let [o (Object.)
        n (classname o)]
    (is (= "Object" n))))

(defn test-fn [] (println "It's -O3 the letter not -03 the number."))

(deftest test-fn-name
  (is (= "test-fn" (fn-name test-fn))))


;;;
;;; Tests for manipulating vectors.
;;;
(deftest test-byte-seq-to-int
  (let [byte-vec [82 17 0 0]
        int-val 4434]
    (is (= int-val (byte-seq-to-int byte-vec)))))

(deftest test-get-int-from-byte-vector
  (let [byte-vec [121 -110 84 79 0 0 0 0 -23 -71 8 0 0 0 0 0 82 17 0 0 82 17 0 0]
        int-val 4434]
    (is (= int-val (get-int-from-byte-vector byte-vec 16))))) 

(deftest test-int-to-byte-vector
  (let [int-val 4434
        byte-vec [82 17 0 0]]
    (is (= byte-vec (subvec (int-to-byte-vector int-val) 0 4)))))

(deftest test-vec-replace
  (let [original-vec [1 2 3 4 5 6]
        expected-vec [1 2 "a" "b" "c" 6]
        changed-vec (vec-replace original-vec 2 ["a" "b" "c"])]
    (is (= expected-vec changed-vec))))

(deftest test-change-int-in-byte-vector
  (let [original-vec [121 -110 84 79 0 0 0 0 -23 -71 8 0 0 0 0 0 82 17 0 0 82 17 0 0]
        expected-vec [121 -110 84 79 0 0 0 0 -23 -71 8 0 0 0 0 0 70 17 0 0 82 17 0 0]
        changed-vec (change-int-in-byte-vector original-vec 16 #(- % 12))]
    (is (= expected-vec changed-vec))))



;;;
;;; Tests for messing with XML.
;;; Primarily for transforming XML data in string format to maps.
;;;
(deftest test-xml-string-to-map
  (let [xml-str "<foo fubar=\"snafu\">bar</foo>"
        expected-map {:tag :foo :attrs {:fubar "snafu"} :content ["bar"]}]
    (is (= expected-map (xml-string-to-map xml-str)))))

(deftest test-stringify-keyword
  (is (= "foo" (stringify-keyword :foo))))

(deftest test-stringify-map
  (let [input-map {:tag :foo :attrs {:fubar "snafu"} :content ["bar"]}
        expected-map {"tag" "foo" "attrs" {"fubar" "snafu"} "content" ["bar"]}]
    (is (= expected-map (stringify-map input-map)))))

(deftest test-xml-string-to-map-stringified
  (let [xml-str "<foo fubar=\"snafu\">bar</foo>"
        expected-map {"tag" "foo" "attrs" {"fubar" "snafu"} "content" ["bar"]}]
    (is (= expected-map (xml-string-to-map-stringified xml-str)))))

