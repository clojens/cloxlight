(ns

  ^{:url "http://clojure.org/concurrent_programming"
    :tools "LightTable, CruxLight, Chromium, Clojure"
    :composer "Rob Jentzema 2013"
    :copyright "Copyright 2008-2012 Rich Hickey"}

  cloxlight.clojuredotorg.concurrent-programming)

(comment
  "The software transactional memory system (STM), exposed through dosync, ref,
  set, alter, et al, supports sharing changing state between threads in a
  synchronous and coordinated manner.")

(comment
  "The dynamic var system, exposed through def, binding, et al, supports isolating
  changing state within threads.")

(comment
  "All ref modifications within a transaction happen or none do. Also, reads of
  refs within a transaction reflect a snapshot of the reference world at a
  particular point in time, i.e. each transaction is isolated from other
  transactions.")

(comment
  "In this example a vector of Refs containing integers is created, then a set
  of threads are set up to run a number of iterations incrementing every Ref.
  This creates extreme contention, but yields the correct result.")

(import '(java.util.concurrent Executors))

(defn test-stm [nitems nthreads niters]
  (let [refs  (map ref (repeat nitems 0))
        pool  (Executors/newFixedThreadPool nthreads)
        tasks (map (fn [t]
                      (fn []
                        (dotimes [n niters]
                          (dosync
                            (doseq [r refs]
                              (alter r + 1 t))))))
                   (range nthreads))]
    (doseq [future (.invokeAll pool tasks)]
      (.get future))
    (.shutdown pool)
    (map deref refs)))

;; to run: evaluate this expression after unquoting it
'(test-stm 10 10 10000)

(comment
  "In typical use refs can refer to Clojure collections, which, being
  persistent and immutable, efficiently support simultaneous speculative
  'modifications' by multiple transactions. ")

(comment
  "Dynamic vars are also mutable references to objects and have a root binding
  which can be established by def, and can be set using `set!`, but only if
  they have been bound to a new storage location thread-locally using binding.
  ")

;; Those bindings and any subsequent modifications to those bindings will only
;; be seen within the thread by code in the dynamic scope of the binding block.
;; Nested bindings obey a stack protocol and unwind as control exits the
;; binding block.

; note: had to add ^:dynamic, which used to be default until 1.3
(def ^:dynamic *v*)

(defn incv [n] (set! *v* (+ *v* n)))

(defn test-vars [nthreads niters]
  (let [pool (Executors/newFixedThreadPool nthreads)
        tasks (map (fn [t]
                     #(binding [*v* 0]
                        (dotimes [n niters]
                          (incv t))
                        *v*))
                   (range nthreads))]
      (let [ret (.invokeAll pool tasks)]
        (.shutdown pool)
        (map #(.get %) ret))))

; run
'(test-vars 10 1000000)
'(incv 4)
; java.lang.ClassCastException: clojure.lang.Var$Unbound cannot be cast to
; java.lang.Number

(comment
  "Dynamic vars provide a way to communicate between different points on the
  call stack without polluting the argument lists and return values of the
  intervening calls. In addition, dynamic vars support a flavor of
  context-oriented programming.")

(defn loves [x y]
  (str x " loves " y))

; fix: wouldn't work with latest clojure had to add
(def ^:dynamic str str)

(defn test-rebind []
  (println (loves "ricky" "lucy"))
  (let [str-orig str]
    (binding [str (fn [& args]
                    (println "Logging str")
                    (apply str-orig args))]
      (println (loves "fred" "ethel")))))

'(test-rebind)
;; ricky loves lucy
;; Logging str
;; fred loves ethel