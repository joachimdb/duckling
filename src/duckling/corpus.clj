(ns duckling.corpus
  (:use     [clojure.tools.logging]
            [clojure.edn :as edn]
            [plumbing.core :except [millis]])
  (:require [aima-clj.logic.algorithms.unify :refer :all]
            [aima-clj.logic.algorithms.unify.extensions]
            [duckling.time.obj :as time]
            [duckling.util :as util]))
;; (remove-ns 'duckling.corpus)
; Checker functions return *nil* when OK, or [expected actual] when not OK

;; (def args [2013 2 18 :day-of-week 1 :day 18 :month 2])
;; (split-with integer? args)
;; => [(2013 2 18) (:day-of-week 1 :day 18 :month 2)]

(defn- vec->date-and-map
  "Turns a vector of args into a date and a map of extra fields"
  [args]
  (let [[date-fields other-keys-and-values] (split-with integer? args)
        token-fields (into {} (map vec (partition 2 other-keys-and-values)))
        timezone (get token-fields :timezone -2)
        date (-> (apply time/t timezone date-fields)
                 (?> (:grain token-fields) (assoc :grain (:grain token-fields))))]
    [date token-fields]))

(defn- pattern->date-and-map
  "Turns a pattern map into a date and a map of extra fields"
  [{:keys [timezone year month day hour minute second grain direction precision]
    :as pattern
    :or {timezone -2}}]
  (let [date (-> (apply time/t timezone (take-while identity (util/select-values pattern [:year :month :day :hour :minute :second])))
                 (?> grain (assoc :grain grain)))
        token-fields (dissoc pattern :year :month :day :hour :minute :second)]
    [date token-fields]))

(defn tkn [p]
  (let [ks (keys p)]
    (fn [context token]
      (let [v (select-keys token ks)]
        (when-not (unify p v)
          (println "pttern: " p)
          (println "source: " v)
          (when (= :dim p) (:dim v)
                (println "pttern: " p)
                (println "source: " v))
          [p v])))))

(defn datetime*
  "Creates a datetime checker function to check if the token is valid"
  [{:keys [timezone year month day hour minute second grain direction precision] :as pattern}]
  (let [[date token-fields] (pattern->date-and-map pattern)
        p (assoc (select-keys token-fields [:direction :precision])
                 :dim :time
                 :value date)]
    (fn [context token]
      (let [v (select-keys token [:dim :value :direction :precision])]
        (when-not (unify p v)
          [p v])))))

(defn datetime
  "Creates a datetime checker function to check if the token is valid"
  [& args]
  (if (and (= 1 (count args))
           (map? (first args)))
    (datetime* first args)
    (let [[date token-fields] (vec->date-and-map args)
          p (assoc (select-keys token-fields [:direction :precision])
                   :dim :time
                   :value date)]
      (fn [context token]
        (let [v (select-keys token [:dim :value :direction :precision])]
          (when-not (unify (:value token) date)
            [p v]))))))

(comment

  ;; pttern:  {:dim :time, :value {:start #object[org.joda.time.DateTime 0x43079115 2013-02-12T16:00:00.000+01:00], :grain :hour}}
  ;; source:  {:dim :time, :value {:start #object[org.joda.time.DateTime 0x3a471d3a 2013-02-12T16:00:00.000-02:00], :grain :hour, :timezone CET}}

  ;; failure
  ;; from test "4pm CET"
  ;; due to rule

  ;; "<time> timezone"
  ;; [(dim :time) (dim :timezone)]
  ;; (set-timezone %1 (:value %2))

  ;; (see duckling.time.prod/set-timezone)
  
  ;; {:dim :time,
  ;;  :value {:start #object[org.joda.time.DateTime 0x3a2327b7 2013-02-12T04:30:00.000-02:00], :grain :second}}
  
  
  (datetime* {:timezone -2 :year 2013 :month 2 :day 12 :hour 4 :minute 30 :second 00})
  (unify)

  (def date (first (vec->date-and-map [2013 2 12 4 30 00])))
  
  
  (def check (datetime 2013 2 12 4 30 00))
  (def token {:dim :time,
              :value
              {:start (clj-time.coerce/from-string "2013-02-12T04:30:00.000-02:00"),
               ;; :timezone -2
               :grain :second},
              :body "right now"})
  (def r (check nil token))
  (def p (first r))
  (def v (second r))
  (unify p v)
  (match p v true :else false)
  (def date (:value p))
  (match (:value token) p true :else false)
  
  (match v p true :else false)
  (def p (:value p))
  (def v (:value v))
  (match p v true :else false)
  
  (match (:value p) (:value v) true :else false)
  (= (first date) (first r))
  (def date (:start (:value (first r))))
  (def token (:start (:value (second r))))
  (= date token)
  (clj-time.core/equal? date token)
  (match date token true :else false)
  )

(defn datetime-interval
  "Creates a datetime interval checker function"
  [from to]
  (let [[start start-fields] (vec->date-and-map from)
        [end end-fields] (vec->date-and-map to)
        date (time/interval start end)]
    (fn [context {:keys [value dim] :as token}]
      (when-not
        (and
          (= :time dim)
          (= value date))
        [date value]))))

(defn number
  "check if the token is a number equal to value.
  If value is integer, it also checks :integer true"
  [value]
  (fn [_ token]
    (when-not
        (and
         (= :number (:dim token))
         (or (not (integer? value)) (:integer token))
         (= (:value token) value))
      [{:dim :number
        :value value} token])))

(defn ordinal
  [value]
  (fn [_ token]
    (when-not
        (and
         (= :ordinal (:dim token))
         (= (:value token) value))
      [{::dim :ordinal
        :calue value} token])))

(defn temperature
  "Create a temp condition"
  [value' & [unit' precision']]
  (fn [_ {:keys [dim value unit precision] :as token}]
    (when-not
        (and
         (= :temperature dim)
         (= value' value)
         (= unit' unit)
         (= precision' precision))
      [value' value])))

(defn distance
  "Create a distance condition"
  [value' & [unit' normalized' precision']]
  (fn [_ {:keys [dim value unit normalized precision] :as token}]
    (when-not
        (and
         (= :distance dim)
         (= value' value)
         (= unit' unit)
         (= normalized' normalized)
         (= precision' precision)))))

(defn money
  "Create a amount-of-money condition"
  [value' & [unit' precision']]
  (fn [_ {:keys [dim value unit precision] :as token}]
    (when-not (and
               (= :amount-of-money dim)
               (= value' value)
               (= unit' unit)
               (= precision' precision))
      [{:dim :amount-of-money
        :value value'
        :unit unit'
        :precision precision'} token])))

(defn place
  "Create a place checker"
  [pnl n]
  (fn [context token]
    (when-not (and
               (= :pnl (:dim token))
               (= n (:n token))
               (= pnl (:pnl token)))
      [{:dim :pnl :n n :pnl pnl} token])))

(defn metric
  "Create a metric checker"
  [cat val]
  (fn [context token]
    (when-not (and
               (= :unit (:dim token))
               (= val (:value token))
               (= cat (:cat token)))
      [{:dim :unit
        :value val
        :cat cat} token])))

(defn quantity
  "Create a quantity condition"
  [value unit & [product]]
  (fn [_ token]
    (when-not (and
               (= :quantity (:dim token))
               (= value (:value token))
               (= unit (:unit token))
               ;; (= product (-> token :value :product))
               )
      [{:dim :quantity
        :value value
        :unit unit} token])))

(defn volume
  "Create a volume condition"
  [value unit & [normalized]]
  (fn [_ token]
    (when-not (and
               (= :volume (:dim token))
               (= value (:value token))
               (= unit  (:unit token))
               ;; (= normalized (-> token :value :normalized))
               )
      [{:dim :volume
        :value value
        :unit unit} token])))


(defn integer
  "Return a func (duckling pattern) checking that dim=number and integer=true,
  optional range (inclusive), and additional preds"
  [& [min max & predicates]]
  (fn [_ token]
    (when-not
        (and (= :number (:dim token))
             (:integer token)
             (or (nil? min) (<= min (:value token)))
             (or (nil? max) (<= (:value token) max))
             (every? #(% token) predicates))
      [{:dim :number
        :integer true} token])))

(defrecord CorpusTest [text checks resource])
(defn- corpus-test [resource]
  (CorpusTest. #{} [] resource))
(defn- add-text [^CorpusTest c text]
  (update-in c [:text] conj text))
(defn- add-check [^CorpusTest c check]
  (update-in c [:checks] conj check))

(defrecord TestResult [test text check-results exception?])

(defn failed? [^TestResult tr]
  (or (:exception? tr)
      (not-any? nil? (:check-results tr))))

(defrecord Corpus [context tests])

(defn get-tests-by-text [corpus text]
    (filter (fn [test] (contains? (:text test) text)) (:tests corpus)))

(defn corpus
  "Parse corpus" ;; TODO should be able to load several files, like rules
  [resource forms]
  (-> (fn [state [head & more :as forms] test context tests]
        ;; (println state head test (count tests))
        (if head
          (case state
            :init (cond (map? head) (recur :test-strings more
                                           (corpus-test resource)
                                           head
                                           tests)
                        :else (throw (Exception. (str "Invalid form at init state. A map is expected for context:" (prn-str head)))))

            :test-strings (cond (string? head) (recur :test-strings more
                                                      (add-text test head)
                                                      context
                                                      tests)
                                (fn? head) (recur :test-checks forms
                                                  test
                                                  context
                                                  tests)
                                :else (throw (Exception. (str "Invalid form at test-strings state: " (prn-str head)))))

            :test-checks (cond (fn? head) (recur :test-checks more
                                                 (add-check test head)
                                                 context
                                                 tests)
                               (string? head) (recur :test-strings forms
                                                     (corpus-test resource)
                                                     context
                                                     (conj tests test))
                               :else (throw (Exception. (str "Invalid form at test-checks stats:" (prn-str head))))))
          (Corpus. context (conj tests test))))
      (apply [:init forms [] nil []])))

(defmacro this-ns "Total hack to get ns of this file at compile time" [] *ns*)

(defn read-corpus
  "Read a list of symbol and return a Corpus map {:context {}, :tests []}"
  [corpus-resource]
  (let [symbols (edn/read-string (slurp corpus-resource))]
    (corpus corpus-resource (map #(binding [*ns* (this-ns)] (eval %)) symbols))))

(comment
  (def corpus-file "/Users/joachim/workspace/duckling/resources/languages/en/corpus/finance.clj")
  (def symbols (edn/read-string (slurp corpus-file)))
  )
