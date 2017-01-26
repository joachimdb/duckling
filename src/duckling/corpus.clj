(ns duckling.corpus
  (:use     [clojure.tools.logging]
            [clojure.edn :as edn]
            [plumbing.core :except [millis]])
  (:require [duckling.time.obj :as time]
            [duckling.util :as util]))
;; (remove-ns 'duckling.corpus)
; Checker functions return *nil* when OK, or [expected actual] when not OK

(defn- vec->date-and-map
  "Turns a vector of args into a date and a map of extra fields"
  [args]
  (let [[date-fields other-keys-and-values] (split-with integer? args)
        token-fields (into {} (map vec (partition 2 other-keys-and-values)))
        date (-> (apply time/t -2 date-fields)
                 (?> (:grain token-fields) (assoc :grain (:grain token-fields)))
                 (?> (:timezone token-fields) (assoc :timezone (:timezone token-fields))))]
    [date token-fields]))

(defn datetime
  "Creates a datetime checker function to check if the token is valid"
  [& args]
  (let [[date token-fields] (vec->date-and-map args)]
    (fn [context token]
        (when-not
          (and
            (= :time (:dim token))
            (util/hash-match (select-keys token-fields [:direction :precision])
                             token)
            (= (-> token :value) date))
          [date (:value token)]))))

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
  (fn [_ token] (when-not
                  (and
                    (= :number (:dim token))
                    (or (not (integer? value)) (:integer token))
                    (= (:value token) value))
                  [value (:value token)])))

(defn ordinal
  [value]
  (fn [_ token] (when-not
                  (and
                    (= :ordinal (:dim token))
                    (= (:value token) value))
                  [value (:value token)])))

(defn temperature
  "Create a temp condition"
  [value' & [unit' precision']]
  (fn [_ {:keys [dim value unit precision] :as token}]
    (not (and
                  (= :temperature dim)
                  (= value' value)
                  (= unit' unit)
                  (= precision' precision)))))

(defn distance
  "Create a distance condition"
  [value' & [unit' normalized' precision']]
  (fn [_ {:keys [dim value unit normalized precision] :as token}]
    (not (and
                  (= :distance dim)
                  (= value' value)
                  (= unit' unit)
                  (= normalized' normalized)
                  (= precision' precision)))))

(defn money
  "Create a amount-of-money condition"
  [value' & [unit' precision']]
  (fn [_ {:keys [dim value unit precision] :as token}]
    (not (and
                  (= :amount-of-money dim)
                  (= value' value)
                  (= unit' unit)
                  (= precision' precision)))))

(defn place
  "Create a place checker"
  [pnl n]
  (fn [token context] (and
                        (= :pnl (:dim token))
                        (= n (:n token))
                        (= pnl (:pnl token)))))

(defn metric
  "Create a metric checker"
  [cat val]
  (fn [token context] (and
                        (= :unit (:dim token))
                        (= val (:val token))
                        (= cat (:cat token)))))

(defn quantity
  "Create a quantity condition"
  [value unit & [product]]
  (fn [token _] (and
                  (= :quantity (:dim token))
                  (= value (-> token :value :value))
                  (= unit (-> token :value :unit))
                  (= product (-> token :value :product)))))

(defn volume
  "Create a volume condition"
  [value unit & [normalized]]
  (fn [token _] (and
                  (= :volume (:dim token))
                  (= value (-> token :value :value))
                  (= unit  (-> token :value :unit))
                  (= normalized (-> token :value :normalized)))))


(defn integer
  "Return a func (duckling pattern) checking that dim=number and integer=true,
  optional range (inclusive), and additional preds"
  [& [min max & predicates]]
  (fn [token]
    (and (= :number (:dim token))
         (:integer token)
         (or (nil? min) (<= min (:value token)))
         (or (nil? max) (<= (:value token) max))
         (every? #(% token) predicates))))

(defrecord CorpusTest [text checks])
(defn- corpus-test []
  (CorpusTest. #{} []))
(defn- add-text [^CorpusTest c text]
  (update-in c [:text] conj text))
(defn- add-check [^CorpusTest c check]
  (update-in c [:checks] conj check))

(defrecord Corpus [context tests])

(comment

  (def t [{:text []}]) ;; init
  (def t (assoc-in t [(dec (count t)) :text (count (:text (peek t)))] (count (:text (first t)))))

  (update-in ())

  (update-in [0 1 2] [0] #(+ 5 %))
  (update-in [0 1 2] [1] #(+ 5 %))
  )

(defn corpus
  "Parse corpus" ;; TODO should be able to load several files, like rules
  [forms]
  (-> (fn [state [head & more :as forms] test context tests]
        ;; (println state head test (count tests))
        (if head
          (case state
            :init (cond (map? head) (recur :test-strings more
                                           (corpus-test)
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
                                                     (corpus-test)
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
    (corpus (map #(binding [*ns* (this-ns)] (eval %)) symbols))))

(comment
  (def corpus-file "/Users/joachim/workspace/duckling/resources/languages/en/corpus/finance.clj")
  (def symbols (edn/read-string (slurp corpus-file)))
  )
