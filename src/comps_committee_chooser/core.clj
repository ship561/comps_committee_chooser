(ns comps-committee-chooser.core
  (:require [clojure.string :as str]
            [net.cgrand.enlive-html :as html]
            [clojure.set :as sets])
  (:use [itsy.core]))

(def ^:dynamic *base-url* "http://www.bc.edu/content/bc/schools/cas/biology/facadmin/annunziato.html")

(def ^:dynamic *story-selector*
     [[:div.text
       (html/but :.advertisement)
       (html/but :.autosStory)
       (html/but :.adCreative)]])

(defn fetch-url [url]
  (html/html-resource (java.net.URL. url)))

(defn get-href [entry]
  (get-in entry [:attrs :href]))

(def faculty-urls
  (let [base-url "http://www.bc.edu"
        url (str base-url
                 "/content/bc/schools/cas/biology/facadmin.html#faculty")]
   (->> (html/select (fetch-url url) [:td :a])
        (map get-href )
        (filter #(re-find #"facadmin" %))
        (map #(str base-url %)))))

(defn get-publications
  "get list of publication urls from the bc faculty website"
  
  [url]
  (->> (html/select (fetch-url url) [:div.text :a])
       (map get-href )
       (filter #(re-find #"nih" %) )))

(defn get-abstract
  "extract the abstract from the pubmed url"

  [url]
  (->> (html/select (fetch-url url) [:div.abstr :p])
       (map html/text )
       first))

(defn kullback-leibler
  
  [P Q]
  (reduce (fn [sum i]
            (let [pi (get P i 0)
                  qi (get Q i 0)]
              (+ sum
                 (if (or (zero? pi)(zero? qi))
                   0.0
                   (* pi (Math/log (/ pi qi)))))))
          0.0 (clojure.set/union (set (keys P)) (set (keys Q)))))

(defn kld [& args] (apply kullback-leibler args))

(defn jensen-shannon [P Q]
  (let [M (reduce (fn [M [k sum]]
                    (assoc M k (/ sum 2)))
                  {} (merge-with + P Q))
        Omega (clojure.set/union (set (keys P)) (set (keys Q)))]
    (* 0.5 (+ (kld P M) (kld Q M)))))
(defn jsd [& args] (apply jensen-shannon args))

(defn word-freqs [s]
  (-> (str/lower-case s)
      (str/replace #"[^\w\s]" "")
      (str/split #" ")
      frequencies))

(defn word-probs

  [s]
  (let [wcount (word-freqs s) 
        nwords (reduce + (vals wcount))]
    (reduce (fn [M [word cnt]]
              (assoc M word (/ cnt nwords)))
            {} wcount)))

(defn most-freq-word [m]
  (reduce (fn [max-entry cur]
            (let [[w c] max-entry
                  cur-cnt (second cur)]
              (if (> cur-cnt c)
                cur
                max-entry)))
          ["" 0] m))

(defn term-freq [word word-map]
  (+ 0.5
     (/ (* 0.5 (word-map word))
        (second (most-freq-word word-map)))))

(defn idf [word word-maps]
  (Math/log
   (/ (count word-maps)
      (->> (map #(contains? % word) word-maps)
           (filter true?)
           count))))

(defn docs->idf [word-maps]
  (let [M (apply merge-with + word-maps)]
    (reduce (fn [m word]
              (assoc m word (idf word word-maps)))
            {} (keys M))))

(defn docs->tf [word-maps]
  (let [M (apply merge-with + word-maps)]
    (reduce (fn [m word]
              (assoc m word (term-freq word M)))
            {} (keys M))))

(defn tfidf [tf idf]
  (reduce (fn [m w]
            (assoc m w (* (tf w) (get idf w 0))))
          {} (keys tf)))

(defn document-sim [m1 m2]
  (let [S (sets/union (set (keys m1)) (set (keys m2)))
        dot-prod (reduce + (map #(* (get m1 % 0) (get m2 % 0)) S))
        norm (fn [v] (Math/sqrt (reduce + (map #(* % %) (vals v)))))]
    (/ dot-prod
       (* (norm m1) (norm m2)))))

(def foo (->> (take 3 faculty-urls)
              (map get-publications)
              (remove empty?)
              doall
              (reduce (fn [V x]
                        (conj V (->> (map get-abstract x)
                                     (remove nil?)
                                     (map word-probs))))
                      [])))

(def foo (let [dict (->> (take 3 faculty-urls)
                          (map get-publications)
                          (remove empty?)
                          doall
                          (reduce (fn [V x]
                                    (conj V (->> (map get-abstract x)
                                                 (remove nil?)
                                                 (map word-freqs))))
                                  []))]
           (doall (map #(vector (docs->tf %) (docs->idf %)) dict))))



(def example-interest
  "The nuclear DNA of eukaryotes is organized with structural and regulatory proteins to form the nucleoprotein complex termed chromatin. The primary functional unit of chromatin is the nucleosome, a particle containing histone proteins and approximately 200 base pairs of DNA. Research in my laboratory is directed toward understanding the processes involved in nucleosome assembly during DNA replication. Just as the DNA in dividing cells must be replicated once each cell cycle, so too must sufficient histones (and other chromatin proteins) be synthesized to assemble nucleosomes on the newly replicated DNA. The proper assembly of chromatin during cell division is of vital importance, because the presence or absence of nucleosomes (and the precise positioning of nucleosomes with respect to DNA sequences) can determine which genes are transcribed, and when. To make our results as relevant as possible to human cell biology, our experiments are performed using HeLa cells, a transformed human cell line maintained in spinner culture. The faithful transmission and assembly of chromatin requires that many independent cellular processes be coordinated. As DNA is being replicated, histones are synthesized, then modified by enzymatic acetylation, transported to the nucleus, and assembled into nucleosomes. Moreover, supercoiled chromatin higher-order structures must first unwind to allow access to the DNA, and then condense again after replication is completed. In my laboratory the specific questions currently being investigated include: the modification status of parental histones that are segregated to progeny chromosomes, and the mechanisms of histone deposition onto newly replicated DNA; the involvement of histone acetylation in nucleosome assembly, and the properties of enzymes (acetyltransferases) involved; the role of histone phosphorylation in regulating chromatin folding; and the isolation and characterization of somatic nucleosome \"assembly factors\" to define the in vivo assembly pathway. In order to address these questions we use a number of approaches, including DNA replication systems (in vivo and in vitro), histone acetylation assays, in vitro assembly reactions using purified components, and DNA supercoiling studies. We also take advantage of antibodies directed against specific histone and non-histone chromatin proteins, to purify and analyze newly replicated nucleosomes and their assembly intermediates. Our aims are to identify major cellular components needed to generate nucleosomes in vivo, and to characterize the stages of chromatin biosynthesis. Ultimately, these studies should provide a better understanding of the regulation of chromatin organization during DNA replication, and of the processes involved in the faithful assembly of transcriptionally active and inactive chromatin structures.")
