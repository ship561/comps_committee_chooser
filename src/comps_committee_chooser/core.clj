(ns comps-committee-chooser.core
  (:require [clojure.string :as str]
            [net.cgrand.enlive-html :as html]
            [clojure.set :as sets]
            [itsy.core :as itsy]
            [opennlp.nlp :as nlp]))

(def ^:dynamic *base-url* "http://www.bc.edu/content/bc/schools/cas/biology/facadmin/annunziato.html")

(def ^:dynamic *story-selector*
     [[:div.text
       (html/but :.advertisement)
       (html/but :.autosStory)
       (html/but :.adCreative)]])

(def get-sentences (nlp/make-sentence-detector "resources/en-sent.bin"))
(def tokenize (nlp/make-tokenizer "resources/en-token.bin"))

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

(defn get-publication-urls
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

(defn get-full-text-url [url]
  (let [pub-links (-> url fetch-url
                      (html/select [:div.icons.portlet :a]))
        free (filter (fn [m] (get-in m [:attrs :free_status])) pub-links)]
    (-> (first free)
        get-href)))

(defn get-full-text [url]
  (as-> (fetch-url url) x
    (html/select x [:div.article.fulltext-view #{:p :h2}])
    (map html/text x)
    (take-while (complement #(re-find #"(?i)^acknowledgments$" %)) x)));take until acknowledgements

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

(defn word-freqs
  "Takes a document D and returns the word frequencies as a hash-map"

  [D]
  (->> (tokenize D)
       (filter #(re-find #"[a-zA-Z]+" %))
       (map str/lower-case)
       frequencies))

(defn word-probs

  [s]
  (let [wcount (word-freqs s) 
        nwords (reduce + (vals wcount))]
    (reduce (fn [M [word cnt]]
              (assoc M word (/ cnt nwords)))
            {} wcount)))

(defn most-freq-word
  "Takes a word map and returns the most frequent word and count in
  the document."

  [m]
  (reduce (fn [max-entry cur]
            (let [c (second max-entry)
                  cur-cnt (second cur)]
              (if (> cur-cnt c)
                cur
                max-entry)))
          m))

(defn calc-tf [word word-map]
  (/ (word-map word)
     (count word-map))
  #_(+ 0.5
     (/ (* 0.5 (word-map word))
        (second (most-freq-word word-map)))))

(defn calc-idf [word word-maps]
  (Math/log10
   (/ (count word-maps)
      (inc (count (filter #(contains? % word) word-maps))))));count docs containing word 

(defn docs->tfidf
  "Takes a set of documets to form a corpus. Calculates the TF-IDF for each document."

  [docs]
  (let [wfreqs (map word-freqs docs)
        tfidf (fn [doc]
                (reduce (fn [M w]
                          (->> (* (calc-idf w wfreqs) (calc-tf w doc))
                               (assoc M w)))
                        {} (keys doc)))]
    (map tfidf wfreqs)))

(defn important-words [n tfidf-maps]
  (map (fn [M]
         (->> (seq M)
              (sort (fn [x y]
                      "count then name"
                      (or (> (second x) (second y))
                          (and (= (second x) (second y))
                               (neg? (compare (first x) (first y)))))))
              (take n)))
       tfidf-maps))

(defn document-sim [m1 m2]
  (let [S (sets/union (set (keys m1)) (set (keys m2)))
        dot-prod (reduce + (map #(* (get m1 % 0) (get m2 % 0)) S))
        norm (fn [v] (Math/sqrt (reduce + (map #(* % %) (vals v)))))]
    (/ dot-prod
       (* (norm m1) (norm m2)))))

(def foo (->> (take 3 faculty-urls)
              (map get-publication-urls)
              (remove empty?)
              doall
              (reduce (fn [V x]
                        (conj V (->> (map get-abstract x)
                                     (remove nil?)
                                     (map word-probs))))
                      [])))

(def foo (let [dict (->> (take 3 faculty-urls)
                          (map get-publications-urls)
                          (remove empty?)
                          doall
                          (reduce (fn [V x]
                                    (conj V (->> (map get-abstract x)
                                                 (remove nil?)
                                                 (map word-freqs))))
                                  []))]
           (doall (map #(vector (docs->tf %) (docs->idf %)) dict))))



(def example-interest1
  "The nuclear DNA of eukaryotes is organized with structural and regulatory proteins to form the nucleoprotein complex termed chromatin. The primary functional unit of chromatin is the nucleosome, a particle containing histone proteins and approximately 200 base pairs of DNA. Research in my laboratory is directed toward understanding the processes involved in nucleosome assembly during DNA replication. Just as the DNA in dividing cells must be replicated once each cell cycle, so too must sufficient histones (and other chromatin proteins) be synthesized to assemble nucleosomes on the newly replicated DNA. The proper assembly of chromatin during cell division is of vital importance, because the presence or absence of nucleosomes (and the precise positioning of nucleosomes with respect to DNA sequences) can determine which genes are transcribed, and when. To make our results as relevant as possible to human cell biology, our experiments are performed using HeLa cells, a transformed human cell line maintained in spinner culture. The faithful transmission and assembly of chromatin requires that many independent cellular processes be coordinated. As DNA is being replicated, histones are synthesized, then modified by enzymatic acetylation, transported to the nucleus, and assembled into nucleosomes. Moreover, supercoiled chromatin higher-order structures must first unwind to allow access to the DNA, and then condense again after replication is completed. In my laboratory the specific questions currently being investigated include: the modification status of parental histones that are segregated to progeny chromosomes, and the mechanisms of histone deposition onto newly replicated DNA; the involvement of histone acetylation in nucleosome assembly, and the properties of enzymes (acetyltransferases) involved; the role of histone phosphorylation in regulating chromatin folding; and the isolation and characterization of somatic nucleosome \"assembly factors\" to define the in vivo assembly pathway. In order to address these questions we use a number of approaches, including DNA replication systems (in vivo and in vitro), histone acetylation assays, in vitro assembly reactions using purified components, and DNA supercoiling studies. We also take advantage of antibodies directed against specific histone and non-histone chromatin proteins, to purify and analyze newly replicated nucleosomes and their assembly intermediates. Our aims are to identify major cellular components needed to generate nucleosomes in vivo, and to characterize the stages of chromatin biosynthesis. Ultimately, these studies should provide a better understanding of the regulation of chromatin organization during DNA replication, and of the processes involved in the faithful assembly of transcriptionally active and inactive chromatin structures.")

(def document1
  "Python is a 2000 made-for-TV horror movie directed by Richard
  Clabaugh. The film features several cult favorite actors, including
  William Zabka of The Karate Kid fame, Wil Wheaton, Casper Van Dien,
  Jenny McCarthy, Keith Coogan, Robert Englund (best known for his
  role as Freddy Krueger in the A Nightmare on Elm Street series of
  films), Dana Barron, David Bowe, and Sean Whalen. The film concerns
  a genetically engineered snake, a python, that escapes and unleashes
  itself on a small town. It includes the classic final girl scenario
  evident in films like Friday the 13th. It was filmed in Los Angeles,
  California and Malibu, California. Python was followed by two
  sequels: Python II (2002) and Boa vs. Python (2004), both also
  made-for-TV films.")

(def document2
  "Python, from the Greek word (πύθων/πύθωνας), is a genus of
  nonvenomous pythons[2] found in Africa and Asia. Currently, 7 species are
  recognised.[2] A member of this genus, P. reticulatus, is among the longest
  snakes known.")

(def document3 
  "The Colt Python is a .357 Magnum caliber revolver
  formerly manufactured by Colt's Manufacturing Company of Hartford,
  Connecticut.  It is sometimes referred to as a Combat Magnum.[1] It
  was first introduced in 1955, the same year as Smith &amp; Wesson's
  M29 .44 Magnum. The now discontinued Colt Python targeted the
  premium revolver market segment. Some firearm collectors and writers
  such as Jeff Cooper, Ian V. Hogg, Chuck Hawks, Leroy Thompson, Renee
  Smeets and Martin Dougherty have described the Python as the finest
  production revolver ever made.")


