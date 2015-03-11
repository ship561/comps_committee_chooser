(defproject comps_committee_chooser "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :jvm-opts ["-Xmx2g"]
  
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [mlabs.jars/clojure-contrib "1.2.0-mlab"]
                 [clojure-csv/clojure-csv "2.0.1"]

                 [enlive "1.1.5"];webscraper
                 [itsy "0.1.1"];webcrawler
                 ;;for net-eval before jaring it
                 [org.clojure/tools.nrepl "0.2.2"]
                 [slingshot "0.10.3"]])
