(defproject bow "0.1.1"
  :description "Functions supporting bag-of-words"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :lein-release {:deploy-via :clojars}
  :plugins [[lein-release "1.0.9"]]
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [pppmap "0.1.0"]

                 ]
  :repl-options {:init-ns bow.core})
