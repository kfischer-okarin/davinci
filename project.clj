(defproject davinci "0.1.0-SNAPSHOT"
  :description "A text editor with extensability at its foundation"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [environ "1.2.0"]
                 [com.googlecode.lanterna/lanterna "3.0.4"]]
  :main ^:skip-aot davinci.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}
             :dev {:env {:davinci-path "."}
                   :dependencies [[org.clojure/test.check "1.1.0"]]}
             :test {:dependencies [[pjstadig/humane-test-output "0.10.0"]]
                    :injections [(require 'pjstadig.humane-test-output)
                                 (pjstadig.humane-test-output/activate!)]}}
  :plugins [[lein-cloverage "1.1.2"]
            [lein-environ "1.2.0"]])
