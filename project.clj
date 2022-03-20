(defproject logos "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [compojure "1.6.1"]
                 [http-kit "2.3.0"]
                 [reagent "1.1.0"]
                 [re-frame "1.2.0"]
                 [ring-cors "0.1.13"]
                 [ring/ring-defaults "0.3.2"]]
  :source-paths ["src/clj" "src/cljs"]
  :test-paths ["test/"]
  :resource-paths ["resources"]
  :main ^:skip-aot logos.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
