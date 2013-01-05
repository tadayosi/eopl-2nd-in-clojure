(defproject eopl-2nd-in-clojure "0.1.0-SNAPSHOT"
  :description "Essentials of Programming Languages exercised in Clojure + ANTLR"
  :url "https://github.com/tadayosi/eopl-2nd-in-clojure"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.antlr/antlr "3.4"]]
  :plugins [[org.clojars.rferraz/lein-antlr "0.2.0"]]
  :prep-tasks ["antlr" "javac" "compile"]
  :aot :all
  :java-source-paths ["target/gen-src"]
  :antlr-src-dir "src"
  :antlr-dest-dir "target/gen-src"
  )
