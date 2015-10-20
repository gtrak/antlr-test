(ns antlr-test.core
  (:require [clojure.java.io :as io]
            [clj-antlr.core :as antlr]
            [zip.visit :as visit]
            [clojure.zip :as zip]
            [clojure.core.match :refer [match]]))

(def grammar (slurp (io/resource "antlr_test/json.g4")))

(def parser (antlr/parser grammar {:throw? false}))

(def sample-json
  "{
    \"glossary\": {
        \"title\": \"example glossary\",
		\"GlossDiv\": {
            \"title\": \"S\",
			\"GlossList\": {
                \"GlossEntry\": {
                    \"ID\": \"SGML\",
					\"SortAs\": \"SGML\",
					\"GlossTerm\": \"Standard Generalized Markup Language\",
					\"Acronym\": \"SGML\",
					\"Abbrev\": \"ISO 8879:1986\",
					\"GlossDef\": {
                        \"para\": \"A meta-markup language, used to create markup languages such as DocBook.\",
						\"GlossSeeAlso\": [\"GML\", \"XML\"]
                    },
					\"GlossSee\": \"markup\"
                }
            }
        }
    }
}")

(def parse-tree (antlr/parse parser sample-json))

(visit/defvisitor vecs-visitor :post [node state]
  (if (and (sequential? node) (not (vector? node)))
    {:node (vec node)}))

(defn unescape
  [val]
  (if (string? val)
    (subs val 1 (dec (.length ^String val)))
    val))

(defn match-member
  [node]
  (match [node]
         [[:member k ":" v]] [(unescape k) (unescape v)]
         :else nil))

(defn match-ast
  [node]
  (match [node]
         [[:jsonString s]] s
         [[:jsonValue s]] s
         ;; [:member "\"GlossSee\"" ":" "\"markup\""]
         [[:jsonObject "{" & members]] (into {} (keep match-member (butlast members)))
         [[:jsonArray "[" & members]] (->> (butlast members)
                                           ;; skip commas
                                           (partition-all 2)
                                           (keep (comp unescape first))
                                           (into []))
         [[:jsonText val]] val
         :else node))

(visit/defvisitor ast-visitor :post
  [node state]
  (if-let [node (match-ast node)]
    {:node node}))

(def cleaned (:node (visit/visit (zip/seq-zip parse-tree) nil [vecs-visitor ast-visitor])))
;;; This is the value of 'cleaned' in EDN, clojure's native data format
(comment
  {"glossary" {"title" "example glossary",
               "GlossDiv" {"title" "S",
                           "GlossList" {"GlossEntry" {"ID" "SGML",
                                                      "SortAs" "SGML",
                                                      "GlossTerm" "Standard Generalized Markup Language",
                                                      "Acronym" "SGML",
                                                      "Abbrev" "ISO 8879:1986",
                                                      "GlossDef" {"para" "A meta-markup language, used to create markup languages such as DocBook.",
                                                                  "GlossSeeAlso" ["GML" "XML"]},
                                                      "GlossSee" "markup"}}}}}
  )



