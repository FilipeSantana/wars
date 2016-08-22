(ns integrativo-xlsowl.core
  (:require [clojure.java.io :as io])
  (:require [dk.ative.docjure.spreadsheet :as xls])
  (:require [clojure.string :as str])
  (:require [clojure.math.combinatorics :as combo])
  (:require [cprop.core :refer [load-config]])
  (:require [clojure.core.async :as async :refer [>! <! >!! <!! chan go go-loop]])
  (:require [plumbing.map :refer [keyword-map]]))

(def conf (load-config))

(def expandmap
  {:protein-name ["Protein"]
   :gene-name ["Gene"]
   :organism ["Organism" "Org"]
   :biological-process ["BioProcess" "BiologicalProcess"]
   :molecular-function ["MolecularFunction" "MolFunc"]
   :cell-component ["CellComponent" "Comp"]
   :phenotype ["Situation"]
   :molecule ["Molecule"]})

(def literal-expandmap
  {:molecule "Homocysteine"})

(defn add-literals [data-list]
  (map (partial merge literal-expandmap) data-list))

(defn split-data-values [data-list]
  (map (fn [data-item]
         (for [entry-key (keys data-item)
               :let [splited-value (str/split (str (entry-key data-item)) #";")]]
           (map (partial hash-map entry-key) splited-value)))
       data-list))

(defn combine-data-values [data-list]
  (map (fn [data-item]
         (apply combo/cartesian-product data-item))
       data-list))

(defn combine-final-data [workbook]
  (let [final-data (->> (xls/select-sheet
                         (get-in conf [:final-data :tab-name] "final-data")
                         workbook)
                        (xls/select-columns {:C :protein-name
                                             :D :gene-name
                                             :E :organism
                                             :F :biological-process
                                             :G :molecular-function
                                             :H :cell-component
                                             :M :phenotype}))]

    (let [begin-final-data (get-in conf [:final-data :begin-line] 7)
          end-final-data (inc (get-in conf [:final-data :end-line] 52))]
      (-> final-data
          (subvec begin-final-data end-final-data)
          (add-literals)
          (split-data-values)
          (combine-data-values)
          ))))

(defn param-pattern [param-name] (re-pattern (str "\\$" param-name "\\$")))

(defn expand-model-item [modelmap datamap]
  (reduce (fn [axiom-str datamap-key]
            (let [param-names (datamap-key expandmap)
                  param-value (datamap-key datamap)]

              (if (and (or (= "" param-value) (nil? param-value))
                       (some #(re-find (param-pattern %) axiom-str) param-names))

                "" ;; ignoring axiom with empty param

                (reduce (fn [axiom-str param-name]
                          (str/replace axiom-str
                                       (param-pattern param-name)
                                       (str/replace param-value #"[^\w\d]" "_")))
                        axiom-str
                        param-names))))

          (:owl-axiom modelmap)
          (keys datamap)))


(def xls-data
  (let  [workbook (xls/load-workbook (get conf :xls-file "resources/Data_and_Generatorv13.xlsm"))

         modelmap-all (->> (xls/select-sheet
                            (get-in conf [:owl-elements :tab-name] "owl-elements2") workbook)
                           (xls/select-columns {:B :owl-axiom}))
         modelmap-body (subvec modelmap-all
                               (get-in conf [:owl-elements :body :begin-line] 29)
                               (inc (get-in conf [:owl-elements :body :end-line] 107)))
         modelmap-heading (subvec modelmap-all
                                  (get-in conf [:owl-elements :heading :begin-line] 1)
                                  (inc (get-in conf [:owl-elements :heading :end-line] 29)))
         model-heading (map :owl-axiom modelmap-heading)]
    (keyword-map workbook modelmap-body model-heading)))


(def ch-final-data (chan 10))
(def ch-datamap-expanded (chan 10))

(defn -main []
  (go (let [final-data-combined (combine-final-data (:workbook xls-data))]
        (doseq [sublist1 final-data-combined]
          (doseq [sublist2 sublist1]
            (>! ch-final-data (reduce merge sublist2)))))
      (async/close! ch-final-data))


  (go-loop []
    (if-let [final-data (<! ch-final-data)]
      (do
        (doseq [modelmap-body (:modelmap-body xls-data)]
          (>! ch-datamap-expanded (expand-model-item modelmap-body final-data)))
        (recur))
      (async/close! ch-datamap-expanded)))



  (time (do (with-open [wtr (clojure.java.io/writer "result.owl")]
              (binding [*out* wtr]
                (apply println (:model-heading xls-data))
                (println)
                (println "\n\n<!--==== END OF HEADING ====-->\n")

                (loop [item-hashes #{}]
                  (when-let [datamap-expanded (<!! ch-datamap-expanded)]
                    (let [item-hash (hash datamap-expanded)]
                      (if (contains? item-hashes item-hash)
                        (recur item-hashes)
                        (do
                          (println datamap-expanded)
                          (recur (conj item-hashes item-hash)))))))
                (println "\n</Ontology>")))
            (println))))
