(ns integrativo-xlsowl.core
  (:require [clojure.java.io :as io])
  (:require [dk.ative.docjure.spreadsheet :as xls])
  (:require [clojure.string :as str])
  (:require [clojure.math.combinatorics :as combo])
  (:require [cprop.core :refer [load-config]]))

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
  (map #(merge {:hash %} %) 
       (map (partial merge literal-expandmap) data-list)))

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

    (-> final-data       
        (subvec 
         (get-in conf [:final-data :begin-line] 7) 
         (inc (get-in conf [:final-data :end-line] 52))
         )
        add-literals
        split-data-values
        combine-data-values
)))

(defn param-pattern [param-name] (re-pattern (str "\\$" param-name "\\$")))

(def workbook 
  (delay (xls/load-workbook 
          (get conf :xls-file "resources/Data_and_Generatorv13.xlsm"))))

(def model 
  (delay (let [all (->> (xls/select-sheet 
                         (get-in conf [:owl-elements :tab-name] "owl-elements2") @workbook)
                        (xls/select-columns {:B :owl-axiom}))
               body-map (subvec all 
                                (get-in conf [:owl-elements :body :begin-line] 29) 
                                (inc (get-in conf [:owl-elements :body :end-line] 107)))
               head-map (subvec all 
                                (get-in conf [:owl-elements :heading :begin-line] 1)
                                (inc (get-in conf [:owl-elements :heading :end-line] 29)))
               head (map :owl-axiom head-map)]

           {:body-map body-map
            :head head })))


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

<<<<<<< Updated upstream
(defn expand-model [workbook datamap-result]
  (let [modelmap-all (->> (xls/select-sheet 
                           (get-in conf [:owl-elements :tab-name] "owl-elements2") workbook)
                           (xls/select-columns {:B :owl-axiom}))
        modelmap-body (subvec modelmap-all 
                              (get-in conf [:owl-elements :body :begin-line] 29) 
                              (inc (get-in conf [:owl-elements :body :end-line] 107)))
        modelmap-heading (subvec modelmap-all 
                              (get-in conf [:owl-elements :heading :begin-line] 1)
                              (inc (get-in conf [:owl-elements :heading :end-line] 29)))
        model-heading (map :owl-axiom modelmap-heading)]

    (cons [(str/join "\n" model-heading)
           "\n\n<!--==== END OF HEADING ====-->\n"] 
          (for [datamap-list datamap-result
                datamap-item datamap-list
                modelmap modelmap-body
                :let [result []]]
            
            (->> (reduce merge datamap-item)
                 (expand-model-item modelmap)
                 (conj result))))))

(defn ppmap
  "Partitioned pmap, for grouping map ops together to make parallel
  overhead worthwhile"
  [grain-size f & colls]
  (apply concat
   (apply pmap
          (fn [& pgroups] (doall (apply map f pgroups)))
          (map (partial partition-all grain-size) colls))))

(defn write-result! [result]
=======
(defn expand-model [workbook datamap-list]
  (for [datamap-item datamap-list
        modelmap (:body-map @model)
        :let [result []]]
    
    (->> (reduce merge datamap-item)
         (expand-model-item modelmap)
         (conj result)))
  )

(defn write-result! [data]
  (println (str "Writing results (" (count data) ")"))
  (with-open [wtr (clojure.java.io/writer "result.owl")]
    (binding [*out* wtr]
      (apply println (:head @model))
      (println "\n\n<!--==== END OF HEADING ====-->\n")

      (let [data-parts (partition-all 4 data)]
        (doall (pmap #(doall 
                       (for [data-part %
                             data-list data-part]
                         (apply println data-list))) 
                data-parts)) 
        (println "\n</Ontology>")))))

(def hashes-read (atom #{}))

(defn remove-duplicates [result]
  (remove nil?
          (map (fn [x] 
                 (if (contains? @hashes-read x) 
                   nil
                   (do (swap! hashes-read conj x) x))) result)) 
  )

(defn -main []
  
  (time (dorun (let [combined-data (combine-final-data @workbook)
                     expanded-data (map #(expand-model @workbook %) combined-data)]
                
                 
                 ;; (doall (pmap #(doall (map (fn [x] (println (str (Thread/currentThread) (count x)))) %)) expanded-data-parts))
                 
;                 (cons [(str/join "\n" model-heading) 
              

                 (write-result! expanded-data)
                 ;; (println "reading, combining and expanding data...")

;                 (println (count (first (concat expanded-data-parts))))
                 (println "finish.")
                 ))))
