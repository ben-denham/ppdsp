(ns ppdsp.dataset.moa
  (:import [ppdsp.dataset.base StaticDataset]
           [moa.streams.generators RandomTreeGenerator]))

;; Dataset schema

(defn- attribute->schema-entry
  "Convert a MOA Attribute into an entry for a dataset schema."
  [attribute]
  (when (not (or (.isNominal attribute)
                 (.isNumeric attribute)))
    (throw (Exception. "Only nominal and numeric attributes are supported.")))
  {:name (.name attribute)
   :options (if (.isNumeric attribute)
              :numeric
              (->> (.getAttributeValues attribute)
                   (into [])))})

(defn- header->schema
  "Convert a MOA Header into a dataset schema."
  [header]
  (let [feature-count (.numInputAttributes header)
        class-count (.numOutputAttributes header)
        feature-attributes (->> (range feature-count)
                                (map #(.inputAttribute header %))
                                (map attribute->schema-entry)
                                (into []))
        class-attribute (->> (.outputAttribute header 0)
                             (attribute->schema-entry))]
    (when (not= class-count 1)
      (throw (Exception. "Only single-class datasets supported.")))
    ;; The class attribute must be last, hence we make
    ;; feature-attributes a vector above.
    (conj feature-attributes class-attribute)))

;; Dataset instances

(defn- instance-attribute->value
  "Returns a dataset-friendly value for a given MOA Attribute in a given
  MOA Instance."
  [instance attribute]
  (let [raw-value (.value instance attribute)]
    (cond
      (.isNumeric attribute) raw-value
      ;; For easy MOA instance construction, nominal record
      ;; values are numeric indexes into the options for the
      ;; column.
      (.isNominal attribute) (int raw-value)
      :else (throw (Exception. "Only nominal and numeric attributes are supported.")))))

(defn- instance->record-values
  "Convert a MOA InstanceExample into a seq of record values."
  [instance-example]
  (let [instance (.getData instance-example)
        feature-count (.numInputAttributes instance)
        class-count (.numOutputAttributes instance)
        feature-values (->> (range feature-count)
                            (map #(.inputAttribute instance %))
                            (map #(instance-attribute->value instance %))
                            (into []))
        class-value (->> (.classAttribute instance)
                         (instance-attribute->value instance))]
    (when (not= class-count 1)
      (throw (Exception. "Only single-class datasets supported.")))
    ;; The class attribute must be last, hence we make
    ;; feature-attributes a vector above.
    (conj feature-values class-value)))

(defn- stream->record-lazyseq
  "Convert a moa InstanceStream into a lazy sequence of records."
  ([stream]
   (stream->record-lazyseq stream 1))
  ([stream next-id]
   (let [record {:id next-id
                 :values (-> (.nextInstance stream)
                             (instance->record-values))}]
     (lazy-seq (cons record (stream->record-lazyseq stream (inc next-id)))))))

;; Datasets

(defn make-moa-random-tree-dataset
  "Create a new dataset based on a MOA random tree generator using the
  provided configuration options."
  ([& {:keys [tree-random-seed num-features max-tree-depth
              first-leaf-level leaf-fraction class-count
              feature-options max-records instance-random-seed]}]
   (let [instance-random-seed (or instance-random-seed tree-random-seed)
         tree-stream (doto (RandomTreeGenerator.)
                       ;; Set the random seeds.
                       (-> (.treeRandomSeedOption)
                           (.setValue tree-random-seed))
                       (-> (.instanceRandomSeedOption)
                           (.setValue instance-random-seed))
                       ;; Configurable number of nominal features.
                       (-> (.numNominalsOption)
                           (.setValue num-features))
                       ;; No numeric features.
                       (-> (.numNumericsOption)
                           (.setValue 0))
                       ;; Options per feature.
                       (-> (.numValsPerNominalOption)
                           (.setValue feature-options))
                       ;; Class values.
                       (-> (.numClassesOption)
                           (.setValue class-count))
                       ;; Set maximum tree depth.
                       (-> (.maxTreeDepthOption)
                           (.setValue max-tree-depth))
                       ;; Only start creating leafs at a certain
                       ;; depth.
                       (-> (.firstLeafLevelOption)
                           (.setValue first-leaf-level))
                       ;; Set the "leafiness" of the tree.
                       (-> (.leafFractionOption)
                           (.setValue leaf-fraction))
                       (.prepareForUse))]
     (StaticDataset. (header->schema (.getHeader tree-stream))
                     (take max-records (stream->record-lazyseq tree-stream))))))

(defn make-moa-numeric-random-tree-dataset
  "Create a new dataset based on a MOA random tree generator using the
  provided configuration options."
  ([& {:keys [tree-random-seed num-features max-tree-depth
              first-leaf-level leaf-fraction class-count
              max-records instance-random-seed]}]
   (let [instance-random-seed (or instance-random-seed tree-random-seed)
         tree-stream (doto (RandomTreeGenerator.)
                       ;; Set the random seeds.
                       (-> (.treeRandomSeedOption)
                           (.setValue tree-random-seed))
                       (-> (.instanceRandomSeedOption)
                           (.setValue instance-random-seed))
                       ;; No nominal features.
                       (-> (.numNominalsOption)
                           (.setValue 0))
                       ;; Configurable number of numeric features.
                       (-> (.numNumericsOption)
                           (.setValue num-features))
                       ;; Class values.
                       (-> (.numClassesOption)
                           (.setValue class-count))
                       ;; Set maximum tree depth.
                       (-> (.maxTreeDepthOption)
                           (.setValue max-tree-depth))
                       ;; Only start creating leafs at a certain
                       ;; depth.
                       (-> (.firstLeafLevelOption)
                           (.setValue first-leaf-level))
                       ;; Set the "leafiness" of the tree.
                       (-> (.leafFractionOption)
                           (.setValue leaf-fraction))
                       (.prepareForUse))]
     (StaticDataset. (header->schema (.getHeader tree-stream))
                     (take max-records (stream->record-lazyseq tree-stream))))))
