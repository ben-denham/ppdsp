(ns ppdsp.classifier.inspectablearf
  (:import [moa.classifiers.meta AdaptiveRandomForest]))

(gen-class
 :name ppdsp.classifier.inspectablearf
 :extends moa.classifiers.meta.AdaptiveRandomForest
 :exposes {ensemble {:get getEnsemble}})
