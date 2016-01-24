;;;; algorithms.asd

(asdf:defsystem #:algorithms
  :description "Some generic algorithms that might come in handy."
  :author "Ed Ye"
  :license "Licenseless Rider"
  :depends-on (#:simple-testing)
  :serial t
  :components ((:file "package")
               (:file "algorithms")))

