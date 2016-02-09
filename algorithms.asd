;;;; algorithms.asd

(asdf:defsystem #:algorithms
  :description "Some generic algorithms that might come in handy."
  :author "Ed Ye"
  :license "Licenseless Rider"
  :serial t
  :depends-on (#:iterate)
  :pathname "src/"
  :components ((:file "package")
               (:file "algorithms")
               (:file "export")))

(asdf:defsystem #:algorithms.tests
  :description "Unit tests for algorithms"
  :author "Ed Ye"
  :license "Licenseless Rider"
  :depends-on (#:simple-testing
               #:iterate
               #:algorithms)
  :serial t
  :pathname "t/"
  :components ((:file "package")
               (:file "tests")))

(defmethod asdf:perform ((op asdf:test-op)
                         (system (eql (asdf:find-system :algorithms))))
  (format t "~2&*************~@
                ** Loading **~@
                *************~%")
  (asdf:oos 'asdf:load-op :algorithms.tests)
  (asdf:oos 'asdf:test-op :algorithms.tests))
