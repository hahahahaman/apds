(in-package :algorithms.tests)

(deftest test-emptyp ()
  (check
    (eql (seq-emptyp (list)) t)
    (eql (seq-emptyp (vector)) t)
    (eql (seq-emptyp (vector 1)) nil)
    (eql (seq-emptyp (list 1 2 3)) nil)))

(deftest test-merge ()
  (let ((s1 (list 1 2 3 4))
        (s2 (vector 2 3 4 5 6))
        (pred1 #'<)
        (pred2 #'>))
    (check
      (equalp (merge 'vector s1 s2 pred1)
              (merge-plz 'vector s1 s2 pred1))
      (equalp (merge 'list s1 s2 pred2)
              (merge-plz 'list s1 s2 pred2)))))

(deftest test-sorting ()
  (let* ((seq (list 1 2 0 3 4 5 6 7 100
                    -1 -20 -30 0.0 100.000 -10.0))
         (pred1 #'<)
         (pred2 #'>)
         (sorted-seq1 (sort (copy-seq seq) pred1))
         (sorted-seq2 (sort (copy-seq seq) pred2)))
    (labels ((compare-sorted-seq1 (func)
               (equalp sorted-seq1 (funcall func seq pred1)))
             (compare-sorted-seq2 (func)
               (equalp sorted-seq2 (funcall func seq pred2)))
             (compare-to-sorted (func)
               (and (compare-sorted-seq1 func)
                    (compare-sorted-seq2 func))))
      (check
        (compare-to-sorted #'insertion-sort)
        (compare-to-sorted #'selection-sort)
        (compare-to-sorted #'merge-sort)))))

(defun run-all-tests ()
  (test-emptyp)
  (test-merge)
  (test-sorting))

;;; Hooking into ASDF
(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system :algorithms.tests))))
  (format t "~2&*******************~@
                ** Starting test **~@
                *******************~%~%")
  (handler-bind ((style-warning #'muffle-warning)) (run-all-tests))
  (format t "~2&*****************************************~@
                **            Tests finished           **~@
                *****************************************~%"))
