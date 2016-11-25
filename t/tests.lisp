(in-package :algorithms.tests)

(deftest test-equal-or ()
  (check
   (eql (equal-or 1 1) t)
   (eql (equal-or 1 '(2 1)) t)
   (eql (equal-or 1 '(0 0 10 10 -1)) nil)
   (eql (equal-or 1 '(a b c)) nil)))

(deftest test-emptyp ()
  (check
   (eql (seq-emptyp (list)) t)
   (eql (seq-emptyp (vector)) t)
   (eql (seq-emptyp (vector 1)) nil)
   (eql (seq-emptyp (list 1 2 3)) nil)))

(deftest test-linear-search ()
  (let ((vec (vector 1 4 5 6 7 12 -1)))
    (macrolet ((test-func (func)
                 `(check
                   (eql (,func vec 12) 5)
                   (eql (,func vec -1) 6)
                   (eql (,func vec -10) nil))))
      (combine-results
       (test-func linear-search)
       (test-func recursive-linear-search)))))

(deftest test-binary-search ()
  (let ((vec (vector -2 -1 1 2 3 4 5 6 10 11 20 30 40 50))
        (list (list 0 0 0 1 1 1 1 2 3 4 5 6 7 8 9)))
    (check
      (eql (binary-search '(1 2 3) 3) 2)
      (eql (binary-search '(-1000 -999 999 1000) 999) 2)
      (eql (binary-search vec -2) 0)
      (eql (binary-search vec -1) 1)
      (eql (binary-search vec 6) 7)
      (equal-or (binary-search list 1) '(3 4 5 6))
      (equal-or (binary-search list 0) '(0 1 2))
      (eql (binary-search list 8) 13)
      (eql (binary-search vec 1000) nil) ;;; TODO: broken
      (eql (binary-search list -1000) nil)

      (eql (recursive-binary-search '(1 2 3) 3 #'< 0) 2)
      (eql (recursive-binary-search '(-1000 -999 999 1000) 999 #'< 0) 2)
      (eql (recursive-binary-search vec -2 #'< 0) 0)
      (eql (recursive-binary-search vec -1 #'< 0) 1)
      (eql (recursive-binary-search vec 6 #'< 0) 7)
      (equal-or (recursive-binary-search list 1 #'< 0) '(3 4 5 6))
      (equal-or (recursive-binary-search list 0 #'< 0) '(0 1 2))
      (eql (recursive-binary-search list 8 #'< 0) 13)
      (eql (recursive-binary-search vec 1000 #'< 0) nil)
      (eql (recursive-binary-search list -1000 #'< 0) nil))))

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
  (let* ((seq (iter (for i from 0 to 1000)
                (collect (* (if (= (random 2) 0) -1 1)
                            (random 100))
                  result-type vector)))
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
        (compare-to-sorted #'merge-sort)
        (compare-to-sorted #'binary-insertion-sort)))))

(defun run-all-tests ()
  (test-equal-or)
  (test-emptyp)
  (test-linear-search)
  (test-binary-search)
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
