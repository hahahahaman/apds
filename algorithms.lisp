;;;; algorithms.lisp

(in-package #:algorithms)

;;; "algorithms" goes here. Hacks and glory await!

(defun insertion-sort (seq &optional (increasing-order t))
  (let ((a (copy-seq seq)))
    (iter (for i from 1 below (length a))
      (let ((key (elt a i))
            (j (1- i)))
        (iter (while (and (>= j 0) (if increasing-order
                                       (> (elt a j) key)
                                       (< (elt a j) key))))
          (setf (elt a (1+ j)) (elt a j))
          (decf j))
        (setf (elt a (1+ j)) key)
        (print a)))
    a))

(defun linear-search (seq value)
  (iter (for i from 0 below (length seq))
    (when (= (elt seq i) value)
      (return i))))

(defun seq-emptyp (seq)
  (or (null seq) (equalp seq (vector))))

(defun recursive-linear-search (seq value)
  (labels ((lookfor (current-seq index)
             (if (seq-emptyp seq)
                 nil
                 (if (eql (elt current-seq 0) value)
                     index
                     (let ((next-seq (subseq current-seq 1)))
                       (if (seq-emptyp next-seq)
                           nil
                           (lookfor next-seq (1+ index))))))))
    (lookfor seq 0)))

(defun add-binary-integer-seqs (n1 n2)
  (let* ((a (reverse n1))
         (b (reverse n2))
         (c (make-array (1+ (max (length a) (length b))) :initial-element 0)))
    (iter (for i from 0 below (length a))
      (incf (aref c i) (elt a i)))
    (iter (for i from 0 below (length b))
      (incf (aref c i) (elt b i)))
    (iter (for i from (1- (length c)) downto 0)
      (when (= (aref c i) 2)
        (incf (aref c (1+ i)) 1)
        (setf (aref c i) 0)))

    (reverse c)))
