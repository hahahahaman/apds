;;;; algorithms.lisp

(in-package #:algorithms)

;;; "algorithms" goes here. Hacks and glory await!

(defun seq-emptyp (seq)
  (or (null seq) (equalp seq (vector))))

(defun equal-or (val check-list &key (equality-op #'eql))
  (if (consp check-list)
      (if (funcall equality-op val (car check-list))
          t
          (equal-or val (cdr check-list) :equality-op equality-op))
      (funcall equality-op val check-list)))

(defun insertion-sort (seq predicate)
  "=> SEQUENCE
Worst case O(n^2).
Average case O(n^2).
Best case O(n)."
  (let ((a (copy-seq seq)))
    (iter (for i from 1 below (length a))
      (let ((key (elt a i))
            (j (1- i)))
        (iter (while (and (>= j 0) (funcall predicate key (elt a j))))
          (setf (elt a (1+ j)) (elt a j))
          (decf j))
        (setf (elt a (1+ j)) key)
        ;; (print a)
        ))
    a))

(defun selection-sort (sequence pred)
  (let ((len (length sequence))
        (seq (copy-seq sequence)))
    (iter (for i from 0 below (1- len))
      (iter (for j from (1+ i) below len)
        (let ((ei (elt seq i))
              (ej (elt seq j)))
          (when (funcall pred ej ei)
            (setf (elt seq i) ej
                  (elt seq j) ei)))))
    seq))

(defun linear-search (seq value)
  "=> INDEX
Worst case O(n)."
  (iter (for i from 0 below (length seq))
    (when (= (elt seq i) value)
      (return i))))

(defun recursive-linear-search (seq value)
  (labels ((lookfor (current-seq index)
             (if (seq-emptyp current-seq)
                 nil
                 (if (eql (elt current-seq 0) value)
                     index
                     (lookfor (subseq current-seq 1) (1+ index))))))
    (lookfor seq 0)))

(defun binary-search (seq val predicate)
  (when (not (seq-emptyp seq))
    (let* ((len (length seq))
           (mid (floor (/ len 2))))
      (iter (while t)
        (let ((elm (elt seq mid)))
          (if (equalp elm val)
              (return mid)
              (if (funcall predicate elm val)
                  (let ((diff (floor (/ (- len mid) 2))))
                    (if (= diff 0)
                        (return)
                        (incf mid diff)))
                  (let ((new-mid (floor (/ mid 2))))
                    (if (= new-mid mid)
                        (return)
                        (setf mid new-mid))))))))))

(defun recursive-binary-search (seq val predicate index)
  "Assumes seq is sorted using PREDICATE."
  (if (seq-emptyp seq)
      nil
      (let* ((len (length seq))
             (mid (floor (/ len 2)))
             (elm (elt seq mid)))
        (if (equalp elm val)
            (+ index mid)
            (if (funcall predicate elm val)
                (recursive-binary-search (subseq seq (1+ mid))
                                         val
                                         predicate
                                         (+ index mid 1))
                (recursive-binary-search (subseq seq 0 mid) val
                                         predicate index))))))

(defun binary-insertion-sort (seq predicate)
  (let ((s (copy-seq seq)))
    (iter (for i from 1 below (length s))
      (let* ((key (elt s i))
             (start 0)
             (end i)
             (mid (+ start (floor (/ (- end start) 2)))))
        (iter (while (< start end))
          (cond ((funcall predicate (elt s mid) key)
                 ;; upper half of array
                 (setf start (1+ mid)
                       mid (+ start (floor (/ (- end start) 2)))))
                (t
                 ;; lower half

                 ;; push elements up by one
                 (iter (for j from (1- end) downto mid)
                   (let ((temp (elt s (1+ j))))
                     (setf (elt s (1+ j)) (elt s j)
                           (elt s j) temp)))

                 (setf end mid
                       mid (+ start (floor (/ (- end start) 2)))))))
        (setf (elt s start) key)))
    s))

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

(defun add-nth (n elem sequence)
  (let* ((len (length sequence))
         (index (min n len)))
    (concatenate (type-of sequence)
                 (subseq sequence 0 index)
                 (list elem)
                 (subseq sequence index))))

(defun merge-plz (result-type s1 s2 pred)
  (labels ((merge-iter (a b result)
             (if (seq-emptyp a)
                 (if (seq-emptyp b)
                     result
                     (merge-iter a nil (concatenate result-type result b)))
                 (if (seq-emptyp b)
                     (merge-iter nil b (concatenate result-type result a))
                     (if (funcall pred (elt a 0) (elt b 0))
                         (merge-iter (subseq a 1)
                                     b
                                     (concatenate result-type
                                                  result (list (elt a 0))))
                         (merge-iter a (subseq b 1)
                                     (concatenate result-type
                                                  result (list (elt b 0)))))))))
    (merge-iter s1 s2 nil)))

(defun merge-sort (seq pred)
  ;; (print seq)
  (let* ((len (length seq))
         (mid (truncate (/ len 2.0))))
    (if (= len 1)
        seq
        (merge-plz (if (consp seq) 'list 'vector)
                   (merge-sort (subseq seq 0 mid) pred)
                   (merge-sort (subseq seq mid) pred)
                   pred))))

(defun sum-of-2=x (seq x)
  (let* ((s (merge-sort seq #'<))
         (len (length s)))
    (iter (for i from 1 below (1- len))
      (when (binary-search (subseq s 0 i) (- x (elt s i)) #'<)
        (return t))
      (when (binary-search (subseq s (1+ i)) (- x (elt s i)) #'<)
        (return t)))))
