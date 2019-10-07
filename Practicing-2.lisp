;;;****************************************************************************
;;; http://www.gigamonkeys.com/book/practical-a-spam-filter.html
;;; 23. Practical: A Spam FIlter
;;;****************************************************************************

;;;****************************************************************************
;; `THE-HEART-OF-A-SPAM-FILTER'
;;;****************************************************************************
(defpackage :com.gigamonkeys.pathnames
  (:use :common-lisp)
  (:export
   :list-directory
   :file-exists-p
   :directory-pathname-p
   :file-pathname-p
   :pathname-as-directory
   :pathname-as-file
   :walk-directory
   :directory-p
   :file-p))

;;;****************************************************************************
(in-package :com.gigamonkeys.pathnames)

;;;****************************************************************************
(defpackage :com.gigamonkeys.spam
  (:use :common-lisp :com.gigamonkeys.pathnames))

;;;****************************************************************************
(in-package :com.gigamonkeys.spam)

;;;****************************************************************************
(defun classify (text)
  ;; Classify the message as spam, ham, or unsure
  (classification (score (extract-features text))))

;;;****************************************************************************
(defparameter *max-ham-score* .4)
(defparameter *min-spam-score* .6)

;;;****************************************************************************
(defun classification (score)
  ;; Score will return near 1 if the message is a spam, near 0 if it's a ham,
  ;; and near .5 if it's unclear.
  (cond
    ((<= score *max-ham-score*) 'ham)
    ((>= score *min-spam-score*) 'spam)
    (t 'unsure)))

;;;****************************************************************************
(defclass word-feature ()
  ((word       
    :initarg :word
    :accessor word
    :initform (error "Must supply :word")
    :documentation "The word this feature represents.")
   (spam-count
    :initarg :spam-count
    :accessor spam-count
    :initform 0
    :documentation "Number of spams we have seen this feature in.")
   (ham-count
    :initarg :ham-count
    :accessor ham-count
    :initform 0
    :documentation "Number of hams we have seen this feature in.")))

;;;****************************************************************************
;; Keep the database of features in a hash table
;; Use DEFVAR rather than DEFPARAMETER because you don't want *feature-database*
;; to be reset if you happen to reload the file containing this definition
;; during development.
(defvar *feature-database* (make-hash-table :test #'equal))

;;;****************************************************************************
;; To clear out the feature database
(defun clear-database ()
  (setf *feature-database* (make-hash-table :test #'equal)))

;;;****************************************************************************
;; Encapsulate in a function that takes a word and returns the appropriate
;; feature, creating it if necessary.
(defun intern-feature (word)
  (or (gethash word *feature-database*)
      (setf (gethash word *feature-database*)
            (make-instance 'word-feature :word word))))

;;;****************************************************************************
;; SPAM> (extract-words "foo bar baz")
;; ("foo" "bar" "baz")
(defun extract-words (text)
  (delete-duplicates
   (cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text)
   :test #'string=))

;;;****************************************************************************
(defun extract-features (text)
  (mapcar #'intern-feature (extract-words text)))

;;;***************************************************************************
;; To print from #<WORD-FEATURE @ #x71ef28aa> to #<WORD-FEATURE "baz" :hams 0 :spams 0>
;; SPAM> (extract-features "foo bar baz foo bar")
;; (#<WORD-FEATURE "baz" :hams 0 :spams 0>
;; #<WORD-FEATURE "foo" :hams 0 :spams 0>
;; #<WORD-FEATURE "bar" :hams 0 :spams 0>)
(defmethod print-object ((object word-feature) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (word ham-count spam-count) object
      (format stream "~s :hams ~d :spams ~d" word ham-count spam-count))))

;;;****************************************************************************
;; `TRAINING-THE-FILTER'
;;;****************************************************************************
(defun train (text type)
  (dolist (feature (extract-features text))
    (increment-count feature type))
  (increment-total-count type))

;;;****************************************************************************
;; (increment-count some-feature 'ham)
(defun increment-count (feature type)
  ;; with E, meaning ECASE should signal an error if key value is anything
  ;; other than one of the keys listed. The CASE would return NIL.
  (ecase type
    (ham (incf (ham-count feature)))
    (spam (incf (spam-count feature)))))

(defvar *total-spams* 0)
(defvar *total-hams* 0)

(defun increment-total-count (type)
  (ecase type
    (ham (incf *total-hams*))
    (spam (incf *total-spams*))))

(defun clear-database ()
  (setf
   *feature-database* (make-hash-table :test #'equal)
   *total-spams* 0
   *total-hams* 0))

;;;****************************************************************************
;; `PER-WORD-STATISTICS'
;;;****************************************************************************
(defun spam-probability (feature)
  (with-slots (spam-count ham-count) feature
    (let ((spam-frequency (/ spam-count (max 1 *total-spams*)))
          (ham-frequency (/ ham-count (max 1 *total-hams*))))
      (/ spam-frequency (+ spam-frequency ham-frequency)))))

(defun bayesian-spam-probability (feature &optional 
					    (assumed-probability 1/2)
					    (weight 1))
  (let ((basic-probability (spam-probability feature))
        (data-points (+ (spam-count feature) (ham-count feature))))
    (/ (+ (* weight assumed-probability)
          (* data-points basic-probability))
       (+ weight data-points))))

;;;****************************************************************************
;; `COMBINING-PROBABILITIES'
;;;****************************************************************************
(defun score (features)
  (let ((spam-probs ()) (ham-probs ()) (number-of-probs 0))
    (dolist (feature features)
      (unless (untrained-p feature)
        (let ((spam-prob (float (bayesian-spam-probability feature) 0.0d0)))
          (push spam-prob spam-probs)
          (push (- 1.0d0 spam-prob) ham-probs)
          (incf number-of-probs))))
    (let ((h (- 1 (fisher spam-probs number-of-probs)))
          (s (- 1 (fisher ham-probs number-of-probs))))
      (/ (+ (- 1 h) s) 2.0d0))))

(defun untrained-p (feature)
  (with-slots (spam-count ham-count) feature
    (and (zerop spam-count) (zerop ham-count))))

(defun fisher (probs number-of-probs)
  "The Fisher computation described by Robinson."
  (inverse-chi-square 
   (* -2 (reduce #'+ probs :key #'log))
   (* 2 number-of-probs)))

;;;****************************************************************************
;; `INVERSE-CHI-SQUARE'
;;;****************************************************************************
(defun inverse-chi-square (value degrees-of-freedom)
  (assert (evenp degrees-of-freedom))
  (min 
   (loop with m = (/ value 2)
      for i below (/ degrees-of-freedom 2)
      for prob = (exp (- m)) then (* prob (/ m i))
      summing prob)
   1.0))

;;;****************************************************************************
;; `TRAINING-THE-FILTER'
;;;****************************************************************************
;; SPAM> (clear-database)
;; SPAM> (classify "Make money fast")
;; SPAM
;; SPAM> (classify "Want to go to the movies?")
;; UNSURE

;;;****************************************************************************
;; Changed to show also the probability
(defun classification (score)
  (values
   (cond
     ((<= score *max-ham-score*) 'ham)
     ((>= score *min-spam-score*) 'spam)
     (t 'unsure))
   score))
;; SPAM> (classify "Make money fast")
;; SPAM
;; 0.863677101854273D0
;; SPAM> (classify "Want to go to the movies?")
;; UNSURE
;; 0.5D0

;; SPAM> (train "Do you have any money for the movies?" 'ham)
;; 1
;; SPAM> (classify "Make money fast")
;; SPAM
;; 0.7685351219857626D0

;; SPAM> (classify "Want to go to the movies?")
;; HAM
;; 0.17482223132078922D0

;;;****************************************************************************
;; `TESTING-THE-FILTER'
;;;****************************************************************************
;; Test the filter with a bunch of files and train it on them.
(defun add-file-to-corpus (filename type corpus)
  (vector-push-extend (list filename type) corpus))

(defparameter *corpus* (make-array 1000 :adjustable t :fill-pointer 0))

(defun add-directory-to-corpus (dir type corpus)
  (dolist (filename (list-directory dir))
    (add-file-to-corpus filename type corpus)))

;; SPAM> (add-directory-to-corpus "mail/spam/" 'spam *corpus*)
;; NIL
;; SPAM> (add-directory-to-corpus "mail/ham/" 'ham *corpus*)
;; NIL

;;;****************************************************************************
;; To test the classifier
(defun test-classifier (corpus testing-fraction)
  (clear-database)
  (let* ((shuffled (shuffle-vector corpus))
         (size (length corpus))
         (train-on (floor (* size (- 1 testing-fraction)))))
    (train-from-corpus shuffled :start 0 :end train-on)
    (test-from-corpus shuffled :start train-on)))

(defparameter *max-chars* (* 10 1024))

(defun train-from-corpus (corpus &key (start 0) end)
  (loop for idx from start below (or end (length corpus)) do
        (destructuring-bind (file type) (aref corpus idx)
          (train (start-of-file file *max-chars*) type))))

(defun test-from-corpus (corpus &key (start 0) end)
  (loop for idx from start below (or end (length corpus)) collect
        (destructuring-bind (file type) (aref corpus idx)
          (multiple-value-bind (classification score)
              (classify (start-of-file file *max-chars*))
            (list 
             :file file
             :type type
             :classification classification
             :score score)))))

;;;****************************************************************************
;; `A-COUPLE-OF-UTILITY-FUNCTIONS'
;;;****************************************************************************
;; Efficient way 
(defun nshuffle-vector (vector)
  (loop for idx downfrom (1- (length vector)) to 1
     for other = (random (1+ idx))
     do (unless (= idx other)
	  (rotatef (aref vector idx) (aref vector other))))
  vector)

(defun shuffle-vector (vector)
  (nshuffle-vector (copy-seq vector)))

(defun start-of-file (file max-chars)
  (with-open-file (in file)
    (let* ((length (min (file-length in) max-chars))
           (text (make-string length))
           (read (read-sequence text in)))
      (if (< read length)
        (subseq text 0 read)
        text))))

;;;****************************************************************************
;; `ANALYZING-THE-RESULTS'
;;;****************************************************************************
(defun result-type (result)
  (destructuring-bind (&key type classification &allow-other-keys) result
    (ecase type
      (ham
       (ecase classification
         (ham 'correct)
         (spam 'false-positive)
         (unsure 'missed-ham)))
      (spam
       (ecase classification
         (ham 'false-negative)
         (spam 'correct)
         (unsure 'missed-spam))))))

;; SPAM> (result-type '(:FILE #p"foo" :type ham :classification ham :score 0))
;; CORRECT

(defun false-positive-p (result)
  (eql (result-type result) 'false-positive))

(defun false-negative-p (result)
  (eql (result-type result) 'false-negative))

(defun missed-ham-p (result)
  (eql (result-type result) 'missed-ham))

(defun missed-spam-p (result)
  (eql (result-type result) 'missed-spam))

(defun correct-p (result)
  (eql (result-type result) 'correct))

;; SPAM> (count-if #'false-positive-p *results*)
;; 6
;; SPAM> (remove-if-not #'false-positive-p *results*)


(defun analyze-results (results)
  (let* ((keys '(total correct false-positive 
                 false-negative missed-ham missed-spam))
         (counts (loop for x in keys collect (cons x 0))))
    (dolist (item results)
      (incf (cdr (assoc 'total counts)))
      (incf (cdr (assoc (result-type item) counts))))
    (loop with total = (cdr (assoc 'total counts))
          for (label . count) in counts
          do (format t "~&~@(~a~):~20t~5d~,5t: ~6,2f%~%"
                     label count (* 100 (/ count total))))))

;; SPAM> (analyze-results *results*)
;; Total:               3761 : 100.00%
;; Correct:             3689 :  98.09%
;; False-positive:         4 :   0.11%
;; False-negative:         9 :   0.24%
;; Missed-ham:            19 :   0.51%
;; Missed-spam:           40 :   1.06%
;; NIL

(defun explain-classification (file)
  (let* ((text (start-of-file file *max-chars*))
         (features (extract-features text))
         (score (score features))
         (classification (classification score)))
    (show-summary file text classification score)
    (dolist (feature (sorted-interesting features))
      (show-feature feature))))

(defun show-summary (file text classification score)
  (format t "~&~a" file)
  (format t "~2%~a~2%" text)
  (format t "Classified as ~a with score of ~,5f~%" classification score))

(defun show-feature (feature)
  (with-slots (word ham-count spam-count) feature
    (format
     t "~&~2t~a~30thams: ~5d; spams: ~5d;~,10tprob: ~,f~%"
     word ham-count spam-count (bayesian-spam-probability feature))))

(defun sorted-interesting (features)
  (sort (remove-if #'untrained-p features) #'< :key #'bayesian-spam-probability))