;;;-----------------------------------------------------------------
;;; Finding the value is very fast, even if there are many entries,
;;; because hashing is used; this is an important advantage of the
;;; hash tables over property lists.
;;; Note that due to the nature of hash tables we can't control the
;;; order in which the entries are provided
;;;-----------------------------------------------------------------

(defparameter *turtles* nil)

(setf *turtles* (make-hash-table :size 9 :test 'eq))

(setf (gethash 'howard-kaylan *turtles*) '(musician lead-singer))
(setf (gethash 'john-barbata *turtles*) '(musician drummer))
(setf (gethash 'leonardo *turtles*) '(ninja leader blue))
(setf (gethash 'donatello *turtles*) '(ninja machines purple))
(setf (gethash 'al-nichol *turtles*) '(musician guitarist))
(setf (gethash 'mark-volman *turtles*) '(musician great-hair))
(setf (gethash 'raphael *turtles*) '(ninja cool rude red))
(setf (gethash 'michaelangelo *turtles*) '(ninja party-dude orange))
(setf (gethash 'jim-pons *turtles*) '(musician bassist))

;; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; |            1           |          2         |          3          |            4            |           5          |            6          |            7          |             8             |          9         |
;; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; |    'howard-kaylan      |   'john-barbata    |     'leonardo       |     'donatello          |     'al-nichol       |    'mark-volman       |       'raphael        |      'michaelangelo       |     'jim-pons      |
;; | (musician lead-singer) | (musician drummer) | (ninja leader blue) | (ninja machines purple) | (musician guitarist) | (musician great-hair) | (ninja cool rude red) | (ninja party-dude orange) | (musician bassist) |
;; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

(with-hash-table-iterator (get-turtle *turtles*)
  (labels ((try (got-one &optional key value)
	     (when got-one ;remember, keys may show up in any order
	       (when (eq (first value) 'ninja)
		 (format t "~%~:(~A~): ~{~A~^, ~}"
			 key (rest value)))
	       (multiple-value-call #'try (get-turtle)))))
    (multiple-value-call #'try (get-turtle)))) ;prints 4 lines

;; Leonardo: LEADER, BLUE
;; Michaelangelo: PARTY-DUDE, ORANGE
;; Donatello: MACHINES, PURPLE
;; Raphael: COOL, RUDE, RED
;; NIL

(hash-table-count *turtles*)
;; 9
(clrhash *turtles*)

(defun print-hash-entry (key value)
    (format t "The value associated with the key ~S is ~S~%" key value))

;; MAPHASH always returns NIL
(maphash #'print-hash-entry *turtles*)
