;; A generic function is an object that you use just like a regular function, but
;; it contains a bunch of methods that determine what happens when you invoke the function

(defgeneric prepend (item thing)
  (:method ((item character) (thing string))
    (concatenate 'string (string item) thing))
  (:method (item (thing cons))
    (cons item thing)))


(defmethod prepend ((item string) (thing string))
  (concatenate 'string item thing))

(prepend "Foo" "Bar")

;; Which methods is called?
;; When a generic function is invoked, the first thing that happens is that the applicable
;; methods are selected. If the type of the argument matches the type specified by the
;; method, then the method is applicable. Inapplicable methods will not participate further.
;;Once the applicable methods are found, they are combined. The “standard method combination”
;;is to simply select the most specific method. So if more than one method is applicable,
;;the one that most closely matches the actual types of the arguments is used.

(defgeneric test-which-method (object)
  (:method (object)
    "This is an object.")
  (:method ((object number))
    "This is a number.")
  (:method ((object integer))
    "This is an integer."))

(test-which-method 27.2)

;; Why do they call it combination rather than selection? Because the less specific methods
;; are not discarded, they can still be called. This is much like calling the base method in
;; other languages.

(defgeneric test-which-method (object)
  (:method (object)
    (list "This is an object."))
  (:method ((object number))
    (cons "This is a number." (call-next-method)))
  (:method ((object integer))
    (cons "This is an integer." (call-next-method))))

(test-which-method 33)
