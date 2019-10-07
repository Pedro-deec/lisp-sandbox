
(defun escreve (x)
  (format t "hello ~a" x))

(defmacro when-mine (condition &rest body)
  `(if ,condition (progn ,@body)))

(defmacro when-mine (condition &rest body)
  `(if ,condition (progn ,@body)))
