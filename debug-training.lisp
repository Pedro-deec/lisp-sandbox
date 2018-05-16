(defclass ns.donna.adaptor.rsr.line ()
  ((rs.train.tasks	:initarg :rs.train.tasks	:initform nil	:accessor rs.train.tasks)))


(defclass ns.donna.adaptor.rs.train.task ()
  ((train.id		:initarg :train.id		:initform ""	:accessor train.id)
   (duty.group.name	:initarg :duty.group.name	:initform ""	:accessor duty.group.name)))


(defun make.ns.donna.adaptor.rsr.line (&rest args)
  (apply 'make-instance 'ns.donna.adaptor.rsr.line args))


(defun make.ns.donna.adaptor.rs.train.task (&rest args)
  (apply 'make-instance 'ns.donna.adaptor.rs.train.task args))


;; (defparameter *account* 
;;   (make-instance 'bank-account :customer-name "Ze" :balance 1000))

(defmethod add-new-rs-train-task ((self.duty ns.donna.adaptor.rsr.line) (self.task ns.donna.adaptor.rs.train.task))
  (push self.task (rs.train.tasks self.duty)))

(defmethod create-data-donna->crews ((type (eql :rolling-stock-roster)) rs.duty)
  ;; Translate Plist (Donna format) to Class (Crews format)
  (let ((new.rs.duty (make.ns.donna.adaptor.rsr.line))
	(rs.tasks (getf rs.duty :rstasks)))
    (dolist (rs.task rs.tasks)
      (let ((new.rs.task (make.ns.donna.adaptor.rs.train.task :train.id (getf rs.task :bewegingcode)
							      :duty.group.name (getf rs.duty :matdienstgroepcode) )))
	;; Translate RS task
	;;(translate-data-donna->crews new.rs.task)
	(add-new-rs-train-task new.rs.duty new.rs.task)))
    ;; Translate RSR line
    (translate-data-donna->crews new.rs.duty)
    new.rs.duty))


;; (500 100.1 100 300.1 300.1 400)
;; 1) prev=nil   curr=500
;; 2) prev=500   curr=100.1
;; 3) prev=100.1 curr=100   <== equal and update
;; 4) prev=100.1 prev=300.1
;; 5) prev=300.1 curr=300.1 <== equal and not update
;; 6) prev=300.1 curr=400



(defmethod translate-data-donna->crews ((self ns.donna.adaptor.rsr.line))
  ;; Aqui vamos percorrer as rs.train.tasks e atribuir .1
  ;; Atencao: Aqui vou percorrer e ver se devo atribuir .1
  ;; Optimizar isto
  ;; Só actualiza o RSR.LINE se existir RS.TRAIN.TASK com ".1" FIND-IF.
  ;; if is a .1 train

  ;; **********************************************************************************************
  ;; FUNCIONAL 
  ;; **********************************************************************************************
  ;; (when (find-if #'(lambda (id)
  ;; 		     ;; train has offset
  ;; 		     (multiple-value-bind (number offset)
  ;; 			 (ns.train.id.to.number id)
  ;; 		       (numberp offset))) (rs.train.tasks self) :key #'train.id)
  ;;   (let ((rs.train.tasks.aux (rs.train.tasks self))
  ;; 	  (previous nil)
  ;; 	  (result nil))
  ;;     (dolist (var rs.train.tasks.aux)
  ;; 	(when previous
  ;; 	  (multiple-value-bind (number.prev offset.prev)
  ;; 	      (ns.train.id.to.number (train.id previous))
  ;; 	    (when offset.prev
  ;; 	      (multiple-value-bind (number.curr offset.curr)
  ;; 		  (ns.train.id.to.number (train.id var))
  ;; 		(when (and (eq number.prev number.curr)
  ;; 			   (null offset.curr))
  ;; 		  (setf (train.id var) (format nil "~a.1" (train.id var))))))))
  ;; 	(push var result) ;; `melhorar-eficiencia'
  ;; 	(setf previous var))
  ;;     (setf (rs.train.tasks self) result)))
  ;; **********************************************************************************************

  (let ((previous nil))
    (dolist (var (rs.train.tasks self))
      (when previous
	(multiple-value-bind (number.prev offset.prev)
	    (ns.train.id.to.number (train.id previous))
	  (when offset.prev
	    (multiple-value-bind (number.curr offset.curr)
		(ns.train.id.to.number (train.id var))
	      (when (and (eq number.prev number.curr)
			 (null offset.curr))
		(setf (train.id var) (format nil "~a.1" (train.id var))))))))
      (setf previous var)))
    
    ;; Sort RSR line by start time of the RS train tasks
    ;;(setf (rs.train.tasks self) (sort-rstasks-by-start-time self))
(values))


(defun ns.train.id.to.number (id)
  (let ((dot (position #\. id :test #'char=)))
    (values (parse-integer id :start 0 :end dot)
            (and dot
                 (parse-integer id :start (+ dot 1))))))

;; (NS.TRAIN.ID.TO.NUMBER "100.1") > 100 1
;; (NS.TRAIN.ID.TO.NUMBER "100") > 100 nil
151.79

(create-data-donna->crews :rolling-stock-roster (list :matdienstgroepcode "AL" :rstasks (list (list :bewegingcode "200")
											      (list :bewegingcode "300.1") 
											      (list :bewegingcode "300.1")
											      (list :bewegingcode "1000") 
											      (list :bewegingcode "1000.1")
											      (list :bewegingcode "5000")
											      (list :bewegingcode "2000.1"))))
