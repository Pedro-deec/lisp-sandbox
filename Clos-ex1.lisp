(defvar *account.numbers* 0)

(defclass bank.account ()
  ((customer.name	:initarg :customer.name
			:initform (error "Must supply a customer name.")
			:accessor customer.name 
			:documentation "Customer's name")
   (balance		:initarg :balance 
			:initform 0 
			:reader balance 
			:documentation "Current account balance")
   (account.number	:initform (incf *account.numbers*) 
			:reader account.number
			:documentation "Account number, unique within a bank.")
   (account.type	:reader account.type
			:documentation "Type of account, one of :gold, :silver, or :bronze.")))

(defparameter *account* 
  (make-instance 'bank.account :customer.name "Pedro" :balance 1000))

(defmethod initialize-instance :after ((account bank.account) &key)
  (let ((balance (slot-value account 'balance)))
    (setf (slot-value account 'account.type)
	  (cond
            ((>= balance 100000) :gold)
            ((>= balance 50000) :silver)
	    (t :bronze)))))

;; (defmethod initialize-instance :after ((account bank-account)
;;                                        &key opening-bonus-percentage)
;;   (when opening-bonus-percentage
;;     (incf (slot-value account 'balance)
;;           (* (slot-value account 'balance) (/ opening-bonus-percentage 100)))))

(defclass bank.account.together (bank.account)
  ((customer.name.2	:initarg :customer.name.2 
			:initform ""
			:accessor customer.name.2)))

(defparameter *account.2*
  (make-instance 'bank.account.together :customer.name "Pedro" :customer.name.2 "Maria" :balance 500))

(defgeneric withdraw (account value)
  (:documentation "Get cash"))

(defmethod deposit ((account bank.account) value)
  (incf (balance account) value))

(defgeneric deposit (account value)
  (:documentation "Put cash"))

(defmethod levantar ((account bank.account) value)
  (when (> (saldo account) value)
    (decf (balance account) value)))
