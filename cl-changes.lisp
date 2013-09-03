;;;; cl-changes.lisp

(in-package #:cl-changes)

(defclass change-sensitive-object (attributes-object)
  ((%changedp  :initform t   :initarg :%changedp  :accessor %changedp)
   (%on-change :initform nil :initarg :%on-change :accessor %on-change
	       :documentation "a function to call when there is a change,
should accept object and slot name as args: (lambda (object slot-name)...)"))
  (:metaclass attributes-class)
  (:documentation "detects changes of the slot values and sets its changedp slot to t
and calls the function in the on-change slot, if the changed slot has an attribute of
:on-change, that function will be called too. the slot's on change function should
accept object as an arg: (lambda (object)...)"))

;;this will make sure that after the object is initialized, every slot that has
;;a :on-change attrib will have it's on-change function called. the function in
;;the %on-change slot will be called once all of the :on-change attrib
;;funcs have been called
(defmethod initialize-instance :after ((object change-sensitive-object) &rest rest)
  (declare (ignore rest))
  (let ((class-slots (closer-mop:class-slots (class-of object))))
    (loop for slot in class-slots
       with slot-name = nil
       with on-change-func = nil
       do
	 (if slot
	     (setf slot-name (closer-mop:slot-definition-name slot)))
	 (if slot-name
	     (setf on-change-func (slot-attrib object slot-name :on-change)))
	 (if on-change-func
	     (funcall on-change-func object)))
    (if (%on-change object)
	(funcall (%on-change object) object slot-name))))

;;when a change has happened, the slot's :on-change function will be called, then the %on-change function
(defmethod (setf closer-mop:slot-value-using-class) :after (new-value class (object change-sensitive-object) slotd)
  (declare (ignorable new-value class slotd))
  (let ((slot-name (closer-mop:slot-definition-name slotd)))
    (if (slot-boundp object '%changedp)
	(cond ((not (or (eq slot-name '%changedp) ;; the rules don't apply to these slots as they are part of the change sensitive class itself
			(eq slot-name '%on-change)))
	       (setf (%changedp object) t)
	       (if (slot-attrib object slot-name :on-change)
		   (funcall (slot-attrib object slot-name :on-change) object))
	       (if (and (slot-boundp object '%on-change) (%on-change object))
		   (funcall (%on-change object) object slot-name)))))))

