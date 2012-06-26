cl-changes

Info

Detects changes of the slot values and sets its %changedp slot to t
and calls the function in the on-change slot, if the changed slot has an attribute of :on-change, that function will be called too.
the format for the slot's :on-change attribute function is (lambda (object)...)
the format for the objects %on-change slot's function is (lambda (object slot-name)...)

-----------------------------------------------------------------------
Example use

CL-CHANGES>
(defclass test (change-sensitive-object)
  ((slot-a     :accessor slot-a        :initarg :slot-a    :initform nil 
	       :attributes (:on-change (lambda (object) 
					 (format t "slot-a of ~a has changed~%" object))))
   (slot-b     :accessor slot-b        :initarg :slot-b    :initform nil)
   (%on-change :accessor %on-change :initarg :%on-change 
	       :initform (lambda (object slot) (format t "~a's ~a changed~%" object slot))))
  (:metaclass attributes-class))

#<ATTRIBUTES-CLASS TEST>

CL-CHANGES> (defparameter obj (make-instance 'test :slot-a "value 1"))
#<TEST {BAEDA39}>'s SLOT-B changed
#<TEST {BAEDA39}>'s %ALL-ATTRIBUTES changed
OBJ

-----------------------------------------------------------------------

;;setting a slot value will trigger the appropriate on-change functions

CL-CHANGES> (setf (slot-a obj) "val2")
#<TEST {BAEDA39}>'s SLOT-A changed  ;;this is from %on-change slots function
slot-a of #<TEST {BAEDA39}> has changed  ;;this is from slot-a's :on-change attribute's function
"val2"

-----------------------------------------------------------------------

;;check to see if the object has changed

CL-CHANGES> (%changedp obj)
T

-----------------------------------------------------------------------

;;set it's changed status to nil

CL-CHANGES> (setf (%changedp obj) nil)
NIL
