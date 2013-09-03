;;;; cl-changes.asd

(asdf:defsystem #:cl-changes
  :serial t
  :description "detects changes of the slot values and sets its
changedp slot to t and calls the function in the on-change slot,
if the changed slot has an attribute of :on-change, that function
will be called too. the format for the slot's on change function is (lambda (object))"
  
  :author "farzadbekran@gmail.com"
  :license "Free software, you can do whatever you want with it"
  :depends-on (#:cl-attribs)
  :components ((:file "package")
               (:file "cl-changes")))

