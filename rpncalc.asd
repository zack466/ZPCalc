(defsystem "rpncalc"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:file "util")
               (:file "conditions")
               (:file "state" :depends-on ("util" "conditions"))
               (:file "main" :depends-on ("util" "state" "conditions")))
  :description ""
  :build-operation "program-op"
  :build-pathname "rpncalc"
  :entry-point (symbol-call :rpncalc :main)
  :in-order-to ((test-op (test-op "rpncalc/tests"))))

(defsystem "rpncalc/tests"
  :author ""
  :license ""
  :depends-on ("rpncalc")
  :components ((:file "test"))
  :description "Test system for rpncalc")
