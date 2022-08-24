(defsystem "rpncalc"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :serial t
  :components ((:file "util")
               (:file "conditions")
               (:file "history")
               (:file "env")
               (:file "state")
               (:file "main"))
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
