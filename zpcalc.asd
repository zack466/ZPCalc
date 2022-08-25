(defsystem "zpcalc"
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
  :build-pathname "zpcalc"
  :entry-point (symbol-call :zpcalc :main)
  :in-order-to ((test-op (test-op "zpcalc/tests"))))

(defsystem "zpcalc/tests"
  :author ""
  :license ""
  :depends-on ("zpcalc")
  :components ((:file "test"))
  :description "Test system for zpcalc")
