(defsystem "zpcalc"
  :version "0.1.0"
  :author "Zachary Huang"
  :license "MIT"
  :depends-on ()
  :serial t
  :components ((:file "util")
               (:file "env")
               (:file "state")
               (:file "conditions")
               (:file "history")
               (:file "actions")
               (:file "packages")
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
