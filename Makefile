rpncalc: *.lisp
	sbcl --eval '(asdf:make :rpncalc)' \
		--eval '(quit)'
	# for smaller executable size, use ECL
	# ecl --eval '(require :asdf)' \
	# 	--eval "(defpackage rpncalc (:use :cl) (:export #:main))" \
	# 	--eval "(asdf:make-build :rpncalc :type :program :epilogue-code '(progn (rpncalc:main) (quit)))" \
	# 	--eval '(quit)'

run: rpncalc
	./rpncalc

interactive:
	sbcl --eval '(asdf:load-system :rpncalc)'

test:
	sbcl --eval '(asdf:test-system :rpncalc)' \
		--eval '(quit)'

build:
	ecl --eval '(require :asdf)'
