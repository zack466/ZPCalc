run: build
	./zpcalc

build: *.lisp
	sbcl --eval '(asdf:make :zpcalc)' \
		--eval '(quit)'
	# for smaller executable size, use ECL
	# ecl --eval '(require :asdf)' \
	# 	--eval "(defpackage zpcalc (:use :cl) (:export #:main))" \
	# 	--eval "(asdf:make-build :zpcalc :type :program :epilogue-code '(progn (zpcalc:main) (quit)))" \
	# 	--eval '(quit)'

interactive:
	sbcl --eval '(asdf:load-system :zpcalc)'

test:
	sbcl --eval '(asdf:test-system :zpcalc)' \
		--eval '(quit)'
