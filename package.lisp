;;;; package.lisp

(defpackage #:calcu-core
  (:use #:cl)
  (:export #:make-calculator
	   #:calculator-x
	   #:calculator-mem
	   #:calculator-error-p
	   #:displayed-value
	   #:reset-calculator
	   #:reset-input
	   #:feed-char
	   #:feed-pm
	   #:feed-unary-operation
	   #:feed-binary-operation
	   #:compute
	   #:m
	   #:mr
	   #:mc
	   #:m+
	   #:m-
	   #:c
	   #:*default-capacity*
	   #:*unary-operations
	   #:*normal-binary-operations
	   #:*percent-binary-operations*
	   #:*text-commands*
	   #:text-calculator))
