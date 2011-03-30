;;; dfsch - Scheme-like Lisp dialect
;;;   Unit testing library
;;; Copyright (c) 2011 Ales Hakl
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(provide :dfsch-unit)
(require :cmdopts)
(require :os)

(define-package :dfsch-unit
  :uses '(:dfsch :cmdopts)
  :exports '(:<test>
             :<failing-test>
             :define-test
             :define-failing-test
             :test-expander
             :run-test
             :assert-equal
             :assert-true
             :assert-false
             :fail
             :run-tests
             :all-tests
             :run-all-tests
             :tests-in-category
             :test-toplevel))

(in-package :dfsch-unit)

(define-variable *tests* ())
(define-variable *test-categories* (make-identity-hash))

(define (print . args)
  (for-each (lambda (i) (display i)) args)
  (newline))

(define-class <test> ()
  ((:name :reader test-name 
          :initarg :name)
   (:categories :reader test-categories 
                :initarg :categories)
   (:proc :reader test-procedure 
          :initarg :proc)))

(define-class <failing-test> <test> ())

(define-method ((initialize-instance :after) (test <test>) &rest args)
  (set! *tests* (cons test *tests*))
  (for-each (lambda (category)
              (map-set! *test-categories* category
                        (cons test (map-ref *test-categories* category ()))))
            (test-categories test)))

(define (test-expander class name categories body)
  `(define ,name
     (letrec ((+this-test+ 
               (make-instance ',class
                              :name ',name
                              :categories ',categories
                              :proc (lambda () 
                                      (catch '+test-result+
                                        (let ((+fail-count+ 0)
                                              (+pass-count+ 0))
                                          ,@body
                                          (test-epilog))))))))))


(define-macro (define-test name categories &rest body)
  (test-expander <test> name categories body))

(define-macro (define-failing-test name categories &rest body)
  (test-expander <failing-test> name categories body))

(define-macro (test-print &rest args)
  `(print (test-name +this-test+) ": " ,@args))

(define-macro (test-epilog)
  `(begin
     (test-print +pass-count+ " passed " 
                 +fail-count+ " failed")
     (if (> +fail-count+ 0)
         :fail
         :pass)))

(define-macro (incr x)
  `(set! ,x (1+ ,x)))

(define-macro (fail &rest args)
  `(begin 
     (test-print "Assertion failed: " ,@args)
     (incr +fail-count+)))

(define-macro (pass)
  `(incr +pass-count+))

(define-macro (assert-equal expr expected)
  (with-gensyms (result exp)
    `(let ((,result ,expr)
           (,exp ,expected))
       (if (equal? ,result ,exp) 
           (pass)
           (fail (format "~s returned ~s expected ~s"
                         ',expr ,result ,exp))))))

(define-macro (assert-true expr)
  `(if ,expr 
       (pass)
       (fail (format "~s is not true"
                     ',expr))))

(define-macro (assert-false expr)
  `(if (not ,expr)
       (pass)
       (fail (format "~s is not false"
                     ',expr))))


(define-method (test-failed (test <failing-test>))
  (print (test-name test) ": \033[0;33mMAYFAIL\033[0;39m")
  :mayfail)

(define-method (test-failed (test <test>))
  (print (test-name test) ": \033[0;31mFAIL\033[0;39m")
  :fail)

(define-method (test-passed (test <test>))
  (print (test-name test) ": \033[0;32mPASS\033[0;39m")
  :pass)

(define-method (run-test (test <test>))
  (print "Running test:" (test-name test))
  (case ((test-procedure test))
    ((:fail) (test-failed test))
    ((:pass) (test-passed test))))

(define (run-tests list &key one-fail? trap-errors?)
  (let ((passed 0) (failed 0) (mayfail 0) (errors 0))
    (catch 'fail
      (for-each (lambda (test)
                  (multiple-value-bind (result err) 
                      (if trap-errors? 
                          (run-test test)
                          (ignore-errors (run-test test)))
                    (case result
                      ((:fail) 
                       (incr failed)
                       (when one-fail? (throw 'fail ())))
                      ((())
                       (print (test-name test) ": \033[0;31mERROR\033[0;39m: " err)
                       (incr failed)
                       (incr errors)
                       (when one-fail? (throw 'fail ())))
                      ((:mayfail)
                       (incr mayfail))
                      ((:pass)
                       (incr passed)))))
                list))
    (print "  ***** Test suite run complete *****")
    (print "Tests passed:            " passed)
    (when (> mayfail 0)
      (print "\033[0;33mTests failed (expected): " mayfail "\033[0;39m"))
    (if (> failed 0)
        (print "\033[0;31mTests failed:            " failed "\033[0;39m")
        (print "Tests failed:            " failed))
    (if (> failed 0)
        :fail
        :pass)))

(define (all-tests)
  (reverse *tests*))

(define (run-all-tests &key one-fail? trap-errors?)
  (run-tests (all-tests) :one-fail? one-fail? :trap-errors? trap-errors?))

(define (tests-in-category name)
  (let ((tests (map-ref *test-categories* name)))
    (unless tests
      (error "No such category" :name name))
    tests))

(define-macro (test-toplevel)
  '(when-toplevel
    (let ((one-fail? ())
          (trap-errors? ()))
      (let ((parser (cmdopts:make-parser)))
        (cmdopts:add-option parser  
                            (lambda (p v) 
                              (set! one-fail? #t))
                            :long-option "one-test-fail")
        (cmdopts:add-option parser  
                            (lambda (p v) 
                              (set! trap-errors? #t))
                            :long-option "trap-errors")
        (parse-list parser (cdr *posix-argv*)))
      (run-all-tests :one-fail? one-fail? :trap-errors? trap-errors?))))
