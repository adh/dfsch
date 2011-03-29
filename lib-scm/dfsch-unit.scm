(provide :dfsch-unit)
(require :cmdopts)
(require :os)

(define-package :dfsch-unit
  :uses '(:dfsch :cmdopts)
  :exports '(:<test>
             :define-test
             :run-test
             :assert-equal
             :assert-true
             :assert-false
             :fail
             :run-tests
             :run-all-tests))

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
          :initarg :proc)
   (:mayfail? :reader test-mayfail?)))

(define-method ((initialize-instance :after) (test <test>) &rest args)
  (set! *tests* (cons test *tests*))
  (for-each (lambda (category)
              (map-set! *test-categories* category
                        (cons test (map-ref *test-categories* category ()))))
            (test-categories test)))

(define-macro (define-test name categories &rest body)
  `(define ,name
     (letrec ((+this-test+ 
               (make-instance <test> 
                              :name ',name
                              :categories ',categories
                              :proc (lambda () 
                                      (catch '+test-result+
                                        (let ((+fail-count+ 0)
                                              (+pass-count+ 0))
                                          ,@body
                                          (test-epilog))))))))))
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


(define-method (test-failed (test <test>))
  (if (test-mayfail? test)
      (begin
        (print (test-name test) ": \033[0;33mMAYFAIL\033[0;39m")
        :mayfail)
      (begin
        (print (test-name test) ": \033[0;31mFAILED\033[0;39m")
        :fail)))

(define-method (test-passed (test <test>))
  (print (test-name test) ": \033[0;32mPASSED\033[0;39m")
  :pass)

(define-method (run-test (test <test>))
  (print "Running test:" (test-name test))
  (case ((test-procedure test))
    ((:fail) (test-failed test))
    ((:pass) (test-passed test))))

(define (run-tests list &key one-fail?)
  (let ((passed 0) (failed 0) (mayfail 0))
    (catch 'fail
      (for-each (lambda (test)
                  (case (run-test test)
                    ((:fail) 
                     (incr failed)
                     (when one-fail? (throw 'fail)))
                    ((:mayfail)
                     (incr mayfail))
                    ((:pass)
                     (incr passed))))
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

(define (run-all-tests)
  (run-tests *tests*))