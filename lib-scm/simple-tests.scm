(provide :simple-tests)
(require :cmdopts)
(require :os)
(define-package :simple-tests :uses '(:dfsch :cmdopts))
(in-package :simple-tests)

(define tests-passed 0)
(define tests-failed 0)

(define one-test-fail ())

(define (print . args)
  (for-each (lambda (i) (display i)) args)
  (newline))

(when (defined? *posix-argv*)
      (let ((parser (make-parser)))
        (add-option parser  
                    (lambda (p v) 
                      (set! one-test-fail #t)
                      (print "Running in strict mode"))
                    :long-option "one-test-fail")
        (parse-list parser (cdr *posix-argv*))))


(define (exit-func fail-status)
  (print)
  (print "***** RESULTS: *****")
  (print "  Tests passed: " tests-passed)
  (if (> tests-failed 0)
      (print "\033[0;31m  Tests failed: " tests-failed "\033[0;39m")
      (print "  Tests failed: " tests-failed))
  (print "  ===========================")
  (print "  Tests total:  " (+ tests-passed tests-failed))
  (if (= tests-failed 0)
      (os:exit 0)
      (os:exit fail-status)))


(define (%test-pass id)
  (print "   Test passed: \033[0;32m" id "\033[0;39m")
  (set! tests-passed (+ tests-passed 1)))
(define (%test-fail id fmt &rest args)
  (print "\033[0;31m!!\033[0;39m Test failed: \033[0;31m" id 
         "\033[0;39m " 
         (apply format fmt args))
  (set! tests-failed (+ tests-failed 1))
  (when one-test-fail
        (print "*** Test failed -- ABORTING ***")
        (exit-func)))

(define (test id exp val)
  (if (equal? exp val)
      (%test-pass id)
      (%test-fail id "was: ~s should be: ~s"
                  exp val)))

(define-macro (test-error id &body body)
  `(if (cdr (detect-errors ,@body))
       (%test-pass ,id)
       (%test-fail ,id "Should signal error")))


(define (group-generator indent separator name statements)
  (define tmp-passed (gensym))
  (define tmp-failed (gensym))
  `(begin
     (print ',indent ',separator " " ',name " " ',separator)
     (let ((,tmp-passed tests-passed)
	   (,tmp-failed tests-failed))
       ,@statements
       (let ((passed (- tests-passed ,tmp-passed))
	     (failed (- tests-failed ,tmp-failed)))
	 (print ',indent ',separator " [passed: " passed " failed: " failed 
		" out of " (+ passed failed) "] " ',separator)))))
  
(define-macro (group name .  statements)
  (group-generator "" "=====" name statements))

(define-macro (sub-group name . statements) 
  (group-generator " " "----" name statements))

(define-macro (ignore . code)
  ())
