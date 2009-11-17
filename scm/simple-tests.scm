(provide 'simple-tests)
(require 'cmdopts)

(define-package :simple-tests :dfsch :cmdopts)
(in-package :simple-tests)

(define tests-passed 0)
(define tests-failed 0)

(define one-test-fail ())

(define (print . args)
  (for-each (lambda (i) (display i)) args)
  (newline))

(when (defined? *posix-argv*)
      (let ((parser (cmdopts:make-parser)))
        (cmdopts:add-option parser "one-test-fail" 
                            (lambda (p v) 
                              (set! one-test-fail #t)
                              (print "Running in strict mode")))
        (cmdopts:parse-list parser (cdr *posix-argv*))))


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
      (unix:exit 0)
      (unix:exit fail-status)))


(define (test id exp val)
  (if (equal? exp val)
      (begin 
        (print "   Test passed: \033[0;32m" id "\033[0;39m")
        (set! tests-passed (+ tests-passed 1)))
      (begin
        (print "\033[0;31m!!\033[0;39m Test failed: \033[0;31m" id 
               "\033[0;39m was: " (object->string exp) 
               " should be: " (object->string val))
        (set! tests-failed (+ tests-failed 1))
        (when one-test-fail
              (print "*** Test failed -- ABORTING ***")
              (exit-func)))))

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
