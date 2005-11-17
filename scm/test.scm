(define tests-passed 0)
(define tests-failed 0)

(define (test id exp val)
  (if (= exp val)
      (begin 
        (print 'Test 'passed: id)
        (set! tests-passed (+ tests-passed 1)))
      (begin
        (print 'Test 'failed: id 'was: exp 'shouldBe: val)
        (set! tests-failed (+ tests-failed 1)))))

;;; Write tests here
;; in form like (test 'whetever1equals2 (+ 1 2) true)
;;


;; Arithmetics

(test 'arith0 (+ 3 4) 7)
(test 'arith1 (+ 3) 3)
(test 'arith2 (* 4) 4)
(test 'arith3 (*) 1)

(test 'arith4 (- 3 4) -1)
(test 'arith5 (- 3 4 5) -6)
(test 'arith6 (- 3) -3)
(test 'arith7 (/ 3 4 5) (/ 3 20))


;; Control flow

(test 'ifTrue (if (> 3 2) 'yes 'no) 'yes)
(test 'ifFalse (if (< 3 2) 'yes 'no) 'no)
(test 'ifEval 
      (if (> 3 2)
          (- 3 2)
          (+ 3 2))
      1)

(test 'condSimple 
      (cond ((> 3 2) 'greater)
            ((< 3 2) 'less))
      'greater)
(test 'condElse
      (cond ((> 3 3) 'greater)
            ((< 3 3) 'less)
            (else 'equal))
      'equal)
(test 'condAlt
      (cond ((assoc 'b '((a 1) (b 2))) => cadr)
            (else #f))
      2)

;;; End of tests
;;
;; Print some statistics and exit apropriately
;;

(print 'Tests 'passed: tests-passed)
(print 'Tests 'failed: tests-failed)
(print '===========================)
(print 'Tests 'total: (+ tests-passed tests-failed))
(if (= tests-passed 0)
    