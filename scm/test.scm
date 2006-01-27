(define tests-passed 0)
(define tests-failed 0)

(define one-test-fail (and (= (vector-length argv) 2)
                           (= (vector-ref argv 1)
                              "--strict")))


(define (test id exp val)
  (if (equal? exp val)
      (begin 
        (print 'Test 'passed: id)
        (set! tests-passed (+ tests-passed 1)))
      (begin
        (print 'Test 'failed: id 'was: exp 'shouldBe: val)
        (set! tests-failed (+ tests-failed 1)))))

(define (delimiter) 
  (print '===========================))

(define (cadr list) 
  (car (cdr list))) ;; We dont have this (yet??)

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

(delimiter)

;; Control flow

(test 'ifTrue (if (> 3 2) 'yes 'no) 'yes)
(test 'ifFalse (if (< 3 2) 'yes 'no) 'no)
(test 'ifEval 
      (if (> 3 2)
          (- 3 2)
          (+ 3 2))
      1)

(delimiter)

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
            (else ()))
      2)
(delimiter)
(test 'caseSimple
      (case (* 2 3)
        ((2 3 5 7) 'prime)
        ((1 4 6 8 9) 'composite))
      'composite)
(test 'caseElse
      (case (car '(c d))
        ((a e i o u) 'vowel)
        ((w y) 'semivowel)
        (else 'consonant))
      'consonant)

(delimiter)

(test 'andTrue (and (= 2 2) (> 2 1)) true)
(test 'andFalse (and (= 2 2) (< 2 1)) ())
(test 'andValue (and 1 2 'c '(f g)) '(f g))
(test 'andEmpty (and) true)

(delimiter)

(test 'orTrue (and (= 2 2) (> 2 1)) true)
(test 'orFalse (and (= 2 2) (< 2 1)) ())


(delimiter)
;; Binding constructs

(test 'let
      (let ((x 2) (y 3))
        (let ((x 7)
              (z (+ x y)))
          (* z x)))   
      35)
(test 'letrec
      (letrec ((even?
                (lambda (n)
                  (if (= n 0)
                      true
                      (odd? (- n 1)))))
               (odd?
                (lambda (n)
                  (if (= n 0)
                      ()
                      (even? (- n 1))))))
        (even? 88))
        true)

(delimiter)

;; Functions

(define (fact x)
  (if (= x 0)
      1
      (* x (fact (- x 1)))))

(test 'fact (fact 5) 120)
(delimiter)

;; Vectors

(let ((v (make-vector 5)))
  (vector-set! v 0 'foo)
  (test 'vectorBase (vector-ref v 0) 'foo)
  (test 'vectorLit (vector-ref '#(0 1 2 3) 1) 1))

(delimiter)

;; Some special cases:

(test 'degeneratedList `(,@'() . foo) 'foo)

;;; End of tests
;;
;; Print some statistics and exit apropriately
;;

(print)
(print 'Tests 'passed: tests-passed)
(print 'Tests 'failed: tests-failed)
(print '===========================)
(print 'Tests 'total: (+ tests-passed tests-failed))
(if (= tests-failed 0)
    (exit 0)
    (exit 'some-tests-failed))
    