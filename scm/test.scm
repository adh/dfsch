(define tests-passed 0)
(define tests-failed 0)

(define one-test-fail (and (= (vector-length argv) 2)
                           (equal? (vector-ref argv 1)
                                   "--strict")))

(define (exit-func)
  (print)
  (print "***** RESULTS: *****")
  (print "  Tests passed: " tests-passed)
  (print "  Tests failed: " tests-failed)
  (print "  ===========================")
  (print "  Tests total: " (+ tests-passed tests-failed))
  (if (= tests-failed 0)
      (exit 0)
      (exit 'some-tests-failed)))


(define (test id exp val)
  (if (equal? exp val)
      (begin 
        (print "   Test passed: " id)
        (set! tests-passed (+ tests-passed 1)))
      (begin
        (print "!! Test failed: " id " was: " exp " should be: " val)
        (set! tests-failed (+ tests-failed 1))
        (if one-test-fail (begin
                            (print "*** Test failed -- ABORTING ***")
                            (exit-func)) ()))))

(define (group name) 
  (print "===== " name " ====="))

(define (sub-group name) 
  (print " --- " name " ---"))


(define (cadr list) 
  (car (cdr list))) ;; We dont have this (yet??)

;;; Write tests here
;; in form like (test 'whetever1equals2 (= 1 2) true)
;;

(group "equivalence")

(sub-group 'eqv?)

(test 'eqv? (eqv? 'a 'a) #t)
(test 'eqv?-proc 
      (let ((p (lambda (x) x)))
        (eqv? p p))
      #t)

(define gen-counter
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) n))))

(test 'proc-ret-0
      (let ((g (gen-counter)))
        (eqv? g g)) 
      #t)
(test 'proc-ret-1
      (eqv? (gen-counter) (gen-counter))
      #f)
        
(sub-group 'eq?)

(test 'eq? (eq? 'a 'a) #t)

(sub-group 'equal?)

(test 'equal?-string 
      (equal? "abc" "abc")
      #t)

(test 'equal?-vector
      (equal? (make-vector 5 'a)
              (make-vector 5 'a))
      #t)

(test 'equal?-fail
      (equal? 1 2)
      #f)


(group "arithmetics")

(test 'arith0 (+ 3 4) 7)
(test 'arith1 (+ 3) 3)
(test 'arith2 (* 4) 4)
(test 'arith3 (*) 1)

(test 'arith4 (- 3 4) -1)
(test 'arith5 (- 3 4 5) -6)
(test 'arith6 (- 3) -3)
(test 'arith7 (/ 3 4 5) (/ 3 20))
(test 'arith-int-div (/i 8 3) 2)

(group "control flow")

(test 'if-true (if (> 3 2) 'yes 'no) 'yes)
(test 'if-false (if (< 3 2) 'yes 'no) 'no)
(test 'if-eval 
      (if (> 3 2)
          (- 3 2)
          (+ 3 2))
      1)

(sub-group 'cond)

(test 'cond-simple 
      (cond ((> 3 2) 'greater)
            ((< 3 2) 'less))
      'greater)
(test 'cond-else
      (cond ((> 3 3) 'greater)
            ((< 3 3) 'less)
            (else 'equal))
      'equal)
(test 'cond-alt
      (cond ((assoc 'b '((a 1) (b 2))) => cadr)
            (else #f))
      2)

(sub-group 'case)

(test 'case-simple
      (case (* 2 3)
        ((2 3 5 7) 'prime)
        ((1 4 6 8 9) 'composite))
      'composite)
(test 'case-else
      (case (car '(c d))
        ((a e i o u) 'vowel)
        ((w y) 'semivowel)
        (else 'consonant))
      'consonant)

(sub-group '(and or))

(test 'and-true (and (= 2 2) (> 2 1)) true)
(test 'and-false (and (= 2 2) (< 2 1)) ())
(test 'and-value (and 1 2 'c '(f g)) '(f g))
(test 'and-empty (and) true)

(test 'or-true (and (= 2 2) (> 2 1)) true)
(test 'or-false (and (= 2 2) (< 2 1)) ())

(sub-group 'do)

(test 'do 
      (do ((vec (make-vector 5))
           (i 0 (+ i 1)))
          ((= i 5) vec)
        (vector-set! vec i i))
      #(0 1 2 3 4))

(group "Binding constructs")

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

(group "functions")

(define (fact x)
  (if (= x 0)
      1
      (* x (fact (- x 1)))))

(test 'fact (fact 5) 120)

(group "vectors")

(let ((v (make-vector 5)))
  (vector-set! v 0 'foo)
  (test 'vector-base (vector-ref v 0) 'foo)
  (test 'vector-lit (vector-ref '#(0 1 2 3) 1) 1))

(group "some special cases")

(test 'degenerated-list-qq `(,@'() . foo) 'foo)


;;; End of tests
;;
;; Print some statistics and exit apropriately
;;

(exit-func)    