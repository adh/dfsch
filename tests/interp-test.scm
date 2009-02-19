(require 'regex)

(define tests-passed 0)
(define tests-failed 0)

(define one-test-fail (and (= (vector-length *posix-argv*) 2)
                           (equal? (vector-ref *posix-argv* 1)
                                   "--strict")))

(define (exit-func)
  (print)
  (print "***** RESULTS: *****")
  (print "  Tests passed: " tests-passed)
  (if (> tests-failed 0)
      (print "\033[0;31m  Tests failed: " tests-failed "\033[0;39m")
      (print "  Tests failed: " tests-failed))
  (print "  ===========================")
  (print "  Tests total:  " (+ tests-passed tests-failed))
  (if (= tests-failed 0)
      (exit 0)
      (exit 'some-tests-failed)))


(define (test id exp val)
;;  (print (object->string val))
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

(define-macro (loop . code)
  `(do () (()) ,@code))

;;; Write tests here
;; in form like (test 'whetever1equals2 (= 1 2) true)
;;

(group "equivalence"
       
       (sub-group eqv?
                  
                  (test 'eqv? (eqv? 'a 'a) #t)
                  (test 'eqv?-proc 
                        (let ((p (lambda (x) x)))
                          (eqv? p p))
                        #t)
                  (test 'eqv?-nil
                        (eqv? () ())
                        #t)
                  (test 'eqv?-t
                        (eqv? () #t)
                        #f)


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
                        #f))
        
       (sub-group eq?
                  (test 'eq? (eq? 'a 'a) #t))

       (sub-group equal?

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
                  
                  (test 'gensyms
                        (eq? (gensym) (gensym))
                        #f)

                  (test 'id-equal?
                        (let ((a (gensym)))
                          (equal? (id a) (id a)))
                        #t)))

(group "arithmetics"

       (sub-group basic
                 (test 'arith0 (+ 3 4) 7)
                 (test 'arith1 (+ 3) 3)
                 (test 'arith2 (* 4) 4)
                 (test 'arith3 (*) 1)
                 
                 (test 'arith4 (- 3 4) -1)
                 (test 'arith5 (- 3 4 5) -6)
                 (test 'arith6 (- 3) -3)
                 (test 'arith7 (/ 3 4 5) (/ 3 20))
                 (test 'arith-int-div (/i 8 3) 2)
                 (test 'arith-mod-inv (mod-inv 3 11) 4))
       (sub-group bignum-related
                 (test 'integer-expt 
                       (integer-expt 1234567890123456789 
                                     789456123 
                                     102030405060708090)
                       67367859943785579)
                 (test 'big-sub 
                       (- 1234567890123456789012345678901234567890
                          1234567890123456789012345678901234567891)
                       -1)
                 (test 'big-mul
                       (* 65536 65536 65536 65536 65536 65536 65536 65536)
                       (* 256 256 256 256 256 256 256 256
                          256 256 256 256 256 256 256 256)))
       (sub-group bitwise-logic
                 (test 'logior (logior 123456789123456789 112233445566778899) 
                       125744530602155799)
                 (test 'logxor (logxor 112233445566778899 123456789123456789)
                       15798826514075910)
                 (test 'logand (logand 112233445566778899 123456789123456789)
                       109945704088079889)))


(group "control flow"
       
       (sub-group if
                 
                 (test 'if-true (if (> 3 2) 'yes 'no) 'yes)
                 (test 'if-false (if (< 3 2) 'yes 'no) 'no)
                 (test 'if-eval 
                       (if (> 3 2)
                           (- 3 2)
                           (+ 3 2))
                       1))

       (sub-group cond

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
                        2))

       (sub-group case

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
                        'consonant))
                  
       (sub-group (and or)

                  (test 'and-true (and (= 2 2) (> 2 1)) true)
                  (test 'and-false (and (= 2 2) (< 2 1)) ())
                  (test 'and-value (and 1 2 'c '(f g)) '(f g))
                  (test 'and-empty (and) true)
                  
                  (test 'or-true (or (= 2 2) (> 2 1)) true)
                  (test 'or-false (or (> 2 2) (< 2 1)) ()))
                  
       (sub-group do

                  (test 'do 
                        (do ((vec (make-vector 5))
                             (i 0 (+ i 1)))
                            ((= i 5) vec)
                          (vector-set! vec i i))
                        #(0 1 2 3 4)))
       (sub-group non-local-exits
                  (test 'catch
                        (catch 'foo
                               (catch 'bar
                                      (throw 'foo 'ok)
                                      'fail)
                               'fail)
                        'ok)
                  (test 'unwind-protect
                        (let ((x 'fail))
                          (catch 'foo 
                                 (unwind-protect 
                                  (throw 'foo 1) 
                                  (set! x 'ok)))
                          x)
                        'ok)))

(group "Binding constructs"

       (sub-group let
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
                       true))
       (sub-group destructuring-bind
                  (test 'simple
                        (destructuring-bind (a b c) '(1 2 3)
                                            b)
                        2)))

(group "functions"

       (define (fact x)
         (if (= x 0)
             1
             (* x (fact (- x 1)))))
       
       (test 'fact (fact 5) 120))

(group "vectors"

       (define v (make-vector 5))
       (vector-set! v 0 'foo)
       (test 'vector-base (vector-ref v 0) 'foo)
       (test 'vector-lit (vector-ref '#(0 1 2 3) 1) 1))

(group "strings"

       (test 'append (string-append "abc" "def") "abcdef")
       (test 'substring (substring "abcdef" 2 4) "cd")
       (test 'utf8-length (string-length "ěšč") 3)
       (test 'utf8-ref (string-ref "ab©" 2) 169)
       (test 'utf8->list (string->list "ab©") '(97 98 169))
       (test 'list->utf8 (list->string '(0x3042 0x3044 0x3046 
                                                0x3048 0x304a))
             "あいうえお")
       (test 'char-upcase (char-upcase 97) 65)
       (test 'titlecase (string-titlecase "foo bar") "Foo Bar")
       (test 'search (string-search "def" "abcdefgh") 3)
       (test 'search-ci (string-search-ci "děf" "abcDĚFgh") 3))

(group "some special cases"

       (test 'degenerated-list-qq `(,@'() . foo) 'foo))

(group "Regular expressions"
       (define r0 (regex:compile "[ab]+"))
       (define r0b (regex:compile "[ab]+" 'basic))
       (define r1 (regex:compile "^([^ ]+) +([^ ]+)$"))
       (test 'extended (regex:match? r0 "baba") #t)
       (test 'basic (regex:match? r0b "baba") #f)
       (test 'substring 
             (regex:substrings r1 "aaa bbb") 
             #(#(0 7 "aaa bbb") #(0 3 "aaa") #(4 7 "bbb")))
       (test 'substring-once 
             (regex:substrings-once "^([^ ]+) +([^ ]+)$" "aaa bbb") 
             #(#(0 7 "aaa bbb") #(0 3 "aaa") #(4 7 "bbb"))))

(group "Format"
       (test 'escaping (format "~~") "~")
       (test 'radix 
             (format "~2r ~:* ~8r ~:* ~10r ~:* ~16r" 123)
             "1111011  173  123  7b")
       (test 'write-char (format "~c" 0x3042) "あ")
       (test 'field-width (format "~15f" '(1 2 3 4)) "      (1 2 3 4)")
       (test 'floats (format "~10,5f" pi) "   3.14159"))

(when () group "object subsystem"
       (sub-group simple-class
                 (define-class <test-class> <object>)
                 (define-method <test-class> (test-dispatch self)
                   #t)
                 (define-method <test-class> (test-ivar-set self value)
                   (slot-set! self 'test-ivar value)
                   #t)
                 (define-method <test-class> (test-ivar-compare self value)
                   (eq? value (slot-ref self 'test-ivar)))
                 
                 (define inst (<test-class>))

                 (test 'type-introspection
                       (get-type inst)
                       <test-class>)
                 (test 'message-dispatch
                       (inst 'test-dispatch)
                       #t)
                 (test 'set-ivar
                       (inst 'test-ivar-set 'test-value)
                       #t)
                 (test 'ref-ivar
                       (inst 'test-ivar-compare 'test-value)
                       #t))
                       
                       

       )

;;; End of tests
;;
;; Print some statistics and exit apropriately
;;
(exit-func)    