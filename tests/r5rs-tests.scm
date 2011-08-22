;; These test cases are examples from R5RS

(define-evaluation-test variables (:language :r5rs)
  (define x 28)
  (x ===> 28))

(define-evaluation-test literals (:language :r5rs)
  ((quote a)                             ===>  a)
  ((quote #(a b c))                      ===>  #(a b c))
  ((quote (+ 1 2))                       ===>  (+ 1 2))

  ('a                           ===>  a)
  ('#(a b c)                    ===>  #(a b c))
  ('()                          ===>  ())
  ('(+ 1 2)                     ===>  (+ 1 2))
  ('(quote a)                   ===>  (quote a))
  (''a                          ===>  (quote a))

  ('"abc"             ===>  "abc")
  ("abc"              ===>  "abc")
  ('145932            ===>  145932)
  (145932             ===>  145932)
  ('#t          ===>  #t)
  (#t           ===>  #t))

(define-evaluation-test procedures (:language :r5rs)
  ((+ 3 4)                           ===>  7)
  (((if #f + *) 3 4)                 ===>  12)

  (((lambda (x) (+ x x)) 4)          ===>  8)

  (define reverse-subtract
    (lambda (x y) (- y x)))
  ((reverse-subtract 7 10)           ===>  3)

  (define add4
    (let ((x 4))
      (lambda (y) (+ x y))))
  ((add4 6)                          ===>  10)

  (((lambda x x) 3 4 5 6)            ===>  (3 4 5 6))
  (((lambda (x y . z) z)
    3 4 5 6)                         ===>  (5 6)))

(define-evaluation-test conditionals (:language :r5rs)
  ((if (> 3 2) 'yes 'no)                   ===>  yes)
  ((if (> 2 3) 'yes 'no)                   ===>  no)
  ((if (> 3 2)
       (- 3 2)
       (+ 3 2))                            ===>  1)

  ((cond ((> 3 2) 'greater)
         ((< 3 2) 'less))                  ===>  greater)
  ((cond ((> 3 3) 'greater)
         ((< 3 3) 'less)
         (else 'equal))                    ===>  equal)
  ((cond ((assv 'b '((a 1) (b 2))) => cadr)
         (else #f))                        ===>  2)

  ((case (* 2 3)
     ((2 3 5 7) 'prime)
     ((1 4 6 8 9) 'composite))             ===>  composite)
  ((case (car '(c d))
     ((a) 'a)
     ((b) 'b))                             ===>  ())  ; implementation defined
  ((case (car '(c d))
     ((a e i o u) 'vowel)
     ((w y) 'semivowel)
     (else 'consonant))                    ===>  consonant)

  ((and (= 2 2) (> 2 1))                   ===>  #t)
  ((and (= 2 2) (< 2 1))                   ===>  #f)
  ((and 1 2 'c '(f g))                     ===>  (f g))
  ((and)                                   ===>  #t)

  ((or (= 2 2) (> 2 1))                    ===>  #t)
  ((or (= 2 2) (< 2 1))                    ===>  #t)
  ((or #f #f #f)                           ===>  #f)
  ((or (memq 'b '(a b c)) 
       (/ 3 0))                            ===>  (b c))

  ;; FIXME: I'm not sure whether this should fail or not, but current
  ;; behavior is clearly incorrect 
  ;; ((let ((=> #f)) (cond (#t => 'ok)))       ===> ok)
  )

(define-evaluation-test assignment (:language :r5rs)
  (define x 2)
  ((+ x 1)                         ===>  3)
  ((set! x 4)                      ===>  4) ; implementation defined
  ((+ x 1)                         ===>  5))

(define-evaluation-test binding-constructs (:language :r5rs)
  ;; let
  ((let ((x 2) (y 3))
     (* x y))                              ===>  6)
  ((let ((x 2) (y 3))
     (let ((x 7)
           (z (+ x y)))
       (* z x)))                           ===>  35)

  ;; let*
  ((let ((x 2) (y 3))
     (let* ((x 7)
            (z (+ x y)))
       (* z x)))                           ===>  70)

  ;; letrec
  ((letrec ((evenp
             (lambda (n)
               (if (zero? n)
                   #t
                   (oddp (- n 1)))))
            (oddp
             (lambda (n)
               (if (zero? n)
                   #f
                   (evenp (- n 1))))))
     (evenp 88))                           ===>  #t))

(define-evaluation-test sequencing-and-iteration (:language :r5rs)
  (define x 0)

  ((begin (set! x 5)
          (+ x 1))                          ===>  6)
  
  ((do ((vec (make-vector 5))
        (i 0 (+ i 1)))
       ((= i 5) vec)
     (vector-set! vec i i))                  ===>  #(0 1 2 3 4))

  ((let ((x '(1 3 5 7 9)))
     (do ((x x (cdr x))
          (sum 0 (+ sum (car x))))
         ((null? x) sum)))                     ===>  25)

  ((let looop ((numbers '(3 -2 1 6 -5))  ; loop is predefined macro
               (nonneg '())
               (neg '()))
     (cond ((null? numbers) (list nonneg neg))
           ((>= (car numbers) 0)
            (looop (cdr numbers)
                   (cons (car numbers) nonneg)
                   neg))
           ((< (car numbers) 0)
            (looop (cdr numbers)
                   nonneg
                   (cons (car numbers) neg))))) 
   ===>  ((6 1 3) (-5 -2))))

(define-evaluation-test quasiquotation (:language :r5rs)
  (`(list ,(+ 1 2) 4)                   ===>  (list 3 4))
  ((let ((name 'a)) 
     `(list ,name ',name))              ===>  (list a (quote a)))
  (`(a ,(+ 1 2) 
       ,@(map abs '(4 -5 6)) b)         ===>  (a 3 4 5 6 b))
  (`(( foo ,(- 10 3)) 
     ,@(cdr '(c)) . ,(car '(cons)))     ===>  ((foo 7) . cons))

  ;; dfsch's quasiquote does not recurse into vectors (yet?)
  ;;(`#(10 5 ,(sqrt 4) 
  ;;       ,@(map sqrt '(16 9)) 8)        ===>  #(10 5 2 4 3 8))
  
  (`(a `(b ,(+ 1 2) 
           ,(foo ,(+ 1 3) d) e) f)      ===>  (a `(b ,(+ 1 2) ,(foo 4 d) e) f))
  ((let ((name1 'x)
         (name2 'y))
     `(a `(b ,,name1 ,',name2 d) e))    ===>  (a `(b ,x ,'y d) e))
  
  ((quasiquote (list (unquote (+ 1 2)) 
                     4))                ===>  (list 3 4))
  ('(quasiquote (list (unquote (+ 1 2)) 
                      4))               ===>  `(list ,(+ 1 2) 4)))


(define-evaluation-test definitions (:language :r5rs)
  (define add3
    (lambda (x) (+ x 3)))
  ((add3 3)                                    ===>  6)
  (define first car)
  ((first '(1 2))                              ===>  1)
  ((let ((x 5))
     (define foo (lambda (y) (bar x y)))
     (define bar (lambda (a b) (+ (* a b) a)))
     (foo (+ x 3)))                            ===>  45)

  ;; for completeness:
  ((let ((x 5))
     (letrec ((foo (lambda (y) (bar x y)))
              (bar (lambda (a b) (+ (* a b) a))))
       (foo (+ x 3))))                         ===> 45))

(define-evaluation-test equivalence-predicates (:language :r5rs)
  ((eqv? 'a 'a)                             ===>  #t)
  ((eqv? 'a 'b)                             ===>  #f)
  ((eqv? 2 2)                               ===>  #t)
  ((eqv? '() '())                           ===>  #t)
  ((eqv? 100000000 100000000)               ===>  #t)
  ((eqv? (cons 1 2) (cons 1 2))             ===>  #f)
  ((eqv? (lambda () 1)
         (lambda () 2))                     ===>  #f)
  ((eqv? #f 'nil)                           ===>  #f)
  ((let ((p (lambda (x) x)))
     (eqv? p p))                            ===>  #t)

  (define gen-counter
    (lambda ()
      (let ((n 0))
        (lambda () (set! n (+ n 1)) n))))
  ((let ((g (gen-counter)))
     (eqv? g g))                            ===>  #t)
  ((eqv? (gen-counter) (gen-counter))       ===>  #f)
  
  (define gen-loser
    (lambda ()
      (let ((n 0))
        (lambda () (set! n (+ n 1)) 27))))
  ((let ((g (gen-loser)))
     (eqv? g g))                            ===>  #t)
  ((eqv? (gen-loser) (gen-loser))           ===>  #f) ; implementation defined

  ((let ((x '(a)))
     (eqv? x x))                            ===>  #t)

  ((eq? 'a 'a)                             ===>  #t)
  ((eq? (list 'a) (list 'a))               ===>  #f)
  ((eq? '() '())                           ===>  #t)
  ((eq? 2 2)                               ===>  #t) ; implementation defined 
  ((eq? #\A #\A)                           ===>  #t) ; (for fixnums only)
  ((eq? car car)                           ===>  #t)
  ((let ((n (+ 2 3)))
     (eq? n n))              ===>  #t) ; implementation defined
  ((let ((x '(a)))
     (eq? x x))              ===>  #t)
  ((let ((x '#()))
     (eq? x x))              ===>  #t)
  ((let ((p (lambda (x) x)))
     (eq? p p))              ===>  #t)

  ((equal? 'a 'a)                          ===>  #t)
  ((equal? '(a) '(a))                      ===>  #t)
  ((equal? '(a (b) c)
           '(a (b) c))                     ===>  #t)
  ((equal? "abc" "abc")                    ===>  #t)
  ((equal? 2 2)                            ===>  #t)
  ((equal? (make-vector 5 'a)
           (make-vector 5 'a))             ===>  #t)
  ((equal? (lambda (x) x)
           (lambda (y) y)) ===> #f))    ; implementation defined 
                                        ; (lambdas are never equal)

(define-evaluation-test numbers (:language :r5rs)
  ((+ 3 4)                         ===>  7)
  ((+ 3)                           ===>  3)
  ((+)                             ===>  0)
  ((* 4)                           ===>  4)
  ((*)                             ===>  1)

  ((- 3 4)                         ===>  -1)
  ((- 3 4 5)                       ===>  -6)
  ((- 3)                           ===>  -3)
  ((/ 3 4 5)                       ===>  3/20)
  ((/ 3)                           ===>  1/3)

  ((gcd 32 -36)                    ===>  4)
  ((gcd)                           ===>  0)
  ((lcm 32 -36)                    ===>  288)
  ;;((lcm 32.0 -36)                  ===>  288.0)  ; unimplemented
  ((lcm)                           ===>  1)

  ;; TODO: accessors
  ;;((numerator (/ 6 4))          ===>  3)
  ;;((denominator (/ 6 4))          ===>  2)

  ((floor -4.3)                  ===>  -5.0)
  ((ceiling -4.3)                ===>  -4.0)
  ((truncate -4.3)               ===>  -4.0)
  ((round -4.3)                  ===>  -4.0)
  
  ((floor 3.5)                   ===>  3.0)
  ((ceiling 3.5)                 ===>  4.0)
  ((truncate 3.5)                ===>  3.0)
  ((round 3.5)                   ===>  4.0)
  
  ((round 7/2)                   ===>  4.0) ; should be probably exact

  ((string->number "100")                ===>  100)
  ((string->number "100" 16)             ===>  256)
  ((string->number "1e2")                ===>  100.0)
  )

(define-evaluation-test pairs-and-lists (:language :r5rs)
  (define x (list 'a 'b 'c))
  (define y x)
  (y                               ===>  (a b c))
  ((list? y)                       ===>  #t)
  ((set-cdr! x 4)                  ===>  (a . 4)) ; implementation defined
  (x                               ===>  (a . 4))
  ((eqv? x y)                      ===>  #t)
  (y                               ===>  (a . 4))
  ((list? y)                       ===>  #f)
  (set-cdr! x x)
  ((list? x)                       ===>  #f)

  ((pair? '(a . b))                ===>  #t)
  ((pair? '(a b c))                ===>  #t)
  ((pair? '())                     ===>  #f)
  ((pair? '#(a b))                 ===>  #f)

  ((cons 'a '())                   ===>  (a))
  ((cons '(a) '(b c d))            ===>  ((a) b c d))
  ((cons "a" '(b c))               ===>  ("a" b c))
  ((cons 'a 3)                     ===>  (a . 3))
  ((cons '(a b) 'c)                ===>  ((a b) . c))


  ((car '(a b c))                  ===>  a)
  ((car '((a) b c d))              ===>  (a))
  ((car '(1 . 2))                  ===>  1)
  (assert-error <error> (car '()))

  ((cdr '((a) b c d))              ===>  (b c d))
  ((cdr '(1 . 2))                  ===>  2)
  (assert-error <error> (cdr '()))

  ((list? '(a b c))             ===>  #t)
  ((list? '())                  ===>  #t)
  ((list? '(a . b))             ===>  #f)
  ((let ((x (list 'a)))
     (set-cdr! x x)
     (list? x))                 ===>  #f)

  ((list 'a (+ 3 4) 'c)                    ===>  (a 7 c))
  ((list)                                  ===>  ())

  ((length '(a b c))                       ===>  3)
  ((length '(a (b) (c d e)))               ===>  3)
  ((length '())                            ===>  0)

  ((append '(x) '(y))                      ===>  (x y))
  ((append '(a) '(b c d))                  ===>  (a b c d))
  ((append '(a (b)) '((c)))                ===>  (a (b) (c)))

  ((append '(a b) '(c . d))                ===>  (a b c . d))
  ((append '() 'a)                         ===>  a)

  ((reverse '(a b c))                      ===>  (c b a))
  ((reverse '(a (b c) d (e (f))))  
   ===>  ((e (f)) d (b c) a))

  ((list-ref '(a b c d) 2)                         ===>  c)

  ((memq 'a '(a b c))                  ===>  (a b c))
  ((memq 'b '(a b c))                  ===>  (b c))
  ((memq 'a '(b c d))                  ===>  #f)
  ((memq (list 'a) '(b (a) c))         ===>  #f)
  ((member (list 'a)
           '(b (a) c))                  ===>  ((a) c))
  ((memq 101 '(100 101 102))           ===>  (101 102)) ; implementation defined
  ((memv 101 '(100 101 102))           ===>  (101 102))

  (define e '((a 1) (b 2) (c 3)))
  ((assq 'a e)                       ===>  (a 1))
  ((assq 'b e)                       ===>  (b 2))
  ((assq 'd e)                       ===>  #f)
  ((assq (list 'a) 
         '(((a)) ((b)) ((c))))       ===>  #f)
  ((assoc (list 'a) 
          '(((a)) ((b)) ((c))))      ===>  ((a)))
  ((assq 5 '((2 3) (5 7) (11 13)))   ===>  (5 7)) ; implementation defined
  ((assv 5 '((2 3) (5 7) (11 13)))   ===>  (5 7)))

(define-evaluation-test vectors (:language :r5rs)
  ((vector 'a 'b 'c)                       ===>  #(a b c))
  ((vector-ref '#(1 1 2 3 5 8 13 21)
               5)                          ===>  8)

  ((let ((vec (vector 0 '(2 2 2 2) "Anna")))
     (vector-set! vec 1 '("Sue" "Sue"))
     vec)                                  ===>  #(0 ("Sue" "Sue") "Anna"))

  ((vector->list '#(dah dah didah))  ===>  (dah dah didah))
  ((list->vector '(dididit dah))     ===>  #(dididit dah)))

(define-evaluation-test control-features (:language :r5rs)
  ((apply + (list 3 4))                      ===>  7)
  
  (define compose
    (lambda (f g)
      (lambda args
        (f (apply g args)))))
  
  (((compose sqrt *) 12 75)                  ===>  30.0)

  ((map cadr '((a b) (d e) (g h)))           ===>  (b e h))

  ((map (lambda (n) (expt n n))
        '(1 2 3 4 5))                        ===>  (1.0 4.0 27.0 256.0 3125.0))

  ((map + '(1 2 3) '(4 5 6))                 ===>  (5 7 9))

  ((let ((count 0))
     (map (lambda (ignored)
            (set! count (+ count 1))
            count)
          '(a b)))                         ===>  (1 2))

  ((let ((v (make-vector 5)))
     (for-each (lambda (i)
                 (vector-set! v i (* i i)))
               '(0 1 2 3 4))
     v)                                        ===>  #(0 1 4 9 16)))

