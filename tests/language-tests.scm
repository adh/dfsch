(define-test test-eq? (:language :equality)
  (assert-true (eq? 'a 'a))
  (assert-false (eq? 'a 'b))
  (assert-false (eq? (gensym) (gensym))))

(define-test test-equal? (:language :equality)
  (assert-true (equal? '(1 2 3) '(1 2 3)))
  (assert-false (equal? '(1 2 3) '(1 2 5)))
  (assert-true (equal? "foo" "foo"))
  (assert-false (equal? "foo" "bar"))
  (assert-true (equal? #(1 2 3) #(1 2 3)))
  (assert-true (equal? '(1 2 ((3) 4 5)) '(1 2 ((3) 4 5))))
  (assert-false (equal? '(1 2 ((3) 4 5)) '(1 2 ((7) 4 5)))))

(define-test test-eqv? (:language :equality)
  (assert-true (eqv? 1.34 1.34)))

(define-test fixnum-arithmetics (:language :numbers)
  (assert-equal (+ 3 4) 7)
  (assert-equal (+ 3) 3)
  (assert-equal (* 4) 4)
  (assert-equal (*) 1)
  
  (assert-equal (- 3 4) -1)
  (assert-equal (- 3 4 5) -6)
  (assert-equal (- 3) -3)
  (assert-equal (/ 3 4 5) (/ 3 20))
  (assert-equal (/i 8 3) 2)
  (assert-equal (mod-inv 3 11) 4)
  
  (assert-true (< 0 (+ most-positive-fixnum 1)))
  (assert-true (> 0 (- most-negative-fixnum 1))))

(define-test bignum-arithmetics (:language :numbers)
  (assert-equal (integer-expt 1234567890123456789 
                              789456123 
                              102030405060708090)
                67367859943785579)
  (assert-equal (- 1234567890123456789012345678901234567890
                   1234567890123456789012345678901234567891)
                -1)
  (assert-equal (* 65536 65536 65536 65536 65536 65536 65536 65536)
                (* 256 256 256 256 256 256 256 256
                   256 256 256 256 256 256 256 256))
  (assert-equal (logior 123456789123456789 112233445566778899) 
                125744530602155799)
  (assert-equal (logxor 112233445566778899 123456789123456789)
                15798826514075910)
  (assert-equal (logand 112233445566778899 123456789123456789)
                109945704088079889)
  (assert-equal (>> 3523532227357930030104576 54) 
                195595330))

(define-test nonlocal-exits (:language :control)
  (assert-true (catch 'foo
                      (catch 'bar
                             (throw 'foo #t)
                             #f)
                      #f))
  (assert-true (let ((x #f))
                 (catch 'foo 
                        (unwind-protect 
                         (throw 'foo 1) 
                         (set! x #t)))
                 x)))

(define-test write->read (:language :reader :writer)
  (define bn (random-bignum 1024))
  (define string (random-bytes 512))
  (assert-equal (string->object (object->string bn)) bn)
  (assert-equal (string->object (object->string string))  string))

(define-test lambda-keywords (:language :functions)
  (define (opt-arg-fun &optional a (b 2) (c 3 c-supplied))
    (list a b c c-supplied))
  (assert-equal (opt-arg-fun ) '(() 2 3 ()))
  (assert-equal (opt-arg-fun 'a) '(a 2 3 ()))
  (assert-equal (opt-arg-fun 'a 'b) '(a b 3 ()))
  (assert-equal (opt-arg-fun 'a 'b 'c) '(a b c true))
  
  (define (key-arg-fun &key a (b 2) (c 3 c-supplied))
    (list a b c c-supplied))
  (assert-equal (key-arg-fun ) '(() 2 3 ()))
  (assert-equal (key-arg-fun :c 9) '(() 2 9 true))
  
  (define (key-rest-arg-fun &rest r &key a (b 2) (c 3 c-supplied))
    (list r a b c c-supplied))
  (assert-equal (key-rest-arg-fun :c 9) '((:c 9) () 2 9 true)))
