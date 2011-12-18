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

(define-test bitwise-logic (:language :numbers)
  (assert-true (logtest 1 7))
  (assert-false (logtest 1 2))
  (assert-true (logtest -2 -1))
  (assert-false (logtest 0 -1))

  (assert-equal (logior 1 2 4 8) 15)
  (assert-equal (logxor 1 3 7 15) 10)
  (assert-equal (logand 16 31) 16)
  (assert-equal (lognot 0) -1)
  (assert-equal (lognot (1+ (lognot 1000))) 999))


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

(define-test destructuring-bind (:language)
  (assert-equal (destructuring-bind (a b c) '(1 2 3)
                                    b)
                2))  

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

(define-test vectors (:language :vectors :collections)
  (define v (make-vector 5))
  (vector-set! v 0 'foo)
  (assert-equal (vector-ref v 0) 'foo)
  (assert-equal (vector-ref '#(0 1 2 3) 1) 1))
  

(define-test string-handling (:language :strings)
  (assert-equal (string-append "abc" "def") "abcdef")
  (assert-equal (substring "abcdef" 2 4) "cd")
  (assert-equal (string-search "def" "abcdefgh") 3)
  (assert-equal (string-split-on-byte "a b,,c" ", ")
                '("a" "b" "c"))
  (assert-equal (string->byte-list "æ©") '(195 166 194 169)))

(define-test string-utf8 (:language :strings :utf8)
  (assert-equal (string-length "ěšč") 3)
  (assert-equal (string-ref "ab©" 2) #\ua9)
  (assert-equal (string->list "ab©") '(#\a #\b #\ua9))
  (assert-equal (list->string '(#\u3042 #\u3044 #\u3046 #\u3048 #\u304a))
                "あいうえお")
  (assert-equal (char-upcase #\a) #\A)
  (assert-equal (string-titlecase "foo bar") "Foo Bar")
  (assert-equal (string-search "def" "abčdefgh") 3)
  (assert-equal (string-search-ci "děf" "abcDĚFgh") 3)
  (assert-equal (string-split-on-character "a©cæ©b" "©")
                '("a" "cæ" "b")))

(define-test format (:language :format)
  (assert-equal (format "~~") "~")
  (assert-equal (format "~2r ~:* ~8r ~:* ~10r ~:* ~16r" 123)
                "1111011  173  123  7b")
  (assert-equal (format "~c" 0x3042) "あ")
  (assert-equal (format "~15f" '(1 2 3 4)) "      (1 2 3 4)")
  (assert-equal (format "~10,5f" pi) "   3.14159"))


(define-class <test-class> () 
  ((test-slot :accessor test-slot 
              :initform :test-slot-init-value
              :initarg :test-slot-initarg)))
(define-class <test-subclass> <test-class> ())

(define-test classes (:language :oop)
  (assert-equal (type-of (make-instance <test-class>))
                <test-class>)
  (assert-equal (test-slot (make-instance <test-class>))
                :test-slot-init-value)
  (assert-equal (test-slot (make-instance <test-subclass>))
                :test-slot-init-value)
  (assert-equal (test-slot (make-instance <test-class> 
                                          :test-slot-initarg :foo))
                :foo))

(define-test methods (:language :oop)
  (define-method (test-fun foo)
    'default)
  (define-method (test-fun (foo <test-class>))
    'test-class)

  (assert-equal (test-fun 't) 'default)
  (assert-equal (test-fun (make-instance <test-class>))
                'test-class)

  (assert-equal (test-slot (make-instance <test-subclass>))
                :test-slot-init-value)

  (define-method ((test-fun :around) (foo <test-subclass>))
    (cons 'subclass (call-next-method)))

  (assert-equal (test-fun (make-instance <test-subclass>))
                '(subclass . test-class)))

(define-test serialization-roundtrip (:language :serialization)
  (assert-equal (deserialize (serialize '(1 2 3 #(a b c) 3.1415 +inf. "foo")))
                '(1 2 3 #(a b c) 3.1415 +inf. "foo")))

(define-test sequences (:language :collections)
  (define l (list   'a 'b 'c 'd 'e 'f))
  (define v (vector 'a 'b 'c 'd 'e 'f))

  (assert-equal (seq-ref l 2) 'c)
  (assert-equal (seq-ref v 3) 'd)

  (seq-set! l 3 'dd)
  (assert-equal (seq-ref l 3) 'dd))

(define-role <<foo>> ()
  ((:foo :accessor foo-accessor)))
(define-role <<derived-foo>> (<<foo>>)
  ((:bar :accessor bar-accessor)))

(define-test roles (:language :oop)
  (define-class <bar> ()
    ()
    :roles (<<foo>>))
  (assert-true (specializer-matches-type? <<foo>> <bar>))
  
  (define-class <quux> ()
    ()
    :roles (<<derived-foo>>))
  (assert-true (specializer-matches-type? <<foo>> <quux>))
  (assert-true (specializer-matches-type? <<derived-foo>> <quux>)))