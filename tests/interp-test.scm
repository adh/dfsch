(require :posix-regex)
(require :xml)
(require :sxml)
(require :unix)
(require :simple-tests)
(require :json)
(require :inet)
(require :zlib)

(define-package :interp-test :uses '(:dfsch :simple-tests))
(in-package :interp-test)

;;; Write tests here
;; in form like (test 'whetever1equals2 (= 1 2) true)
;;

(group "equivalence"
       
       (sub-group eqv?
                  
                  (test 'eqv? (eqv? 'a 'a) #t)
                  (test 'eqv?-false (eqv? 'a 'b) #f)
                  (test 'eqv?-num (eqv? 1 1) #t)
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
                  (test 'eq? (eq? 'a 'a) #t)
                  (test 'eq?-false (eq? 'a 'b) #f)
                  (test 'gensyms
                        (eq? (gensym) (gensym))
                        #f))

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
       (sub-group overflow
                  (test 'int-add (< 0 (+ most-positive-fixnum 1)) #t)
                  (test 'int-sub (> 0 (- most-negative-fixnum 1)) #t))
       (sub-group bignum-related
                  (test 'integer-expt 
                        (integer-expt 1234567890123456789 
                                      789456123 
                                      102030405060708090)
                        67367859943785579)
                  (test 'integer-expt2
                        (integer-expt 18675517624143580017362896922095425634043117362033394240130435130388602943541848087020624186330340297520090148379027335962264941267564450615244255491512344627974221842624203841614526458970679862942941976867717594997801525370645470003311025119308558623463911164602154587834502111742726697522984298872425398143 
                                      17985152009067448013538116410483656193325428378718972102000333966272708752064212945913265343209003579719041168011785051575205146925793561106735938084000942781328717550491476470770748694433255849990820421282362560286553727624617474888515273094274540663244744841210528736802612199557316690734543174396863190273 
                                      25641596484908052133293759467322210580011499296382380670222674189601777616690574396021529559647765132353959298381229484099465273974846254225812590806891103675952627696507869854468082991603881845629491984125414446012038304768055260732775839473801612280912034600666301196492897026412985863459591512064238914603)
                        1337)
                  (test 'integer-expt-neg
                        (integer-expt -256 9)
                        -4722366482869645213696)
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
                        109945704088079889)
                  (test '>> (>> 3523532227357930030104576 54) 
                        195595330)))


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

       (sub-group when-unless
                  (test 'when-true (when #t 1) 1)
                  (test 'when-false (when #f 1) ())
                  (test 'unless-true (unless #t 1) ())
                  (test 'unless-false (unless #f 1) 1))
       
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
                          (* x y))           
                        6)
                  (test 'let2
                        (let ((x 2) (y 3))
                          (let ((x 7)
                                (z (+ x y)))
                            (* z x)))   
                        35)
                  (test 'let*
                        (let ((x 2) (y 3))
                          (let* ((x 7)
                                 (z (+ x y)))
                            (* z x)))                   
                        70)
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
                  (test 'named-let
                        (let loop ((numbers '(3 -2 1 6 -5))
                                   (nonneg '())
                                   (neg '()))
                          (cond ((null? numbers) (list nonneg neg))
                                ((>= (car numbers) 0)
                                 (loop (cdr numbers)
                                       (cons (car numbers) nonneg)
                                       neg))
                                ((< (car numbers) 0)
                                 (loop (cdr numbers)
                                       nonneg
                                       (cons (car numbers) neg)))))   
                        '((6 1 3) (-5 -2))))
       (sub-group destructuring-bind
                  (test 'simple
                        (destructuring-bind (a b c) '(1 2 3)
                                            b)
                        2)))
(group "write->read"
       (define bn (random-bignum 1024))
       (define string (random-bytes 512))
       (test 'bignum (string->object (object->string bn)) bn)
       (test 'string (string->object (object->string string)) string)

       )

(group "functions"

       (define (fact x)
         (if (= x 0)
             1
             (* x (fact (- x 1)))))
       
       (test 'fact (fact 5) 120)

       (define (opt-arg-fun &optional a (b 2) (c 3 c-supplied))
         (list a b c c-supplied))
       (test 'optional-1 (opt-arg-fun ) '(() 2 3 ()))
       (test 'optional-2 (opt-arg-fun 'a) '(a 2 3 ()))
       (test 'optional-3 (opt-arg-fun 'a 'b) '(a b 3 ()))
       (test 'optional-4 (opt-arg-fun 'a 'b 'c) '(a b c true))
       
       (define (key-arg-fun &key a (b 2) (c 3 c-supplied))
         (list a b c c-supplied))
       (test 'keyword-1 (key-arg-fun ) '(() 2 3 ()))
       (test 'keyword-2 (key-arg-fun :c 9) '(() 2 9 true))

       (define (key-rest-arg-fun &rest r &key a (b 2) (c 3 c-supplied))
         (list r a b c c-supplied))
       (test 'keyword-and-rest (key-rest-arg-fun :c 9) '((:c 9) () 2 9 true)))


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
       (test 'search-utf (string-search "def" "abčdefgh") 3)
       (test 'search-ci (string-search-ci "děf" "abcDĚFgh") 3)
       (test 'string-split-on-byte (string-split-on-byte "a b,,c" ", ")
             '("a" "b" "c"))
       (test 'string-split-on-character 
             (string-split-on-character "a©cæ©b" "©")
             '("a" "cæ" "b"))
       (test 'byte-list (string->byte-list "æ©") '(195 166 194 169)))

(group "some special cases"

       (test 'degenerated-list-qq `(,@'() . foo) 'foo))

(group "POSIX regular expressions"
       (define r0 (posix-regex:compile "[ab]+"))
       (define r0b (posix-regex:compile "[ab]+" :basic))
       (define r1 (posix-regex:compile "^([^ ]+) +([^ ]+)$"))
       (test 'extended (posix-regex:match? r0 "baba") #t)
       (test 'basic (posix-regex:match? r0b "baba") #f)
       (test 'substring 
             (posix-regex:substrings r1 "aaa bbb") 
             #(#(0 7 "aaa bbb") #(0 3 "aaa") #(4 7 "bbb")))
       (test 'substring-once 
             (posix-regex:substrings-once "^([^ ]+) +([^ ]+)$" "aaa bbb") 
             #(#(0 7 "aaa bbb") #(0 3 "aaa") #(4 7 "bbb"))))

(group "Format"
       (test 'escaping (format "~~") "~")
       (test 'radix 
             (format "~2r ~:* ~8r ~:* ~10r ~:* ~16r" 123)
             "1111011  173  123  7b")
       (test 'write-char (format "~c" 0x3042) "あ")
       (test 'field-width (format "~15f" '(1 2 3 4)) "      (1 2 3 4)")
       (test 'floats (format "~10,5f" pi) "   3.14159"))

(group "Object system"
       (let ()
         (define-class <test-class> () 
           ((test-slot :accessor test-slot 
                       :initform :test-slot-init-value
                       :initarg :test-slot-initarg)))
         (test 'make-instance 
               (type-of (make-instance <test-class>))
               <test-class>)
         (test 'init-form
               (test-slot (make-instance <test-class>))
               :test-slot-init-value)
         (test 'init-arg
               (test-slot (make-instance <test-class> 
                                         :test-slot-initarg :foo))
               :foo)

         (define-method (test-fun foo)
           'default)
         (define-method (test-fun (foo <test-class>))
           'test-class)

         (test 'default-method (test-fun 't) 'default)
         (test 'simple-specializer 
               (test-fun (make-instance <test-class>))
               'test-class)

         (define-class <subclass> <test-class> ())

         (test 'inherited-initform 
               (test-slot (make-instance <subclass>))
               :test-slot-init-value)

         (define-method ((test-fun :around) (foo <subclass>))
           (cons 'subclass (call-next-method)))

         (test 'around-method
               (test-fun (make-instance <subclass>))
               '(subclass . test-class))

         ))


(group "XML support"
       (sub-group sxml
                  (test 'parse-string
                        (xml:sxml-parse-string "<a foo=\"bar &quot;\">c<b/></a>")
                        '("a" (:attributes ("foo" "bar \"")) "c" ("b")))
                  (test 'emit-string
                        (xml:sxml-emit-string '(:a (:attributes (foo "bar \"")) 
                                                  (b)))
                        "<a foo=\"bar &quot;\"><b /></a>")
                  (test 'emit-string-shorthand
                        (xml:sxml-emit-string '(:a :foo "bar \"" 
                                                  (b)))
                        "<a foo=\"bar &quot;\"><b /></a>")))

(group "JSON support"
       (test 'parse-json
             (json:parse-string "[1, 2, 3]")
             '(1 2 3))
       
       (define (canonical-alist a)
         (sort-list! a
                     (lambda (a b)
                       (string<? (car a) (car b)))))
       (define (hash->canonical-alist h)
         (canonical-alist (hash->alist h)))

       (define *json-test-data* (alist->hash '(("foo" (1 2 3))
                                               ("bar" ("a" "b" (3.14))))))
       (test 'json-roundtrip
             (hash->canonical-alist (json:parse-string (json:emit-string *json-test-data*)))
             (hash->canonical-alist *json-test-data*)))

(group "Internet data handling"
       (test 'http-query->alist
             (inet:http-query->alist "q=foo&bla=a%20b")
             '(("q" "foo") ("bla" "a b")))
       (test 'base64
             (inet:base64-decode (inet:base64-encode "abcdef\n\r\"01234567890"))
             #"abcdef\n\r\"01234567890"))

(group "Zlib"
       (test 'compress-uncompress
             (zlib:uncompress (zlib:compress "test test\0test"))
             "test test\0test"))

(group "Regressions"
       (test 'gensym-write-segfault 
             (let ((str (object->string (gensym))))
               #t)
             #t)
       (test 'negative-divide (/ -1 2) -1/2)
       (test 'fracnum-absolute-value (abs -1/2) 1/2))

(group "Serialization"
       (test 'serdes-roundtrip
             (deserialize (serialize '(1 2 3 #(a b c) 3.1415 "foo")))
             '(1 2 3 #(a b c) 3.1415 "foo")))

;;; End of tests
;;
;; Print some statistics and exit apropriately
;;
(when-toplevel
 (exit-func 1))
