(define-evaluation-test gensym-write-segfault (:regression)
  ((let ((str (object->string (gensym))))
     #t)
   ===> #t))

(define-evaluation-test negative-divide (:regression)
  ((/ -1 2) ===> -1/2))

(define-evaluation-test fracnum-absolute-value (:regression)
  ((abs -1/2) ===> 1/2))
