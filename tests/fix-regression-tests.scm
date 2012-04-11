(define-evaluation-test gensym-write-segfault (:regression)
  ((let ((str (object->string (gensym))))
     #t)
   ===> #t))

(define-evaluation-test negative-divide (:regression)
  ((/ -1 2) ===> -1/2))

(define-evaluation-test fracnum-absolute-value (:regression)
  ((abs -1/2) ===> 1/2))

(define-test gensym-print (:language :numbers)
  (let ((l1 (string->object (object->string (let ((x (gensym))) 
                                             (list x x)))))
        (l2 (string->object (object->string (let ((x (unintern 'mnau))) 
                                             (list x x))))))
    (assert-equal (car l1) (cadr l1))
    (assert-equal (car l2) (cadr l2))))
