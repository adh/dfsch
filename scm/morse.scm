;; Binary tree for decoding: (character dot dash)

(define morse-decode-tree '(() 
			    ("e" 
			     ("i"
			      ("s"
			       ("h" () ())
			       ("v" () ()))
			      ("u" 
			       ("f" () ()) 
			       ()))
			     ("a"
			      ("r"
			       ("l" ()())
			       ())
			      ("w" 
			       ("p" () ())
			       ("j" () ()))))
			    ("t"
			     ("n"
			      ("d"
			       ("b" () ())
			       ("x" () ()))
			      ("k"
			       ("c" () ())
			       ("y" () ())))
			     ("m"
			      ("g"
			       ("z" () ())
			       ("q" () ()))
			      ("o" () ())))))

;; decoding function: -  - dot
;;                    -- - dash 
(define (morse-decode list) 
  (define (loop list root)
    (cond ((null? root) ())
	  ((null? list) (car root))
	  ((= (car list) '-) (loop (cdr list) 
				       (car (cdr root))))
	  ((= (car list) '--) (loop (cdr list) 
				       (car (cdr (cdr root)))))))
  (loop list morse-decode-tree))

