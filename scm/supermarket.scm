;; This is typical assignment from introductory courses on operating systems
;; (and good test for dfsch's multithreading support)

(define free-tellers (channel:create))
(define exits (channel:create))
(define io (mutex:create))

(define log
  (let ((counter 1))
    (lambda args
      (mutex:lock io)
      (apply print (append (list "[" counter "] ") args))
      (set! counter (+ 1 counter))
      (mutex:unlock io))))

(define thread-list ())

(define (start-thread function arguments)
  (let ((thread (thread:create (lambda ()
                                 (thread:detach (thread:self))
                                 (apply function arguments))
                               ())))
    (set! thread-list (cons thread thread-list))))

(define (collect-threads)
  (for-each 
   (lambda (thread)
     (letrec ((thread-return (channel:read exits))
              (thread-type (car thread-return))
              (thread-id (cdr thread-return)))
       (log "thread exited " thread-type thread-id)))
   thread-list))

(define (teller max-customers)
  (let ((my-id (gensym)) (my-channel (channel:create)))
    (let main-loop ((iter 0))
      (sleep 1)
      (log "teller " my-id " available")
      (channel:write free-tellers (cons my-channel my-id))
      
      (letrec ((cust-data (channel:read my-channel))
               (customer (car cust-data))
               (cust-channel (car customer))
               (cust-id (cdr customer))
               (cust-items (cdr cust-data))
               (sum 0))

        (log "teller " my-id " got customer " cust-id " number " iter)

        (for-each (lambda (item) 
                    (let ((name (vector-ref item 0))
                          (price (vector-ref item 1)))
                      (sleep 1)
                      (log "teller " my-id " item " name " @ " price)
                      (set! sum (+ sum price))))
                  cust-items)

        (log "teller " my-id " sum is " sum)
        (channel:write cust-channel sum)
        (letrec ((payment (channel:read my-channel))
                 (return (- payment sum)))
          (log "teller " my-id " received " payment " returning " return)
          (channel:write cust-channel return)
          ))
      
      (when (< iter max-customers)
            (main-loop (+ iter 1))))

    (log "teller " my-id " going home")
    (channel:write exits (cons 'teller my-id))))

(define (customer items)
  (let ((my-id (gensym)) 
        (my-channel (channel:create)))
    (letrec ((teller (channel:read free-tellers))
             (teller-channel (car teller))
             (teller-id (cdr teller)))
      (sleep 1)
      (log "customer " my-id " got teller " teller-id)
      (channel:write teller-channel 
                     (cons (cons my-channel my-id) 
                           items))

      (letrec ((sum (channel:read my-channel))
               (pay (* sum 1.10)))
        (log "customer " my-id " sum is " sum " paying " pay)
        (sleep 1)
        (channel:write teller-channel pay)
        (let ((return (channel:read my-channel)))
          (log "customer " my-id " got " return " back, going home")
          (channel:write exits (cons 'customer my-id)))))))

(thread:create collect-threads ())

(let top-loop ()
  (start-thread teller '(10))
  
  (let loop ((i 10))
    (start-thread customer (list (cons (vector 'foo i) '(#(bar 20) #(quux 30)))))
    (when (> i 0)
          (loop (- i 1))))
  (top-loop))
  
