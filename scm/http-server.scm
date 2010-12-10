;;; dfsch - Scheme-like Lisp dialect
;;;   Slightly more user-friendly HTTP server
;;; Copyright (c) 2010 Ales Hakl
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(provide :http-server)
(require :http)
(require :inet)
(require :socket-port)
(require :threads)

(when-toplevel
 (require :gcollect))

(define-package :http-server
  :uses '(:dfsch :http :inet))
(in-package :http-server)

(define-class <server> ()
  ((hostname :reader server-hostname 
             :initarg :hostname 
             :initform "0.0.0.0")
   (port :reader server-port
         :initarg :port
         :initform 8080)
   (handler-map :reader server-handler-map
                :initform ())
   (vhosts? :reader server-vhosts?
                  :initarg :vhosts?)))

(define-method (add-handler! (server <server>) path handler)
  (slot-set! server :handler-map
             (append (server-handler-map server)
                     (list (list path handler)))))

(define-method (find-handler (server <server>) path)
  (catch 'found
         (for-each (lambda (hr)
                     (when (handler-matches? (car hr) path)
                           (throw 'found (cadr hr))))
                   (server-handler-map server))))

(define-method (handler-matches? (prefix <string>) path)
  (string-starts-with? path prefix))

(define-class <transaction> ()
  ((request :reader request-object)
   (path :reader path)
   (form-data :reader form-data)
   (response-headers :accessor response-headers)
   (response-status :accessor response-status)
   (response-port :reader response-port)))

(define-method (get-request-header (txn <transaction>) name)
  (let ((hdr (assoc name (request-headers (request-object txn)))))
    (if hdr
        (cadr hdr)
        ())))

(define-method (set-response-header! (txn <transaction>) name value)
  (let ((hdr (assoc name (response-headers txn))))
    (if hdr
        (set-car! (cdr hdr) value)
        (slot-set! txn :response-headers
                   (cons (list name value)
                         (response-headers txn))))))

(define-method (initialize-instance (txn <transaction>) req :key vhosts?)
  (slot-set! txn :request req)
  (slot-set! txn :path
             (if vhosts?
                 (string-append (or (get-request-header txn "Host") "")
                                "/"
                                (request-uri req))
                 (request-uri req)))
  (slot-set! txn :form-data (parse-form-data txn))
  (slot-set! txn :response-headers (list (list "Content-Type" 
                                               "text/html; charset=utf-8")))
  (slot-set! txn :response-status 200)
  (slot-set! txn :response-port (string-output-port)))

(define-method (parse-form-data (txn <transaction>))
  (let ((data
         (if (string=? (request-method (request-object txn))
                       "POST")
             (request-body (request-object txn))
             (car (or (cdr (http-split-query 
                            (request-uri (request-object txn))))
                      '(""))))))
    (http-query->alist data)))

(define-method (finalize-transaction (txn <transaction>))
  (let ((res 
         (make-response :status (response-status txn)
                        :headers (response-headers txn)
                        :body (string-output-port-value (response-port txn)))))

    ;; To prevent accidental memory leaks
    (slot-set! txn :request ())
    (slot-set! txn :form-data ())
    (slot-set! txn :response-headers ())
    (slot-set! txn :response-port ())
    res))

(define-method (serve-client (server <server>) request)
  (letrec ((txn (make-instance <transaction> request 
                               :vhosts? (server-vhosts? server)))
           (handler (find-handler server (path txn))))
    (handler txn)
    (finalize-transaction txn)))
           

(define-method (run-server (server <server>))
  (server-socket-run-accept-loop (tcp-bind (server-hostname server)
                                           (server-port server))
                                 (lambda (sock)
                                   (run-http-server sock 
                                                    (lambda (req)
                                                      (serve-client server
                                                                    req))))))

(define-method (run-server-in-background (server <server>))
  (threads:thread-detach (threads:thread-create run-server (list server))))
  


(when-toplevel
 (define s (make-instance <server> :port 2080 :hostname "localhost"))
 (add-handler! s "/" (lambda (txn)
                       (display "Hello world!" (response-port txn))
                       (set-response-header! txn "Content-Type" "text/plain")))
 (add-handler! s "/gc-stats"
               (lambda (txn)
                 (display "  dfsch memory statistics\r\n" (response-port txn))
                 (display "===========================\r\n\r\n" 
                          (response-port txn))
                 (display (format "Number of GC cycles: ~a\r\n\r\n" (gc-count))
                          (response-port txn))
                 (display (format "~a bytes free in ~a byte heap\r\n\r\n" 
                                  (gc-free-bytes) (gc-heap-size))
                          (response-port txn))
                 (display (format "~a bytes allocated since last GC\r\n" 
                                  (gc-bytes-since-gc))
                          (response-port txn))
                 (display (format "~a bytes allocated total\r\n" 
                                  (gc-total-bytes))
                          (response-port txn))

                 (set-response-header! txn "Content-Type" "text/plain")))
                 
 (run-server s))