;;; dfsch - Scheme-like Lisp dialect
;;;   Example usage of http-server module
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


(require :http-server)
(require :gcollect)
(require :cmdopts)
(use-package :http-server)

(define port 2080)
(define hostname "localhost")
(let ((p (cmdopts:make-parser)))
  (cmdopts:add-option p 
                      (lambda (p v) 
                        (set! port (string->object v)))
                      :long-option "port"
                      :has-argument #t)
  (cmdopts:add-option p 
                      (lambda (p v) 
                        (set! hostname v))
                      :long-option "hostname"
                      :has-argument #t)
  (cmdopts:parse-list p (cdr *posix-argv*)))

(define s (make-instance <server> :port port :hostname hostname))
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

(run-server s)