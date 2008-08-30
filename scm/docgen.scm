#!/usr/bin/env dfsch-repl
; documentation generator for dfsch

(require 'unix-utils)

(unless (= (vector-length *posix-argv*) 3)
        (exit "usage: docgen.scm <input> <output>"))

(define input-name (vector-ref *posix-argv* 1))
(define output-name (vector-ref *posix-argv* 2))

(define document (load:read-scm input-name))

(ensure-directory output-name)

(define *html-head-add* "")
(define *html-end-body* "")

(define (emit-html-file name title content-function)
  (let ((os (open-file-port (string-append output-name "/" name ".html") "w")))
    (port-write-buf "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n" os)
    (port-write-buf "<html xmlns=\"http://www.w3.org/1999/html/\"><head><title>" os)
    (port-write-buf title os)
    (port-write-buf "</title>" os)
    (port-write-buf *html-head-add* os)
    (port-write-buf "</head><body><h1>" os)
    (port-write-buf title os)
    (port-write-buf "</h1>" os)
    (content-function (lambda (str)
                        (port-write-buf str os)))
    (port-write-buf *html-end-body* os)
    (port-write-buf "</body></html>\n" os)))

(define (internal-link o where name)
  (o (string-append "<a class=\"internal\" href=\""
                    where ".html\">" name "</a")))

(emit-html-file "index" "Dfsch documentation" 
                (lambda (o) 
                  (o "<ul>")
                  (for-each 
                   (lambda (statement)
                     (o (format "<li>~s</li>" statement)))
                   document)
                  (o "</ul")))

