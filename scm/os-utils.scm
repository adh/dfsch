(provide :os-utils)
(require :os)

(define-package :os-utils
  :uses '(:dfsch :os)
  :exports '(:directory?
             :ensure-directory
             :read-directory))
(in-package :os-utils)

(define (directory? path)
  (let ((stat (os:stat path)))
    (if (null? stat)
        ()
        (stat :isdir))))

(define (ensure-directory path)
  (unless (directory? path)
          (os:mkdir path 0755)))

(define (ignore-dirent? dirent)
  (or (eq? (string-ref dirent 0) #\.)
      (eq? (string-ref dirent (- (string-length dirent) 1)) #\~)))

(define (read-directory directory)
  
  (let ((dd (os:opendir directory)))
    (let loop ((list ()))
      (let ((dirent (os:readdir dd)))
        (if (null? dirent)
            list
            (if (ignore-dirent? dirent)
                (loop list)
                (loop (cons (read-entry (string-append directory "/" dirent))
                            list))))))))
