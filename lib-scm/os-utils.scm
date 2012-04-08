(provide :os-utils)
(require :os)

(define-package :os-utils
  :uses '(:dfsch :os)
  :exports '(:directory?
             :ensure-directory
             :directory->list))
(in-package :os-utils)

(define (directory? path)
  (let ((stat (os:stat path)))
    (if (null? stat)
        ()
        (stat :isdir))))

(define (ensure-directory path)
  (unless (directory? path)
          (os:mkdir path 0755)))

(define (directory->list directory &key full-paths? filter)  
  (let ((dd (os:opendir directory)))
    (let next ((list ()))
      (let ((dirent (os:readdir dd)))
        (if (null? dirent)
            list
            (if (and filter (not (filter dirent)))
                (next list)
                (next (cons (if full-paths? 
                                (string-append directory "/" dirent)
                                dirent)
                            list))))))))
