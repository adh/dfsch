(provide 'unix-utils)
(require 'unix)

(define (directory? path)
  (let ((stat (unix:stat path)))
    (if (null? stat)
        ()
        (stat 'isdir))))

(define (ensure-directory path)
  (unless (directory? path)
          (unix:mkdir path 0755)))

