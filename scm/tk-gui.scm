(require :tk-gui-interface)
(define-package :tk-gui :dfsch :tk-gui-interface)
(in-package :tk-gui)

(define *waited-window* ())

(define-class <context> ()
  ((interpreter :reader context-interpreter)
   (top-level-window :reader context-toplevel-window)))

(define-macro (make-context &rest args)
  `(make-instance <context> (create-interpreter)
                  ,@args))

(define-method (initialize-instance (context <context>) 
                                    interpreter 
                                    &key withdraw-toplevel)
  (slot-set! context :interpreter interpreter)
  (slot-set! context :top-level-window
             (make-instance <window> context "."))
  (window-on-delete (context-toplevel-window context)
                    (lambda (window)
                      (withdraw-window window)))
  (when withdraw-toplevel
        (withdraw-window (context-toplevel-window context))))  

(define-class <widget> ()
  ((path :reader widget-path :initarg :path)
   (context :reader widget-context :initarg :context)))

(define-method (initialize-instance (widget <widget>) parent type path 
                                    &rest args)
  (slot-set! widget :path (translate-widget-path parent path))
  (slot-set! widget :context (widget-context parent))
  (eval-list (widget-interpreter widget)
             (append (list type (widget-path widget))
                     args)))

(define unique-widget-name
  (let ((counter 0))
    (lambda ()
      (set! counter (+ 1 counter))
      (format ".win~16r" counter))))

(define-macro (make-widget parent type &rest args)
  `(make-instance <widget> ,parent ,type (unique-widget-name) ,@args))

(define-method (translate-widget-path (widget <widget>) path)
  (string-append (widget-path widget) path))


(define-class <window> <widget>
   ((command-list)
    (on-delete :accessor window-on-delete)
    (destroyed? :initform ()
                :reader window-destroyed?)))

(define-method (widget-interpreter (widget <widget>))
  (context-interpreter (widget-context widget)))


(define unique-command-name
  (let ((counter 0))
    (lambda ()
      (set! counter (+ 1 counter))
      (format "dfsch_cmd~16r" counter))))

(define-macro (make-window context &rest args)
  `(make-instance <window> ,context (unique-widget-name) ,@args))

(define-method (initialize-instance (win <window>) context path &rest args)
  (slot-set! win :path path)
  (slot-set! win :context context)
  (create-command! (widget-interpreter win)
                   (window-delete-command-name win)
                   (lambda ()
                     (if (window-on-delete win)
                         ((window-on-delete win) win)
                         (destroy-window win))))
  (unless (string=? path ".")
          (apply eval (cons (widget-interpreter win)
                            (cons "toplevel" 
                                  (cons path args)))))
  (eval (widget-interpreter win)
        "wm" "protocol" path "WM_DELETE_WINDOW" 
        (window-delete-command-name win)))

(define-method (destroy-window (win <window>))
  (for-each (lambda (command)
              (delete-command! (window-interpreter win) command))
            (slot-ref win :command-list))
  (delete-command! (widget-interpreter win) 
                   (window-delete-command-name win))
  (eval (widget-interpreter win) "destroy" (widget-path win))
  (slot-set! win :destroyed? #t)
  (when (eq? win *waited-window*)
        (throw 'waited-window-destroyed win)))

(define-method (bind-command (win <window>) proc)
  (let ((cmd-name (unique-command-name)))
    (create-command (widget-interpreter win) cmd-name proc)
    (slot-set! win :command-list (cons cmd-name (slot-ref win :command-list)))
    cmd-name))

(define-method (window-delete-command-name (win <window>))
  (format "dfsch_delete_handler_~a" (widget-path win)))

(define-method (wait-for-window (win <window>))
  (when (window-destroyed? win)
        (error "Cannot wait for destroyed window" :window win))
  (set! *waited-window* win)
  (catch 'waited-window-destroyed
         (event-loop))
  (set! *waited-window* ()))

(define-method (window-title (win <window>) &optional new)
  (if new
      (eval (widget-interpreter win) "wm" "title" (widget-path win) new)
      (eval (widget-interpreter win) "wm" "title" (widget-path win))))

(define-method (withdraw-window (win <window>))
  (eval (widget-interpreter win) "wm" "withdraw" (window-path win)))

(define (wait)
  (event-loop))


                       
