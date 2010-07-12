(require :tk-gui)

(use-package :tk-gui)

(define context (make-context :withdraw-toplevel #t))

(define window (make-window context))
(window-on-delete window 
                  (lambda (window)
                    (when (string=? (message-box context
                                                 :message "Really quit?"
                                                 :type "yesno"
                                                 :icon "question")
                                    "yes")
                          (destroy-window window))))
(window-title window "tk-gui demo")

(define-widgets window
  (:entry () 
          :grid (:row 0 :column 0) 
          :contents() 
          :variable entry)
  (:button (:text "The Button") 
           :grid (:row 0 :column 1)
           :events ((:command
                     (lambda () 
                       (message-box context
                                    :message 
                                    (format "Value is: ~s" 
                                            (widget-command entry 
                                                            "get")))))))
  (:button (:text "Button dialog") :grid (:row 1 :column 0)
           :events ((:command 
                     (lambda ()
                       (button-dialog context
                                      '("Nyaa" "Nyuu" "Nyan" "Knik" "Uguu")
                                      :title "foo?"
                                      :text "Neko?"))))))

;; (define entry
;;    (pack-widget 
;;     (make-widget window "entry" )))
;; (widget-command entry "insert" "0" "The Entry")
  

;; (bind-event (pack-widget 
;;              (make-widget window "button" 
;;                           :text "The Button"))
;;             :command (lambda ()

(wait-for-window window)