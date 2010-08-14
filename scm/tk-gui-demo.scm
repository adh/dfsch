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
                          (destroy-window! window))))
(window-title window "tk-gui demo")

(define-widgets window
  (<entry> () 
           :grid (:row 0 :column 0) 
           :variable entry)
  (:button (:text "The Button") 
           :grid (:row 0 :column 1)
           :events ((:command
                     (lambda () 
                       (message-box context
                                    :message 
                                    (format "Value is: ~s" 
                                            (get-value entry)))))))
  (:button (:text "Second Button" :bg "red")
           :grid (:row 0 :column 2)
           :events ((:command
                     (lambda ()
                       (set-value! entry "Foo")))))
  (<check-button> (:text "Check-button")
                  :grid (:row 1 :column 0)
                  :variable check)
  (:button (:text "Value?")
           :grid (:row 1 :column 1)
           :events ((:command 
                     (lambda ()
                       (message-box context 
                                    :message (if (get-value check)
                                                 "Checkbox is checked"
                                                 "Checkbox is lonely"))))))
  (:button (:text "Flash")
           :grid (:row 1 :column 2)
           :events ((:command (lambda () (flash-button! check)))))
  (:frame () :grid (:row 2 :column 0 :columnspan 3)
          :contents
          ((:button (:text "Button dialog") :pack ()
                    :events ((:command 
                              (lambda ()
                                (button-dialog context
                                               '("Nyaa" "Nyuu" "Nyan" "Knik" "Uguu")
                                               :title "foo?"
                                               :text "Neko?")))))
           (:button (:text "Open dialog") :pack ()
                    :events ((:command
                              (lambda ()
                                (open-dialog context)))))
           (:button (:text "Color dialog") :pack ()
                    :events ((:command
                              (lambda ()
                                (color-dialog context)))))))
  (:frame () :grid (:row 3 :column 0)
          :contents
          ((<radio-button> (band "HF" :text "HF") :pack ())
           (<radio-button> (band "VHF" :text "VHF") :pack ())
           (<radio-button> (band "UHF" :text "UHF") :pack ())))
  (:frame () :grid (:row 3 :column 1)
          :contents
          ((<radio-button> (band "Ka" :text "Ka") :pack ())
           (<radio-button> (band "K" :text "K") :pack ())
           (<radio-button> (band "Ku" :text "Ku") :pack ())))
  (:button (:text "Tx") :grid (:row 3 :column 2)
           :events ((:command
                     (lambda ()
                       (message-box context 
                                    :message (format "Tx at ~a" 
                                                     (get-value band))))))))

(wait-for-window window)