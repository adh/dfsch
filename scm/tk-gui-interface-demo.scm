(require :tk-gui-interface)

(define *interp* (tk-gui-interface:create-interpreter ))
(tk-gui-interface:eval *interp* "text" ".text")
(tk-gui-interface:create-command! 
 *interp* "eval_cb"
 (lambda (&rest args) 
   (tk-gui-interface:eval *interp* 
                          "tk_messageBox" 
                          :message (object->string 
                                    (eval-proc (string->object-list 
                                                (tk-gui-interface:eval *interp* ".text" "get" "0.0" "end"))
                                               (top-level-environment))))))
(tk-gui-interface:eval *interp* "button" ".eval" 
                       :command "eval_cb" 
                       :text "Eval!")

(tk-gui-interface:eval *interp* "wm" "title" "." "dfsch evaluator")
(tk-gui-interface:eval *interp* "pack" ".text")
(tk-gui-interface:eval *interp* "pack" ".eval")
(tk-gui-interface:event-loop *interp*)