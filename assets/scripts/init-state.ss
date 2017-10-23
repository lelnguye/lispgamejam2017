;; init-state.ss
;; by tsip

(define init-state
    (let ((i-state (game-state "init-state"))
          (init-init-f 
            (lambda (s)
                (set-property! s "current-frame" -1)
                (set-property! s "total-frames" 10)
                (set-property! s "playing" #t)
                (set-property! s "logo-animation" "logo-loop.png")))
          (init-handle-f 
            (lambda (s ls) 
                (let ((event-ls (list-ref ls 0))
                      (loop-events-f
                        (lambda (event-ls f)
                            (if (not (null? event-ls))
                                (let* ((e (car ls))
                                       (e-type (js-ref e "type")))
                                    (if (or (string=? e-type "keypress")
                                            (string=? e-type "click"))
                                        (begin
                                            (set-property! s "playing" #f)
                                            (item-execute s "exit")))))))))))
          (init-update-f
            (lambda (s)
                (let ((cf (get-property s "current-frame"))
                      (tf (get-property s "total-frames")))
                    (if (< cf tf)
                        (set-property! "current-frame" (+ cf 1))
                        (begin
                            (set-property! "playing" #f)
                            (item-execute s "exit"))))))
          (init-render-f
            (lambda (s ls)
                (let ((renderer (car ls)))
                    'todo)))
          (init-exit-f
            (lambda (s)
                (let ((gsm (get-property s "gsm")))
                    (begin
                        (pop! gsm)
                        (push! gsm play-state)))))
        ) 
        (item-execute i-state "construct" 

    ))
