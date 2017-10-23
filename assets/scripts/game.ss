;; game.ss
;; by tsip
;; 
;; contains functions for creating game objects

(define (game-state name)
    (let ((s (item name "")))
        (begin
            (add-action!
                s
                "construct"
                (lambda (s ls)
                  (let ((g (list-ref ls 0))
			(init-f (list-ref ls 1))
                        (handle-f (list-ref ls 2))
                        (update-f (list-ref ls 3))
                        (render-f (list-ref ls 4))
                        (receive-f (list-ref ls 5)))
                        (if (and (procedure? init-f) 
                                 (procedure? handle-f)
                                 (procedure? update-f)
                                 (procedure? render-f))
                            (set-property! s "game" g)
                            (add-action! s "init" init-f)
                            (add-action! s "handle" handle-f)
                            (add-action! s "update" update-f)
                            (add-action! s "render" render-f)
			    (add-action! s "receive" receive-f)))))
	    (add-action! s "receive" (lambda (s ls) 'receive))
            (add-action! s "init" (lambda (s) 'init))
            (add-action! s "handle" (lambda (s ls) 'handle))
            (add-action! s "update" (lambda (s) 'update))
            (add-action! s "render" (lambda (s) 'render))
            s)))

(define (game title description version author)
    (let ((g (item title description)))
        (begin
            (set-property! g "version" version)
            (set-property! g "author" author)
            (set-property! g "action-keys" (list (list) (list))) 
            (set-property! g "gsm" (state-machine))
            (set-property! g "assets-folder" "assets")
            (set-property! g "images-folder" "images")
            (set-property! g "sounds-folder" "sounds")
            (set-property! g "music-folder" "music")
            (set-property! g "scripts-folder" "scripts")
            (add-action!
                g
                "construct"
                (lambda (g ls) 
		  (let ((game-world (list-ref ls 0))
			(init-state (list-ref ls 1))
			(gsm (get-property g "gsm")))
            (add-action! 
                g
                "receive"
                (lambda (g ls)
                    (let ((event (car ls))
			  (gsm (get-property g "gsm")))
		      (let ((current-state (peek gsm)))
			(item-execute current-state "receive" event)))))
            (add-action!
                g
                "remove-key!"
                (lambda (g ls)
                    (let ((key-ind (list-ref ls 0))
                          (action-keys (get-property g "action-keys")))
                        (let ((keys (cadr action-keys)))
                            (if (< key-ind (length keys))
                            (set-car! (list-tail keys key-ind) 'NaN))))))
            (add-action!
                g
                "map-action-to-key!"
                (lambda (g ls)
                    (let ((action (list-ref ls 0))
                          (key (list-ref ls 1))
                          (action-keys (get-property g "action-keys"))
                          (remove-key! (get-action g "remove-key!")))
                        (if (and (string? action) (symbol? key))
                            (let ((actions (car action-keys)) (keys (cadr action-keys)))
                                (if (null? actions)
                                    (begin
                                        (set-car! action-keys (cons action actions))
                                        (set-car! (list-tail action-keys 1) (cons key keys)))
                                    (let ((action-ind (binary-search actions action #t string<? string=?))
                                          (key-ind (index-of keys key equal?)))
                                        (begin
                                            (if key-ind ;; remove previous key binding
                                                (remove-key! key-ind))
                                            (if (and (< action-ind (length actions))
                                                     (string=? action (list-ref actions action-ind)))
                                                (set-car! (list-tail keys action-ind) key)
                                                (begin 
                                                    (insert-at! actions action action-ind)
                                                    (insert-at! keys key action-ind)))))))))))
           g)))


                       





