;; character.ss
;; by tsip


;; character
(define (character name description inventory-size) 
    (let ((c (item name description)))
        (begin
            (set-property! c "health" 100)
            (set-property! c "skills" (list))
            (set-property! c "inventory" (make-list-of inventory-size 'NaN))
            (set-property! c "state-machine" (state-machine))
            (set-property! c "sprite" 'NaN) 
            (add-action!
                c
                "add-item!"
                (lambda (c ls)
                    (let ((item (car ls))
                          (slot (cadr ls))
                          (inventory (get-property c "inventory")))
                        (if (< slot (length inventory))
                            (set-car! (list-tail inventory slot) item)))))
            (add-action!
                c
                "remove-item!"
                (lambda (c ls)
                    (let ((slot (car ls))
                          (inventory (get-property c "inventory")))
                        (if (< slot (length inventory))
                            (set-car! (list-tail inventory slot) 'NaN)))))
            (add-action!
                c
                "get-item"
                (lambda (c ls)
                    (let ((slot (car ls))
                          (inventory (get-property c "inventory")))
                        (if (< slot (length inventory))
                            (list-ref inventory slot)
                            #f))))
            (add-action!
                c
                "get-health"
                (lambda (c) (get-property c "health")))
            (add-action!
                c
                "modify-health!"
                (lambda (c ls)
                    (let ((modifier (car ls))
                          (health (get-property c "health")))
                        (let ((new-health
                                (if (> (+ health modifier) 100)
                                    100
                                    (if (< (+ health modifier) 0)
                                        0
                                        (+ health modifier)))))
                            (set-property! c "health" new-health)))))
            (add-action!
                c
                "set-skill!"
                (lambda (c ls)
                    (let ((name (car ls))
                          (level (cadr ls))
                          (skills (get-property c "skills")))
                        (let ((skill (cons name level)))
                            (if (null? skills)
                                (set-property! c "skills" (cons skill skills))
                                (let ((skill-ind (binary-search skills name #t name<? name=?)))
                                    (if (and (< skill-ind (length skills))
                                             (name=? name (list-ref skills skill-ind)))
                                        (set-cdr! (list-ref skills skill-ind) level)
                                    (insert-at! skills skill skill-ind))))))))
            (add-action!
                c
                "modify-skill!"
                (lambda (c ls)
                    (let ((name (car ls))
                          (modifier (cadr ls))
                          (skills (get-property c "skills")))
                        (let ((skill-ind (binary-search skills name #f name<? name=?)))
                            (if skill-ind 
                                (let ((skill (list-ref skills skill-ind)))
                                    (set-cdr! skill (+ (cdr skill) modifier))))))))
            (add-action!
                c
                "get-skill"
                (lambda (c ls)
                    (let ((name (car ls))
                          (skills (get-property c "skills")))
                        (let ((skill-ind (binary-search skills name #f name<? name=?)))
                            (if skill-ind 
                                (let ((skill (list-ref skills skill-ind)))
                                    (cdr skill))
                                #f)))))
            c)))


