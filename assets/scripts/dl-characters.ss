;; dl-characters.ss
;; by tsip


(define (set-current-light c ls)
  (let* ((light-ind (car ls))
	 (current-light-ind (get-property c "current-light"))
	 (current-light (item-execute c "get-item" current-light-ind))
	 (new-light (item-execute c "get-item" light-ind)))
    (if (not (equal? new-light 'NaN))
      (begin
	(item-execute current-light "switch")
	(set-property! c "current-light" light-ind)))))

(define kid-A (character "kid A" "an orphan. parents died during the 'event'" 5))

(item-execute kid-A "add-item!" (create-candle) 0)
(item-execute kid-A "set-skill!" "speed" 2)
(item-execute kid-A "set-skill!" "strength" 5) 

(set-property! kid-A "image" "assets/images/kid-a.png")
(set-property! kid-A "portrait" "assets/images/portrait-a.png")
(set-property! kid-A "current-light" 0)

(add-action! kid-A "set-current-light" set-current-light)

(define kid-B (character "kid B" "lost child." 3))

(item-execute kid-B "add-item!" (create-candle) 0)
(item-execute kid-B "set-skill!" "speed" 4)
(item-execute kid-B "set-skill!" "strength" 3) 

(set-property! kid-B "image" "assets/images/kid-b.png")
(set-property! kid-B "portrait" "assets/images/portrait-b.png")
(set-property! kid-B "current-light" 0)


(add-action! kid-B "set-current-light" set-current-light)
