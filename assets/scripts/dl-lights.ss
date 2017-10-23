;; dl-lights.ss
;; by tsip

;; list of lights that are turned on
(define on-lights '())

(define (update-on-lights)
  (letrec (
	(loop
	  (lambda (ls)
	    (if (null? ls)
	      '()
	      (let* ((light (car ls))
		     (on-state (get-property light "on")))
		(if on-state
		  (cons light (loop (cdr ls)))
		  (loop (cdr ls))))))))
    (set! on-lights (loop on-lights))))


;; light source
(define (light-source name description alpha turns)
  (let ((light (item name description)))
    (begin
      (set-property! light "alpha" alpha)
      (set-property! light "on" #f)
      (set-property! light "turns" turns)
      (set-property! light "decrement" (/ alpha turns))
      (add-action!
	light
	"switch"
	(lambda (light)
	  (let* ((on (get-property light "on")))
	    (begin
	     (set-property! light "on" (not on))
	     (if (not on) (set! on-lights (cons light on-lights)))))))
      (add-action!
	light 
	"turn"
	(lambda (light)
	  (let* ((alpha (get-property light "alpha"))
		 (decrement (get-property light "decrement"))
		 (new-alpha (- alpha decrement)))
	    (if (> new-alpha 0)
	      (set-property! light "alpha" new-alpha)
	      (set-property! light "alpha" 0)))))
      light)))

(define (create-candle)
  (light-source "candle" "small light. lasts for 5 turns" 0.7 5))

(define (create-torch)
  (light-source "torch" "medium light. lasts for 8 turns" 0.8 8))
