;; state.ss
;; by tsip
;;

;; creates a state machine, which is simply a stack of states
(define (state-machine) (list -1 '()))

;; pushes a state to the top of the state machine
(define (push! state-machine state)
    (let ((head (car state-machine)) (states (cadr state-machine)))
        (if (null? states)
            (set-car! (list-tail state-machine 1) (cons state states))
            (insert-at! states state (length state)))
        (set-car! state-machine (+ head 1))))

(define (peek state-machine) 
    (let ((head (car state-machine)) (states (cadr state-machine)))
        (if (> head -1) (list-ref states head) #f)))

(define (pop! state-machine)
    (let ((head (car state-machine)) (states (cadr state-machine)))
        (if (> head -1)
            (begin
                (set-car! state-machine (- head 1))
                (remove-element-at! states head))
            #f)))




