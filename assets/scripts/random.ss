;; random.ss
;; by tsip
;;
;; function for creating random numbers

(define p 'E)
(define q 'E)
(define M 'E)

(define prevX 'E)

;; blum-blum-shub pseudo random number generator 
(define (seed s upper-bound)
    (if (and (equal? prevX 'E) (< s upper-bound))
        (let ((primes (list-primes upper-bound)))
            (set! p (list-ref primes (- (length primes) 2)))
            (set! q (list-ref primes (- (length primes) 1)))
            (set! M (* p q)) 
            (set! prevX s))
        #f))


(define (bbs)
    (if (equal? prevX 'E)
        (seed 512 1024)) 
    (let ((x (mod (* prevX prevX) M)))
        (set! prevX x)
        x))

;; random - generates a random number between 0 and 1
(define (random) (/ (bbs) M))
