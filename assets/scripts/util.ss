;; util.ss
; by tsip
;;
;; miscellaneous functions 

;; checks that all values in a list evaluates to true
(define (apply-and ls)
    (if (null? ls)
        #t
        (if (not (car ls))
            #f
            (apply-and (cdr ls)))))

;; for loop
(define (for counter loop-cond-f mutate-f action-f)
    (if (loop-cond-f counter)
        (begin
            (action-f counter)
            (for (mutate-f counter) loop-cond-f mutate-f action-f))))


;; creates a list of prime numbers from 
;; 2 to the given upper bound
(define (list-primes upper-bound)
    (let ((gen-list (lambda (n f) (if (= n upper-bound) '() (cons n (f (+ n 1) f)))))
          (remove-multiples-f 
            (lambda (ls n f)
                (if (null? ls ) 
                    '()
                    (let ((v (car ls)))
                        (if (and (not (= v n))
                                (= (mod v n) 0))
                            (f (cdr ls) n f)
                            (cons v (f (cdr ls) n f)))))))
          
          (get-primes-f
            (lambda (ls i remove-multiples-f f)
                (if (>= i (length ls))
                    ls
                    (let ((n (list-ref ls i))
                          (new-list (lambda (n) (remove-multiples-f ls n remove-multiples-f))))
                        (f (new-list n) (+ i 1) remove-multiples-f f))))))
        (get-primes-f (gen-list 2 gen-list) 0 remove-multiples-f get-primes-f)))



;; helper functions to test if a string is equal
;; to a paired property
(define (name-compare compare-f name pair) (compare-f name (car pair)))
(define (name<? name pair) (name-compare string<? name pair))
(define (name=? name pair) (name-compare string=? name pair))

;; adds a named pair to a list inside a list
(define (add-pair! list-ind item name value)
    (let ((lop (list-ref item list-ind))
          (v (if (list? value) (cons value '()) value)))
        (let ((pair (cons name v)))
            (if (null? lop)
                (set-car! (list-tail item list-ind) (cons pair lop))
                (let ((pair-count (length lop)) 
                      (pair-ind (binary-search lop name #t name<? name=?)))
                    (if (and (< pair-ind pair-count)
                             (name=? name (list-ref lop pair-ind)))
                        (set-cdr! (list-ref lop pair-ind) v)
                        (insert-at! lop pair pair-ind)))))))

;; searches for a value inside a list and returns its
;; index. if the value cannot be found, the function
;; returns false if aprox-ind? is set to true
;;
;; ls            - sorted list 
;; val           - value to be searched
;; approx-ind?   - flag to return approximate index
;; sort-f        - function that determines sorting order
;; compare-f     - compare function 
(define (binary-search ls val approx-ind? sort-f compare-f)
    (let ((start 0)
          (end (- (length ls) 1))
          (find-ind-f
            (lambda (start end f)
                (if (> start end)
                    (if approx-ind? start #f)
                    (let ((m (floor (/ (+ start end) 2)))) ;; mid point
                        (let ((m-val (list-ref ls m)))
                            (let ((is-first-half? (sort-f val m-val)))
                                (if (compare-f val m-val)
                                    m
                                    (if is-first-half? (f start (- m 1) f) (f (+ m 1) end f))))))))))
        (find-ind-f start end find-ind-f)))

(define (index-of ls elem compare-f)
    (let ((loop-f
            (lambda (i f)
                (if (>= i (length ls))
                    #f
                    (if (compare-f elem (list-ref ls i))
                        i
                        (f (+ i 1) f))))))
        (loop-f 0 loop-f)))

;; (permanently) inserts the value into the list at the 
;; given index
;; 
;; ls            - list to update
;; val           - value to insert
;; ind           - index where the value will be inserted
(define (insert-at! ls val ind)
    (let ((len (length ls))
          (list-set! (lambda (ls ind val) (set-car! (list-tail ls ind) val)))  ;; sets the value at given index
          (snoc! (lambda (ls val) (set-cdr! (list-tail ls (- (length ls) 1)) (cons val '()))))) ;; appends val to end of list
        (if (and (>= ind 0) (< ind len))
            (let ((push-down!
                    (lambda (oi ni f)
                        (if (>= oi ind)
                            (let ((o-val (list-ref ls oi)))
                                (begin
                                    (if (< ni len)
                                        (list-set! ls ni o-val)
                                        (snoc! ls o-val))
                                    (f (- oi 1) (- ni 1) f)))))))
                (begin 
                    (push-down! (- len 1) len push-down!)
                    (list-set! ls ind val)))
            (snoc! ls val))))


;; removes element at the given index from the list
;;
;; ls            - list to update
;; ind           - index
(define (remove-element-at! ls ind)
    (let ((len (length ls)))
        (if (< ind len)
            (let ((val (list-ref ls ind))
                  (push-up!
                    (lambda (oi ni f)
                        (if (= oi len)
                            (set-cdr! (list-tail ls (- ni 1)) '())
                            (begin
                                (set-car! (list-tail ls ni) (list-ref ls oi))
                                (f (+ oi 1) (+ ni 1) f))))))
                (push-up! (+ ind 1) ind push-up!)
                val)
            #f)))

(define (make-list-of list-length val)
    (if (= 0 list-length) '() (cons val (make-list-of (- list-length 1) val))))

(define (symbol<? sym1 sym2) (string<? (symbol->string sym1) (symbol->string sym2)))
