;; matrix 
;; by tsip

;;
;; contains structures and functions for matrices

;; creates a matrix
;;
;; num-rows - the number of rows for the matrix
;; num-cols - the number of columns for the matrix
;; vals - values for the matrix. if no values are provided, 
;;        the function creates a matrix filled with zeroes
(define (matrix num-rows num-cols . vals)
    (let ((val-count (length vals))
          (create-row-f
            (lambda (row-index col-index val-count v f)
                (if (= col-index num-cols)
                    '()
                    (let ((ind (+ (* row-index num-cols) col-index)))
                        (if (< ind val-count)
                            (cons (list-ref vals ind) (f row-index (+ col-index 1) val-count v f))
                            (cons v (f row-index (+ col-index 1) val-count v f)))))))
          (create-matrix-f
            (lambda (row-index create-row-f val-count v f)
                (if (= row-index num-rows)
                    '()
                    (cons (create-row-f row-index 0 val-count v create-row-f) (f (+ row-index 1) create-row-f val-count v f))))))
        (create-matrix-f 0 create-row-f val-count (if (= val-count 1) (car vals) 0) create-matrix-f)))


(define (num-rows matrix) (length matrix))
(define (num-cols matrix) (length (list-ref matrix 0)))

;; checks if item is a matrix
(define (matrix? matrix)
    (and (list? matrix)
        (apply-and (map list? matrix))
        (let ((ls-lengths (map length matrix))
              (equal-lengths? 
                (lambda (ls prev-len f) 
                    (if (null? ls) 
                        #t 
                        (let ((len (car ls)))
                            (if (not (= len prev-len)) #f (f (cdr ls) len f)))))))
            (equal-lengths? ls-lengths (car ls-lengths) equal-lengths?))))


(define (matrix-ref matrix ri ci)
    (if (matrix? matrix)  (list-ref (list-ref matrix ri) ci) #f))

(define (matrix-set! matrix ri ci value)
    (if (matrix? matrix)
        (let ((row (list-ref matrix ri)))
            (set-car! (list-tail row ci) value))))


(define (multiply-matrices m1 m2)
    (if (and (matrix? m1) (matrix? m2))
        (let ((m1-cols (num-cols m1))
              (m2-rows (num-rows m2)))
            (if (= m1-cols m2-rows)
                (let ((m1-rows (num-rows m1))
                      (m2-cols (num-cols m2))
                      (m-vals (list)))
                    (let ((multiply-vals-f
                            (lambda (ri ci i f)
                                (if (= i m1-cols)
                                    0
                                    (let ((m1-val (matrix-ref m1 ri i))
                                          (m2-val (matrix-ref m2 i ci)))
                                        (+ (* m1-val m2-val) (f ri ci (+ i 1) f))))))
                          (loop-cols-f
                            (lambda (ri ci multiply-vals-f f)
                                (if (= ci m2-cols)
                                    '()
                                    (let ((val (multiply-vals-f ri ci 0 multiply-vals-f)))
                                        (cons val (f ri (+ ci 1) multiply-vals-f f))))))
                          (loop-rows-f
                            (lambda (ri loop-cols-f multiply-vals-f f)
                                (if (= ri m1-rows)
                                    '()
                                    (let ((row (loop-cols-f ri 0 multiply-vals-f loop-cols-f)))
                                        (cons row (f (+ ri 1) loop-cols-f multiply-vals-f f)))))))
                        (loop-rows-f 0 loop-cols-f multiply-vals-f loop-rows-f)))))))
