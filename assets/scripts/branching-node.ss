;; branching-node.ss
;; by tsip
;; 
;; contains methods to create nodes that can have multiple children


;; null value / empty-node
(define NaN 'NaN)
(define (is-empty-node? node) (equal? node NaN))

(define (value? v)
	(or (and (symbol? v) (not (is-empty-node? v))) (string? v) (number? v) (boolean? v) (and (list? v) (not (= (length v) 2)))))

;; creates the node
(define (node value) (if (value? value) (cons value NaN) #f))

(define (node? n)
	(if (pair? n)
		(let ((val (value n))
			  (loc (children n))
			  (nodes? 
				  (lambda (loc f)
					  (if (or (is-empty-node? loc) (null? loc))
						  #t
						  (let ((child (car loc)))
							  (if (node? child)
								  (f (cdr loc) f)
								  #f))))))
			(and (value? val) (nodes? loc nodes?)))
	#f))


;; gets the children of the node
(define children cdr)

;; value stored at the given node
(define (value node) (car node))

;; checks if the node is a leaf 
(define (leaf? node) (is-empty-node? (children node)))
