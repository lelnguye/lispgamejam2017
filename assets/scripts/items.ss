;; items.ss
;; by tsip
;;
;; contains methods and data structures for objects that can be
;; interacted with in the game world


;; items are objects in a scene that can be interacted with
(define (item name description) 
    (let ((properties-list (list))
          (actions-list (list)))
        (list name description properties-list actions-list)))

(define (item-properties item) (list-ref item 2))
(define (item-actions item) (list-ref item 3))


;; a property is a pairing between a string name and a value
(define (set-property! item name value)
    (if (string? name) (add-pair! 2 item name value)))

;; an action is a pairing between a string name and a procedure
(define (add-action! item name action)
    (if (and (string? name) (procedure? action))
        (add-pair! 3 item name action)))

;; retrieves a property for the item
;; if the item does not have a property with the given name, then
;; this returns #f
(define (get-property item name)
    (let ((properties (item-properties item)))
        (let ((property-ind (binary-search properties name #f name<? name=?)))
            (if property-ind 
                (let ((property (list-ref properties property-ind)))
                    (if (list? property)
                        (cadr property)
                        (cdr property)))
                #f))))

;; retrieves an action from the item
;; if there are no actions with the given name, this method returns #f
(define (get-action item name)
    (let ((actions (item-actions item)))
        (let ((action-ind (binary-search actions name #f name<? name=?)))
            (if action-ind (cdr (list-ref actions action-ind)) #f))))

(define (item-execute item action-name . args)
    (let ((action (get-action item action-name)))
        (if action
            (if (> (length args) 0)
                (action item args)
                (action item))
            #f)))

