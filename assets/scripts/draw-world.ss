;; draw-world.ss
;; by tsip


;; draw the tiles of the dungeon 
(define (draw-dungeon-tiles context dungeon drawing-rect)
    (let* ((d-tiles (get-property dungeon "tileset"))
           (tileset-img (load-image (car d-tiles)))
           (rect-details (car drawing-rect))
           (drawing-x (list-ref rect-details 0))
           (drawing-y (list-ref rect-details 1))
           (drawing-width (list-ref rect-details 2))
           (drawing-height (list-ref rect-details 3))
           (row-count (get-property dungeon "rows"))
           (col-count (get-property dungeon "columns"))
           (first-tile (item-execute dungeon "get-tile" 0 0))
           (tile-bounds (get-property first-tile "bounds"))
           (tile-width (list-ref tile-bounds 2))
           (tile-height (list-ref tile-bounds 3))
           (dungeon-width (* tile-width col-count))
           (dungeon-height (* tile-height row-count))
           (scale-width-factor (/ drawing-width dungeon-width))
           (scale-height-factor (/ drawing-height dungeon-height))
           (scaled-width (* tile-width scale-width-factor))
           (scaled-height (* tile-height scale-height-factor)))
        (letrec (
            (loop-dungeon
                (lambda (ri ci)
                    (if (< ri row-count)
                        (let* ((tile (item-execute dungeon "get-tile" ri ci))
                               (tile-bounds (get-property tile "bounds"))
                               (tile-x (list-ref tile-bounds 0))
                               (tile-y (list-ref tile-bounds 1))
                               (dest-x (* ci scaled-width))
                               (dest-y (* ri scaled-height))
                               (end-ci? (>= ci (- col-count 1))))
                            (begin
                                (draw-slice 
                                    context 
                                    tileset-img 
                                    tile-x
                                    tile-y
                                    tile-width
                                    tile-height
                                    dest-x
                                    dest-y
                                    scaled-width
                                    scaled-height)
                                (if end-ci?
                                    (loop-dungeon (+ ri 1) 0)
                                    (loop-dungeon ri (+ ci 1)))))))))
            (save-context context)
            (loop-dungeon 0 0)
            (restore-context context))))
                                    
        
(define (draw-dungeon-items context dungeon drawing-rect)
    (let* ((items (item-execute dungeon "list-objects"))
           (num-items (length items))
           (rect-details (car drawing-rect))
           (drawing-x (list-ref rect-details 0))
           (drawing-y (list-ref rect-details 1))
           (drawing-width (list-ref rect-details 2))
           (drawing-height (list-ref rect-details 3))
           (row-count (get-property dungeon "rows"))
           (col-count (get-property dungeon "columns"))
           (first-tile (item-execute dungeon "get-tile" 0 0))
           (tile-bounds (get-property first-tile "bounds"))
           (tile-width (list-ref tile-bounds 2))
           (tile-height (list-ref tile-bounds 3))
           (dungeon-width (* tile-width col-count))
           (dungeon-height (* tile-height row-count))
           (scale-width-factor (/ drawing-width dungeon-width))
           (scale-height-factor (/ drawing-height dungeon-height))
           (scaled-width (* tile-width scale-width-factor))
           (scaled-height (* tile-height scale-height-factor)))
        (letrec (
            (loop-items
                (lambda (items)
		 	(console-log "noob noob")
                    (if (not (null? items))
                        (let* ((item (car items))
                               (item-image (load-image (get-property item "image")))
                               (item-position (get-property item "position"))
                               (dest-x (* (car item-position) scaled-width))
                               (dest-y (* (cdr item-position) scaled-height)))
                            (begin
			      	(console-log (format "~a ~a ~a ~a" (car item) dest-x dest-y item-position)
                                (draw-image 
                                    context 
                                    item-image 
                                    dest-x 
                                    dest-y 
                                    scaled-width 
                                    scaled-height)
                                (loop-items (cdr items))))))))
           
            (begin
	      	(console-log (format "~a" (length items))) 
                (save-context context)
                (loop-items items)                 
                (restore-context context)))))


(define (darken context dungeon drawing-rect)
    (let* ((rect-details (car drawing-rect))
           (drawing-x (list-ref rect-details 0))
           (drawing-y (list-ref rect-details 1))
           (drawing-width (list-ref rect-details 2))
           (drawing-height (list-ref rect-details 3))
           (row-count (get-property dungeon "rows"))
           (col-count (get-property dungeon "columns"))
           (first-tile (item-execute dungeon "get-tile" 0 0))
           (tile-bounds (get-property first-tile "bounds"))
           (tile-width (list-ref tile-bounds 2))
           (tile-height (list-ref tile-bounds 3))
           (dungeon-width (* tile-width col-count))
           (dungeon-height (* tile-height row-count))
           (scale-width-factor (/ drawing-width dungeon-width))
           (scale-height-factor (/ drawing-height dungeon-height))
           (scaled-width (* tile-width scale-width-factor))
           (scaled-height (* tile-height scale-height-factor)))
        (letrec (
            (loop-dungeon
                (lambda (ri ci)
                    (if (< ri row-count)
                        (let* ((tile (item-execute dungeon "get-tile" ri ci))
                               (tile-alpha (get-property tile "alpha"))
                               (tile-bounds (get-property tile "bounds"))
                               (tile-x (list-ref tile-bounds 0))
                               (tile-y (list-ref tile-bounds 1))
                               (dest-x (* ci scaled-width))
                               (dest-y (* ri scaled-height))
                               (end-ci? (>= ci (- col-count 1)))
                               (tile-rect (rectangle dest-x dest-y scaled-width scaled-height)))
                            (begin
                                (set-fill! tile-rect "black")
                                (set-global-alpha context (- 1 tile-alpha))
                                (draw-rectangle context tile-rect)
                                (if end-ci?
                                    (loop-dungeon (+ ri 1) 0)
                                    (loop-dungeon ri (+ ci 1)))))))))
                (begin
                    (save-context context)
                    (set-global-composite-op context "multiply")
                    (loop-dungeon 0 0)
                    (restore-context context)))))
