;; tiles.ss
;; by tsip
;; 

;; creates a tileset from the given image
;; 
;; tileset-img - image file name with the tiles
;; tile-width - the width of a tile
;; tile-height - the height of a tile
;; image-width - width of the tileset image
;; image-height - height of the tileset image
(define (tileset tileset-img tile-width tile-height image-width image-height)
    (if (and (string? tileset-img)
             (number? tile-width)
             (number? tile-height))
        (let ((create-tiles-f
                (lambda (x y f)
                    (let ((rect (list x y tile-width tile-height))
                          (next-x (+ tile-width x)))
                        (let ((next-y (if (>= next-x image-width) (+ tile-height y) y))
                              (tile (create-tile (format "tile_~a_~a" x y) rect)))
                            (if (>= next-y image-height)
                                (cons tile '())
                                (if (>= next-x image-width)
                                    (cons tile (f 0 next-y f))
                                    (cons tile (f next-x next-y f)))))))))
            (list tileset-img (create-tiles-f 0 0 create-tiles-f)))
        #f))

;; creates a tile object
(define (create-tile name bounds)
    (let ((tile (item name "")))
        (begin
            (set-property! tile "bounds" bounds)
            (set-property! tile "alpha" 1)
            (set-property! tile "passable" #t)
            tile)))


(define (get-tileset-image-file tileset) (car tileset))


(define (get-tile tileset tile-ind)
    (let ((tiles (cadr tileset)))
        (list-ref tiles tile-ind)))


