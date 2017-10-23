;; world.ss
;; by tsip
;;
;; contains structures and functions for creating 2D maps and stuff

;; function to create a dungeon / room
;; 
;; name for the dungeon
;; layout - the layout of the dungeon
;; items-list - list of items in the dungeon
;; characters-list - list of characters in the dungeon
(define (create-dungeon name description tileset num-rows num-cols)
    (let ((layout (matrix num-rows num-cols -1))
          (obj-layer (matrix num-rows num-cols 'NaN))
          (dungeon (item name description))
          (coordinates<? 
            (lambda (p q)
                (or (< (car p) (car q))
                    (and (= (car p) (car q)) (< (cdr p) (cdr q)))))))
        (begin
            (set-property! dungeon "layout" layout)
            (set-property! dungeon "objects" obj-layer)
            (set-property! dungeon "tileset" tileset)
            (set-property! dungeon "rows" num-rows)
            (set-property! dungeon "columns" num-cols)
            (set-property! dungeon "exits" (list))
            (add-action!
                dungeon
                "set-tile!"
                (lambda (dungeon ls)
                    (let ((row-ind (list-ref ls 0))
                          (col-ind (list-ref ls 1))
                          (tile-ind (list-ref ls 2))
                          (layout (get-property dungeon "layout"))
                          (tileset (get-property dungeon "tileset")))
                        (let ((tiles (cadr tileset)))
                            (if (< tile-ind (length tiles))
                                (matrix-set! layout row-ind col-ind tile-ind))))))
            (add-action!
                dungeon
                "get-tile"
                (lambda (dungeon ls)
                    (let ((row-ind (list-ref ls 0)) 
                          (col-ind (list-ref ls 1))
                          (layout (get-property dungeon "layout"))
                          (tileset (get-property dungeon "tileset")))
                        (let ((tile-ind (matrix-ref layout row-ind col-ind)))
                            (get-tile tileset tile-ind)))))
            (add-action!
                dungeon
                "add-object!"
                (lambda (dungeon ls)
                    (let ((row-ind (list-ref ls 0))
                          (col-ind (list-ref ls 1))
                          (item (list-ref ls 2))
                          (row-count (get-property dungeon "rows"))
                          (col-count (get-property dungeon "columns"))
                          (obj-layer (get-property dungeon "objects")))
                        (if (and (< row-ind row-count) (< col-ind col-count))
                            (let ((obj-at (matrix-ref obj-layer row-ind col-ind))
                                  (position (cons row-ind col-ind)))
                                (if (equal? obj-at 'NaN)
                                    (begin
                                        (matrix-set! obj-layer row-ind col-ind item)
                                        (set-property! item "position" position))))))))
            (add-action!
                dungeon
                "remove-object!"
                (lambda (dungeon ls)
                    (let ((row-ind (list-ref ls 0))
                          (col-ind (list-ref ls 1))
                          (obj-layer (get-property dungeon "objects")))
                        (let ((item (matrix-ref obj-layer row-ind col-ind)))
                            (if (not (equal? item 'NaN))
                                (begin
                                    (matrix-set! obj-layer row-ind col-ind 'NaN)
                                    (set-property! item "position" 'NaN)))))))
            (add-action!
                dungeon
                "move-object!"
                (lambda (dungeon ls)
                    (let ((item (list-ref ls 0))
                          (new-row-ind (list-ref ls 1))
                          (new-col-ind (list-ref ls 2))
                          (obj-layer (get-property dungeon "objects")))
                        (let ((item-pos (get-property item "position"))
                              (position (cons new-row-ind new-col-ind)))
                            (let ((item-ri (car item-pos))
                                  (item-ci (cdr item-pos)))
                                (if (and (eq? item (matrix-ref obj-layer item-ri item-ci))
                                         (equal? (matrix-ref obj-layer new-row-ind new-col-ind) 'NaN))
                                    (begin
                                        (matrix-set! obj-layer item-ri item-ci 'NaN)
                                        (matrix-set! obj-layer new-row-ind new-col-ind item)
                                        (set-property! item "position" position))))))))
            (add-action! 
                dungeon
                "get-object"
                (lambda (dungeon ls)
                    (let ((row-ind (list-ref ls 0))
                          (col-ind (list-ref ls 1))
                          (obj-layer (get-property dungeon "objects")))
                        (matrix-ref obj-layer row-ind col-ind))))
            (add-action!
                dungeon
                "list-objects"
                (lambda (dungeon)
                    (let ((obj-layer (get-property dungeon "objects"))
                          (row-count (get-property dungeon "rows"))
                          (col-count (get-property dungeon "columns")))
                        (let ((loop-f
                                (lambda (ri ci f)
                                    (if (>= ri row-count)
                                        '()
                                        (let ((end-col? (= ci (- col-count 1))))
                                            (let ((obj (matrix-ref obj-layer ri ci))
                                                  (next-ri (if end-col? (+ ri 1) ri))
                                                  (next-ci (if end-col? 0 (+ ci 1))))
                                                (if (not (equal? obj 'NaN))
                                                    (cons obj (f next-ri next-ci f))
                                                    (f next-ri next-ci f))))))))
                            (loop-f 0 0 loop-f)))))
            (add-action!
                dungeon
                "add-exit!"
                (lambda (dungeon ls)
                    (let ((x (list-ref ls 0))
                          (y (list-ref ls 1))
                          (exits (get-property dungeon "exits"))
                          )
                        (let ((exit (cons x y)))
                            (if (null? exits)
                                (set-property! dungeon (cons exit exits))
                                (let ((ind (binary-search exits exit #t coordinates<? equal?)))
                                    (if (or (> ind (length exits))
                                            (not (equal? (list-ref exits ind) exit)))
                                        (insert-at! exits exit ind))))))))
            (add-action!
                dungeon
                "is-exit?"
                (lambda (dungeon ls)
                    (let ((x (list-ref ls 0))
                          (y (list-ref ls 1))
                          (exits (get-property dungeon "exits")))
                        (binary-search exits (cons x y) #f coordinates<? equal?))))
            dungeon))) 

;; the world is a collection of connected dungeons / rooms
(define (world name description tileset rows columns)
    (let ((world (item name description))
          (world-map (matrix rows columns)))
        (begin
            (set-property! world "map" world-map)
            (set-property! world "characters" (list))
            (set-property! world "tileset" tileset)
            (set-property! world "rows" rows)
            (set-property! world "columns" columns)
            (set-property! world "dungeon-rows" 8)
            (set-property! world "dungeon-cols" 13)
            (add-action!
                world
                "get-neighbors"
                (lambda (world ls)
                    (let ((row-ind (list-ref ls 0))
                          (col-ind (list-ref ls 1)))
                        (let ((N (cons -1 0))
                              (E (cons 0 1))
                              (S (cons 1 0))
                              (W (cons 0 -1))
                              (get-dungeon
                                (lambda (wmap ri ci)
                                    (let ((row (+ row-ind ri)) (col (+ col-ind ci)))
                                        (if (and (> row -1) (> col -1)
                                                 (< row (num-rows wmap)) (< col (num-cols wmap)))
                                            (matrix-ref wmap row col)
                                            #f))))
                              (loop-f
                                (lambda (dir-ls wmap get-dungeon-f f)
                                    (if (null? dir-ls) 
                                        '()
                                        (let ((direction (car dir-ls)))
                                            (let ((ri (car direction)) (ci (cdr direction)))
                                                (let ((d (get-dungeon-f wmap ri ci))) 
                                                    (if d
                                                        (cons d (f (cdr dir-ls) wmap get-dungeon-f f))
                                                        (f (cdr dir-ls) wmap get-dungeon-f f)))))))))
                            (loop-f (list N E S W) (get-property world "map") get-dungeon loop-f)))))
            (add-action!
                world
                "get-dungeon"
                (lambda (world ls)
                    (let ((row-ind (list-ref ls 0))
                          (col-ind (list-ref ls 1))
                          (wmap (get-property world "map")))
                        (matrix-ref wmap row-ind col-ind))))
            (add-action! 
                world
                "add-dungeon!"
                (lambda (world ls)
                    (let ((row-ind (list-ref ls 0))
                          (col-ind (list-ref ls 1))
                          (dungeon-name (list-ref ls 2))
                          (dungeon-description (list-ref ls 3))
                          (wmap (get-property world "map"))
                          (tileset (get-property world "tileset"))
                          (d-rows (get-property world "dungeon-rows"))
                          (d-cols (get-property world "dungeon-cols")))
                        (let ((d (create-dungeon dungeon-name dungeon-description tileset d-rows d-cols)))
                            (begin
                                (matrix-set! wmap row-ind col-ind d))))))
            world)))
                        

