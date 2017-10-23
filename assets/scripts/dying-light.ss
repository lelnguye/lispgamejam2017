;; dying-light.ss
;; by tsip
;;
;; the game

(let ((files-list
        '("assets/scripts/util.ss"
          "assets/scripts/matrix.ss"
          "assets/scripts/random.ss"
          "assets/scripts/items.ss"
          "assets/scripts/state.ss"
          "assets/scripts/character.ss"
          "assets/scripts/tiles.ss"
          "assets/scripts/world.ss"
          "assets/scripts/biwas-canvas.ss"
          "assets/scripts/biwas-canvas-images.ss"
          "assets/scripts/biwas-animation.ss"
          "assets/scripts/biwas-events.ss"
          "assets/scripts/draw-world.ss"
          "assets/scripts/dl-lights.ss"
          "assets/scripts/dl-characters.ss"
          "assets/scripts/dl-maps.ss"
            ))
        (load-files-f
            (lambda (files-list f)
                (if (not (null? files-list))
                    (begin (load (car files-list)) (f (cdr files-list) f))))))
    (load-files-f files-list load-files-f))

(define title "... the dying of the light")
(define description "a story")
(define author "tsip")
(define world-pane-id 'world-pane) ;; canvas where the maps are drawn to
(define items-pane-id 'items-pane) ;; canvas where the items are drawn to 
(define ui-pane-id 'ui-pane) ;; canvas where ui is drawn to


;; fades the screen to black
(define (fade-to-black context canvas-width canvas-height alpha-inc sleep-time)
    (let ((alpha 0)
          (canvas-rect (rectangle 0 0 canvas-width canvas-height)))
        (letrec ((fade
                    (lambda ()
                        (begin
                            (if (< alpha 1)
                                (begin
                                    (set! alpha (+ alpha alpha-inc))
                                    (call-animation-frame fade)))
                            (set-global-alpha context alpha)
                            (draw-rectangle context canvas-rect)
                            (sleep sleep-time)
                            ))))
            (begin
                (set-fill! canvas-rect "black")
                (set-global-composite-op context "multiply")
                (call-animation-frame fade)))))

(define (spawn-player-controlled-characters dungeon)
  (begin
	  (item-execute dungeon "add-object!" 4 6 kid-A)
	  (item-execute dungeon "add-object!"  4 8 kid-A)))


(define (start)
    (let* ((canvas (get-canvas items-pane-id))
           (canvas-width (js-ref canvas "width"))
           (canvas-height (js-ref canvas "height"))
           (world-context (canvas-context world-pane-id))
           (context (canvas-context items-pane-id))
           (logo-ctr 0)
           (logo-sprite (create-sprite "assets/images/logo-loop.png" 3200 800 10 0 0 640 400))
           (logo-width  640) 
           (logo-height 400)  
           (logo-x (/ (- canvas-width logo-width) 2))
           (logo-y (/ (- canvas-height logo-height) 2))
           (dw-tiles (tileset "assets/images/tiles2.png" 64 64 1024 384))
           (dark-world (world "dark world" "" dw-tiles 5 5))
           (now (current-date))       
           (s (+ (date-millisecond now)
             (date-second now)
             (date-minute now)
             (date-hour now))))
      (begin
        ;; initialize random
        (seed s 1024)
        (random)
        (random)
        (setup-world dark-world)
        (save-context context)
        (animate-fixed-sprite context logo-sprite logo-x logo-y logo-width logo-height 0.7 (lambda () (begin (set! logo-ctr (+ 1 logo-ctr)) (< logo-ctr 10))))
        (sleep 2)
        ;;(fade-to-black context canvas-width canvas-height 0.02 0.3)
        (restore-context context)
        (clear-rect context 0 0 canvas-width canvas-height)
	(let ((start (item-execute dark-world "get-dungeon" 0 0)))
	  	(begin
		  	(spawn-player-controlled-characters start)
			(draw-dungeon-items context start (rectangle 0 0 canvas-width canvas-height))
			(draw-dungeon-tiles world-context start (rectangle 0 0 canvas-width canvas-height)))
        )))


