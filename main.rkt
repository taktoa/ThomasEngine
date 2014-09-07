#lang racket
(require 
  "texture.rkt"
  racket/gui
  racket/runtime-path)

(define-runtime-path RUNTIME_DIR ".")

(define test-frame
  (new frame%
       [label "Testing"]
       [style '(no-resize-border)]))

(define width 960)
(define height 540)

(define test-ac
  (new texture-canvas% 
       [parent test-frame]
       [texture-path (build-path RUNTIME_DIR "bigtexture.png")]))

(send test-ac set-position 400 400)

(send test-frame show #t)

(thread
 (lambda ()
   (let loop ([x 400] [y 400])
     (send test-ac set-position x y)
     (sleep 0.2)
     (loop (+ x 1) (+ y 1)))))

(thread
 (lambda ()
   (let loop ()
     (send test-frame refresh)
     (sleep 0.5)
     (loop))))


;#lang racket
;(require 
;  "ascii.rkt"
;  racket/gui
;  racket/runtime-path)
;
;(define-runtime-path RUNTIME_DIR ".")
;
;(define test-frame
;  (new frame%
;       [label "Testing"]
;       [style '(no-resize-border)]))
;
;(define tile-w 9)
;(define tile-h 16)
;(define tileset-w 16)
;(define tileset-h 16)
;(define canvas-w 32)
;(define canvas-h 32)
;(define tileset-path (build-path RUNTIME_DIR "tiles.png"))
;
;(define test-ac
;  (new ascii-canvas% 
;       [parent test-frame]
;       [tiles (tileset tileset-path tile-w tile-h tileset-w tileset-h)]
;       [canvas-width canvas-w]
;       [canvas-height canvas-h]))
;
;(for* ([xi (in-range canvas-w)]
;       [yi (in-range canvas-h)])
;  (send test-ac write 0 xi yi))
;
;(send test-frame show #t)
;
;(thread
; (lambda ()
;   (let loop ()
;     (send test-ac write 
;           (random 256)
;           (random canvas-w) (random canvas-h))
;     (sleep 0.1)
;     (loop))))
;
;(thread
; (lambda ()
;   (let loop ()
;     (send test-frame refresh)
;     (sleep 0.5)
;     (loop))))
