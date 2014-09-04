#lang racket
(require 
  "ascii.rkt"
  racket/gui
  racket/runtime-path)

(define-runtime-path RUNTIME_DIR ".")

(define test-frame
  (new frame%
       [label "Testing"]
       [style '(no-resize-border)]))

(define tile-w 9)
(define tile-h 16)
(define tileset-w 16)
(define tileset-h 16)
(define canvas-w 40)
(define canvas-h 20)
(define tileset-path (build-path RUNTIME_DIR "tiles.png"))

(define test-ac
  (new ascii-canvas% 
       [parent test-frame]
       [tiles (tileset tileset-path tile-w tile-h tileset-w tileset-h)]
       [canvas-width canvas-w]
       [canvas-height canvas-h]))

(for* ([xi (in-range canvas-w)]
       [yi (in-range canvas-h)])
  (send test-ac write 0 xi yi))

(send test-frame show #t)

(thread
 (lambda ()
   (sleep 2.5)
   (let loop ()
     (send test-ac write 
           35
           (random canvas-w) (random canvas-h))
     (sleep 0.1)
     (loop))))

(thread
 (lambda ()
   (let loop ()
     (send test-frame refresh)
     (sleep 0.5)
     (loop))))
