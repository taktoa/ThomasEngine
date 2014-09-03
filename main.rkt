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

(define cw 40)
(define ch 20)

(define test-ac
  (new ascii-canvas% 
       [parent test-frame]
       [tiles (tileset (build-path RUNTIME_DIR "tiles.png") 9 16 16 16)]
       [canvas-width cw]
       [canvas-height ch]))

(for* ([xi (in-range cw)]
       [yi (in-range ch)])
  (send test-ac write #\nul xi yi))

(send test-frame show #t)

(thread
 (lambda ()
   (sleep 2.5)
   (let loop ()
     (send test-ac write 
           #\#
           (random cw) (random ch))
     (sleep 0.1)
     (loop))))

(thread
 (lambda ()
   (let loop ()
     (send test-frame refresh)
     (sleep 0.5)
     (loop))))
