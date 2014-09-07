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
     (sleep 0.005)
     (loop (+ x 1) (+ y 1)))))

(thread
 (lambda ()
   (let loop ()
     (send test-frame refresh)
     (sleep 0.01)
     (loop))))