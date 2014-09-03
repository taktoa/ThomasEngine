#lang racket

(provide
 (all-defined-out))
  
; A simple matrix ADT
(struct matrix (width height data) #:mutable #:constructor-name make-matrix-struct)

(define (make-matrix width height [gen (lambda (x y) #f)]) 
  (make-matrix-struct width height
    (for/vector ([i (in-range (* width height))])
      (gen (quotient i height) (remainder i height)))))

(define (matrix-ref matrix x y) 
  (unless (< -1 x (matrix-width matrix)) 
    (raise-argument-error 'matrix-ref (format "integer in range [0, ~a]" (- (matrix-width matrix) 1)) x))
  (unless (< -1 y (matrix-height matrix)) 
    (raise-argument-error 'matrix-ref (format "integer in range [0, ~a]" (- (matrix-height matrix) 1)) y))
  (vector-ref (matrix-data matrix) (+ y (* x (matrix-height matrix)))))

(define (matrix-set! matrix x y val) 
  (unless (< -1 x (matrix-width matrix)) 
    (raise-argument-error 'matrix-ref (format "integer in range [0, ~a]" (- (matrix-width matrix) 1)) x))
  (unless (< -1 y (matrix-height matrix)) 
    (raise-argument-error 'matrix-ref (format "integer in range [0, ~a]" (- (matrix-height matrix) 1)) y))
  (vector-set! (matrix-data matrix) (+ y (* x (matrix-height matrix))) val))