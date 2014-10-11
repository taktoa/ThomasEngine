; entity.rkt
; Copyright 2014 Remy E. Goldschmidt <taktoa@gmail.com>
; This file is part of ThomasEngine.
;    ThomasEngine is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    ThomasEngine is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with ThomasEngine. If not, see <http://www.gnu.org/licenses/>.

#lang racket
(require
  data/queue)

(provide
 (all-defined-out))

(define (make-entity prop)
  (new entity% [properties prop]))

(define entity%
  (class object%
    ;; Class fields
    (init-field
     ;[update-queue (make-queue)]
     [properties (hash)])
    
    ;; Private functions
    ; Merge changes into the properties
    ;    (define/private (prop-merge! changes)
    ;      (hash-for-each
    ;       changes
    ;       (λ (k v)
    ;         (if (eq? v 'delete)
    ;             (hash-remove! properties k)
    ;             (hash-set! properties k v)))))
    
    ;; Public functions
    ; Commit changes in the update queue
    ;    (define/public (update!)
    ;      (for ([u (queue->list update-queue)])
    ;        (prop-merge! u))
    ;      (set! update-queue (make-queue)))
    
    ; Change a property
    (define/public prop-update
      (case-lambda
        [(k v) (prop-update properties k v)]
        [(p k v) (if (equal? v 'delete)
                     (hash-remove p k)
                     (hash-set p k v))]))
    
    ; Change multiple properties at once
    (define/public (modify-props changes)
      (define cl (hash->list changes))
      (define (loop c ph)
        (cond
          [(eq? c '()) ph]
          [else (define nph
                  (apply
                   (λ (p k v) (prop-update p k v))
                   ph
                   (first c)))
                (loop (rest c) nph)]))
      (loop cl properties))
    
    ; Get a specific property of the entity
    (define/public (prop-get k) (hash-ref properties k))    
    
    ; Get all properties of this entity
    (define/public (prop-get-all) (hash-copy properties))
    
    ;; Class initialization
    (super-new)))