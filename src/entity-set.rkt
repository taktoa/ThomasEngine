; entity-set.rkt
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
  data/queue
  "entity.rkt")

(provide
 (all-defined-out))

(define entity-set%
  (class object%
    ;; Class fields
    (field
     [update-queue (make-queue)]
     [entity-hash (make-hash)])
    
    ;; Private functions
    ; Set the properties of one entity
    (define/private (set-entity-properties! name props)
      (hash-update! entity-hash name
                    (λ (e) (make-entity (send e modify-props props)))))
    
    ; Apply one update
    (define/private (apply-update! c)
      (match c
        [(cons n 'add) (hash-set! entity-hash n (make-entity (hash)))]
        [(cons n 'delete) (hash-remove! entity-hash n)]
        [(cons n change) (set-entity-properties! n change)]
        [else (raise-argument-error 'apply-update! "change pair" c)]))
    
    ; Clear queue
    (define/private (clear-queue!)
      (set! update-queue (make-queue)))
    
    ;; Public functions
    ; Queue up an entity addition
    (define/public (add-entity name)
      (enqueue! update-queue (cons name 'add)))
    
    ; Queue up an entity removal
    (define/public (rem-entity name)
      (enqueue! update-queue (cons name 'delete)))
    
    ; Queue up entity changes
    (define/public (set-entity-properties name props)
      (enqueue! update-queue (cons name props)))
    
    ; Queue up a single entity change
    (define/public (set-entity-property name key value)
      (set-entity-properties name (hash key value)))
    
    ; Get all entities
    (define/public (get-entities)
      (update!)
      (hash-copy entity-hash))
    
    ; Get one entity
    (define/public (get-entity name)
      (hash-ref entity-hash name))
    
    ; Get all properties of an entity
    (define/public (get-entity-properties name)
      (send (get-entity name) prop-get-all))
    
    ; Get a property of an entity
    (define/public (get-entity-property name prop)
      (send (get-entity name) prop-get prop))
    
    ; Apply all updates in queue and clear it
    (define/public (update!)
      (for ([c (in-queue update-queue)])
        (apply-update! c))
      (clear-queue!))
    
    ;; Class initialization
    (super-new)))