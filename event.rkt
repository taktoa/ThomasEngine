; event.rkt
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
  racket/gui
  racket/set)

(provide
 (all-defined-out))

(define evt-handler%
  (class object% 
    ;; Class fields
    (field [pressed-keys (mutable-set)])
    
    ;; Private functions
    ; Translate an event to a more usable form
    (define/private (translate-event e)
      (cond
        [(is-a? e mouse-event%) (mouse-translate-event e)]
        [(is-a? e key-event%) (key-translate-event e)]))
    
    ; Unwrap mouse event type, x, and y, and pass them to mouse-translate
    (define/private (mouse-translate-event e)
      (mouse-translate (send e get-event-type) (send e get-x) (send e get-y)))
    
    ; Translate mouse event type, x, and y to a list
    (define/private (mouse-translate t x y) (list t x y))
    
    ; Unwrap the key-codes from the key-event and pass them to key-translate
    (define/private (key-translate-event e)
      (key-translate (send e get-key-code) (send e get-key-release-code)))
    
    ; Translate raw key-events to more useable pairs
    (define/private (key-translate x y)
      (cond
        [(eq? x 'release) (list y 'release)]
        [(eq? y 'press)   (list x 'press)]
        [true             (void)]))
    
    ; Add or remove a key from the key-set
    (define/private (set-pressed-keys l)
      (match l
        [(list x 'press)   (set-add! pressed-keys x)]
        [(list x 'release) (set-remove! pressed-keys x)]
        [_                 (void)]))
    
    ;; Public functions
    ; Getter for key capture thread and key state
    (define/public (get-key-thread) key-thread)
    (define/public (get-key-state) pressed-keys)
    
    ; Utility function for determining whether or not a key is pressed
    (define/public (is-pressed? c) (set-member? pressed-keys c))
    
    ;; Class initialization
    ; Key capture thread
    (define key-thread
      (thread (lambda ()
                (let loop ()
                  (set-pressed-keys (translate-event (thread-receive)))
                  (loop)))))
    
    (super-new)))