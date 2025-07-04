#lang racket
(require "player.rkt"
         "enemy.rkt"
         "missile.rkt")
(provide initial-world world-draw world-next world-control)

(struct world (me enemy missile) #:transparent)

(define initial-world
  (world initial-player initial-enemy "none"))

(define (world-draw w)
  (define bg (draw-missile (world-missile w)))
  (define with-enemy (draw-enemy (world-enemy w) bg))
  (draw-player (world-me w) with-enemy))

(define (world-next w)
  (define new-enemy (next-enemy (world-enemy w) (world-missile w)))
  (define new-missile (next-missile (world-missile w) (world-enemy w)))
  (world (world-me w) new-enemy new-missile))

(define (world-control w key)
  (define-values (new-me new-missile)
    (control-player (world-me w) (world-missile w) key))
  (world new-me (world-enemy w) new-missile))