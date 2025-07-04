#lang racket
(require "world.rkt"
         2htdp/universe)

(define (start)
  (big-bang initial-world
            (to-draw world-draw)
            (on-tick world-next)
            (on-key  world-control)))

(start)