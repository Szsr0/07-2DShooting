#lang racket
(require 2htdp/image
         "enemy.rkt")

(provide missile-x missile-y
         draw-missile next-missile)

(define MISSILE (circle 10 "solid" "red"))
(define SCENE-WIDTH 400)
(define SCENE-HEIGHT 600)

(define (missile-x m) (car m))
(define (missile-y m) (cadr m))

(define (move-missile m)
  (list (missile-x m) (- (missile-y m) 8)))

(define (missile-alive? m e)
  (and (not (string? m)) (not (string? e))
       (>= (missile-y m) 0)
       (not (hit? m e))))

(define (next-missile m e)
  (cond [(string? m) "none"]
        [(missile-alive? m e) (move-missile m)]
        [else "none"]))

(define (draw-missile m)
  (cond [(string? m) (empty-scene SCENE-WIDTH SCENE-HEIGHT)]
        [else (place-image MISSILE
                           (missile-x m) (missile-y m)
                           (empty-scene SCENE-WIDTH SCENE-HEIGHT))]))