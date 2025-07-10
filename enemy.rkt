#lang racket
(require 2htdp/image)

(provide enemy-x enemy-y enemy-v
         initial-enemy draw-enemy next-enemy
         ENEMY-SIZE hit?)

(define SCENE-WIDTH 400)
(define ENEMY-SIZE 40)
(define ENEMY (square ENEMY-SIZE "solid" "blue"))

(define (enemy x y v) (list x y v))
(define (enemy-x e) (car e))
(define (enemy-y e) (cadr e))
(define (enemy-v e) (caddr e))

(define initial-enemy (enemy 200 120 5))

(define (move-enemy e)
  (cond
    [(< (enemy-x e) (/ ENEMY-SIZE 2))
     (enemy (/ ENEMY-SIZE 2) (enemy-y e) (- (enemy-v e)))]
    [(> (enemy-x e) (- SCENE-WIDTH (/ ENEMY-SIZE 2)))
     (enemy (- SCENE-WIDTH (/ ENEMY-SIZE 2)) (enemy-y e) (- (enemy-v e)))]
    [else
     (enemy (+ (enemy-x e) (enemy-v e)) (enemy-y e) (enemy-v e))]))

(define (draw-enemy e scene)
  (cond [(string? e) scene]
        [else (place-image ENEMY (enemy-x e) (enemy-y e) scene)]))

(define (hit? m e)
  (and (not (string? m))
       (not (string? e))
       (< (abs (- (car m) (enemy-x e))) (/ ENEMY-SIZE 2))
       (< (abs (- (cadr m) (enemy-y e))) (/ ENEMY-SIZE 2))))

(define (next-enemy e m)
  (cond [(string? e) "none"]
        [(not (hit? m e)) (move-enemy e)]
        [else "none"]))