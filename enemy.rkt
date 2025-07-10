#lang racket
(require 2htdp/image)

(provide enemy-x enemy-y enemy-v
         initial-enemies draw-enemies next-enemies)

(define SCENE-WIDTH 400)
(define ENEMY-SIZE 60)
(define ENEMY (square ENEMY-SIZE "solid" "orange"))

;; 敵の構造----------
(define (enemy x y v) (list x y v))
(define (enemy-x e) (car e))
(define (enemy-y e) (cadr e))
(define (enemy-v e) (caddr e))

;; 初期の敵リスト(複数体)----------
(define initial-enemies
  (list
   (enemy 100 100 3)
   (enemy 200 120 -4)
   (enemy 300 150 5)))

;; 敵の移動----------
(define (move-enemy e)
  (cond
    [(< (enemy-x e) (/ ENEMY-SIZE 2))
     (enemy (/ ENEMY-SIZE 2) (enemy-y e) (- (enemy-v e)))]
    [(> (enemy-x e) (- SCENE-WIDTH (/ ENEMY-SIZE 2)))
     (enemy (- SCENE-WIDTH (/ ENEMY-SIZE 2)) (enemy-y e) (- (enemy-v e)))]
    [else
     (enemy (+ (enemy-x e) (enemy-v e)) (enemy-y e) (enemy-v e))]))

;; ミサイルが敵に当たったか----------
(define (enemy-hit? m e)
  (and (not (string? m))
       (< (abs (- (car m) (enemy-x e))) (/ ENEMY-SIZE 2))
       (< (abs (- (cadr m) (enemy-y e))) (/ ENEMY-SIZE 2))))
;; 単体の敵を次の状態に(撃破されてたら "none")
(define (next-enemy e missiles)
  (cond [(ormap (lambda (m) (enemy-hit? m e)) missiles) "none"]
        [else (move-enemy e)]))

;; 複数の敵の次の状態----------
(define (next-enemies enemies missiles)
  (map (lambda (e) (if (string? e) "none" (next-enemy e missiles))) enemies))

;; 複数の敵を描画----------
(define (draw-enemies enemies scene)
  (foldl (lambda (e s)
           (if (string? e) s
               (place-image ENEMY (enemy-x e) (enemy-y e) s)))
         scene
         enemies))