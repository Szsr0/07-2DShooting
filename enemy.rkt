#lang racket
(require 2htdp/image)

(provide enemy-x enemy-y enemy-v enemy-hp
         initial-enemies draw-enemies next-enemies)

(define SCENE-WIDTH 400)
(define ENEMY-SIZE 60)
(define ENEMY (square ENEMY-SIZE "solid" "orange"))

;; 敵の構造----------
(define (enemy x y v hp) (list x y v hp))
(define (enemy-x e) (car e))
(define (enemy-y e) (cadr e))
(define (enemy-v e) (caddr e))
(define (enemy-hp e) (list-ref e 3))

;; 初期の敵リスト(複数体)----------
(define initial-enemies
  (list
   (enemy 100 100 3 5)
   (enemy 200 120 -4 5)
   (enemy 300 150 5 5)))

;; 敵の移動----------
(define (move-enemy e)
  (let* ((x (enemy-x e))
         (y (enemy-y e))
         (v (enemy-v e))
         (hp (enemy-hp e))
         (half-size (/ ENEMY-SIZE 2)))
    (cond
      ((< x half-size)
       (enemy half-size y (- v) hp))
      ((> x (- SCENE-WIDTH half-size))
       (enemy (- SCENE-WIDTH half-size) y (- v) hp))
      (else
       (enemy (+ x v) y v hp)))))

;; ミサイルが敵に当たったか----------
(define (enemy-hit? m e)
  (and (not (string? m))
       (< (abs (- (car m) (enemy-x e))) (/ ENEMY-SIZE 2))
       (< (abs (- (cadr m) (enemy-y e))) (/ ENEMY-SIZE 2))))

;; 単体の敵を次の状態に(撃破されてたら "none")
(define (next-enemy e missiles)
  (cond
    ((ormap (lambda (m) (enemy-hit? m e)) missiles)
     (if (<= (sub1 (enemy-hp e)) 0)
         "none"
         (enemy (enemy-x (move-enemy e))
                (enemy-y (move-enemy e))
                (enemy-v (move-enemy e))
                (sub1 (enemy-hp e)))))
    (else (move-enemy e))))

;; 複数の敵の次の状態----------
(define (next-enemies enemies missiles)
  (map (lambda (e) (if (string? e) "none" (next-enemy e missiles)))
       enemies))

;; 複数の敵を描画----------
(define (draw-enemies enemies scene)
  (foldl (lambda (e s)
           (if (string? e) 
               s
               (let* ((x (enemy-x e))
                      (y (enemy-y e))
                      (hp (enemy-hp e))
                      (base (place-image ENEMY x y s))
                      (hp-text (text (number->string hp) 20 "white")))
                 (place-image hp-text x y base))))
         scene
         enemies))