#lang racket
(require 2htdp/image)

(provide enemy-x enemy-y enemy-v
         initial-enemy draw-enemy next-enemy)

(define SCENE-WIDTH 400)
(define ENEMY-SIZE 60)
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

(define (next-enemy e missiles)
  (cond [(string? e) "none"]
        [(ormap (lambda (m)
                  (and (not (string? m))
                       (< (abs (- (car m) (car e))) (/ ENEMY-SIZE 2))
                       (< (abs (- (cadr m) (cadr e))) (/ ENEMY-SIZE 2))))
                missiles)
         "none"]
        [else (move-enemy e)]))

(define (draw-enemy e scene)
  (cond [(string? e) scene]
        [else (place-image ENEMY (enemy-x e) (enemy-y e) scene)]))

;; ------------------------------
;; ▼ 以下:複数の敵に対応した追加コード ▼
;; ------------------------------

;; 初期の敵リスト(複数体)
(define initial-enemies
  (list
   (enemy 100 100 3)
   (enemy 200 120 -4)
   (enemy 300 150 5)))

;; 複数の敵の次の状態を計算
(define (next-enemies enemies missiles)
  (map (lambda (e) (next-enemy e missiles)) enemies))

;; 複数の敵を描画
(define (draw-enemies enemies scene)
  (foldl (lambda (e s) (draw-enemy e s)) scene enemies))
