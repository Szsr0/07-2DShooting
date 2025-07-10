#lang racket
(require 2htdp/image
         "enemy.rkt")

(provide missile-x missile-y
         draw-missiles next-missiles move-missile
         hit? any-hit?)

(define MISSILE (circle 10 "solid" "red"))
(define SCENE-WIDTH 400)
(define SCENE-HEIGHT 600)
;; 循環回避のため敵サイズを定義
(define ENEMY-SIZE 40)

;; ミサイル位置取得
(define (missile-x m) (car m))
(define (missile-y m) (cadr m))

;; ミサイル1つの移動/弾のスピードを引数で調整可能に
(define (move-missile m speed)
  (list (missile-x m) (- (missile-y m) speed)))

;; next-missiles の引数にレベルを追加して調整
(define (next-missiles missiles e level)
  (filter (lambda (m) (missile-alive? (move-missile m (+ 8 (* level 2))) e))
          (map (lambda (m) (move-missile m (+ 8 (* level 2)))) missiles)))

;; ミサイルが画面内で、かつ敵に当たっていないかどうか
(define (missile-alive? m e)
  (and (>= (missile-y m) 0)
       (not (string? e))
       (not (hit? m e))))

;; 全ミサイルを描画
(define (draw-missiles missiles)
  (foldr (lambda (m scene)
           (place-image MISSILE (missile-x m) (missile-y m) scene))
         (empty-scene SCENE-WIDTH SCENE-HEIGHT)
         missiles))

;; hit?,any-hit?定義
(define (hit? m e)
  (and (not (string? m))
       (not (string? e))
       (< (abs (- (car m) (car e))) (/ ENEMY-SIZE 2))
       (< (abs (- (cadr m) (cadr e))) (/ ENEMY-SIZE 2))))

(define (any-hit? missiles e)
  (ormap (lambda (m) (hit? m e)) missiles))
