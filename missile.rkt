#lang racket
(require 2htdp/image
         "enemy.rkt") ; 複数の敵に対応したファイルを読み込む

(provide missile-x missile-y
         draw-missiles next-missiles move-missile
         hit? any-hit?)

(define MISSILE (circle 10 "solid" "cyan"))
(define SCENE-WIDTH 400)
(define SCENE-HEIGHT 600)
(define ENEMY-SIZE 40) ; 敵サイズ（敵ファイルと整合性を保つため定義）

;; ミサイル位置取得
(define (missile-x m) (car m))
(define (missile-y m) (cadr m))

;; ミサイル1つの移動
(define (move-missile m speed)
  (list (missile-x m) (- (missile-y m) speed)))

;; 敵リストに対応した next-missiles
(define (next-missiles missiles enemies level)
  (define speed (+ 8 (* level 2)))
  (filter (lambda (m)
            (missile-alive? (move-missile m speed) enemies))
          (map (lambda (m) (move-missile m speed)) missiles)))

;; 敵リストに対応した生存チェック
(define (missile-alive? m enemies)
  (and (>= (missile-y m) 0)
       (not (any-hit? (list m) enemies)))) ; 1発と全敵の判定

;; 全ミサイルを描画
(define (draw-missiles missiles)
  (foldr (lambda (m scene)
           (place-image MISSILE (missile-x m) (missile-y m) scene))
         (empty-scene SCENE-WIDTH SCENE-HEIGHT)
         missiles))

;; 単体の敵とミサイルの衝突判定
(define (hit? m e)
  (and (not (string? m))
       (not (string? e))
       (< (abs (- (car m) (car e))) (/ ENEMY-SIZE 2))
       (< (abs (- (cadr m) (cadr e))) (/ ENEMY-SIZE 2))))

;; 敵リストに対して、ミサイルがどれかに当たったか
(define (any-hit? missiles enemies)
  (ormap (lambda (e)
           (ormap (lambda (m) (hit? m e)) missiles))
         enemies))
