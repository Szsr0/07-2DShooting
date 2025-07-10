#lang racket
(require 2htdp/image)

(provide initial-player draw-player control-player update-player)

(define SCENE-WIDTH 400)
(define SCENE-HEIGHT 600)
(define ME (triangle 30 "solid" "black"))

;; 初期状態：(x座標, 最終発射時刻[tick])
(define initial-player (list (/ SCENE-WIDTH 2) 0))

;; プレイヤーの描画（yは画面下固定）
(define (draw-player player scene)
  (place-image ME
               (car player)
               (- SCENE-HEIGHT (/ (image-height ME) 2))
               scene))

;; プレイヤーの更新：on-tick用（時刻+1）
(define (update-player player)
  (list (car player) (add1 (cadr player))))

;; control-player:
;; - me: プレイヤー状態 (list x last-fire-tick)
;; - missile-list: ミサイルのリスト
;; - key: 入力キー
;; - 現在の tick は `cadr me` が持ってる
(define (control-player me missile-list key)
  (define me-x (car me))
  (define last-fire (cadr me))

  (cond
    [(string=? key "left")
     (values (list (max 0 (- me-x 5)) last-fire) missile-list)]

    [(string=? key "right")
     (values (list (min SCENE-WIDTH (+ me-x 5)) last-fire) missile-list)]

    ;; スペースキー → 0.5秒（＝5tick）以上経っていれば発射
    [(string=? key " ")
     (if (>= (- last-fire 5) 0)
         ;; 発射可能 → 新しいミサイル追加、時刻更新
         (values (list me-x 0)
                 (cons (list me-x (- SCENE-HEIGHT (image-height ME)))
                       missile-list))
         ;; まだ発射できない
         (values me missile-list))]

    [else
     (values me missile-list)]))
