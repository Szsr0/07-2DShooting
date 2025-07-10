#lang racket
(require 2htdp/image
         "player.rkt"
         "enemy.rkt"
         "missile.rkt")

(provide initial-world world-draw world-next world-control)

;; プレイヤー（me）: (list x last-fired-tick)
;; enemy : 単一
;; missiles : リスト（複数）
(struct world (me enemies missiles score level) #:transparent)

;; 初期状態
(define initial-world
  (world initial-player initial-enemies '() 0 0))

;; 描画関数
(define (world-draw w)
  (define bg (draw-missiles (world-missiles w)))
  (define with-enemies (draw-enemies (world-enemies w) bg))
  (define with-player (draw-player (world-me w) with-enemies))
  (define with-score
    (place-image
     (text (string-append "Score: " (number->string (world-score w))) 16 "black")
     60 20 with-player))
  (place-image
   (text (string-append "Power: Lv" (number->string (world-level w))) 16 "red")
   300 20 with-score))

;; 状態更新（on-tick）
(define (world-next w)
  (define old-enemies (world-enemies w))
  ;; level を取得
  (define lvl (world-level w))
  ;;　ミサイル移動が先
  (define moved-missiles (next-missiles (world-missiles w) old-enemies lvl))
  ;; 移動後のミサイルで敵の更新
  (define new-enemies (next-enemies old-enemies moved-missiles))
  (define new-me (update-player (world-me w)))
   ;; 撃破された敵の数(old: normal → new: "none" になったもの)
  (define defeated-count
    (count (lambda (pair)
             (and (not (string? (car pair))) (string? (cdr pair))))
           (map cons old-enemies new-enemies)))

  ;; スコア・レベル更新
  (define new-score (+ (world-score w) (* 100 defeated-count)))
  (define new-level (quotient new-score 500))

  (world new-me new-enemies moved-missiles new-score new-level))

(define (world-control w key)
  (define-values (new-me new-missiles)
    (control-player (world-me w) (world-missiles w) key))

  (world new-me
         (world-enemies w)
         new-missiles
         (world-score w)
         (world-level w)))
