#lang racket
(require 2htdp/image
         "player.rkt"
         "enemy.rkt"
         "missile.rkt")

(provide initial-world world-draw world-next world-control)

;; ワールド構造体----------
(struct world (me enemies missiles score level) #:transparent)

;; 初期状態----------
(define initial-world
  (world initial-player initial-enemies '() 0 0))

;; 描画関数----------
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

;; 状態更新----------
(define (world-next w)
  (define old-enemies (world-enemies w))
  ;; level を取得
  (define lvl (world-level w))
  ;; ミサイル移動が先
  (define moved-missiles (next-missiles (world-missiles w) old-enemies lvl))
  ;; 移動後のミサイルで敵の更新
  (define new-enemies (next-enemies old-enemies moved-missiles))
  (define new-me (update-player (world-me w)))
  ;; 撃破された敵のペアリスト(old -> new)
  (define defeated-enemies
    (filter (lambda (pair)
              (and (not (string? (car pair))) (string? (cdr pair))))
            (map cons old-enemies new-enemies)))

  ;; 撃破スコア計算(ボスは多め)
  (define defeated-score
  (apply +
         (map (lambda (pair)
                (if (equal? (enemy-type (car pair)) "boss")
                    1000
                    100))
              defeated-enemies)))
  (define new-score (+ (world-score w) defeated-score))
  (define new-level (quotient new-score 500))
  ;; 敵を補充
  (define next-enemies-list
  (if (andmap string? new-enemies) ; 全部 "none" なら全滅
      (generate-enemies new-level) ; レベルに応じて敵を生成
      new-enemies))
  ;; 新しい世界状態
  (world new-me next-enemies-list moved-missiles new-score new-level))

(define (world-control w key)
  (define lvl (world-level w))
  ;; 発射間隔をレベルに応じて調整
  (define fire-rate (max 1 (- 10 lvl)))

  (define-values (new-me new-missiles)
    (control-player (world-me w) (world-missiles w) key fire-rate))

  (world new-me
         (world-enemies w)
         new-missiles
         (world-score w)
         (world-level w)))
