#lang racket
(require 2htdp/image
         "player.rkt"
         "enemy.rkt"
         "missile.rkt")
(provide initial-world world-draw world-next world-control)

(struct world (me enemy missile score level) #:transparent)

(define initial-world
  (world initial-player initial-enemy "none" 0 1))

;;レベルアップ処理
(define (add1-if-multiple score unit level)
  (if (and (> score 0) (= (remainder score unit) 0))
      (add1 level)
      level))

(define (world-draw w)
  (define base (draw-missile (world-missile w)))
  (define with-enemy (draw-enemy (world-enemy w) base))
  (define with-player (draw-player (world-me w) with-enemy))
  (define score-text (text (string-append "Score: " (number->string (world-score w))) 15 "black"))
  (define level-text (text (string-append "Power: Lv" (number->string (world-level w))) 15 "red"))
  (place-image level-text 60 20
    (place-image score-text 60 40
      with-player)))

;; 敵撃破時、スコアとレベルを更新
(define (world-next w)
  (define m (world-missile w))
  (define e (world-enemy w))
  (define is-hit? (hit? m e))
  (define new-enemy (if is-hit? "none" (next-enemy e m)))
  (define new-missile (if is-hit? "none" (next-missile m e (world-level w))))
  (define new-score (if is-hit? (+ (world-score w) 100) (world-score w)));;スコア増加量
  (define new-level (add1-if-multiple new-score 500 (world-level w)));;レベルアップに必要なスコア
  (world (world-me w) new-enemy new-missile new-score new-level))

(define (world-control w key)
  (define-values (new-me new-missile)
    (control-player (world-me w) (world-missile w) key))
  (world new-me (world-enemy w) new-missile (world-score w) (world-level w)))