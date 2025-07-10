#lang racket
(require 2htdp/image)

(provide enemy-x enemy-y enemy-v enemy-hp
         initial-enemies draw-enemies next-enemies generate-enemies enemy-type)

(define SCENE-WIDTH 400)
(define ENEMY-SIZE 60)
(define BOSS-SIZE 150)

;; 敵の構造----------
(define (enemy x y v hp type) (list x y v hp type))
(define (enemy-x e) (car e))
(define (enemy-y e) (cadr e))
(define (enemy-v e) (caddr e))
(define (enemy-hp e) (list-ref e 3))
(define (enemy-type e) (list-ref e 4))

;; 初期の敵リスト(複数体)----------
(define initial-enemies
  (list
   (enemy 100 100 3 3 "normal")
   (enemy 200 120 -4 3 "normal")
   (enemy 300 150 5 3 "normal")))

;; 敵の移動----------
(define (move-enemy e)
  (let* ((x (enemy-x e))
         (y (enemy-y e))
         (v (enemy-v e))
         (hp (enemy-hp e))
         (type (enemy-type e))
         (half-size (if (>= hp 20) (/ BOSS-SIZE 2) (/ ENEMY-SIZE 2))))
    (cond
      ((< x half-size)
       (enemy half-size y (- v) hp type))
      ((> x (- SCENE-WIDTH half-size))
       (enemy (- SCENE-WIDTH half-size) y (- v) hp type))
      (else
       (enemy (+ x v) y v hp type)))))

;; ミサイルが敵に当たったか----------
(define (enemy-hit? m e)
  (and (not (string? m))
       (< (abs (- (car m) (enemy-x e))) (if (>= (enemy-hp e) 20) (/ BOSS-SIZE 2) (/ ENEMY-SIZE 2)))
       (< (abs (- (cadr m) (enemy-y e))) (if (>= (enemy-hp e) 20) (/ BOSS-SIZE 2) (/ ENEMY-SIZE 2)))))

;; 単体の敵を次の状態に(撃破されてたら "none")
(define (next-enemy e missiles)
  (cond
    ((ormap (lambda (m) (enemy-hit? m e)) missiles)
     (if (<= (sub1 (enemy-hp e)) 0)
         "none"
         (enemy (enemy-x (move-enemy e))
                (enemy-y (move-enemy e))
                (enemy-v (move-enemy e))
                (sub1 (enemy-hp e))
                (enemy-type e))))
    (else (move-enemy e))))

;; 複数の敵の次の状態----------
(define (next-enemies enemies missiles)
  (map (lambda (e) (if (string? e) "none" (next-enemy e missiles)))
       enemies))

;; 敵の色判定----------
(define (enemy-color hp)
  (cond
    [(>= hp 20) "green"]
    [(>= hp 10) "yellow"]
    [(>= hp 3) "orange"]
    [(>= hp 1) "red"]
    [else "gray"]))

;; 複数の敵を描画----------
(define (draw-enemies enemies scene)
  (foldl (lambda (e s)
           (if (string? e)
               s
               (let* ((hp (enemy-hp e))
                      (x (enemy-x e))
                      (y (enemy-y e))
                      (size (if (>= hp 20) BOSS-SIZE ENEMY-SIZE))
                      (color (enemy-color hp))
                      (img (square size "solid" color))
                      (base (place-image img x y s))
                      (hp-text (text (number->string hp) 24 "white")))
                 (place-image hp-text x y base))))
         scene
         enemies))

;; レベルに応じた敵リストを再生成----------
(define (generate-enemies level)
  (define (random-nonzero-speed)
    (let ((v (- 3 (random 7)))) ; -4〜3
      (if (= v 0) (random-nonzero-speed) v))) ; v ≠ 0になるまで再挑戦

  (if (>= level 5)
      ;; Lv5以上ならボス1体
      (list (enemy (/ SCENE-WIDTH 2) 100 3 (* 10 level) "boss"))
      ;; 通常の敵(3体)
      (list
       (enemy (+ 40 (random (- SCENE-WIDTH 80))) 100 (random-nonzero-speed) (+ 2 level) "normal")
       (enemy (+ 40 (random (- SCENE-WIDTH 80))) 120 (random-nonzero-speed) (+ 2 level) "normal")
       (enemy (+ 40 (random (- SCENE-WIDTH 80))) 140 (random-nonzero-speed) (+ 2 level) "normal"))))