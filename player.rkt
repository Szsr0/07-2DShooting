#lang racket
(require 2htdp/image)

(provide initial-player draw-player control-player)

(define SCENE-WIDTH 400)
(define SCENE-HEIGHT 600)
(define ME (triangle 30 "solid" "black"))

(define initial-player (/ SCENE-WIDTH 2))

(define (draw-player me-x scene)
  (place-image ME me-x (- SCENE-HEIGHT (/ (image-height ME) 2)) scene))

(define (control-player me-x missile key)
  (cond
    [(string=? key "left")
     (values (max 0 (- me-x 5)) missile)]
    [(string=? key "right")
     (values (min SCENE-WIDTH (+ me-x 5)) missile)]
    [(string=? key " ")
     (if (string? missile)
         (values me-x (list me-x (- SCENE-HEIGHT (image-height ME))))
         (values me-x missile))]
    [else
     (values me-x missile)]))