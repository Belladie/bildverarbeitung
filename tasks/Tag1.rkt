#lang racket
(require vigracket)

(define img (load-image
             (build-path vigracket-path "images/blox.gif")))

(define racket-img (image->racket-image img))


(save-image img "/Volumes/students/home/2budde/Praktikum/lala.JPG")

(show-image (gsmooth img 1.0))
(show-image (ggradient img 1.0))

(show-image (image-map - img (gsmooth img 1.0)))

(define schwelle 90.0)
             
(define (Hilfsfunktion x)
  (if (< x schwelle)
      5.0
      255.0))

(define (mask bild)
  (image-map Hilfsfunktion bild))



(show-image (mask img))

(define anzahl 
  (image-reduce max (labelimage (mask img)) 0 ))