#lang racket
(require vigracket)

(define blox (load-image
              (build-path vigracket-path "images/blox.gif")))

(define lenna (load-image
               (build-path vigracket-path "images/lenna_face.png")))

#| 
(show-image lenna)
(show-image (image->red lenna))
(show-image (image->green lenna))
(show-image (image->blue lenna))

(show-image (append (image->red lenna)(image->green lenna)(image->blue lenna)))
(show-image (append (image->green lenna)(image->red lenna)(image->blue lenna)))
(show-image (append (image->green lenna)(image->blue lenna)(image->red lenna)))

(show-image (gsmooth lenna 1.0))
(show-image (gsmooth lenna 5.0))
(show-image (ggradient lenna 1.0))
(show-image (ggradient lenna 0.1))
|#
;3.a)
(show-image (image->red lenna))
(define schwelle 200.0)

(define (Hilfsfunktion x)
  (if (< x schwelle)
      0.0
      255.0))

(define (mask bild)
  (image-map Hilfsfunktion bild))

(define (betone-rot farbbild)
  (let((red (list(first farbbild)))
       (green (list (second farbbild)))
        (blue (list (third farbbild))))
    (append (mask red) green blue)))

(define (betone-blau farbbild)
  (let((red (list(first farbbild)))
       (green (list (second farbbild)))
        (blue (list (third farbbild))))
    (append red green (mask blue))))

(define (mask-color farbbild)
  (let((red (list(first farbbild)))
       (green (list (second farbbild)))
        (blue (list (third farbbild))))
    (append (mask red) (mask green) (mask blue))))
;3.b)
(define (mask-rot farbbild)
  (let((red (list(first farbbild))))
       
    (append (mask red) (mask red) (mask red))))

(define rotbild (show-image (betone-rot lenna)))
(define blaubild (show-image (betone-blau lenna)))
rotbild
blaubild
(show-image (mask-color lenna))
(show-image (mask-rot lenna))

;3c)
(define (hübschfunction maske farbbild scale)
  (image-map (lambda (m b gb)
               (if (= m 0.0)
                   b
                   gb))
             maske farbbild (gsmooth farbbild scale)))
  