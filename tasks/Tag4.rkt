#lang racket

(require vigracket)
(require (rename-in 2htdp/image
                    (save-image save-plt-image)
                    (image-width plt-image-width)
                    (image-height plt-image-height)))
(require lang/posn)


(define bspimage (load-image "/Users/2budde/Desktop/Praktikum/Muehle/IMG_0635.jpg"))

;---------------------------------------------------------------------------------------------
;verkleinert das Bild zu einem Bild mit 600x??? oder ???x600. Falls weder die Breite noch die Höhe
;größer als 600 wird das bild zurückgegeben
;Paramter image: Ein Farbbild/Graubild 
;return: Ein Farbbild/Graubild (max (image-width image) (image-height image)) < 600

(define (verkleinere-breite image)
  (if ( < (image-width image) 600)
      image
      (resizeimage image 600 (*(/ 600 (image-width image)) (image-height image)) 1)))

(define (verkleinere-höhe image)
  (if (< (image-height image) 600)
      image
      (resizeimage image (*(/ 600 (image-height image)) (image-width image)) 600 1)))

(define (verkleinere image)
  (if (< (image-height image) (image-width image))
      (verkleinere-breite image)
      (verkleinere-höhe image)))
;Beispiel

;(show-image (verkleinere (load-image "images/Muehle/3.jpg")))

;---------------------------------------------------------------------------------------------

(show-image (verkleinere bspimage))

(save-image (verkleinere bspimage) "/Users/2budde/Desktop/Praktikum/Muehle/11.jpg") 