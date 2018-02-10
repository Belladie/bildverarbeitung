#lang racket
(require racket/include)
(require vigracket)
(require (rename-in 2htdp/image
                    (save-image save-plt-image)
                    (image-width plt-image-width)
                    (image-height plt-image-height)))
;--------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------

(require "gerade.rkt")
(require "kleineBilder.rkt")

;rotiert mit bogenmaß nach links mit #t.
(define (rotateimage-bogenmaß img bogenmaß interpolation links?)
  (if links?
  (rotateimage img (radian->grad bogenmaß) interpolation)
  (rotateimage img (radian->grad (- (* 2 pi) bogenmaß)) interpolation)))
;-----------------------------------------------------------------------------------------------------------------------------

;nützliche bildersammlung
;--------------------------------------------------------------------------------------------------------------------------
;zeigt dir das erste bild
(define img1 (erstell-kleines-bild "images/Muehle/1.jpg"))
;zeigt dir das erste bild mit einer funktion
(define img1-mit-funktion (lambda (proc) (proc img1)))
;zeigt dir ein beliebiges bild
(define img2 (lambda(x)(erstell-kleines-bild (string-append "images/Muehle/" (number->string (+ x 1)) ".jpg"))))
(define img2-mit-funktion (lambda(x proc)(proc (erstell-kleines-bild (string-append "images/Muehle/" (number->string (+ x 1)) ".jpg")))))

;zeigt dir ein beliebiges bild mit funktion
(define zeige (lambda (x) (show-image (img2 x))))
(define zeige-mit-funktion (lambda (nummer proc) (show-image (proc (img2 nummer)))))
;zeigt dir eine anzahl von bilder 
(define zeige-von-1 (lambda (x) (map (compose show-image) (build-list x img2))))
(define zeige-von-1-mit-funktion (lambda (bis proc) (map (compose show-image proc) (build-list bis  img2))))
;-------------------------------------------------------------------------------------------------------------------------
;nützliche feldbetrachtung
(define (finde img)
  (image-map (lambda (r g b) (if (and (> r 170) (>  b 170) (> g 170)) 50.0 0.0)) (image->red img) (image->blue img)
             (image->green img)))

;gibt den weißen spielfeld zurück

;gibt die nördlichen punkte zurück
(define (nordpunkte img) (spielbrett-suche-von-oben img 0 0))

(define (spielbrett-suche-von-oben img x y)
  (cond ((<= (image-width img) x) '())
        ((<= (image-height img) y) (spielbrett-suche-von-oben img (+ x 1) 0))
        ((> (car (image-ref img x y)) 0.0) (cons (cons x y) (spielbrett-suche-von-oben img (+ x 1) 0)))
        (else (spielbrett-suche-von-oben img x (+ y 1)))))

(define (südpunkte img) 
  (map (lambda (punkt) (cons (car punkt) (- (image-height img) (cdr punkt)))) (nordpunkte (reflectimage img 1))))

(define (westpunkte img)
  (map (lambda (punkt) (cons (cdr punkt) (car punkt))) (nordpunkte (transponiere-bild img))))

(define (ostpunkte img)
  (map (lambda (punkt)  (cons (cdr punkt) (car punkt))) (südpunkte (transponiere-bild img))))
;---------------------------------------------------------------------------------------------------------------------
;Berechnet anhand einer Punktemenge die optimalste Gerade und eine gute Gerade


(define (ist-punkt-in-gerade punkt gerade)
  (if (senklot? gerade)
      (= (gerade 'wert?) (x-wert punkt))
      (if (positive? (steigung? gerade))
      (<= (round (gerade (- (x-wert punkt) 1))) (y-wert punkt) (round (gerade (+ 1 (x-wert punkt)))))
      (<= (round (gerade (+ (x-wert punkt) 1))) (y-wert punkt) (round (gerade (-(x-wert punkt) 1))))
      )))
      

;sammelt alle möglichen Geraden aus einer Punktemenge
(define (gib-geradengleichungen-von punkteliste)
  (if (null? punkteliste)
      '()
      (append (gib-geradengleichungen-von-punkt (car punkteliste) (cdr punkteliste)) (gib-geradengleichungen-von (cdr punkteliste)))))

;sammelt aus einem punkt zu einer Punktemenge alle möglichen Geraden.
(define (gib-geradengleichungen-von-punkt punkt punkteliste)
  (if (null? punkteliste) '() (cons (ermittle-geradenfunktion-oder-senklot punkt (car punkteliste))
                                   (gib-geradengleichungen-von-punkt punkt (cdr punkteliste)))))

(define (invers-punktliste punktliste)
  (map (lambda (punkt) (cons (cdr punkt) (car punkt))) punktliste))

;nimmt jedes nte-element und gibt sie als liste wieder
;anstelle von 100 punkten nimmt es bei n = 10 jedes 10.te element mit |result| = 10
(define (jeden-nte-element liste n)
  (jeden-nte-element-wrapper liste n n))

(define (jeden-nte-element-wrapper liste i n)
  (cond ((null? liste) '())
        ((= 1 i) (cons (car liste) (jeden-nte-element-wrapper (cdr liste) n n)))
        (else (jeden-nte-element-wrapper (cdr liste) (- i 1) n))))
  

;berechnet prozentual die zugehörigkeit der paarliste zu der funktion.
(define (funktionszugehörigkeit gerade paarliste)
  (let ((treffer (foldr (lambda (punkt sum) (if (ist-punkt-in-gerade punkt gerade) (+ sum 1) sum)) 0 paarliste)))
    (/ treffer (length paarliste))))


(define (optimalste-gerade gerademenge paarliste)
  (foldr (lambda (gerade optgerade)
           (if (> (funktionszugehörigkeit gerade paarliste)
               (funktionszugehörigkeit optgerade paarliste)) gerade optgerade)) (senklot 0) gerademenge))

;ermittelt aus einer punktmenge die beste gerade!
(define (optimalgerade paarliste)
  (optimalste-gerade (gib-geradengleichungen-von paarliste) paarliste))

;ermittelt aus einer punktmenge eine gute gerade.
(define (gutegerade paarliste)
  (optimalste-gerade (gib-geradengleichungen-von (jeden-nte-element paarliste 3)) (jeden-nte-element paarliste 3)))
;---------------------------------------------------------------------------------------------------------------- 
(define (mean x1 x2)
  (/ (+ x1 x2) 2))
;die nordgerade
(define (spielbrett-nordgerade spielbrett-weiß-img)
  (let* ((n-punkte (nordpunkte spielbrett-weiß-img)))
    (gutegerade n-punkte)))

;die südgerade
(define (spielbrett-südgerade spielbrett-weiß-img)
  (let* ((s-punkte (südpunkte spielbrett-weiß-img)))
   (gutegerade s-punkte)))

;die ostgerade
(define (spielbrett-ostgerade spielbrett-weiß-img)
  (let* ((o-punkte (invers-punktliste (ostpunkte spielbrett-weiß-img))))
    (inverse-geradefunktion (gutegerade o-punkte))))

;die westgerade
(define (spielbrett-westgerade spielbrett-weiß-img)
  (let* ((w-punkte (invers-punktliste (westpunkte spielbrett-weiß-img))))
    (inverse-geradefunktion (gutegerade w-punkte))))
;------------------------------------------------------------------------------------------------------------------
;Steigung
;> (punktliste->punktlisteXsteigungliste '((0 . 0) (1 . 1) (2 . 2)))
;'((1 (0 . 0)) (1 (1 . 1)))
(define (punktliste->punktlisteXsteigungliste punktliste)
  (let* ((punktlist-mit-nachbar (map (lambda (punkt1 punkt2) (cons punkt1 punkt2))  (reverse (cdr (reverse punktliste)))  (cdr punktliste))))
  (map (lambda (doppeltpunkt) 
         (cons (steigung-berechnen (car doppeltpunkt) (cdr doppeltpunkt)) (list (car doppeltpunkt)))) punktlist-mit-nachbar)))

(define (steigungspunkte punktliste ref?)
  (foldr (lambda (x sum) (if (ref? (car x)) (cons (cadr x) sum) sum)) '() (punktliste->punktlisteXsteigungliste punktliste)))

(define (aufsteigendepunkte punktliste)
  (steigungspunkte punktliste positive?))

(define (abfallendepunkte punktliste)
  (steigungspunkte punktliste negative?))

;---------------------------------------------------------------------------------------------------------------
;ermittelt aus einem spielbrett-weiß-img (also nur die Spielfläche die weiß ist
;eine annähernd optimale gerade für die Ränder

;dreht das Bild richtig
(define (rotate-spielbrett img)
  (let* ((weißes-spielbrett (finde img))
         (gerade-s (spielbrett-südgerade (finde img)))
         (punkt1 '(0 . 0))
         (winkelpunkt (schnittpunkt gerade-s (geradefunktion 0 0)))
         (punkt3 (cons 100 (gerade-s 100))))
    (if (> (steigung? gerade-s) 0)
    (rotateimage-bogenmaß img (radian? punkt1 winkelpunkt punkt3) 1 #t)
    (rotateimage-bogenmaß img (radian? punkt1 winkelpunkt punkt3) 1 #f)
    )))

(define (gib-eckpunkte img)
  (let* ((n-gerade (spielbrett-nordgerade (finde img)))
         (s-gerade (spielbrett-südgerade (finde img)))
         (w-gerade (spielbrett-westgerade (finde img)))
         (o-gerade (spielbrett-ostgerade (finde img)))
         (no-schnitt (schnittpunkt n-gerade o-gerade))
         (nw-schnitt (schnittpunkt n-gerade w-gerade))
         (sw-schnitt (schnittpunkt s-gerade w-gerade))
         (so-schnitt (schnittpunkt s-gerade o-gerade)))
    (list no-schnitt so-schnitt sw-schnitt nw-schnitt)))

(define (schneide-weißes-feld spielbrett-image)
  (let* ((rota-image (rotate-spielbrett spielbrett-image))
         (eckpunkte (gib-eckpunkte rota-image))
         (no-schnitt (car eckpunkte))
         (so-schnitt (cadr eckpunkte))
         (sw-schnitt (caddr eckpunkte))
         (nw-schnitt (cadddr eckpunkte))
         (neu-xstart (round (mean (x-wert nw-schnitt) (x-wert sw-schnitt))))
         (neu-ystart (round (mean (y-wert nw-schnitt) (y-wert no-schnitt))))
         (neu-xend (round (mean (x-wert no-schnitt) (x-wert so-schnitt) )))
         (neu-yend (round (mean (y-wert so-schnitt) (y-wert sw-schnitt)))))
         (crop neu-xstart neu-ystart (- neu-xend neu-xstart) (- neu-yend neu-ystart) (image->racket-image rota-image))))

(define (erstell-weißes-feld path)
 (let ((weißes-feld-path (string-append  "images/Muehle/weißes-feld/" "weiß-feld" (dateiname path)))
        (weißes-feld-ort (string-append "images/Muehle/weißes-feld")))
  (cond 
    ((not (file-exists? path)) (error "Die Datei im angegeben Pfad existiert nicht."))
    ((not (jpg? path)) (error "Die Datei ist keine jpg-Datei"))
    ((file-exists? weißes-feld-path) (loadimage weißes-feld-path))
    ((not (directory-exists? weißes-feld-ort)) 
     (begin 
       (displayln "weißesfeld-ordner wurde anscheinend nicht gefunden und neu erzeugt")
       (with-handlers ([exn:fail:filesystem? (lambda (exn) (error "/kleineordner konnte nicht erstellt werden"))])
                            (make-directory weißes-feld-ort)) 
             (erstell-kleines-bild path)
             ))
    ((save-image (racket-image->image (schneide-weißes-feld (erstell-kleines-bild path)))
                                        weißes-feld-path) (loadimage weißes-feld-path))
    (else (error "Bilddatei existiert.weißes-feld kann nicht erstellt werden. Dateiformat falsch?")))))

;----------------------------------------------------------------------------------------------------------


;----------------------------------------------------------------------------------------------------------
;radian->winkel
(define (radian->grad radian)
  (/ (* radian 180) pi))

;abstand zweier punkte
(define (abstand p1 p2)
  (sqrt (+ (sqr (- (car p1) (car p2)))
           (sqr (- (cdr p1) (cdr p2))))))

;kosinussatz zur berechnung des winkels
(define (radian? punkt1 winkelpunkt punkt3)
  (let ((a (abstand winkelpunkt punkt3) )
        (b (abstand winkelpunkt punkt1))
        (c (abstand punkt1 punkt3)))
    (acos (/ (- (sqr c) (sqr a) (sqr b)) -2 a b))))
    
      
   
  


  




    (provide (all-defined-out))






                                        
