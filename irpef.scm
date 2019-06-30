;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname irpef) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Definizione dei parametri per il calcolo dell'irpef

; Soglie delle aliquote
(define soglie
  '(0 15000 28000 55000 75000))

; Aliquote percentuali in formato decimale
(define aliquote
  '(0.23 0.27 0.38 0.41 0.43))
;;----------------------------------------------------

;; Alcune funzioni utili

; Controlla se siamo all'ultimo indice della lista
(define is-last-index?
  (lambda (pos list)
    (>= pos (- (length list) 1)
    )
  ))

; Ottiene l'elemento successivo all'interno di una lista (torna 0 se all'ultimo indice, ininfluente ai fini del programma)
(define get-next-list
  (lambda (pos list)
    (if (is-last-index? pos list)
        0
        (list-ref list (+ pos 1))
        )
    )
  )
;;-----------------------------------------------------

;; Calcolo dell'irpef

; Calcola il valore dell'imposta in base all'aliquota e alla soglia raggiunta
(define calcola-imposta
  (lambda (perc r)
    (* perc r)
    )
  )

; Funzione ricorsiva di calcolo dell'irpef dato un reddito r
(define irpef
  (lambda (r pos)
    (let ([next (get-next-list pos soglie)]
          [current (list-ref soglie pos)])
     (let ([max (- next current)]
           [diff (- r current)])
       (if (or (<= r next) (is-last-index? pos soglie))
           (calcola-imposta (list-ref aliquote pos) diff)
           (+ (calcola-imposta (list-ref aliquote pos) max) (irpef r (+ pos 1)))
           )
       )
     )
    )
  )

; Punto di accesso al programma
(define calcola-irpef
  (lambda (r)
    (irpef r 0)
    )
  )
;;---------------------------------------------------------

(calcola-irpef 40000)