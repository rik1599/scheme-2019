;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lcs-llcs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define llcs ; lunghezza della stringa massima di occorrenze
  (lambda (strA strB)
    (cond ((or (string=? strA "") (string=? strB "")) 0)                              ; caso base: una o tutte due le stringhe sono vuote
          ((char=? (string-ref strA 0) (string-ref strB 0))                           ; Se la prima lettera è uguale
           (+ 1 (llcs (substring strA 1) (substring strB 1))))
          (else (max (llcs strA (substring strB 1)) (llcs (substring strA 1) strB)))) ; Se la prima lettera è diversa
    ))

(define longer
  (lambda (strA strB)
    (let ([l-A (string-length strA)] [l-B (string-length strB)])
      (if (< l-A l-B)
          strB
          strA)
    )
    ))

(define lcs
  (lambda (strA strB)
    (cond ((or (string=? strA "") (string=? strB "")) "")    ; caso base: una o tutte due le stringhe sono vuote
          ((char=? (string-ref strA 0) (string-ref strB 0))  ; Se la prima lettera è uguale
           (string-append
            (substring strA 0 1)
            (lcs (substring strA 1) (substring strB 1))
            ))
          (else                                              ; Se la prima lettera è diversa
           (longer
            (lcs strA (substring strB 1))
            (lcs (substring strA 1) strB)
            ))
          )
    ))

(llcs "AGACTGAACATAC" "GATCCGACTAC")
(lcs "AGACTGAACATAC" "GATCCGACTAC")

(define all-lcs
  (lambda (strA strB)
    (cond ((or (string=? strA "") (string=? strB "")) (list ""))    ; caso base: una o tutte due le stringhe sono vuote
          ((char=? (string-ref strA 0) (string-ref strB 0))  ; Se la prima lettera è uguale
           (all-append
            (substring strA 0 1)
            (all-lcs (substring strA 1) (substring strB 1))
            ))
          (else                                              ; Se la prima lettera è diversa
           (all-longer
            (all-lcs strA (substring strB 1))
            (all-lcs (substring strA 1) strB)
            ))
          )
    ))

(define all-append
  (lambda (u s)
    (if (null? s)
        null
        (cons
         (string-append u (car s))
         (all-append u (cdr s)))
        )
    ))

(define all-longer
  (lambda (x y)
    (cond [(< (string-length (car x)) (string-length (car y))) y]
          [(> (string-length (car x)) (string-length (car y))) x]
          [else (append x y)]
          )
    ))

(all-lcs "AGACTGAACATAC" "GATCCGACTAC")