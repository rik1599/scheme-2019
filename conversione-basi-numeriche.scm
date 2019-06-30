;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname conversione-basi-numeriche) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;-------------------- Conversione da base 10 a base qualsiasi ------------------------
(define dec-to-base ; val=stringa
  (lambda (n B) ; n=intero, B=base di conversione
    (let ([lsc (number->string (remainder n B))]
          [q (quotient n B)])
      (if (< n B)
        lsc
        (string-append (dec-to-base q B) lsc))
    )))

;------------------ Conversione da base qualsiasi a base 10 ----------------------------
(define digit-val ; val=integer
  (lambda (b) ; b=stringa
    (cond [(string=? b "A") 10]
          [(string=? b "B") 11]
          [(string=? b "C") 12]
          [(string=? b "D") 13]
          [(string=? b "E") 14]
          [(string=? b "F") 15]
          [else (string->number b)])
    )
    )

(define base-to-dec ; val=integer >=0
  (lambda (n B) ; n=string di 0-9, B=integer Base del numero
    (let ([L (string-length n)])
      (let ([lsd (string (string-ref n (- L 1)))] [pr (substring n 0 (- L 1))])
        (if (= L 1)
            (digit-val lsd)
            (+
             (* B (base-to-dec pr B)) ; delega
             (digit-val lsd)
             )
            )
      ))))

;------------------ Conversione tra basi qualsiasi -----------------------------------
(define base-to-base ; val=integer >=0
  (lambda (n Bn Bval) ; n=string di 0-9, Bs=base di n, Bval=base del risultato
    (dec-to-base
     (base-to-dec n Bn)
     Bval)
    ))
(base-to-base "80" 8 2)