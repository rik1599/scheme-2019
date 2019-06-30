;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |laboratorio 8_11_2018|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Ritorna la posizione del punto decimale all'interno della stringa
(define get-point-pos ; val=integer
  (lambda (n)         ; n=string
    (let ([point-tail (memv #\. (string->list n))])
      (if (list? point-tail)
          (- (string-length n) (length point-tail))
          0)
     )
    )
  )

; Ritorna il numero di cifre decimali dopo il punto
(define n-dec-digit ; val=integer
  (lambda (n)       ; n=string
    (let ([point-tail (memv #\. (string->list n))])
      (if (list? point-tail)
          (- (length point-tail) 1)
          0)
     )
    )
  )

; Converte una cifra nel suo valore numerico corrispondente all'interno dell'alfabeto
(define digit-val   ; val=integer
  (lambda (d A pos) ; d=char, A=string, pos=integer
    (if (char=? d (string-ref A pos))
        pos
        (digit-val d A (+ pos 1)))
    )
  )

; Converte un numero n dalla base B a base 10 (algoritmo di accumulazione del risultato)
(define convert   ; val=integer
  (lambda (n A B) ; n=string, A=string, B=integer
    (let ([L (string-length n)])
      (let ([lsd (string-ref n (- L 1))] [pr (substring n 0 (- L 1))])
        (if (= L 1)
            (digit-val lsd A 0)
            (+
             (* B (convert pr A B)) ; delega
             (digit-val lsd A 0)
             )
            ))
      )
    )
  )

; Toglie l'eventuale carattere di segno presente nella stringa numerica
(define unsigned    ; val=integer
  (lambda (val sgn) ; val=string, sgn=char
    (if (or (char=? sgn #\-) (char=? sgn #\+))
        (substring val 1)
        val)
    )
  )

(define rep->number ; val=real number
  (lambda (A n)    ; n=string, A=string (alphabet)
    (let ([B (string-length A)]
          [first-char (string-ref n 0)]
          [point (get-point-pos n)]
          [dec-digit (n-dec-digit n)])
      (let ([int-val (if (= point 0)
                         n
                         (string-append (substring n 0 point) (substring n (+ 1 point)))
                         )
                     ]
            [sgn (if (char=? first-char #\-) -1 +1)]
            )
        (* sgn (/ (convert (unsigned int-val first-char) A B) (expt B dec-digit)))
        )
      )
    )
  )

(define bin-rep->number
  (lambda (n)
    (rep->number "01" n)
    )
  )

(bin-rep->number "+1101")
(bin-rep->number "0")
(bin-rep->number "10110.011")
(bin-rep->number "-0.1101001")

(rep->number "zu" "-uuzz")
(rep->number "0123" "+21.1")
(rep->number "01234" "-10.02")
(rep->number "0123456789ABCDEF" "0.A")
(rep->number "0123456789ABCDEF" "1CF.0")
