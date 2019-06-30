;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sistema-ternario) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define digit-val
  (lambda (d)
    (cond ((char=? d #\+) +1)
          ((char=? d #\-) -1)
          ((char=? d #\.) 0))
    )
  )

(define tern-to-dec
  (lambda (n) ; n=string di 0-9, B=integer Base del numero
    (let ([L (string-length n)])
      (let ([lsd (string-ref n (- L 1))] [pr (substring n 0 (- L 1))])
        (if (= L 1)
            (digit-val lsd)
            (+
             (* 3 (tern-to-dec pr)) ; delega
             (digit-val lsd)
             )
            )
      )))
  )

(tern-to-dec "+.-")

(define caller
  (lambda (n)
    (tern-to-dec-tail-rec n 0 0)
    ))

(define tern-to-dec-tail-rec
  (lambda (n esp s)
    (if (string=? n "")
            s
            (let ([L (string-length n)])
              (let ([lsd (string-ref n (- L 1))] [pr (substring n 0 (- L 1))])
                (tern-to-dec-tail-rec
                 pr
                 (+ esp 1)
                 (+ (* (digit-val lsd) (expt 3 esp)) s))
                )))
    ))

(caller "+.-")

(define val-rep
  (lambda (r)
    (cond ((= r +1) "+")
          ((= r -1) "-")
          ((= r 0)  "."))
    )
  )

(define dec-to-tern ; val=stringa ternaria
  (lambda (n) ; n=intero
    (let ([q (quotient n 3)] [r (remainder n 3)])
          (cond ((= r 2) (string-append (dec-to-tern (- q 1)) "-"))
                ((= r -2) (string-append (dec-to-tern (+ q 1)) "+"))
                (else (if (= q 0)
                          (val-rep r)
                          (string-append (dec-to-tern q) (val-rep r)))
                      )
            ))
      )
    )

(dec-to-tern 3)