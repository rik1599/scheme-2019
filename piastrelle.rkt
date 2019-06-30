;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname piastrelle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define colors
  (lambda (n)
    (cond ((= n 1) 2)
          ((= n 2) 3)
          (else (+
                 (colors (- n 1))
                 (colors (- n 2))
                 ))
          )
    )
  )

(define size
    (lambda (n)
    (cond ((= n 1) 1)
          ((= n 2) 2)
          (else (+
                 (size (- n 1))
                 (size (- n 2))
                 ))
          )
    )
  )