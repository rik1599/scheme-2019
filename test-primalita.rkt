;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname test-primalita) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define test
  (lambda (n min max)
    (cond ((> min max) #true)
          ((= (remainder n min) 0) #false)
          (else (test n (+ min 2) max))
          )
    ))

(define is-prime?
  (lambda (n)
    (cond ((= n 2) #true)
          ((or (even? n) (= n 1)) #false)
          ((< (sqrt n) 3) (test n 3 (- n 1)))
          (else (test n 3 (floor (sqrt n))))
          )
    ))

(define intervallo
  (lambda (n)
    (if (= n 1)
        (cons 1 null)
        (append (intervallo (- n 1)) (cons n null))
        )
    ))

(define primi-rec
  (lambda (x n)
    (cond ((> x n) null)
          ((is-prime? x) (cons x (primi-rec (+ x 1) n)))
          (else (primi-rec (+ x 1) n)))
    ))

(define lista-primi
  (lambda (n)
    (primi-rec 2 n)
    ))