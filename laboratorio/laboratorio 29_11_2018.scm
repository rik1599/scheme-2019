;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |laboratorio 29_11_2018|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; -------------- Creazione lista dei numeri primi -----------------
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

(define primi-rec
  (lambda (x n)
    (cond ((> x n) null)
          ((is-prime? x) (cons x (primi-rec (+ x 1) n)))
          (else (primi-rec (+ x 1) n)))
    ))

(define prime-list
  (lambda (n)
    (primi-rec 2 n)
    ))

; -------------- Calcola il fattore primo -----------------
(define next-factor
  (lambda (n lista-primi)
    (let ([div (car lista-primi)])
      (if (= (remainder n div) 0)
          div
          (next-factor n (cdr lista-primi))
          )
      )
    ))

; -------------- Rimuove elementi duplicati da una lista -----------------
(define list-no-duplicates
  (lambda (lst)
    (if (null? lst)
        null
        (cons
         (car lst)
         (list-no-duplicates (filter (lambda (x) (not (equal? x (car lst)))) (cdr lst)))
         )
        )
    ))

; -------------- Conta in numero di volte in cui x è in una lista -----------------
(define count
  (lambda (x lst)
    (cond ((null? lst) 0)
          ((= x (car lst)) (+ 1 (count x (cdr lst))))
          (else (count x (cdr lst)))
          )
    ))

(define list-count-duplicates
  (lambda (elements lst)
    (cond ((null? elements) null)
          (else
           (cons
            (list (car elements) (count (car elements) lst))
            (list-count-duplicates (cdr elements) lst)))
          )
    ))



; -------------- Procedure finali -----------------
(define prime-factors
  (lambda (n)
    (if (= n 1)
        null
        (let ([lista-primi (prime-list n)])
          (let ([div (next-factor n lista-primi)])
            (cons div (prime-factors (quotient n div)))
            ))
        )
    ))

(prime-factors 7)
(prime-factors 9)
(prime-factors 28)
(prime-factors 39)
(prime-factors 540)
(prime-factors 1617)


(define short-prime-factors
  (lambda (n)
    (list-no-duplicates (prime-factors n))
    ))

(short-prime-factors 7)
(short-prime-factors 9)
(short-prime-factors 28)
(short-prime-factors 39)
(short-prime-factors 540)
(short-prime-factors 1617)

(define prime-facts-degs
  (lambda (n)
    (let ([lst (prime-factors n)])
      (list-count-duplicates (list-no-duplicates lst) lst)
      )
    ))

(prime-facts-degs 7)
(prime-facts-degs 9)
(prime-facts-degs 28)
(prime-facts-degs 39)
(prime-facts-degs 540)
(prime-facts-degs 617)