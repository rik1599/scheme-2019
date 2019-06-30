;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Esame) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define f        ; intero
  (lambda (s)    ; lista di interi positivi
    (g s 0)
    ))

(define g
  (lambda (s b)
    (if (null? s)
        0
        (if (<= (car s) b)
            (g (cdr s) b)
            (max (g (cdr s) b)
                 (+ (g (cdr s) (car s)) 1))
            ))))

(define mh                    ; val: intero
  (lambda (i j)               ; i, j: interi non negativi
    (if (or (= i 0) (= j 0))
          1                                                                           
          (+ (md (- i 1) j) (mr i (- j 1)))
          )))
  (define md                    ; spostamento precedente “in giù”
    (lambda (i j)
      (if (or (= i 0) (<= j 1))
          1
          (+ (md (- i 1) j)
             (mr i (- j 2)))
          )))
  (define mr                    ; spostamento precedente “a destra”
    (lambda (i j)
      (if (or (<= i 1) (= j 0) )                                                                           
          1
          (+ (md (- i 2) j)
             (mr i (- j 1)))
          )))

  (define is-identifier?
    (lambda (s)
      (if (= (string-length s) 0)
          #false
          (let ([f-char (string-ref s 0)] [list-c (string->list (substring s 1))])
            (cond [(char-alphabetic? f-char) (check-chars list-c #true)]
                  [else #false]
              )))
      ))

  (define check-chars
    (lambda (s current)
      (if (null? s)
          current
          (if (or (char-alphabetic? (car s)) (char-numeric? (car s)) (char=? (car s) #\_))
              (check-chars (cdr s) #true)
              #false)
          )
      ))
  