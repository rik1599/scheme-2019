;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |laboratorio 20_12_2018|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ----------------------------- Parte 1 ---------------------------------------
(define alphabet
  '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\V #\X))

(define get-char-code ; val=int
  (lambda (c i)       ; c=char, i=int
    (if (char=? c (list-ref alphabet i))
        i
        (get-char-code c (+ i 1)))
    ))

(define cod-cesare ; val=char
  (lambda (rot)    ; rot=int
    (lambda (c)    ; c=char
      (let ([code (get-char-code c 0)]
            [alpha-length (length alphabet)])
        (if (> (+ code rot) (- alpha-length 1))
            (list-ref alphabet (+ code (- alpha-length) rot))
            (list-ref alphabet (+ code rot)))
        )
      )))

((cod-cesare 3) #\A)
((cod-cesare 3) #\B)
((cod-cesare 3) #\E)
((cod-cesare 3) #\R)
((cod-cesare 3) #\X)

; ----------------------------- Parte 2 ---------------------------------------
(define H               ; val=int
  (lambda (neutro rule) ; neutro=lambda, rule=lambda
    (lambda (m n)
      (if (= n 0)
          (neutro m)
          (rule m ((H neutro rule) m (- n 1)))
          )
      )))

(define s2
  (lambda (u v)
    (+ v 1)
    ))

(define add (H (lambda (x) x) s2))
(define mul (H (lambda (x) 0) add))
(define pow (H (lambda (x) 1) mul))