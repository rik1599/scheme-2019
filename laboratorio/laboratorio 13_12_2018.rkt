;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |laboratorio 13_12_2018|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Es 1
(define match
 (lambda (u v)
   (if (or (string=? u "") (string=? v ""))
       ""
       (let ( (uh (string-ref u 0))
              (vh (string-ref v 0))
              (s (match (substring u 1) (substring v 1)))
              )
         (if (char=? uh vh)
             (string-append (string uh) s)
             (string-append "*" s)
             ))
       )))
; ----------------------------------------------------------

; Es 2
(define offset (char->integer #\0))

(define last-digit
  (lambda (base) (integer->char (+ (- base 1) offset)) ))

(define next-digit
  (lambda (dgt) (string (integer->char (+ (char->integer dgt) 1))) ))

(define increment
  (lambda (num base) ; 2 <= base <= 10
    (let ((digits (string-length num)))
      (if (= digits 0)
          "1"
          (let ((dgt (string-ref num (- digits 1))))
            (if (char=? dgt (last-digit base))
                (string-append (increment (substring num 0 (- digits 1)) base)
                 "0")
                (string-append (substring num 0 (- digits 1)) (next-digit dgt))
                ))
          ))))
; ------------------------------------------------------------

; Es 3
(define lcs ; valore: lista di terne
  (lambda (u v) ; u, v: stringhe
    (lcs-rec 1 u 1 v)
    ))

(define lcs-rec
  (lambda (i u j v)
    (cond ((or (string=? u "") (string=? v "")) null)
          ((char=? (string-ref u 0) (string-ref v 0))
           (cons (list i j (string (string-ref u 0)))
            (lcs-rec (+ i 1) (substring u 1) (+ j 1) (substring v 1)) ))
          (else
           (better
            (lcs-rec i u (+ j 1) (substring v 1))
            (lcs-rec (+ i 1) (substring u 1) j v)
            ))
          )))

(define better
  (lambda (x y)
    (if (< (length x) (length y)) y x)
    ))
; ----------------------------------------------------

; Es 4
(define cyclic-string
  (lambda (pattern length)
    (let ([pos (modulo length (string-length pattern))])
      (if (= length 0)
          ""
          (string-append
           (cyclic-string pattern (- length 1))
           (string
            (string-ref
             pattern
             (if (= pos 0) (- (string-length pattern) 1) (- pos 1))
             )))
          )
      )))
; -----------------------------------------------------

; Es 5
(define av
  (lambda (lst)
    (if (= (length lst) 1)
        null
        (let ([sum (+ (car lst) (car (cdr lst)))])
          (cons
           (cond [(< sum 0) -1] [(> sum 0) +1] [else 0])
           (av (cdr lst))
           ))
        )
    ))
; -----------------------------------------------------

; Es 6
(define r-val
  (lambda (n)
    (r-val-rec (substring n 1) -1)
    ))

(define r-val-rec
  (lambda (n pos)
    (if (string=? n "")
        0
        (+ (expt 2 pos) (r-val-rec (substring n 1) (- pos 1)))
        )
    ))
; ----------------------------------------------------

; Es 7
(define shared
  (lambda (u v)
    (cond [(or (null? u) (null? v)) null]
          [(search? (car u) v) (cons (car u) (shared (cdr u) v))]
          [else (shared (cdr u) v)]
          )
    ))

(define search?
  (lambda (x lst)
    (cond [(null? lst) #f]
          [(= x (car lst)) #t]
          [else (search? x (cdr lst))]
          )
    ))
; -----------------------------------------------------

; Es 8
(define parity-check-failures
  (lambda (lst)
    (parity-check-failures-rec lst 0)
    ))

(define parity-check-failures-rec
  (lambda (lst pos)
    (cond [(null? lst) null]
          [(even? (parity (car lst))) (parity-check-failures-rec (cdr lst) (+ pos 1))]
          [else (cons pos (parity-check-failures-rec (cdr lst) (+ pos 1)))]
          )
    ))

(define parity
  (lambda (bin)
    (cond [(string=? bin "") 0]
          [(char=? (string-ref bin 0) #\0) (parity (substring bin 1))]
          [else (+ 1 (parity (substring bin 1)))]
          )
    ))
; -----------------------------------------------------

; Es 9
(define closest-pair
  (lambda (lst)
    (closest-pair-rec lst (- (car lst) (car (cdr lst))) (list (car lst) (car (cdr lst))))
    ))

(define closest-pair-rec
  (lambda (lst diff min-pair)
      (if (= (length lst) 2)
          min-pair
          (let ([next-diff (- (list-ref lst 1) (list-ref lst 2))]
                [new-pair (list (list-ref lst 1) (list-ref lst 2))])
            (if (> diff next-diff)
                (closest-pair-rec (cdr lst) diff min-pair)
                (closest-pair-rec (cdr lst) next-diff new-pair)
                ))
          )
    ))
;-------------------------------------------------------

; Es 10
(define sorted-char-list
  (lambda (s)
    (sort-list (list-no-duplicates (string->list s)))
    ))

(define sort-list
  (lambda (lst)
    (if (null? lst)
        null
        (insertToSortedList (car lst) (sort-list (cdr lst)))
        )
    ))

(define insertToSortedList
  (lambda (x lst)
    (if (or (null? lst) (char<=? x (car lst)))
        (cons x lst)
        (cons (car lst) (insertToSortedList x (cdr lst)))
        )
    ))

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