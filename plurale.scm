;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname plurale) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define last-position ; Get the position of the last char in a string p 
  ( lambda (p)        ; p: string
     (- (string-length p) 1) ))

(define plu ; Create the plural words
  ( lambda (p l)     ; p: string, l: string
     (string-append (substring p 0 (last-position p)) l)))

(define mas-plu
  ( lambda(p) ; p: string
     (plu p "i")))

(define fem-plu
  ( lambda(p) ; P: string
     (plu p "e")))

(define is-fem?
  (lambda (p) ; p: string
    (char=?
      (string-ref p (last-position p))
      #\a
      )))

(define sing-to-plu
  (lambda (p)
    (if (is-fem? p)
        (fem-plu p)
        (mas-plu p)
        )))