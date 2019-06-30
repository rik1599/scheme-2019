;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname conigli-fibonacci) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define fertile-pairs
  (lambda (t)
    (if (<= t 0)
        1
        (+ (fertile-pairs (- t 1)) (newly-born-pairs (- t 1)))
        )
    ))

(define newly-born-pairs
  (lambda (t)
    (if (<= t 0)
        0
        (fertile-pairs (- t 1)))
    ))

(define conigli
  (lambda (t)
    (+ (fertile-pairs t) (newly-born-pairs t))
    ))