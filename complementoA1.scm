;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname complementoA1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define compl
 (lambda (bit)
  (if (string=? bit "0") "1" "0")))

(define compl-a-1 
 (lambda (seq)
  (if (> (string-length seq) 0)
    (string-append 
     (compl (substring seq 0 1))
     (compl-a-1 (substring seq 1))
   )
   "")))
(compl-a-1 "0011010")