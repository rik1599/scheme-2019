;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname controllo-parita) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define count
  (lambda (bit)
    (if (string=? bit "1") 1 0)))

(define parita
  (lambda (seq)
    (if (> (string-length seq) 0)
        (+ (count (substring seq 0 1)) (parita (substring seq 1)) )
        0)
    ))

(define controllo
  (lambda (seq)
    (string-append
     (if (even? (parita seq)) "OK " "ERROR: ")
     (number->string (parita seq))
    )))

(controllo "111")