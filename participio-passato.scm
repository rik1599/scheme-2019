;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname participio-passato) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define position
  (lambda (p l) ; p string, l int
    (- (string-length p) l)))

(define get-coniugazione ; ritorna "are", "ere", "ire"
  (lambda (p) ; p string
    (substring p (position p 3))))

(define create-participio
  (lambda (v lp suff)
    (string-append
     (substring v 0 (position v lp))
     suff)
    ))

(define is2con?
  (lambda (p)
    (string=?
     (get-coniugazione p)
     "ere"
     )))

(define participio
  (lambda (v)
    (if (is2con? v)
        (create-participio v 3 "uto")
        (create-participio v 2 "to")
      )
    )
  )