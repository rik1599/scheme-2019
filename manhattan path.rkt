;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |manhattan path|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define manhattan-path
  (lambda (i j)
    (cond ((= i 0) 1)
          ((= j 0) 1)
          (else
           (+
            (manhattan-path i (- j 1))
            (manhattan-path (- i 1) j)))
          ))
  )

(manhattan-path 3 5)
(manhattan-path 1 2)
(manhattan-path 2 3)


(define modi
  (lambda (n k)
    (if (or (= k 1) (= k n))
        1
        (+ (modi (- n 1) (- k 1)) (* k (modi (- n 1) k)))
        )
    )
  )