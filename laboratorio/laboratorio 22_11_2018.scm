;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |laboratorio 22_11_2018|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define manhattan-3d
  (lambda (x y z)
    (cond ((= x 0) (manhattan-path y z))
          ((= y 0) (manhattan-path x z))
          ((= z 0) (manhattan-path x y))
          (else
           (+
            (manhattan-3d x y (- z 1))
            (manhattan-3d x (- y 1) z)
            (manhattan-3d (- x 1) y z))
          ))
  ))

(manhattan-3d 0 0 7)
(manhattan-3d 2 0 2)
(manhattan-3d 1 1 1)
(manhattan-3d 1 1 5)
(manhattan-3d 2 3 1)
(manhattan-3d 2 3 3)
