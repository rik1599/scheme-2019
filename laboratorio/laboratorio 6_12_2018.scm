;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |laboratorio 6_12_2018|) (read-case-sensitive #t) (teachpacks ((lib "hanoi.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "hanoi.ss" "installed-teachpacks")) #f)))
(define hanoi-moves
  (lambda (n k s d t s-val d-val t-val)
    (let ([disk (expt 2 (- n 1))])
      (cond [(= n 0)    (list (list s s-val) (list d d-val) (list t t-val))]
            [(< k disk) (hanoi-moves (- n 1) k          s t d (+ s-val 1) t-val       d-val)]
            [else       (hanoi-moves (- n 1) (- k disk) t d s t-val       (+ d-val 1) s-val)]
          )
      )))

(define hanoi-disks
  (lambda (n k)
    (hanoi-moves n k 1 2 3 0 0 0)
    ))

(define hanoi-picture-rec
  (lambda (max n k s d t s-val d-val t-val img)
    (let ([disk (expt 2 (- n 1))])
      (cond [(= n 0)    img]
            [(< k disk) (hanoi-picture-rec max (- n 1) k          s t d (+ s-val 1) t-val       d-val (above (disk-image n max s s-val) img))]
            [else       (hanoi-picture-rec max (- n 1) (- k disk) t d s t-val       (+ d-val 1) s-val (above (disk-image n max d d-val) img))]
            )
      )))

(define hanoi-picture
  (lambda (n k)
    (hanoi-picture-rec n n k 1 2 3 0 0 0 (towers-background n))
    ))

(define hanoi-picture-list
  (lambda (n)
    (reverse (hanoi-picture-list-rec n 0 null))
    ))

(define hanoi-picture-list-rec
  (lambda (n k pic-list)
    (if (> k (- (expt 2 n) 1))
        pic-list
        (hanoi-picture-list-rec n (+ k 1) (cons (hanoi-picture n k) pic-list))
        )))

(hanoi-disks 3 0)
(hanoi-disks 3 1)
(hanoi-disks 3 2)
(hanoi-disks 3 3)
(hanoi-disks 3 4)
(hanoi-disks 3 5)
(hanoi-disks 3 6)
(hanoi-disks 3 7)
(hanoi-disks 5 13)
(hanoi-disks 15 19705)
(hanoi-disks 15 32767)

(hanoi-picture 5 0)
(hanoi-picture 5 13)
(hanoi-picture 5 22)
(hanoi-picture 5 31)
(hanoi-picture 15 19705)