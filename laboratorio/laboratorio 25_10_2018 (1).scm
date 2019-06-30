;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |laboratorio 25_10_2018 (1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define get-last-char
  (lambda (p)
    (string-ref p (- (string-length p) 1))
    ))

(define is-maschile?
  (lambda (p)
    (or (char=? (get-last-char p) #\o) (char=? (get-last-char p) #\i))
    ))

(define is-singolare?
  (lambda (p)
    (or (char=? (get-last-char p) #\o) (char=? (get-last-char p) #\a))
    ))

(define articolo
  (lambda (p)
    (if (is-maschile? p)
        (if (is-singolare? p) "il" "i")
        (if (is-singolare? p) "la" "le")
    )
    ))


(define get-con
  (lambda (v)
    (substring v (- (string-length v) 3))
    ))

(define is-1con?
  (lambda (v)
    (string=? (get-con v) "are")))

(define do-coniuga
  (lambda (v suff)
    (string-append
     (substring v 0 (- (string-length v) 3))
     suff)
    ))

(define coniuga
  (lambda (v s)
    (if (is-1con? v)
        (if (is-singolare? s)
            (do-coniuga v "a")
            (do-coniuga v "ano"))
        (if (is-singolare? s)
            (do-coniuga v "e")
            (do-coniuga v "ono")))
    ))

(define frase
  (lambda (soggetto verbo complemento)
    (string-append
     (articolo soggetto) " "
     soggetto " "
     (coniuga verbo soggetto) " "
     (articolo complemento) " "
     complemento)
    ))

(frase "gatto" "cacciare" "topi")
(frase "mucca" "mangiare" "fieno")
(frase "sorelle" "leggere" "novella")
(frase "bambini" "amare" "favole")
(frase "musicisti" "suonare" "pianoforti")
(frase "cuoco" "friggere" "patate")
(frase "camerieri" "servire" "clienti")
(frase "mamma" "chiamare" "figlie")