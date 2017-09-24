;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |problem 4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
(define-struct pos (x y))
;; A Pos is a (make-pos Nat Nat)

;; A Grid is a (listof (listof Char))
;; requires: both inner and outer lists of Grid are non-empty

(define-struct state (puzzle pieces))
;; A State is a (make-state Grid (listof Grid))

(define (superimpose base top position)
  (superimpose/v2 base top position 0))

(define (superimpose/v2 base top position y-val)
  (cond [(empty? base) empty]
        [(empty? top) base]
        [(<= (pos-y position) y-val)
         (cons (create-rows (first base) (first top) position 0 y-val)
               (superimpose/v2 (rest base) (rest top) position (add1 y-val)))]
        [else (cons (first base)
                    (superimpose/v2 (rest base) top position (add1 y-val)))]))

(define (create-rows base-row top position x-val y-val)
  (cond [(empty? base-row) empty]
        [(empty? top) base-row]
        [(> (pos-x position) x-val)
         (cons (first base-row)
               (create-rows
                (rest base-row)
                top
                position
                (add1 x-val)
                y-val))]
        [(char=? #\. (first top))
         (cons (first base-row)
               (create-rows (rest base-row)
                            (rest top)
                            position
                            (add1 x-val)
                            y-val))]
        [else
         (cons (first top)
               (create-rows (rest base-row)
                            (rest top)
                            position
                            (add1 x-val)
                            y-val))]))


(superimpose '((#\b #\. #\. #\. #\.)(#\b #\b #\b #\. #\.)(#\. #\b #\. #\. #\.))
             '((#\a #\a)(#\a #\a)(#\a #\.))
             (make-pos 0 0))