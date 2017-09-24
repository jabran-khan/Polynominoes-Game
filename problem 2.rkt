;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |problem 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))

;; (all-orientations poly) consumes a polynomino, poly, and produces
;;   a list of all the orientations of a polynomino
;; all-orientations: Grid -> (listof Grid)
;; Examples:



(define (all-orientations poly)
  (local [; (flip-x lst) consumes a polynomino and flips vertically
          ; flip-x: Grid -> Grid
          (define (flip-x lst)
            (foldl (lambda (x y)
                     (cons x y))
                   empty
                   lst))
          ; (flip-y lst) consumes a polynomino and flips horizontally
          ; flip-y: Grid -> Grid
          (define (flip-y lst)
            (cond [(empty? lst) empty]
                  [else (cons (foldl (lambda (x y)
                                       (cons x y))
                                     empty
                                     (first lst))
                              (flip-y (rest lst)))]))
          ; (inverse row row-length lst) consumes a polynomino lst, the
          ;   the length of the row, row-length, and the current row number
          ;   and produces a polynomino with the rows and columns flipped
          ; inverse: Nat Nat Grid -> Grid
          (define (inverse row row-length lst)
            (local [; (create-row lst col) consumes a row of the polynomino
                    ;   and a column number and creates the new row of the
                    ;   inverse
                    ; create-row: (listof Char) Nat -> (listof Char)
                    (define (create-row lst col)
                      (cond [(empty? lst) empty]
                            [else (cons (nth-value (first lst) col)
                                        (create-row (rest lst) col))]))
                    ; (nth-value lst n) consumes a row and returns the nth
                    ;   element of the row
                    ; nth-value: (listof Char) Nat -> Nat
                    (define (nth-value lst n)
                      (cond [(<= n 0) (first lst)]
                            [else (nth-value (rest lst) (sub1 n))]))]
              (cond
                [(= row row-length) empty]
                [else (cons (create-row lst row)
                            (inverse (add1 row) row-length lst))])))
          (define row-length (length (first poly)))
          (define list-of-poly
            (list poly
                  (flip-x poly)
                  (flip-y poly)
                  (flip-x (flip-y poly))
                  (inverse 0 row-length poly)
                  (flip-x (inverse 0 row-length poly))
                  (flip-y (inverse 0 row-length poly))
                  (flip-x (flip-y (inverse 0 row-length poly)))))]
    (foldr (lambda (x y)
             (cond [(not (member? x y))
                    (cons x y)]
                   [else y]))
           empty
           list-of-poly)))

(all-orientations '((#\a #\a)(#\a #\a)(#\a #\.)))
(all-orientations '((#\a #\a #\a)(#\. #\a #\.)(#\. #\a #\.)))
(all-orientations '((#\a #\a)(#\a #\a)(#\a #\a)))
(all-orientations '((#\. #\a #\.)(#\a #\a #\a)(#\. #\a #\.)))