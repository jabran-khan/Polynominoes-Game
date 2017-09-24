;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |problem 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
(define-struct pos (x y))

;;************************* PROBLEM 1, PART A **********************

;; (build-2dlist num1 num2 f) consumes two numbers, num1 and num2,
;;   representing the width and height of the created list respectively
;; build-2dlist: Nat Nat (X X -> Y) -> (listof Y)
;; Examples:
(check-expect (build-2dlist 5 6 +)
              (list (list 0 1 2 3 4)
                    (list 1 2 3 4 5)
                    (list 2 3 4 5 6)
                    (list 3 4 5 6 7)
                    (list 4 5 6 7 8)
                    (list 5 6 7 8 9)))
(check-expect (build-2dlist 2 3 *)
              (list (list 0 0)
                    (list 0 1)
                    (list 0 2)))

(define (build-2dlist num1 num2 f)
  (build-list num2
              (lambda (x)
                (build-list num1
                            (lambda (y)
                              (f x y))))))
;; Tests:
(check-expect (build-2dlist 2 2 <)
              (list (list false true)
                    (list false false)))
(check-expect (build-2dlist 2 2 =)
              (list (list true false)
                    (list false true)))
(check-expect (build-2dlist 2 2 >)
              (list (list false false)
                    (list true false)))
(check-expect (build-2dlist 3 3 (lambda (x y)
                                  (+ x 0)))
              (list (list 0 0 0)
                    (list 1 1 1)
                    (list 2 2 2)))

;;************************** PART B *****************************

;; (all-positions w h) consumes numbers w and h and produces all possible
;;   in a grid with height h and width w
;; all-positions: Nat Nat -> (listof Pos)
;; Examples:
(lists-equiv? (all-positions 2 3) (list (make-pos 0 0) (make-pos 1 0)
                                        (make-pos 0 1) (make-pos 1 1)
                                        (make-pos 0 2) (make-pos 1 2)))

(define (all-positions w h)
  (foldr (lambda (x y) (append x y)) empty (build-2dlist h w make-pos)))


(all-positions 5 6)