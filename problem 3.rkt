;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |problem 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
(define-struct pos (x y))
;; A Pos is a (make-pos Nat Nat)



;; (first-empty-pos grid) takes a grid and returns a pos which gives
;;   the position of the first empty space in the grid
;; first-empty-pos: Grid -> (anyof Pos Bool)
;; Examples:
(check-expect (first-empty-pos '((#\. #\.))) (make-pos 0 0))
(check-expect (first-empty-pos '((#\A #\A))) false)

(define (first-empty-pos grid)
  (local [;;(first-empty-pos/lst grid y-val) takes a grid and the row number
          ;;  y-val and returns the position of the first empty space
          ;; first-empty-pos/lst Grid Nat -> (anyof Pos Bool)
          (define (first-empty-pos/lst grid y-val)
            (cond [(empty? grid) false]
                  [(pos? (find-x-val (first grid) y-val 0))
                   (find-x-val (first grid) y-val 0)]
                  [else (first-empty-pos/lst (rest grid) (add1 y-val))]))
          ;;(find-x-val grid y-val x-val) consumes a grid, current row num
          ;;  y-val and current column, x-val, and returns the position of
          ;;  the first empty space
          ;; find-x-val: Grid Nat Nat -> (anyof Pos List)
          (define (find-x-val grid y-val x-val)
            (cond [(empty? grid) empty]
                  [(char=? (first grid) #\.)
                   (make-pos x-val y-val)]
                  [else (find-x-val (rest grid) y-val (+ 1 x-val))]))]
    (first-empty-pos/lst grid 0))) 

;;Tests:
(check-expect (first-empty-pos '((#\A #\A #\.)(#\. #\B #\.)))
              (make-pos 2 0))
(check-expect (first-empty-pos '((#\A #\A #\A)(#\. #\B #\.)))
              (make-pos 0 1))
(check-expect (first-empty-pos '((#\A #\A #\A)(#\A #\B #\.)))
              (make-pos 2 1))
(check-expect (first-empty-pos '((#\A #\A #\A)(#\V #\B #\A)))
              false)
(check-expect (first-empty-pos '((#\A)(#\A)))
              false)
(check-expect (first-empty-pos '((#\A #\A #\A)(#\C #\B #\E)(#\F #\B #\.)))
              (make-pos 2 2))