;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |problem 5|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
(define-struct pos (x y))
;; A Pos is a (make-pos Nat Nat)

;; A Grid is a (listof (listof Char))
;; requires: both inner and outer lists of Grid are non-empty

(define-struct state (puzzle pieces))
;; A State is a (make-state Grid (listof Grid))



;; (first-empty-pos grid) takes a grid and returns a pos which gives
;;   the position of the first empty space in the grid
;; first-empty-pos: Grid -> (anyof Pos Bool)
(define (first-empty-pos grid)
  (local [;;(first-empty-pos/lst grid y-val) takes a grid and the row number
          ;;  y-val and returns the position of the first empty space
          ;; first-empty-pos/lst Grid Nat -> (anyof Pos Bool)
          (define (first-empty-pos/lst grid y-val)
            (local [(define find-pos (find-x-val (first grid) y-val 0))]
              (cond [(empty? (first grid)) false]
                    [(pos? find-pos) find-pos]
                    [else (first-empty-pos/lst (rest grid) (add1 y-val))])))
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

(define (neighbours grids)
  (local [(define empty-pos (first-empty-pos (state-puzzle grids)))
          (define (search-row list col)
            (cond [(empty list) false]
                  [(and (= (pos-x empty-pos) col)
                        (not (char=? (first list) #\.))) true]
                  [(= (pos-x empty-pos) col) false]
                  [else (search-row (rest list) (add1 col))]))
          (define (search-empty stat row)
            (cond [(empty? stat) false]
                  [(= (pos-y empty-pos) row) (search-row (first stat) 0)]
                  [else (search-empty (rest stat) (add1 row))]))
          (define (check-empty los)
            (filter (lambda (x)
                      (cond [(search-empty x 0) true]
                            [else false]))
                    los))]
          
))