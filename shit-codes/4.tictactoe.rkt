#lang racket
;; Racket Lisp, 2017-05-23
;; Starting game with (start-game)

(struct point (row column))
(struct gamestat (number board current-player winning) #:transparent)
(define (init-game)
  (gamestat 1 (make-vector 9 '_) 'o 'not-ended))
(define (empty? cell)
  (eq? cell '_))
(define (opposite player)
  (cond ((eq? player 'o) 'x)
        ((eq? player 'x) 'o)
        ('t 'INVALID)))
(define (invalid? task)
  (eq? task 'INVALID))
(define (get-cell board pos [normalize? #t])
  (vector-ref board (flatten pos normalize?)))
(define (set!-cell board pos data [normalize? #t])
  (vector-set! board (flatten pos normalize?) data))
(define (flatten pos normalize?)
  (if normalize?
      (let ([row (dec (point-row pos))]
            [col (dec (point-column pos))])
        (+ (* row 3) col))
      (let ([row (point-row pos)]
            [col (point-column pos)])
        (+ (* row 3) col))))
(define (copy-board board)
  (let ([new-board (make-vector 9)])
    (for ([i 9])
      (vector-set! new-board i (vector-ref board i)))
    new-board))
(define (dec x)
  (- x 1))
(define (inc x)
  (+ x 1))

(define (play game hand-pos)
  (let ([result (make-move game hand-pos)])
    (if (invalid? result)
        (expect-re-input game)
        (if (ended? result)
            (game-end-with result)
            (expect-next-input result)))))

(define (make-move game hand-pos)
  (define (eval-game board)
    (define (bingo? board way)
      (define (check-horizontal board way)
        (if (bingo-a-line? (get-cell board (point way 0) #f)
                           (list (get-cell board (point way 1) #f)
                                 (get-cell board (point way 2) #f)))
            #t (bingo? board (+ (inc way) 3))))
      (define (check-vertical board way)
        (if (bingo-a-line? (get-cell board (point 0 way) #f)
                           (list (get-cell board (point 1 way) #f)
                                 (get-cell board (point 2 way) #f)))
            #t (bingo? board (inc way))))
      (define (check-diagonal board)
        (or
         (bingo-a-line? (get-cell board (point 0 0) #f)
                        (list (get-cell board (point 1 1) #f)
                              (get-cell board (point 2 2) #f)))
         (bingo-a-line? (get-cell board (point 2 0) #f)
                        (list (get-cell board (point 1 1) #f)
                              (get-cell board (point 0 2) #f)))))
      (define (bingo-a-line? x xs)
        (if (null? xs)
            #t
            (if (and (not (empty? x)) (eq? x (car xs)))
                (bingo-a-line? x (cdr xs)) #f)))
      (cond [(< way 3) (check-horizontal board way)]
            [(< way 6) (check-vertical board (- way 3))]
            ['t (check-diagonal board)]))
    (define (ippai? board idx)
      (if (< idx 9)
          (if (empty? (vector-ref board idx))
              #f (ippai? board (inc idx)))
          #t))
    (if (bingo? board 0)
        'win
        (if (ippai? board 0)
            'draw 'not-ended)))
  (define (entry game hand-pos)
    (let* ([new-board (copy-board (gamestat-board game))]
           [x (set!-cell new-board hand-pos (gamestat-current-player game))])
      (gamestat (inc (gamestat-number game))
                new-board
                (opposite (gamestat-current-player game))
                (eval-game new-board))))
  (if (empty? (get-cell (gamestat-board game) hand-pos))
      (entry game hand-pos)
      'INVALID))
(define (expect-re-input game)
  (expect-next-input game))
(define (ended? game)
  (not (eq? (gamestat-winning game) 'not-ended)))
(define (game-end-with game-result)
  ; void function
  (begin
    (showboard (gamestat-board game-result))
    (if (win? game-result)
        (show-winner (gamestat-current-player game-result))
        (displayln "draw"))))
(define (show-winner player)
  ; void function
  (begin
    (display (opposite player))
    (displayln " won!")))
(define (win? game)
  (eq? (gamestat-winning game) 'win))
(define (expect-next-input game)
  (begin
    (showboard (gamestat-board game))
    (display (gamestat-current-player game))
    (displayln " turn")
    (displayln "(input [row] [column])")
    game))
(define (showboard board)
  ; void function
  (for ([i 3])
    (for ([j 3])
      (display (get-cell board (point i j) #f))
      (display " "))
    (newline)))
(define (input hand-row hand-col)
  (set! *game* (play *game* (point hand-row hand-col))))

(define (start-game)
  (begin
    (set! *game* (init-game))
    (expect-next-input *game*)))
(define *game* null)