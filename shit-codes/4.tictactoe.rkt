#lang racket
;; Racket Lisp, 2017-05-23
;; 2017-05-24 add minimax AI
;; Starting game with (start-game)

(struct point (row column) #:transparent)

(struct gamestat (number board current-player winning) #:transparent)

(define *game* null)

(define (start-game)
  (begin
    (set! *game* (init-game))
    (expect-next-input *game*)))

(define (expect-next-input game)
  (begin
    (showboard (gamestat-board game))
    (display (gamestat-current-player game))
    (displayln " turn")
    (displayln "(input [row] [column])")
    (displayln "or (input-ai)")
    game))

(define (expect-re-input game)
  (begin
    (displayln "INVALID MOVE. Try again.")
    (expect-next-input game)))

(define (input hand-row hand-col)
  (set! *game* (play *game* (point hand-row hand-col))))

(define (input-ai)
  (let ([fittest-point (cadr (calc-ai 'max *game* 0))])
    (set! *game* (play *game* fittest-point))))

(define (play game hand-pos)
  (let ([result (make-move game hand-pos)])
    (if (invalid? result)
        (expect-re-input game)
        (if (ended? result)
            (game-end-with result)
            (expect-next-input result)))))

(define (game-end-with game-result)
  (begin
    (showboard (gamestat-board game-result))
    (if (win? game-result)
        (show-winner (gamestat-current-player game-result))
        (displayln "draw"))
    null))

(define (init-game)
  (gamestat 1 (make-vector 9 '_) 'o 'not-ended))

(define (showboard board)
  ; void function
  (for ([i 3])
    (for ([j 3])
      (display (get-cell board (point i j) #f))
      (display " "))
    (newline)))

(define (show-winner player)
  ; void function
  (begin
    (display (opposite player))
    (displayln " won!")))

(define (make-move game hand-pos)
  (define (entry game hand-pos)
    (let* ([new-board (copy-board (gamestat-board game))])
      (set!-cell new-board hand-pos (gamestat-current-player game))
      (gamestat (inc (gamestat-number game))
                new-board
                (opposite (gamestat-current-player game))
                (eval-game new-board))))
  (if (empty? (get-cell (gamestat-board game) hand-pos))
      (entry game hand-pos)
      'INVALID))

(define (eval-game board)
    (if (is-there-bingo? board)
        'win
        (if (ippai? board)
            'draw 'not-ended)))

(define (is-there-bingo? board)
  (define (bingo-a-line? line)
    (not (eq? (foldl (lambda (x y) (if (eq? x y) x '_)) (car line) (cdr line)) '_)))
  (define (bingo-vertical? board)
    (or
     (bingo-a-line? (get-a-line board (list 0 inc) (list 0 const) 3))
     (bingo-a-line? (get-a-line board (list 0 inc) (list 1 const) 3))
     (bingo-a-line? (get-a-line board (list 0 inc) (list 2 const) 3))))
  (define (bingo-horizontal? board)
    (or
     (bingo-a-line? (get-a-line board (list 0 const) (list 0 inc) 3))
     (bingo-a-line? (get-a-line board (list 1 const) (list 0 inc) 3))
     (bingo-a-line? (get-a-line board (list 2 const) (list 0 inc) 3))))
  (define (bingo-diagonal? board)
    (or
     (bingo-a-line? (get-a-line board (list 0 inc) (list 0 inc) 3))
     (bingo-a-line? (get-a-line board (list 2 dec) (list 0 inc) 3))))
  (or
   (bingo-vertical? board)
   (bingo-horizontal? board)
   (bingo-diagonal? board)))

; (calc-ai) : <list <Integer> <point>>
; minimax : <symbol ('mini or 'max)>
; game : <gamestat>
(define (calc-ai minimax game depth)
  (define (reverse minimax)
    (cond ((eq? minimax 'mini) 'max)
          ((eq? minimax 'max) 'mini)
          (else 'INVALID)))
  (let ([able-points (get-able-points game)]
	[win-option (if (eq? minimax 'mini) -1 1)])
    ; scores : <list <list <Integer> <point>> ...>
    (let ([scores 
           (map (lambda (a-point)
                  (let ([result (make-move game a-point)])
                    (if (ended? result)
                        (if (win? result)
                            (list (* win-option 10) a-point)
                            (list 0 a-point))
                        (calc-ai (reverse minimax) result (inc depth)))))
                able-points)])
      (let ([chosen (choose-point minimax scores)])
        (list (* win-option (- (car chosen) depth)) (cadr chosen))))))
; choose-point : <list <Integer> <point>>
(define (choose-point minimax scores)
  (let ([cmp-proc (if (eq? minimax 'mini) < >)])
    (foldl (lambda (x y) (if (cmp-proc (car x) (car y)) x y))
                  (car scores)
                  scores)))
(define (get-able-points game)
  (let ([able-points '()])
    (for ([i 3])
      (for ([j 3])
        (if (empty? (get-cell (gamestat-board game) (point i j) #f))
            (set! able-points (cons (point (inc i) (inc j)) able-points))
            null)))
    able-points))

(define (ippai? board)
  (define (ippai?-iter board idx)
    (if (< idx 9)
        (if (empty? (vector-ref board idx))
            #f (ippai?-iter board (inc idx)))
        #t))
  (ippai?-iter board 0))

(define (opposite player)
  (cond ((eq? player 'o) 'x)
        ((eq? player 'x) 'o)
        ('t 'INVALID)))

(define (empty? cell)
  (eq? cell '_))

(define (invalid? task)
  (eq? task 'INVALID))

(define (ended? game)
  (not (eq? (gamestat-winning game) 'not-ended)))

(define (win? game)
  (eq? (gamestat-winning game) 'win))

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

(define (dec x)
  (- x 1))
(define (inc x)
  (+ x 1))
(define (const x) x)

(define (copy-board board)
  (let ([new-board (make-vector 9)])
    (for ([i 9])
      (vector-set! new-board i (vector-ref board i)))
    new-board))

(define (get-a-line board rule-row rule-col idx)
  (let ([row-start (car rule-row)]
        [row-func (cadr rule-row)]
        [col-start (car rule-col)]
        [col-func (cadr rule-col)])
    (if (> idx 0)
        (cons (get-cell board (point row-start col-start) #f)
              (get-a-line board
                          (list (row-func row-start) row-func)
                          (list (col-func col-start) col-func)
                          (dec idx)))
        null)))
