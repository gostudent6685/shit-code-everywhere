#lang racket
(require json)
;; Racket Lisp, 2017-05-24

(define (money-sum s)
  (foldl (lambda (x y) (+ (car x) y))
          0
          s))
(define (read-moneyio [filename "moneyio.json"])
  (with-handlers ([exn:fail? (lambda (e) (string->jsexpr "[]"))])
    (with-input-from-file filename read-json)))
(define (write-moneyio data [filename "moneyio.json"])
  (call-with-output-file filename (lambda (file) (write-json data file #:encode 'all)) #:exists 'replace))

(define (load-moneyio [filename "moneyio.json"])
  (set! *moneyio* (read-moneyio filename)))
(define (save-moneyio [filename "moneyio.json"])
  (write-moneyio *moneyio* filename))

(define (spend money comment)
  (set! *moneyio* (append *moneyio* (list (list (- money) comment)))))
(define (get money comment)
  (set! *moneyio* (append *moneyio* (list (list money comment)))))
(define (sum)
  (money-sum *moneyio*))
(define (combine-in-one [comment "combined"])
  (set! *moneyio* (list (list (sum) comment))))

(define *moneyio* null)

(define (help)
  (begin
    (displayln "moneyio is very simple money history manager.")
    (displayln "Load your money history by '(load-moneyio)'.")
    (displayln "If you spent money, then just call '(spend [money] [comment])'.")
    (displayln "If you got money, then call '(get [money] [comment])'.")
    (displayln "Save your money history by '(save-moneyio)'.")
    (displayln "You can open your history whenever you want by calling '*moneyio*'")
    (displayln "If you no longer need old history, then combine all history by '(combine-in-one)")))