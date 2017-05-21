#lang racket
;;; Racket Lisp, 2017-05-21
(require slideshow/code)
(require pict)

(define (save-pict the-pict name kind)
  (define bm (pict->bitmap the-pict))
  (send bm save-file name kind))

(define (save-code code-pict)
  (save-pict code-pict "C:\\code.png" 'png))
