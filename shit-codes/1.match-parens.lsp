;;Emacs Lisp, 2017-05-20
;;;;;;;;;;verfy if parens matched;;;;;;;;;
(defun test-patch-parens ()
  (and 
   (eq (match-parens '( { BEGIN < BEGIN < > { } END > END } ))
       t)
   (eq (match-parens '( { BEGIN < BEGIN < > { } BEGIN > END } ))
       nil))

(defun match-parens (parens)
  (match-parens-onstack '() parens))

(defun match-parens-onstack (stack parens)
  (if parens
      (cond ((in (car parens) (openparens))
	     (match-parens-onstack (cons (car parens) stack)
				   (cdr parens)))
	    ((in (car parens) (closeparens))
	     (if (match?paren (car stack) (car parens))
		 (match-parens-onstack (cdr stack) (cdr parens))
	       nil))
	    ('t (match-parens-onstack stack (cdr parens))))
    (not stack)))

(defun in (item the-list)
  (if the-list
      (if (eq item (car the-list))
	  t
	  (in item (cdr the-list)))
      nil))

(defun match?paren (open close)
  (cond ((eq open '{) (eq close '}))
	((eq open '<) (eq close '>))
	((eq open 'BEGIN) (eq close 'END))))

(defun openparens ()
  '({ < BEGIN))

(defun closeparens ()
  '(} > END))
