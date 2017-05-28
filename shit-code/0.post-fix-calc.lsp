;; Emacs Lisp, 2017-05-20
;;;;;;;;;;Stack post-fix calculator;;;;;;;
(defun test-calc-infix ()
  (equal (calc-infix '(2 expt 5 - 2 / 5 * 9)) '(54)))

(defun test-calc ()
  (equal (calc '(2 5 expt 2 - 5 / 9 *)) '(54)))

(defun calc (fomula)
  (calc-stack '() fomula))

(defun calc-stack (stack fomula)
  (if fomula
      (if (numberp (car fomula))
	  (calc-stack (cons (car fomula) stack) (cdr fomula))
	  (calc-stack (reduce-stack-with (car fomula) stack)
		      (cdr fomula)))
      stack))

(defun in (item the-list)
  (if the-list
      (if (eq item (car the-list))
	  t
	  (in item (cdr the-list)))
      nil))

(defun reduce-stack-with (operator stack)
  ;; No zero-devision handling, but it can be added easily, in fact.
  (cons (let ((num2 (car stack))
	      (num1 (cadr stack)))
	  (funcall operator num1 num2)) (cddr stack)))

(defun calc-infix (fomula)
  (calc (infix-to-postfix fomula)))

(defun infix-to-postfix (fomula)
  (letrec ((rec 
	   (lambda (fomula)
	     (if fomula
		 (cons (cadr fomula) 
		       (cons (car fomula) (funcall rec (cddr fomula))))
	       nil))))
    (cons (car fomula) (funcall rec (cdr fomula)))))
