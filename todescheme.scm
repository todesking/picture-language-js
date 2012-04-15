(define cadr (lambda (tgt) (car (cdr tgt)))) ;;
(define caddr (lambda (tgt) (car (cdr (cdr tgt))))) ;;
(define (for-each proc target)
	(if (null? target)
	  ()
	  (begin
		(proc (car target))
		(for-each proc (cdr target))))) ;;
"dummy"
