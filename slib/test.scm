(load "./wttree.scm")
(use srfi-1)
(use srfi-27)
(random-source-randomize! default-random-source)

(define x (alist->wt-tree number-wt-type '((-3)
					   (-6)
					   (3)
					   (1)
					   (7)
					   (-2)
					   (5))))
;;(wt-tree/valid? x)
;;(wt-tree/valid? (wt-tree/delete-min x))

(define (uniq x)
  (define (func y z)
    (if (and (not (null? z)) (equal? y (car z)))
	z
	(cons y z)))
  (fold-right func () x))

(define (random-list n)
  (list-tabulate n
		 (lambda (dummy)
		   (random-integer n))))

(print (wt-tree/valid? x))
(print (wt-tree/valid? (wt-tree/delete-min x)))
