;;
;; Preamble
;;

(use srfi-1)
(use srfi-27)
(random-source-randomize! default-random-source)

;;
;; Macro
;;

(define-syntax dotimes
  (syntax-rules ()
    ((_ (var count result ...) body ...)
     (do ((var 0 (+ 1 var)))
         ((>= var count) result ...)
       body ...))))

(define-syntax dolist
  (syntax-rules ()
    ((_ (var list-form result-form ...) statement ...)
     (do ((n-list list-form (cdr n-list)))
         ((null? n-list) result-form ...)
       (let ((var (car n-list)))
         statement ...)))))
;;
;; Utilities for wt-tree
;;

(define (random-alist n)
  (zip (random-list n)))

(define integer-scale 10)

(define (random-list n)
  (let ((range (* n integer-scale)))
    (list-tabulate n
		   (lambda (dummy)
		     (random-integer range)))))

(define (from-alist al)
  (alist->wt-tree number-wt-type al))

(define (to-list tree)
  (wt-tree/fold (lambda (k v l) (cons k l)) '() tree))

(define (uniq x)
  (define (func y z)
    (if (and (not (null? z)) (equal? y (car z)))
	z
	(cons y z)))
  (fold-right func () x))

;;
;; Engine
;;

(define number-of-range 10)

(define (ladder i)
  (let* ((unit (quotient number-of-tests number-of-range))
	 (size (* unit (+ (quotient i unit) 1))))
    size))

(define (run-test lst i)
  (let* ((func (car lst))
	 (syms (cdr lst))
	 (size (ladder i))
	 (args (map (type-to-data size) syms)))
    (if (apply func args)
	#t
	args)))

(define (type-to-data size)
  (lambda (type)
    (cond
     ((eq? type 'alist)
      (random-alist size))
     ((eq? type 'int)
      (random-integer size))
     (else
      (error "Unknown type: " type)))))

;;
;; property tests
;;

(define (prop-alist->wt-tree alst)
  (wt-tree/valid? (from-alist alst)))

(define (prop-wt-tree/add alst k v)
  (wt-tree/valid? (wt-tree/add (from-alist alst) k v)))

(define (prop-wt-tree/delete alst)
  (let* ((tree (from-alist alst))
	 (len (length alst))
	 (k (car (list-ref alst (quotient len 2)))))
    (wt-tree/valid? (wt-tree/delete tree k))))

(define (prop-wt-tree/delete-min alst)
  (wt-tree/valid? (wt-tree/delete-min (from-alist alst))))

(define (prop-wt-tree/lookup alst)
  (let* ((tree (from-alist alst))
	 (len (length alst))
	 (k (car (list-ref alst (quotient len 2)))))
    (eq? (wt-tree/lookup tree k #f) '())))

(define (prop-wt-tree/add-lookup alst k v)
  (let ((tree (wt-tree/add (from-alist alst) k v)))
    (eq? (wt-tree/lookup tree k #f) v)))

(define (prop-wt-tree/union alst1 alst2)
  (let ((t1 (from-alist alst1))
	(t2 (from-alist alst2)))
    (wt-tree/valid? (wt-tree/union t1 t2))))

(define (prop-wt-tree/union-model alst1 alst2)
  (let* ((l1 (uniq (sort (map car alst1))))
	 (l2 (uniq (sort (map car alst2))))
	 (model (sort (lset-union eq? l1 l2)))
	 (t1 (from-alist alst1))
	 (t2 (from-alist alst2))
	 (this (sort (to-list (wt-tree/union t1 t2)))))
    (equal? model this)))

(define (prop-wt-tree/intersection alst1 alst2)
  (let ((t1 (from-alist alst1))
	(t2 (from-alist alst2)))
    (wt-tree/valid? (wt-tree/intersection t1 t2))))

(define (prop-wt-tree/intersection-model alst1 alst2)
  (let* ((l1 (uniq (sort (map car alst1))))
	 (l2 (uniq (sort (map car alst2))))
	 (model (sort (lset-intersection eq? l1 l2)))
	 (t1 (from-alist alst1))
	 (t2 (from-alist alst2))
	 (this (sort (to-list (wt-tree/intersection t1 t2)))))
    (equal? model this)))

(define (prop-wt-tree/difference alst1 alst2)
  (let ((t1 (from-alist alst1))
	(t2 (from-alist alst2)))
    (wt-tree/valid? (wt-tree/difference t1 t2))))

(define (prop-wt-tree/difference-model alst1 alst2)
  (let* ((l1 (uniq (sort (map car alst1))))
	 (l2 (uniq (sort (map car alst2))))
	 (model (sort (lset-difference eq? l1 l2)))
	 (t1 (from-alist alst1))
	 (t2 (from-alist alst2))
	 (this (sort (to-list (wt-tree/difference t1 t2)))))
    (equal? model this)))

;;
;; test db
;;

(define test-alist
  (list
   (list "alist->wt-tree" prop-alist->wt-tree 'alist)
   (list "prop-wt-tree/add" prop-wt-tree/add 'alist 'int 'int)
   (list "wt-tree/delete" prop-wt-tree/delete 'alist)
   (list "wt-tree/delete-min" prop-wt-tree/delete-min 'alist)
   (list "prop-wt-tree/lookup" prop-wt-tree/lookup 'alist)
   (list "prop-wt-tree/add-lookup" prop-wt-tree/add-lookup 'alist 'int 'int)
   (list "prop-wt-tree/union" prop-wt-tree/union 'alist 'alist)
   (list "prop-wt-tree/union-model" prop-wt-tree/union-model 'alist 'alist)
   (list "prop-wt-tree/intersection" prop-wt-tree/intersection 'alist 'alist)
   (list "prop-wt-tree/intersection-model" prop-wt-tree/intersection-model 'alist 'alist)
   (list "prop-wt-tree/difference" prop-wt-tree/difference 'alist 'alist)
   (list "prop-wt-tree/difference-model" prop-wt-tree/difference-model 'alist 'alist)))

;;
;; main
;;

(define number-of-tests 300)

(define (main args)
  (dolist (prop test-alist)
     (guard (dummy (else '()))
	(let ((tag (car prop))
	      (test (cdr prop)))
	  (format #t "~a: testing ~d cases... " tag number-of-tests)
	  (flush)
	  (dotimes (i number-of-tests)
	     (let ((ret (run-test test i)))
	       (unless (eq? ret #t)
		  (print "FAIL")
		  (format #t "~d/~d: ~a\n" i number-of-tests ret)
		  (raise "Property invalid"))))
	  (print "PASS")
	  (flush))))
  0)
