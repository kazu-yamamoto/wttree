;;
;; Preamble
;;

;(use srfi-1)
;(use srfi-27)
(load-option 'format)
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

(define-syntax block
  (syntax-rules ()
    ((_ escape body ...)
     (call-with-current-continuation
      (lambda (escape) body ...)))))

(define (sort1 lst)
  (sort lst <))

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

(define (prop-wt-tree/fold alst)
  (let* ((model (uniq (sort1 (map car alst))))
	 (tree (from-alist alst))
	 (this (to-list tree)))
    (equal? model this)))

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
  (let* ((l1 (uniq (sort1 (map car alst1))))
	 (l2 (uniq (sort1 (map car alst2))))
	 (model (sort1 (lset-union eq? l1 l2)))
	 (t1 (from-alist alst1))
	 (t2 (from-alist alst2))
	 (this (sort1 (to-list (wt-tree/union t1 t2)))))
    (equal? model this)))

(define (prop-wt-tree/intersection alst1 alst2)
  (let ((t1 (from-alist alst1))
	(t2 (from-alist alst2)))
    (wt-tree/valid? (wt-tree/intersection t1 t2))))

(define (prop-wt-tree/intersection-model alst1 alst2)
  (let* ((l1 (uniq (sort1 (map car alst1))))
	 (l2 (uniq (sort1 (map car alst2))))
	 (model (sort1 (lset-intersection eq? l1 l2)))
	 (t1 (from-alist alst1))
	 (t2 (from-alist alst2))
	 (this (sort1 (to-list (wt-tree/intersection t1 t2)))))
    (equal? model this)))

(define (prop-wt-tree/difference alst1 alst2)
  (let ((t1 (from-alist alst1))
	(t2 (from-alist alst2)))
    (wt-tree/valid? (wt-tree/difference t1 t2))))

(define (prop-wt-tree/difference-model alst1 alst2)
  (let* ((l1 (uniq (sort1 (map car alst1))))
	 (l2 (uniq (sort1 (map car alst2))))
	 (model (sort1 (lset-difference eq? l1 l2)))
	 (t1 (from-alist alst1))
	 (t2 (from-alist alst2))
	 (this (sort1 (to-list (wt-tree/difference t1 t2)))))
    (equal? model this)))

;;
;; test db
;;

(define test-alist
  (list
   (list "alist->wt-tree" prop-alist->wt-tree 'alist)
   (list "wt-tree/fold" prop-wt-tree/fold 'alist)
   (list "wt-tree/add" prop-wt-tree/add 'alist 'int 'int)
   (list "wt-tree/delete" prop-wt-tree/delete 'alist)
   (list "wt-tree/delete-min" prop-wt-tree/delete-min 'alist)
   (list "wt-tree/lookup" prop-wt-tree/lookup 'alist)
   (list "wt-tree/add-lookup" prop-wt-tree/add-lookup 'alist 'int 'int)
   (list "wt-tree/union" prop-wt-tree/union 'alist 'alist)
   (list "wt-tree/union-model" prop-wt-tree/union-model 'alist 'alist)
   (list "wt-tree/intersection" prop-wt-tree/intersection 'alist 'alist)
   (list "wt-tree/intersection-model" prop-wt-tree/intersection-model 'alist 'alist)
   (list "wt-tree/difference" prop-wt-tree/difference 'alist 'alist)
   (list "wt-tree/difference-model" prop-wt-tree/difference-model 'alist 'alist)))

;;
;; main
;;

(define number-of-tests 300)

(format #t "\n")
(dolist (prop test-alist)
   (let ((tag (car prop))
	 (test (cdr prop)))
     (format #t "~A: testing ~A cases... " tag number-of-tests)
     (flush-output)
     (block break
       (dotimes (i number-of-tests)
	 (let ((ret (run-test test i)))
	   (if (not (eq? ret #t))
	       (begin
		 (format #t "FAIL\n")
		 (format #t "~A/~A: ~A\n" i number-of-tests ret)
		 (break 1)))))
       (format #t "PASS\n")
       (flush-output))))