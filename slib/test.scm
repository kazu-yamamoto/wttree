;;
;; Preamble
;;

(load "./wttree.scm")
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

;;
;; Utilities for wt-tree
;;

(define (random-alist n)
  (zip (random-list n)))

(define (random-list n)
  (list-tabulate n
		 (lambda (dummy)
		   (random-integer n))))

(define (from-alist al)
  (alist->wt-tree number-wt-type al))

;;
;; Engine
;;

(define (run-test lst)
  (let ((func (car lst))
	(args (cdr lst)))
    (apply func (map type-to-data args))))

(define (type-to-data type)
  (let ((size number-of-tests)) ;; ugh!
    (cond
     ((eq? type 'alist)
      (random-alist size))
     ((eq? type 'int)
      (random-integer size)))))

;;
;; property tests
;;

(define (prop-alist->wt-tree alst)
  (wt-tree/valid? (from-alist alst)))

;;
;; test db
;;

(define test-alist
  (list (list prop-alist->wt-tree 'alist)))

;;
;; main
;;

(define number-of-tests 100)

(let ((x (car test-alist)))
  (dotimes (i number-of-tests)
    (if (run-test x)
	(print "OK")
	(print "NG"))))

