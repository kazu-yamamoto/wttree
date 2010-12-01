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

(define (random-list n)
  (list-tabulate n
		 (lambda (dummy)
		   (random-integer n))))

(define (from-alist al)
  (alist->wt-tree number-wt-type al))

;;
;; Engine
;;

(define (ladder i)
  (let* ((unit (quotient number-of-tests 10))
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

(define (prop-wt-tree/delete-min alst)
  (wt-tree/valid? (wt-tree/delete-min (from-alist alst))))

;;
;; test db
;;

(define test-alist
  (list
   (list "alist->wt-tree" prop-alist->wt-tree 'alist)
   (list "wt-tree/delete-min" prop-wt-tree/delete-min 'alist)))

;;
;; main
;;

(define number-of-tests 1000)

(dolist (prop test-alist)
  (let ((tag (car prop))
	(test (cdr prop)))
    (format #t "~a: testing ~d cases... " tag number-of-tests)
    (flush)
    (dotimes (i number-of-tests)
       (let ((ret (run-test test i)))
	 (unless (eq? ret #t)
	    (print "FAIL")
	    (format #t "~d/~d: ~a\n" i number-of-tests ret)
	    (error "Property invalid"))))
    (print "PASS")
    (flush)))
