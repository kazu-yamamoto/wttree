(load "./wttree.scm")

(define x (alist->wt-tree number-wt-type '((-3 . -3)
					   (-6 . -6)
					   (3 . 3)
					   (1 . 1)
					   (7 . 7)
					   (-2 . -2)
					   (5 . 5))))
;;(wt-tree/valid? x)
;;(wt-tree/valid? (wt-tree/delete-min x))


