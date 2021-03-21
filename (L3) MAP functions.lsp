;6. Return the list of nodes of a tree of type (1) accessed inorder

; This is an auxiliary function to get the left subtree
;   get_left(l1l2..ln, nV, nE) = []				, n = 0
;							   = []				, nV = nE+1
;							   = l1 U l2 U get_left(l3..ln, nV+1, nE+l2),  otherwise
(defun get_left(tree nV nE)
	(cond
		((null tree) nil)
		((= nV (+ 1 nE)) nil)
		(t (cons (car tree) (cons (cadr tree) (get_left (cddr tree) (+ 1 nV) (+ (cadr tree) nE)))))
	)
)
(defun left(tree)
	(get_left (cddr tree) 0 0)
)


; This is an auxiliary function to get the right subtree
;   get_right(l1l2..ln, nV, nE) = []				, n = 0
;							    = l1..ln			, nV = nE+1
;							    = get_right(l3..ln, nV+1, nE+l2),  otherwise
(defun get_right(tree nV nE)
	(cond
		((null tree) nil)
		((= nV (+ 1 nE)) tree)
		(t (get_right (cddr tree) (+ 1 nV) (+ (cadr tree) nE)))
	)
)
(defun right(tree)
	(get_right (cddr tree) 0 0)
)



; inorder(l1l2..ln) = [],       n = 0
;					= inorder(left(l1..n)) U l1 U inorder(right(l1..ln))  otherwise
(defun inorder (tree)
	(cond
		((null tree) nil)
		(t (append (inorder (left tree)) (list (car tree)) (inorder (right tree))))
		;(t (append (inorder (left tree)) (cons (car tree) (inorder (right tree)))))
	)
)



(print (inorder2 '(A 2 B 0 C 2 D 0 E 0)))
(print (inorder2 '(a 2 b 2 c 1 i 0 f 1 g 1 m 0 d 2 e 0 h 0)))


;    A                                     a
;  /   \								 /   \
; B     C                               b     d
;      / \							  /  \   /  \
;     D   E							 c   f  e   h
;									/   /
;								   i   g





; Version with lambda and a function that returns both left and right subtree

; tree -> (left, right)
; get_subtrees(l1..ln, nV, nE) = [], n=0
;							   = ([], l1..ln),  nV=nE+1
;							   = (l1 U l2 U get_subtrees(l3..ln, nV+1, nE+l2)[1],
;										get_subtrees(l3..ln, nV+1, nE+l2)[2]), otherwise 

(defun get_subtrees (tree nV nE)
	(cond 
		((null tree) (list nil nil))
		((= nV (+ 1 nE)) (list nil tree))
		(t  (
			  (lambda (v)
				(list (cons (car tree) (cons (cadr tree) (car v))) (cadr v))
			  )
			  (get_subtrees (cddr tree) (+ 1 nV) (+ (cadr tree) nE))
			)
		)
	)
)

(defun subtrees (tree)
	(get_subtrees (cddr tree) 0 0)
)


(defun inorder2 (tree)
	(cond
		((null tree) nil)
		(t ((lambda (v)
		     (append (inorder2 (car v)) (list (car tree)) (inorder2 (cadr v))))
		   (subtrees tree)
		   )
		)
	)
)
