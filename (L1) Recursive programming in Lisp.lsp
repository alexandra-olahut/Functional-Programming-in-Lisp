;a) A linear list is given. Eliminate from the list all elements from N to N steps, N-given.

; eliminate(l1l2..ln, N, step) = [],    n=0
;  							   = eliminate(l2..ln, N, step+1),          step%N=0
;							   = l1 U eliminate(l2..ln, N, step+1),     otherwise
;	main(l1..ln, N) = eliminate(l1..ln, N, 1)

(defun eliminateNtoN (L N S)
	(cond
		((null L) nil)
		((= (MOD S N) 0) (eliminateNtoN (cdr L) N (+ S 1)))
		(T (cons (car L) (eliminateNtoN (cdr L) N (+ S 1))))
	)
)
(defun mainA (L N)
	(eliminateNtoN L N 1)
)


;----------------------------------------------------------------------------------------------------------

;b) Write a function to test if a linear list of integer numbers has a "valley" aspect (a list has a valley
;aspect if the items decrease to a certain point and then increase. Eg. 10 8 6 17 19 20). A list must
;have at least 3 elements to fullfill this condition.

;valley(l1l2..ln, direction) = true,                n=1, dir=1 (increasing)
;                            = false,               n=1, dir=0 (decreasing)
;						     = false,               l1>l2, dir=1 (increasing) 
;						  	 = valley(l2..ln, 0),   l1>l2, dir=0 (decreasing)
;							 = valley(l2..ln, 1),   l1<l2 

; * 1-increasing 0-decreasing
; main_valley(l1l2..ln) = false,                  n<3 or l1<l2 
;						= valley(l1..ln, 0),      otherwise

(defun valley (L dir)
	(cond
		((and (= (length L) 1) (= dir 1)) T)
		((and (= (length L) 1) (= dir 0)) nil)
		((and (> (car L) (cadr L)) (= dir 1)) nil)
		((and (> (car L) (cadr L)) (= dir 0)) (valley (cdr L) 0))
		((< (car L) (cadr L)) (valley (cdr L) 1))
	)
)
(defun valley_main (L)
	(cond
		((or (< (length L) 3) (< (car L) (cadr L))) nil)
		(T (valley L 0))
	)
)


;--------------------------------------------------------------------------------------------------------------

;c) Build a function that returns the minimum numeric atom from a list, at any level.

;minList(l1l2..ln) = 999999                                  n=0
;				   = min (l1 , minList(l2..ln))              l1 is a number
;                  = min (minList(l1) , minList(l2..ln))     l1 is a list 
;				   = minList(l2..ln)                         otherwise

; --- works for numbers smaller than 999999 (which can be changed to a larger number or a maximum given number as parameter)
;     But a list with at least a numeric atom
(defun minList (L)
	(cond 
		((null L) 999999)
		((numberp (car L)) (min (car L) (minList (cdr L))))
		((listp (car L)) (min (minList (car L)) (minList (cdr L))))
		(T (minList (cdr L)))
	)
)


; --- version with collector variable works for any case (cMin is the current minimum)

;minListC(l1l2..ln, cMin) = cMin               									 n=0
;						  = minListC (l2..ln, l1)       						 cMin=null and l1-number
;						  = minListC (l2..ln, min(cMin , l1))      				 l1-number 
;						  = minListC (l2..ln, min(cMin , minListC(l1, cMin)))    l1-list
;						  = minListC (l2..ln, cMin)                              otherwise

(defun minListC (L cMin)
	(cond
		((null L) cMin)    ; when reaching the end of the list we return the minimum
		((and (null cMin) (numberp (car L))) (minListC (cdr L) (car L)))   ; when we find the first number atom we initialize the minimum
		((numberp (car L)) (minListC (cdr L) (min cMin (car L))))       ; min is the minimum between the new number and the current minimum
		((listp (car L)) (minListC (cdr L) (min cMin (minListC (car L) cMin))))   ; if l1 is a list we find the new minimum in that list based on the current minimum
		(T (minListC (cdr L) cMin))   ; otherwise we just go forward with the sublist
	)
)
; wrapper function initializing minimum with null
(defun minListM (L)
	(minListC L nil)
)	


;---------------------------------------------------------------------------------------------------------------

;d) Write a function that deletes from a linear list of all occurrences of the maximum element.


;maxElem(l1..ln) = l1,   					  n=1
;				 = max(l1, maxElem(l2..ln)),  otherwise
(defun maxElem (L)
	(cond
		((= (length L) 1) (car L))
		(T (max (car L) (maxElem (cdr L))))
	)
)

;deleteE(l1l2..ln, e) = [],   					  n=0
;					  = deleteE(l2..ln, e),  	  l1=e
;					  = l1 U deleteE(l2..ln, e),  otherwise
(defun deleteE (L e)
	(cond
		((null L) nil)
		((= e (car L)) (deleteE (cdr L) e))
		(T (cons (car L) (deleteE (cdr L) e)))
	)
)

; deleteMax(L) = deleteE(L, maxElem(L))
(defun deleteMax (L)
	(deleteE L (maxElem L))
)




(defun sumv (l1 l2) 
	(cond
		((null l1) nil)
		(T (cons (+ (car l1) (car l2)) (sumv (cdr l1) (cdr l2))))
	)
)














