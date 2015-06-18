;Jacob Besse
;LISP Program

;TO RUN: In gcl compiler > (load "lisp_besse.lisp")
;			 > (runTree)

;It will then prompt you for the size of the tree (integers >= 2)
;Then will ask for file name for test (include quotes, e.g. "filename")
;Will return tree of given size 





;This function adds a number to the node. It is only called if the node is not full.
; grader: please restrict lines to 80 characters.  -0
;L is the list, C is the number to be added and M is an accumulator list.
(defun fitIn (L C M)
	(cond
		;If the list is empty return the accumulator. 
		((null (car L)) (append M (cons C nil)))

		;If the the value given is less than the next value of the list.
		;Return the Accumlated List + the number + the rest of the list.
		((<= C (car L)) (append M (cons C nil) L))
		;Else, recurse of the list moving to the next value.
		;Add previous value to accumulator.
		(T (fitIn (cdr L) C (append M (cons (car L) nil))))
	)
)

;Expand works with insert.  This function adds the nils to the node when its size is reached.
;If Num == 1, it adds the nil in between each number of the node.
;If Num == 2, it recursively calls its self.  This allows the function to dig down the tree 
;until it finds a node it can insert the number into.  Then calls the insert function.
(defun expand (L C M Num Size)
	(cond 
		;Adding nils to filled node.
		((= Num 1)
			(cond
				;If the last value of list add nil to end of list.
				((null(car L)) (expand (append M (cons nil nil)) C nil 2 Size))
				;Else add nil before the value of list and recurse to next value of list.
				(T (expand (cdr L) C (append M (cons nil( cons (car L) nil))) 1 Size))
			)
		)
		;Recursively finds node to insert on.
		((= Num 2)

			(cond
				;Number given was larger than every value in node. Insert at last child.
				((= (List-length L) 1) (nconc M (cons (insert (car L) C Size) nil )))
				;If number is smaller than value in node, recursively insert into that child.
				((<= C (cadr L)) (nconc M (cons (insert (car L) C Size) nil) (cdr L) ))
				;Else check the next value of the node.
				(T (expand (cdr (cdr L)) C (append M (cons(car L) (cons (cadr L) nil) )) 2 Size))
			)
		)
	)
)

	
;This function calls fitIn which inserts the number into the proper location in the node.
;Or if the node is full calls expand with finds the child of the node to place the number into.
(defun insert (L A Size)
	(cond	
		;If the node is not full, add the number to proper location of node
		((< (list-length L) Size)
			(fitIn L A nil))

		;Node is full
		(T 

			(cond
				;If nils have not been added to node, add them.
				((= (list-length L) Size)
					(expand L A nil 1 Size))
			
			;Else find a nonfull node to add number to.
			(T(expand L A nil 2 Size)))
		)
	)

) 

;This is the constructor.  It takes in 2 list and the size of the tree.
;L1 is the tree that is being grown.
;L2 is the list that was read in.
;Size is the size of the tree wanted.
(defun createTree (L1 L2 Size)
	(cond
		;If the read in list is empty the tree is complete, return the list.
		((null (car L2))  L1)

		;Else recurse on the next value on the list.
		(T (createTree (insert L1 (car L2) Size) (cdr L2) Size))

))

;This is the function to call to run the program.
;Ask the user for the size of the tree wanted.  Number greated than 2.
(defun runTree ()
	(format t "~%~%Size of Tree: ")

	;Read in size and call openFile
	(openFile(READ))
)


;Opens the file.  The file needs to be in " " to open.
(defun openFile (Size)
	(format t "~%~%Input a File with quotes: ")

	; Call the constructor
	(createTree nil (READ(OPEN(READ)))  (- Size 1))
)

