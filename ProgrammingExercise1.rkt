;Anna He
;EECS 345 Programming Exercise 1

;1. inorder? - Takes a list of numbers and returns #t if the numbers are in non-decreasing order, #f otherwise. 
(define inorder?
  (lambda (l)
    (cond
      ((null? l)#t)
      ((null? (flatten l))#t)
      ((null?(cdr l))#t)
      ((>(car l)(car(cdr l)))#f)
      (else (inorder? (cdr l))))))

;2. dotproduct - Takes two lists of numbers and computes their dot product. If one list is longer, the extra numbers are ignored.
(define dotproduct
  (lambda (l1 l2)
    (cond
      ((null? l1) 0)
      ((null? l2) 0)
      (else(+(*(car l1)(car l2))(dotproduct(cdr l1)(cdr l2)))))))

;3. squareroot - Takes val (the number to perform sqrt on) and iter, the number of iterations to compute newton's method.
;Newton's method -> new = old - ((old * old)-value)/(2*old)
(define squareroot
  (lambda (val iter)
    (cond
      ((zero? iter) val)
      ((zero? val) val)
      (else (- (squareroot val (- iter 1)) (/(-(* (squareroot val (- iter 1))(squareroot val (- iter 1))) val)(* 2 (squareroot val (- iter 1)))))))))

;4. removesubsequence - Takes 2 lists of atoms. Removes the first occurences of l1 from l2, the resulting list is returned.
(define removesubsequence
  (lambda (l1 l2)
    (cond
      ((null? l2)'())
      ((null? l1)l2)
      ((eq? (car l1)(car l2))(removesubsequence(cdr l1)(cdr l2)))
      (else (cons (car l2)(removesubsequence l1 (cdr l2)))))))

;5. reverse* - Takes a nested list and reverses the contents of the list and all nested lists.
(define reverse*
  (lambda (l)
    (cond
      ((null? l) '())
      ((and (list? (car l))(null? (cdr l)))(cons (reverse* (car l)) '()))
      ((and (list? (car l))(list? (cdr l)))(myappend (reverse* (cdr l))(cons (reverse* (car l)) '())))
      ((list? (car l))(cons (reverse* (cdr l))(reverse* (car l))))
      (else (myappend (reverse* (cdr l)) (cons (car l) '()))))))

;myappend - Takes 2 lists and appends the first list to the second list resulting in one list.
;(myappend '(1 2 3) '(1 2 3 4) -> (1 2 3 1 2 3 4)
(define myappend
  (lambda (l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (myappend (cdr l1) l2)))))

;6. first* - Takes a list of lists and returns the first atom that appears in the list, regardless of how nested it is.
(define first*
  (lambda (l)
    (cond
      ((null? l) '())
      ((list? (car l)) (first* (car l)))
      (else (car l)))))

;7. last* - Takes a list of lists and returns the last atom that appears in the list, regardless of how nested it is.
(define last*
  (lambda (l)
    (cond
      ((null? l) '())
      ((and (list? (car l)) (pair? (cdr l))) (last* (cdr l)))
      ((and (list? (car l)) (null? (cdr l))) (last* (car l)))
      ((null? (cdr l)) (car l))
      (else (last* (cdr l))))))

;8. numorder*? - Takes a possibly nested list of numbers, and returns #t if the values of the entries in the list are in non-decreasing
;order. The value of a number is the number. The value of a list is the sum of hte values in that list.
(define numorder*?
  (lambda (l)
    (cond
      ((null? l)#t)
      ((null? (flatten l))#t)
      ((and(number? (car l))(null? (cdr l)))#t)
      ((and(list? (car l))(null? (cdr l)))(numorder*? (car l)))
      ((and(list? (car l))(number? (car (cdr l))))(and (or(<(getsum (flatten(car l)))(car (cdr l)))(=(getsum (flatten(car l)))(car (cdr l))))(and (numorder*?(car l))(numorder*?(cdr l)))))
      ((and(list? (car l))(list? (car (cdr l))))(and (or(<(getsum (flatten (car l)))(getsum (flatten(car (cdr l)))))(=(getsum (flatten(car l)))(getsum (flatten(car (cdr l))))))(and (numorder*?(car l))(numorder*?(cdr l)))))
      ((and (number? (car l))(number? (car (cdr l))))(and (or(<(car l)(car (cdr l)))(=(car l)(car (cdr l))))(numorder*?(cdr l))))
      ((and (number? (car l))(list? (car (cdr l))))(and (or(<(car l)(getsum (flatten (car (cdr l)))))(=(car l)(getsum (flatten (car(cdr l))))))(numorder*?(cdr l))))
      (else #f))))
      
;getsum - Takes a list and computes the sum of all the elements in the list.
;(getsum '(1 2 3)) -> 6)
(define getsum
  (lambda (l)
    (cond
      ((null? l)0)
      ((null? (flatten l))0)
      ((null? (cdr l)) (car l))
      (else (+ (car l) (getsum (cdr l)))))))

;flatten - Takes a possibly nested list of elements and returns one list that contains all elements.
;(flatten '((((1)(((2 3)4 5) 6)(7))8)9))) -> (1 2 3 4 5 6 7 8 9)
(define flatten
  (lambda (l)
    (cond
      ((null? l) '())
      ((list? (car l)) (myappend (flatten (car l)) (flatten(cdr l))))
      (else (cons (car l) (flatten (cdr l)))))))
        
;9. vectormult - Takes a row vector(list of numbers) and matrix(list of lists of numbers) and multiplies the vector times the matrix.
;The result is a vector where the ith element of the result is the dotproduct of the input vector and the ith column of the matrix.
;It is assumed that the length of the vector matches the number of rows of the matrix.
(define vectormult
  (lambda (vector matrix)
    (cond
      ((null? matrix) '())
      (else (and(list? (car matrix))(list? (cdr matrix)))(mergelists (tolists (car vector) (car matrix)) (vectormult (cdr vector) (cdr matrix)))))))

;toLists - Takes a vector and a list, multiplies each element in the list by the vector, and the resulting list is returned.
;(tolists '4 '(1 2 3)) -> (4 8 12)
(define tolists
  (lambda (vector l)
    (cond
      ((null? l) '())
      (else (myappend (cons (* vector (car l)) '())(tolists vector (cdr l)))))))

;mergelists - Takes two lists and adds the ith element of list1 with the ith element of list2, and returns the result as a list.
;(mergelists '(1 2 3) '(2 3 4)) -> (3 5 7)
(define mergelists
  (lambda (list1 list2)
    (cond
      ((null? list1) list2)
      ((null? list2) list1)
      (else (cons (+(car list1)(car list2)) (mergelists (cdr list1) (cdr list2)))))))
  
;10 matrixmultiply - Takes two matrices (a list of lists of numbers) and multiplies them. It is assumed that the number of columns 
;of the first matrix is equal to the number of rows of the second matrix.
(define matrixmultiply
  (lambda (list1 list2)
    (cond
      ((null? list1) '())
      ((null? (cdr list1)) (cons (vectormult (car list1) list2) '()))
      (else (cons (vectormult (car list1) list2) (matrixmultiply (cdr list1) list2))))))