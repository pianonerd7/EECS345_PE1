;Anna He
;EECS 345 Programming Exercise 1

;1. inorder (t)
(define inorder?
  (lambda (l)
    (cond
      ((null? l)#t)
      ((null? (flatten l))#t)
      ((null?(cdr l))#t)
      ((>(car l)(car(cdr l)))#f)
      (else (inorder? (cdr l))))))

;2. dotproduct (t)
(define dotproduct
  (lambda (l1 l2)
    (cond
      ((null? l1) 0)
      ((null? l2) 0)
      (else(+(*(car l1)(car l2))(dotproduct(cdr l1)(cdr l2)))))))

;3. squareroot
(define squareroot?
  (lambda (val iter)
    (cond
      ((zero? iter) val)
      ((zero? val) val)
      (else (squareroot (- val (/ (-(* val val) 5) (* 2 val))) (- iter 1))))))

(define squareroot
  (lambda (val iter)
    (cond
      ((zero? val) val)
      ((eq? iter (squareroot v
      (else (- (squareroot val (+ 1 iter)) (/(-(* (squareroot val (+ 1 iter))(squareroot val (+ 1 iter))) val)(* 2 (squareroot val (+ 1 iter))))))))))))

;4. removesubsequence (t)
(define removesubsequence
  (lambda (l1 l2)
    (cond
      ((null? l2)'())
      ((null? l1)l2)
      ((eq? (car l1)(car l2))(removesubsequence(cdr l1)(cdr l2)))
      (else (cons (car l2)(removesubsequence l1 (cdr l2)))))))

;5. reverse* (t)
(define reverse*
  (lambda (l)
    (cond
      ((null? l) '())
      ((and (list? (car l))(null? (cdr l)))(cons (reverse* (car l)) '()))
      ((and (list? (car l))(list? (cdr l)))(myappend (reverse* (cdr l))(cons (reverse* (car l)) '())))
      ((list? (car l))(cons (reverse* (cdr l))(reverse* (car l))))
      (else (myappend (reverse* (cdr l)) (cons (car l) '()))))))

;myappend
(define myappend
  (lambda (l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (myappend (cdr l1) l2)))))

;6. first* (t)
(define first*
  (lambda (l)
    (cond
      ((null? l) '())
      ((list? (car l)) (first* (car l)))
      (else (car l)))))

;7. last* (t)
(define last*
  (lambda (l)
    (cond
      ((null? l) '())
      ((and (list? (car l)) (pair? (cdr l))) (last* (cdr l)))
      ((and (list? (car l)) (null? (cdr l))) (last* (car l)))
      ((null? (cdr l)) (car l))
      (else (last* (cdr l))))))

;8. numorder*?
;list is null
;car is list, car car is list, cdr is null --> numorder* car list
;car is list, car car is list --> numorder* car l, numorder* cdr l
;car is list 
;else
;can check if each nested list is in order by invoking inorder?
;then summing and flattening and invoking inorder again

(define numorder*?
  (lambda (l)
    (cond
      ((null? l)#t)
      ((null? (flatten l))#t)
      ((and (and (list? (car l))(list? (car (car l))))(and(null? (cdr l))(numorder*? (car l)))) (inorder? (flatten (cons (getsum (car l)) '()))))
      ((and (and (list? (car l))(list? (car (car l))))(and(numorder*? (car l))(numorder*? (cdr l))))(inorder? (flatten (cons (getsum (car l))(numorder*? (cdr l)))))
      ((list? (car l))(inorder? (car l)))
      (else (inorder? (cons (car l)(numorder*? (cdr l)))))))))

(define numorder*
  (lambda (l)
    (cond
      ((null? l) #t)
      ((and(list? (car l))(list?(car (cdr l))))(and(and (inorder? car l)(inorder? (car (cdr l))))(or(<(getsum (car l))(getsum (car (cdr l))))(=(getsum (car l))(getsum (car (cdr l)))))))
      ((and(list? (car l))(null? (car (cdr l))))))))

(define numorder
  (lambda (l)
    (cond
      ((null? l)#t)
      ((null? (flatten l))#t)
      ((and(number? (car l))(null? (cdr l)))#t)
      ((and(list? (car l))(null? (cdr l)))(numorder (car l)))
      ((and(list? (car l))(number? (car (cdr l))))(and (or(<(getsum (flatten(car l)))(car (cdr l)))(=(getsum (flatten(car l)))(car (cdr l))))(and (numorder (car l))(numorder(cdr l)))))
      ((and(list? (car l))(list? (car (cdr l))))(and (or(<(getsum (flatten (car l)))(getsum (flatten(car (cdr l)))))(=(getsum (flatten(car l)))(getsum (flatten(car (cdr l))))))(and (numorder (car l))(numorder(cdr l)))))
      ((and (number? (car l))(number? (car (cdr l))))(and (or(<(car l)(car (cdr l)))(=(car l)(car (cdr l))))(numorder (cdr l))))
      ((and (number? (car l))(list? (car (cdr l))))(and (or(<(car l)(getsum (flatten (car (cdr l)))))(=(car l)(getsum (flatten (car(cdr l))))))(numorder(cdr l))))
      (else #f))))
      

(define getsum
  (lambda (l)
    (cond
      ((null? l)0)
      ((null? (flatten l))0)
      ((null? (cdr l)) (car l))
      (else (+ (car l) (getsum (cdr l)))))))

(define flatten
  (lambda (l)
    (cond
      ((null? l) '())
      ((list? (car l)) (myappend (flatten (car l)) (flatten(cdr l))))
      (else (cons (car l) (flatten (cdr l)))))))
        
;9. vectormult (t)
(define vectormult
  (lambda (vector matrix)
    (cond
      ((null? matrix) '())
      ((and(list? (car matrix))(list? (cdr matrix)))(mergelists (tolists (car vector) (car matrix)) (vectormult (cdr vector) (cdr matrix))))
      (else -1))))

;(tolists '(1 2 3) '(1 2 3)) -> (1 4 9)
(define tolists
  (lambda (vector l)
    (cond
      ((null? l) '())
      (else (myappend (cons (* vector (car l)) '())(tolists vector (cdr l)))))))

(define mergelists
  (lambda (list1 list2)
    (cond
      ((null? list1) list2)
      ((null? list2) list1)
      (else (cons (+(car list1)(car list2)) (mergelists (cdr list1) (cdr list2)))))))
  
;10 matrixmultiply (t)
(define matrixmultiply
  (lambda (list1 list2)
    (cond
      ((null? list1) '())
      ((null? (cdr list1)) (cons (vectormult (car list1) list2) '()))
      (else (cons (vectormult (car list1) list2) (matrixmultiply (cdr list1) list2))))))