;Anna He
;EECS 345 Programming Exercise 1

;1. inorder (t)
(define inorder?
  (lambda (l)
    (cond
      ((null? l)#t)
      ((null?(cdr l))#t)
      ((>(car l)(car(cdr l)))#f)
      (else (inorder (cdr l))))))

;2. dotproduct (t)
(define dotproduct
  (lambda (l1 l2)
    (cond
      ((null? l1) 0)
      ((null? l2) 0)
      (else(+(*(car l1)(car l2))(dotproduct(cdr l1)(cdr l2)))))))

;3. squareroot
(define squareroot
  (lambda (val iter)
    (cond
      ((zero? iter) val)
      ((zero? val) val)
      (else (squareroot (- val (/ (-(* val val) 5) (* 2 val))) (- iter 1))))))

;4. removesubsequence (t)
(define removesubsequence
  (lambda (l1 l2)
    (cond
      ((null? l2)'())
      ((null? l1)l2)
      ((eq? (car l1)(car l2))(removesubsequence(cdr l1)(cdr l2)))
      (else (cons (car l2)(removesubsequence l1 (cdr l2)))))))

;5. reverse*
(define reverse*
  (lambda (l)
    (cond
      ((null? l) '())
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
(define numorder*?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((and(list? (car l))(null? (cdr l))) #t)
      ((and(list? (car l))(> (car l)(numorder* (cdr l)))) #f)
      ((and (pair? (car l))(pair? (cdr (car l)))) (* (car l)(numorder*? (cdr (car l)))))
      ((> (car l)(numorder*? (cdr l))) #f)
      (else #f))))

;9. vectormult
(define vectormult
  (lambda (vector matrix)
    (cond
      ((null? vector) cons 0 '())
      ((list? (car matrix))))))
