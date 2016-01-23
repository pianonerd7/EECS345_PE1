;Anna He
;EECS 345 Programming Exercise 1

;inorder
(define inorder?
  (lambda (l)
    (cond
      ((null? l)#t)
      ((null?(cdr l))#t)
      ((>(car l)(car(cdr l)))#f)
      (else (inorder (cdr l))))))

;dotproduct
(define dotproduct
  (lambda (l1 l2)
    (cond
      ((null? l1) 0)
      ((null? l2) 0)
      (else(+(*(car l1)(car l2))(dotproduct(cdr l1)(cdr l2)))))))

;squareroot
;(define squareroot
;  (lambda (value iter)
;    (cond

;removesubsequence
(define removesubsequence
  (lambda (l1 l2)
    (cond
      ((null? l2)'())
      ((null? l1)l2)
      ((eq? (car l1)(car l2))(removesubsequence(cdr l1)(cdr l2)))
      (else (cons (car l2)(removesubsequence l1 (cdr l2)))))))

;reverse*
(define reverse*
  (lambda (l)
    (cond
      ((null? l) '())
      ((list? (car l))(cons (reverse* (car l))(reverse* (cdr l))))
      (else (cons (reverse* (cdr 1)) (car l))))))