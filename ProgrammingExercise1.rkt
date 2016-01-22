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