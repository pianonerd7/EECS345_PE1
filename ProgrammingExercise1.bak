;Anna He
;EECS 345 Programming Exercise 1

;inorder
(define inorder
  (lambda (l)
    (cond
      ((null? l)#t)
      ((null?(cdr l))#t)
      ((>(car l)(car(cdr l)))#f)
      (else (inorder (cdr l))))))