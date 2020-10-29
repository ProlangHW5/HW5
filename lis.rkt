(define (with_first_val seq largest rest)
  (helper (cons  (car rest) seq) (car rest) (cdr rest)))

(define (without_first_val seq largest rest)
  (helper seq largest (cdr rest)))

(define (helper seq largest rest)
  (cond
    ((null? rest) seq)
    ((and (null? seq) (or (null? rest) (null? (cdr rest))) ) rest)
    ((or (null? seq) (> (car rest) largest))
     (if (> (length (with_first_val seq largest rest)) (length (without_first_val seq largest rest)) )
         (with_first_val seq largest rest)
         (without_first_val seq largest rest))
    
  )
  
  ((or (null? seq) (equal? (car rest) largest))
     (if (> (length (with_first_val seq largest rest)) (length (without_first_val seq largest rest)) )
         (with_first_val seq largest rest)
         (without_first_val seq largest rest))
    
  )
    (else (helper seq largest (cdr rest)))
 ))

(define (lis_slow lst)
  (define seq '())
  (define largest '())
  (reverse (helper seq largest lst)))

(define list1 '(1 2 3 2 4 1 2))
(define list2 '(2 4 3 1 2 1))
(define list3 '(4 3 4 4 4 2 3 3 3 3 3 3 3 3))
