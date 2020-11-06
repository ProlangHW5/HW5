;;Contract: seq largest rest -> (helper (car rest+ seq) (car rest) (cdr rest))
;;Purpose: call helper adding the next largest to sequence and moving on to the
;;next values and re assigning the largest val
;;seq, current longest sequence 
;;largest, the largest value in the seq
;;rest, rest of the values left
;;Example: (with_first_val (1 2) 2 (3 4))-> (helper (1 2 3) 3 (4))
;;Definition:
(define (with_first_val seq largest rest)
  (helper (cons  (car rest) seq) (car rest) (cdr rest)))

;;Contract: seq largest rest-> (helper seq largest (cdr rest))
;;Purpose:move on to the next value
;;seq, current longest sequence 
;;largest, the largest value in the seq
;;rest, rest of the values left
;;Example: (without_first_val (1 2) 2 (1 1))-> (helper (1 2) 2 (1))
;;Definition:
(define (without_first_val seq largest rest)
  (helper seq largest (cdr rest)))

;;Contract: seq largest rest-> longest non-decreasing subsequence-> seq
;;Purpose: return the longest non-decreasing subsequence
;;seq, current largest non-decreasing subsequence
;;largest, the largest value in the seq
;;rest, rest of the values left
;;Example: (helper () () (1 2 3 4 2))-> (1 2 3 4)
;;Definition:
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

;;Contract: lst-> return longest non-decreasing subsequence
;;Purpose: call helper which returns the longest non-decreasing subsequence
;;lst, the input list
;;Example (lis_slow (2 1 2 3 1))-> (1 2 3)
;;Definition: 
(define (lis_slow lst)
  (define seq '())
  (define largest '())
  (reverse (helper seq largest lst)))

(define list1 '(1 2 3 2 4 1 2))
(define list2 '(2 4 3 1 2 1))
(define list3 '(4 3 4 4 4 2 3 3 3 3 3 3 3 3))
(define list4 '(1 2 3 2))