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




;; Contract: lis_fast : (list int) -> (list int)
;; Purpose: returns the longest non-decreasing subsequence in polynomial time
;; Example: (define list1 '(1 2 3 2 4 1 2)) should produce (1 2 3 4)
;; Definition:
(define (lis_fast lst)
  (define count 1)
  (if (not(null? lst))(reverse(helper_fst (cons (cons (car lst) '()) '()) (cdr lst) lst count))
      lst
      )
)

;; Contract: helper_fst : ((list (list int))(list int)(list int) int) -> (list int)
;; Purpose: helper function to return the longest non-decreasing subsequence in polynomial time
;; Example: (helper_fst ((1)) (2 3 2 4 1 2) (1 2 3 2 4 1 2) 1) should produce (4 3 2 1)
;; Definition:
(define (helper_fst lstlst lst full_lst count)
  (define size (length full_lst))
  (define count2 0)
  (define currlst '())
  (if (null? lst) (longest lstlst size 0 0 '()) 
      (helper_fst2 (car lst) (cdr lst) lstlst currlst full_lst count count2)
  )
)

;; Contract: helper_fst2 : (int (list int)(list (list int))(list int)(list int) int int) -> (list int)
;; Purpose: helper function to return the longest non-decreasing subsequence in polynomial time
;;          if count2 = count, then appends the current non-decreasing subsequence to the list of list of subsequences
;;          if the int at count in the full list is greater than or equal to the int at count2 in the full list and the size of the current subsequence is greater than +1 of the subsequence at count2 in the list of list of subsequences
;;             then add that int to the current subsequence
;; Example: (helper_fst ((1)) (2 3) ((1)) () (1 2 3) 1 0) should produce ((1) (1 2) (1 2 3))
;; Definition:
(define (helper_fst2 head tail lstlst2 currlst full_lst count count2)
  (define size_curr ( + (length currlst) 1))  
     (if(equal? count2 count) (helper_fst (append lstlst2 (cons(cons head currlst) '())) tail full_lst (+ count 1))
        (if(equal? #t (and(> (list-ref full_lst count) (list-ref full_lst count2)) (> (+ (length (list-ref lstlst2 count2)) 1) size_curr))) (helper_fst2 head tail lstlst2 (cons (list-ref full_lst count2) currlst) full_lst count (+ count2 1))
           (if(equal? #t (and(equal? (list-ref full_lst count) (list-ref full_lst count2)) (> (+ (length (list-ref lstlst2 count2)) 1) size_curr))) (helper_fst2 head tail lstlst2 (cons (list-ref full_lst count2) currlst) full_lst count (+ count2 1))
              (helper_fst2 head tail lstlst2 currlst full_lst count (+ count2 1))
            )
        )
     )
  )

;; Contract: longest : ((list (list int)) int 0 0 ()) -> (list int)
;; Purpose: returns the longest non-decreasing subsequence from a list of  non-decreasing subsequences
;; Example: (longest ((2 2 2) (4 4 4 4)) 7 0 0 0)) should produce (4 4 4 4)
;; Definition:
(define (longest lstlst3 size max count3 max_k)
  (if (equal? count3 size) max_k
      (if (> (length (list-ref lstlst3 count3)) max) (longest lstlst3 size (length (list-ref lstlst3 count3)) (+ count3 1) (list-ref lstlst3 count3))
          (longest lstlst3 size max (+ count3 1) max_k)
      )
  )
)


(define list1 '(1 2 3 2 4 1 2))
(define list2 '(2 4 3 1 2 1))
(define list3 '(4 3 4 4 4 2 3 3 3 3 3 3 3 3))
(define list4 '(1 2 3 2))
