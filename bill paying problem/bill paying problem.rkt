;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |bill paying problem|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;; partitions : list -> list of lists
;; partition a list xs into n parts in all possible ways
(define (partitions xs n)
  (cond
    [(= 1 n) (list xs)]
    [else 
     (cons (list (first xs))
           (partitions (rest xs) (sub1 n)))]))

;(define (parts v n)
;  (local
;    {(define initv v)
;     (define len (vector-length v))
;     (define resultv
;       (make-vector n (lambda (a) (vector 0 0 0))))
;     (define (lp i)
;       (if (= i len)
;           (void)
;           (

;; partint : nat -> nat
;; partition an integer into n integers
;(define (partint i n)
;  (local
;    {(define result (box empty))
;     (define (firstlp i j n)
;       (if (= i 0)
;           0
;           (begin (set-box result 
;                           (cons (i (firstlp (sub1 i)
   


;; ! : nat -> nat
;; compute the factorial of a given nat
(define (! n)
  (foldl * 1 (build-list n (lambda (a) (add1 a)))))

;; choose : nat nat -> nat
;; compute the binomial coefficient for n objects
;; taken k at a time
(define (choose n k)
  (foldl 
   * 
   1 
   (build-list 
    k 
    (lambda (i) (/ (+ n 
                      (- (add1 i) k)) 
                   (add1 i))))))

(define (stirling n k)
  (* (/ 1 (! k))
     (foldl +
            0
            (build-list
             (add1 k)
             (lambda (j) (* (expt -1 j)
                            (choose k j)
                            (expt (- k j) n)))))))
;; a person is a numlist
;; where each number in the list represents
;; a bill that that person has

(define (listsum ns)
  (foldl + 0 ns))
(define (flatlist xs)
  (foldl append empty xs))
;; extremepos : (num num -> bool) numvect -> nat
;; return the index of the position where
;; (f i x) where i is the value at that index and
;; x is the value at any other index
;; is always true
(define (extremepos v f)
  (local
    {(define len (vector-length v))
     (define minsf (box 0))
     (define (lp i)
       (cond
         [(= i len) (unbox minsf)]
         [else
          (if (f (vector-ref v i) 
                 (vector-ref v (unbox minsf)))
              (begin (set-box! minsf i)
                     (lp (add1 i)))
              (lp (add1 i)))]))}
    (lp 0)))
;; list-pos : nat natlist -> nat
;; returns the index of the first position
;; that n appears in ns when it is known that
;; n is in ns
(define (list-pos n ns)
  (local
    {(define (lp ns i)
       (cond
         [(= n (first ns)) i]
         [else (lp (rest ns) (add1 i))]))}
    (lp ns 0)))
;(define (split c ppl)
;  (local
;    {(define total (flatlist ns))
;     (define totalval (listsum total))
;     ;; boxppl : list num -> void
;     ;; create a box with value 0 for each person in people
;     (define (boxppl ppl i)
;       (cond
;         [(empty? ppl) (void)]
;         [(else
;           (begin (define (var-append indexedbox i) (box empty))
;                  (boxppl (rest ppl)  
;                          (add1 i))))]))

(define (split c ppl)
  (local
    {(define total (flatlist ppl))
     (define billsbox (box total))
     (define (amtpaid) (listsum (unbox billsbox)))
     (define totalval (listsum total))
     (define numofppl (length ppl))
     (define cpp (ceiling (/ c numofppl)))
     (define denoms '(1 2 5 10 20 50 100))
     ;; val->ind : nat -> nat
     ;; returns the index value associated with a bill
     ;; with value n
     (define (val->ind n)
       (list-pos n denoms))
     ;; ind->val : nat -> nat
     ;; returns the value associated with a given index i
     (define (ind->val i)
       (vector-ref (vector 1 2 5 10 20 50 100) i))
     (define billsvect (make-vector 7 0))
     ;; countlp : numlist -> void
     ;; run through a list of bills, updating billsvect so that
     ;; it reflects the number of bills of each denomination
;     (define (countlp bs)
;       (cond
;         [(empty? bs) billsvect]
;         [else
;          (local 
;            {(define pos (val->ind (first bs)))}
;            (begin (vector-set! billsvect 
;                                pos
;                                (add1 (vector-ref billsvect pos)))
;                   (countlp (rest bs))))]))
;     (define no-value (countlp total))
     (define owedvect 
       (build-vector 
        numofppl 
        (lambda (a) (- (listsum (list-ref ppl a)) cpp))))
     ;; amtowed : num -> num
     ;; returns the difference between
     ;; the cost/person and how much the person has paid
     (define (amtowed n)
       (- n cpp))
     ;; gelt : nat -> nat
     ;; returns the greatest element of (unbox billsbox)
     ;; that is less than n
     (define (gelt n)
       (local {(define possibills 
                 (filter (lambda (a) (<= a n)) (unbox billsbox)))}
         (if (empty? possibills)
             empty
             (argmax identity
                     possibills))))
     (define possvect
       (build-vector
        numofppl
        (lambda (a) (box empty))))
     ;; sublp : nat list -> list
     ;; build a list of the bills person i should receive
     ;; and update billsbox to reflect the remaining
     ;; available bills
;     (define (sublp i acc)
;       (local 
;         {(define owedtoi (vector-ref owedvect i))}
;         (if (= 0 owedtoi)
;             acc
;             (local {(define givei (gelt owedtoi))}
;               (begin (vector-set! owedvect
;                                   i
;                                   (- owedtoi givei))
;                      (set-box! billsbox
;                                (remove givei 
;                                        (unbox billsbox)))
;                      (sublp i (cons givei acc)))))))
     (define (sublp i)
       (local 
         {(define owedtoi (vector-ref owedvect i))}
         (if (= 0 owedtoi)
             (void)
             (local {(define givei (gelt owedtoi))
                     (define possbox (vector-ref possvect i))}
               (if (empty? givei)
                   (void)
                   (begin (vector-set! owedvect
                                       i
                                       (- owedtoi givei))
                          (set-box! billsbox
                                    (remove givei
                                            (unbox billsbox)))
                          (set-box! possbox
                                    (cons givei
                                          (unbox possbox)))
                          (sublp i)))))))
     ;; lp : nat list -> void
     ;; run sublp on all the people in ppl
;     (define (lp j acc)
;      (cond
;        [(= j numofppl) acc]
;        [else
;         (lp (add1 j)
;             (cons (sublp j empty) acc))]))
     (define (lp j)
       (cond
         [(= j numofppl) (void)]
         [else (begin (sublp j)
                      (lp (add1 j)))]))
     (define (resolve)
       (cond
         [(= c (amtpaid)) (void)]
         [else
          (local {(define giveback 
                    (argmin identity (unbox billsbox)))
                  (define worstoff
                    (extremepos owedvect >))
                  (define bestoff
                    (extremepos owedvect <))
                  (define worstbox
                    (vector-ref possvect worstoff))
                  (define bestbox
                    (vector-ref possvect bestoff))
                  (define putin
                    (argmin identity (unbox bestbox)))}
            (begin (set-box! billsbox 
                             (cons putin 
                                   (remove giveback
                                           (unbox billsbox))))
                   (set-box! worstbox
                             (cons giveback (unbox worstbox)))
                   (set-box! bestbox
                             (remove putin (unbox bestbox)))
                   (vector-set! owedvect
                                worstoff
                                (- (vector-ref owedvect worstoff)
                                   giveback))
                   (vector-set! owedvect
                                bestoff
                                (+ (vector-ref owedvect bestoff)
                                   putin))
                   (resolve)))]))
     (define (testvect)
       (list (unbox billsbox)
       (build-vector numofppl 
                     (lambda (a) (list (vector-ref owedvect a)
                                       (unbox (vector-ref possvect a)))))))}         
    (begin (lp 0)
           (testvect))))

(define group1 (list (list 1 1 10) (list 5 1 1 20) (list 1 5 20)))
;; I should write a function called resolve that is called when (gelt owedtoi) does not exist
;; eg in the case of 27, group 1, at the last iteration of sublp (ie the iteration on i=2)
;; have person2 take a 20 then take the difference as close to equally as possible from
;; persons 0 and 1 (ie a dollar from each) 
;; 
;; putin should probably be optimized
;(split 27 group1)
;(list 1 5 20 5 1 1 20 1 1 10)
;> (list 5 20 5 20 1 1 10)
;(list 5 20 5 20 1 1 10)
;> (list 5 20 20)
;(list 5 20 20)
;> (list 1 5 1 1 1 1 10)
;(list 1 5 1 1 1 1 10)

           