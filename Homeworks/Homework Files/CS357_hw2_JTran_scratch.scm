;; CS 357L - Homework 2
;;
;; John Tran
;; UNM ID: 101821704
;;
;; 2/18/2022

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define a function deepen-n that takes two parameters, ls and n. This function should wrap n pairs of parens around each top level element in ls

;; For example:
;;– (deepen-n ’() 10) => ()
;;– (deepen-n ’(1 2 3 4 5) 0) => (1 2 3 4 5)
;;– (deepen-n ’(1 2 3 4 5) 1) => ((1) (2) (3) (4) (5))
;;– (deepen-n ’(1 2 3 4 5) 2) => (((1)) ((2)) ((3)) ((4)) ((5)))
;;– (deepen-n ’(1 2 3 4 5) 3) => ((((1))) (((2))) (((3))) (((4))) (((5))))
;;– (deepen-n ’((1 2) (3 4) ((5) 6)) 2) => ((((1 2))) (((3 4))) ((((5) 6))))

;; original attempt - worked, just needed the extra lambda below to give it the value of n
;(define deepen-element
;  (lambda (element n)
;    (cond
;      ((zero? n)
;       element)
;      (else
;       (deepen-element (cons element '()) (- n 1))))))

;; trying nested lambdas to try and feed a function with only 1 arg into map
;(define deepen-element
;  (lambda (n)
;    (lambda (element n)
;      (cond
;        ((zero? n)
;         element)
;        (else
;         (deepen-element (cons element '()) (- n 1)))))))

;; tried with just the first element of each sublist instead...just for fun
;; actually, made a roundabout way to just get the car of the first element n times, which is the same of getting the car once...
;(define deepen-element
;  (lambda (ls n)
;    (cond
;      ((zero? n)
;       (car ls))
;      (else
;       (deepen-element (cons (car ls) '()) (- n 1))))))

;; now this works in only enclosing the first element of each sublist with the specified n pairs of parens -- sort of roundabout since only the first intial case is special (enclose first element in parens, and just add parens afterwards)
;(define deepen-element
;  (lambda (ls n)
;    (cond
;      ((zero? n)
;       (car ls))
;      (else
;       (deepen-element (cons (cons (car ls) '()) '()) (- n 1))))))

;; move null check to first-element to also enclose empty list too...
;(define atom?
;  (lambda (x)
;    (and (not (pair? x))
;         (not (null? x)))))

;(define deepen-element-helper
;  (lambda (ls n first-element-flag)
;    (cond
;      ((zero? n)
;       ls)
;      (first-element-flag
;       (if (or (atom? ls)
;               (null? '()))
;           (deepen-element-helper (cons ls '()) (- n 1) #f)
;           (deepen-element-helper (cons (car ls) '()) (- n 1) #f)))
;      (else
;       (deepen-element-helper (cons ls '()) (- n 1) first-element-flag)))))
;
;(define deepen-element
;  (lambda (ls n)
;    (deepen-element-helper ls n #t)))

(define deepen-n
  (lambda (ls n)
    (cond
      ((null? ls)
       '())
      (else
       (map (lambda (element)
              (deepen-element element n)) ls)))))

;(deepen-n '() 10)
;(deepen-n '(1 2 3 4 5) 0)
;(deepen-n '(1 2 3 4 5) 1)
;(deepen-n '(1 2 3 4 5) 2)
;(deepen-n '(1 2 3 4 5) 3)
;(deepen-n '((1 2) (3 4) ((5) 6)) 4)

(deepen-n '((1 2) (3 4) ((5) 6) () 8 9) 4)
(cons '() '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define a function insert-left-all that takes three parameters, new old ls. This function should insert new to the left of every occurrence of old in ls. This function should recurse into sublists to find all occurrences of old.

;;For example:
;;– (insert-left-all ‘z ’a’()) => ()
;;– (insert-left-all ‘z ’a’(a ((b a) ((a (c)))))) => (z a ((b z a) ((z a (c)))))
;;– (insert-left-all ‘z ’a’(((a)))) => (((z a)))

(define insert-left-all
  (lambda (new old ls element newls)
    (cond
      ((null? ls)
       '())
      (cond
        ((null? element)
         (insert-left-all new old (cdr ls) element newls))
        ((pair? (car element))
         (insert-left-all new old ls (car element) newls))
        (else
         ))
      ((eq? (cons ls) old)
       (insert-left-all new old (cdr ls) )))))

;(define insert-left-element
;  (lambda (new old original-element current-element newelement)
;    (cond
;      ((null? element)
;       newelement)
;      ((pair? (car element))
;       (insert-left-element new old original-element (car current-element) new-element))
;      (else
;       (if (eq? (car current-element) old)
;           (insert-left-element new old original-element (cdr current-element) (cons )))))))

(define insert-left-element
  (lambda (new old original-element current-element newelement)
    (cond
      ((null? element)
       newelement)
      ((pair? (car element))
       (insert-left-element new old original-element (car current-element) new-element))
      (else
       (if (eq? (car current-element) old)
           (insert-left-element new old original-element (cdr current-element) (cons )))))))



;; since we need to recurse into the sublists, we could use make-deep
;; NOT: modified so we can do something else if ls is not a pair -- check for our old case
(define make-deep
  (lambda (proc)
    (letrec
        ((go
          (lambda (ls)
            (if (pair? ls)
                (proc (map go ls))
                ls))))
      go)))

(define flat-recur
  (lambda (seed proc)
    (letrec
        ((go
          (lambda (ls)
            (if (null? ls)
                seed
                (proc (car ls)
                      (go (cdr ls)))))))
      go)))

(define insert-left-level
  (lambda (new old)
    (flat-recur '() (lambda (x ys)
                    (if (eq? x old)
                        (append (list new old)
                                ys)
                        (cons x ys))))))

;; check for singleton...
;(define insert-left-all
; (lambda (new old ls)
;   ((make-deep (lambda (ls)
;                ls))
;    ls)))

(define insert-left-all
  (lambda (new old ls)
    ((make-deep (insert-left-level new old))
     ls)))

(insert-left-all 'z 'a '())
(insert-left-all 'z 'a '(a ((b a) ((a (c))))))
(insert-left-all 'z 'a '(((a))))



;(if (pair? x)
;                     (make-deep x)
;                     x))


;((make-deep (lambda (x)
;              (cond
;                ((null? x)
;                 '())
;                ((pair? x)
;                 (cons x 1))
;                ((eq? x old)
;                 (cons new old))
;                (else
;                 (cons x 2)))))
;    ls)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define two functions mk-asc-list-of-int and mk-desc-list-of-ints iteratively (tail recursion). These function should take a single argument n. They should produce either an ascending list from 1 to n or a descending list from n to 1.

;; Hint: Use a helper function.

;;For example:
;;– (mk-asc-list-of-ints 0) => ()
;;– (mk-asc-list-of-ints 1) => (1)
;;– (mk-asc-list-of-ints 5) => (1 2 3 4 5)
;;– (mk-desc-list-of-ints 0) => ()
;;– (mk-desc-list-of-ints 1) => (1)
;;– (mk-desc-list-of-ints 5) => (5 4 3 2 1)

;; Assuming n cannot be a negative number...
;; The functions written below were based off the iota procedure shown in lecture

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mk-asc-list-of-ints
  (lambda (n)
    (letrec
        ((go
          (lambda (n acc)
            (if (= n 0)
                acc
                (go (- n 1)
                    (cons n acc))))))
      (go n '()))))

(define mk-desc-list-of-ints
  (lambda (n)
    (letrec
        ((go
          (lambda (ncount acc)
            (if (> ncount n)
                acc
                (go (+ ncount 1)
                    (cons ncount acc))))))
      (go 1 '()))))

;(define mk-desc-list-of-ints
;  (lambda (n)
;    (letrec
;        ((go
;          (lambda (ncount acc)
;            (if (= ncount 0)
;                acc
;                (go (- ncount 1)
;                    (append acc (list ncount)))))))
;      (go n '()))))

;; Example commented out, as requested
(mk-asc-list-of-ints 0)
(mk-asc-list-of-ints 1)
(mk-asc-list-of-ints 5)
(mk-desc-list-of-ints 0)
(mk-desc-list-of-ints 1)
(mk-desc-list-of-ints 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define a function iota-iota that takes an argument n.
;;
;; This function must return a list of pairs of integers such that:
;;– (iota-iota 1) => ((1 . 1))
;;– (iota-iota 2) => ((1 . 1) (1 . 2) (2 . 1) (2 . 2))
;;– (iota-iota 3) => ((1 . 1) (1 . 2) (1 . 3) (2 . 1) (2 . 2) (2 . 3) (3 . 1) (3 . 2) (3 . 3))
;;
;;– Any helper functions should be tail-recursive and defined within the body of iota-iota using a letrec.
;;
;;– Hint: Define a function iota which takes an argument n and returns a list in the following range: [1, n]. Remember this function must ultimately be defined within iota-iota.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define iota-iota
  (lambda (n)
    (letrec
        ((flat-recur
          (lambda (seed proc)
            (letrec
                ((go
                  (lambda (ls)
                    (if (null? ls)
                        seed
                        (proc (car ls)
                              (go (cdr ls)))))))
              go)))
         (iota
          (lambda (m acc)
            (cond
              ((= m 0)
               acc)
              (else
               (iota (- m 1) (cons m acc))))))
         (one-iota-iota
          (lambda (element)
            (lambda (ls)
              ((flat-recur '() (lambda (x ys)
                                (cons (cons element x) ys))) ls)))))
;      ((one-iota-iota 1) (iota n '()))
;      (map one-iota-iota (iota n '()))
;      (((flat-recur '() (lambda (ls-input)
;                          (lambda (x ys)
;                            (append (x ls) ys)))) (map one-iota-iota (iota n '()))) (iota n '()))
;      ((map (lambda (x)
;             ((lambda (ls)
;               (x ls)) (iota n '())) (map one-iota-iota (iota n '())))))
;      (map (lambda (x)
;             (let ((ls (iota n '())))
;               (x ls))) (map one-iota-iota (iota n '())))
      (apply append (map (lambda (x)
             (let ((ls (iota n '())))
               (x ls))) (map one-iota-iota (iota n '()))))
      )))

(iota-iota 1) 
(iota-iota 2)
(iota-iota 3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define a tail-recursive function digits->number that takes a list of digits, ds, and returns the number represented by those digits.
;;
;;For example:
;;– (digits->number ’(1 2 3 4)) => 1234
;;– (digits->number ’(7 6 1 5)) => 7615
;;
;;– Any helper functions should be tail-recursive and defined within the
;;body of digits->number using a letrec.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note: the built-in length function will be used instead of defining our own
;; it would have been better for me if I just used letrec* to ensure the bindings were compiled in the order they were defined (top to bottom), but the instructions only mentioned letrec, so I just stuck everything in evaluation part of the letrec

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define digits->number
  (lambda (ds)
    (letrec
        ((iota
          (lambda (n acc)
            (if (= n 0)
                acc
                (iota (- n 1)
                    (cons n acc)))))

         (myiota
          (lambda (iota-proc)
            (let ((mylength (length ds)))
              (map (lambda (x)
                     (- x 1))
                   (reverse (iota-proc mylength '()))))))

         (mypowers
          (lambda (iota-list)
            (map (lambda (x)
                 (expt 10 x))
               iota-list)))

         
         (my-decimal-places
          (lambda (powers)
            (map * ds powers)))

         )

;         (digits->numbers-helper
;          (let* ((mylength (length ds))
;                 (myiota (iota mylength '()))
;                 (mypowers (map (lambda (x)
;                                  (expt 10 x))
;                                myiota))
;                 (my-decimal-places
;                  (map * ds mypowers)))
;            my-decimal-places)))
      
;      ((let* ((mylength (length ds))
;                 (myiota (iota mylength '()))
;                 (mypowers (map (lambda (x)
;                                  (expt 10 x))
;                                myiota))
;                 (my-decimal-places
;                  (map * ds mypowers)))
;            (apply + my-decimal-places)))
      
;      (apply + (map * ds
;                    (map (lambda (x)
;                           (expt 10 x))
;                         (iota (length ds) '()))))

      (apply + (my-decimal-places (mypowers (myiota iota))))
      )))

(digits->number '(1 2 3 4)) 
(digits->number '(7 6 1 5))

;(define something
;  (lambda (ls)
;    (letrec
;        ((a
;          '(1 2 3))
;         (b
;          (map (lambda (x)
;                 (+ 1 x)) a)))
;      (apply + b))))
;
;(something '(1 2 3))

;(letrec ((is-even? (lambda (n)
;                       (or (zero? n)
;                           (is-odd? (- n 1)))))
;           (is-odd? (lambda (n)
;                      (and (not (zero? n))
;                           (is-even? (- n 1))))))
;    (is-odd? 11))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define a function cond->if that takes a cond expression, expr, as an argument and transforms it into an equivalent if expression.

;;For example:
;;– (cond->if ’(cond ((> x y) (- x y)) ((< x y) (- y x)) (else 0))) => (if (> x y) (- x y) (if (< x y) (- y x) 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define body cdr)

(define predicate
  (lambda (condition)
    (car condition)))

(define resolution
  (lambda (condition)
    (cadr condition)))

(define flat-recur
  (lambda (seed proc)
    (letrec
        ((go
          (lambda (ls)
            (if (null? ls)
                seed
                (proc (car ls)
                      (go (cdr ls)))))))
      go)))

(define one-cond->if
  (lambda (condition)
    (if (eq? 'else (car condition))
        (cdr condition)
        (cons 'if (cons (predicate condition) (cons (resolution condition) '()))))))

;; original attempt
;(define cond->if
;  (lambda (expr)
;    ((flat-recur '() (lambda (x ys)
;                      (append (one-cond->if x) ys))) (body expr)))) 

; second attempt
;(define cond->if
;  (lambda (expr)
;    ((flat-recur '() (lambda (x ys)
;                       (if (and (pair? ys) (eq? 'else (car ys)))
;                           (append (one-cond->if x) ys)
;                           (append (one-cond->if x) (list ys)))))
;     (body expr))))


(define cond->if
  (lambda (expr)
    ((flat-recur '() (lambda (x ys)
                       (cond
                         ((null? ys)
                          (cadr x))
                         (else
                          (append (one-cond->if x) (list ys))))))
                       
     (body expr))))


(cond->if '(cond ((> x y) (- x y)) ((< x y) (- y x)) (else 0))) 


(one-cond->if '((> x y) (- x y)))
;(append '(1 2 3) '())
;((lambda (x) (+ x 1)) 2)
(cons 'a '())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define a function sine (note the e at the end) which takes a number x as an argument and returns sin(x). This function must approximate sin using the first 100 terms of the Taylor series for sin. This series is given as follows: sin(x) = x^1/1! - x^3/3! + x^5/5! - x^7/7! + x^9/9! + ...
;
;;– Any helper functions should be defined within the body of sine using
;;letrec.
;;– Extra Credit: Define this function without using or reinventing expt
;;or fact.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sine
  (lambda (x)
    (letrec
        ;; modified to keep track of alternating operation (both in the higher level lambda and when calling proc on the car of ls and the recursive call on the cdr of ls)
        ((flat-recur
          (lambda (seed proc op)
            (letrec
                ((go
                  (lambda (ls)
                    (if (null? ls)
                        seed
                        (proc op (car ls)
                              (go (cdr ls)))))))
              go)))

         (iota
          (lambda (n acc)
            (if (= n 0)
                acc
                (iota (- n 1)
                    (cons n acc)))))

         (iota-odds
          (lambda (n iota-proc)
            (map (lambda (x)
                   (+ (* 2 (- x 1)) 1)) (iota-proc n '()))))

         (fact
           (lambda (n iota-proc)
             (apply * (iota-proc n '()))))

         ;; assuming only + or - operations are used
         (alternate-plus-minus-op
           (lambda (op)
             (if (eq? + op)
                 -
                 +)))

         (add-series
           (lambda (ls)
             ((flat-recur 0 (lambda (op x ys)
                              ((alternate-plus-minus-op op) x ys))
                          +)
              ls)))
         )

;      (iota-odds 100 iota)
;      (map (lambda (arg)
;             (/ (expt x arg) (fact arg iota))) (iota-odds 100 iota))
      (add-series (map (lambda (arg)
             (/ (expt x arg) (fact arg iota))) (iota-odds 100 iota)))
      )))

(define sine-extra
  (lambda (x)
    ((lambda (n)
      (letrec
          ;; also keeps track of an accumulator list to check if all the terms are accounted for (debug purposes)
          ((get-term
            (lambda (curr-n curr-element op-switch-proc curr-op acc acc-list)
              (cond
                ((eq? curr-n (+ (* 2 (- n 1)) 1))
                 (curr-op acc curr-element)
;                 (cons curr-element acc-list)
                 )
                (else
                 (get-term (+ curr-n 2)
                           (/ (* curr-element x x) (* (+ curr-n 1) (+ curr-n 2)))
                           op-switch-proc
                           (op-switch-proc curr-op)
                           (curr-op acc curr-element)
                           (cons curr-element acc-list))))))

           ;; assuming only + or - operations are used
           (alternate-plus-minus-op
            (lambda (op)
              (if (eq? + op)
                  -
                  +)))


           )
        (get-term 1
                  x
                  alternate-plus-minus-op
                  +
                  0
                  '())
        )
       ) 100)))

;(define test
;  (lambda (x)
;    ((lambda (y)
;      (+ (* 2 x) (* 3 y))) 1)))
 
(define pi '3.14159265358979323)
;(sin (/ pi 2))
;(sine (/ pi 2))
;(sine-extra (/ pi 2))
(sin 2)
(sine 2)
(sine-extra 2)

;(test 100)
 



