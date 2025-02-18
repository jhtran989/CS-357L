;; CS 357L - Homework 1
;; John Tran
;; UNM ID: 101821704

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 2.2.1

;; Convert the following arithmetic expressions into Scheme expressions and evaluate them.

;; FIXME: actually, review fractions in Scheme...
;; well, it looks like typing fractions directly (like 1/2 as 1/2) works
;; for now, just sticking with the prefix notation

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a.	1.2 × (2 - 1/3) + -8.7

(+ (* 1.2 (- 2 (/ 1 3))) -8.7)

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; b.	(2/3 + 4/9) ÷ (5/11 - 4/3)

(/ (+ (/ 2 3) (/ 4 9)) (- (/ 5 11) (/ 4 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; c.	1 + 1 ÷ (2 + 1 ÷ (1 + 1/2))

(+ 1 (/ 1 (+ 2 (/ 1 (+ 1 (/ 1 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; d.	1 × -2 × 3 × -4 × 5 × -6 × 7

;; apparently, we can just put a negative sign in front (instead of something like (- 0 x) for some magnitude we want to make negative)
(* 1 -2 3 -4 5 -6 7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Using the symbols a and b and the function cons we could construct the
;; list (a b) by evaluation the following expression: (cons ’a (cons ’b ’())).
;; Using the symbols a, b, c, d, and the functions cons, construct the following
;; lists without using quoted lists(i.e. ’(a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (a b c d)

(cons 'a (cons 'b (cons 'c (cons 'd '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (a (b c d))

;; REMOVE
;(cons (cons 'a '()) (cons 'b (cons 'c (cons 'd '()))))

(cons 'a (cons (cons 'b (cons 'c (cons 'd '()))) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (a (b c) d)

(cons 'a (cons(cons 'b (cons 'c '())) (cons 'd '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ((a b) (c d))

(cons (cons 'a (cons 'b '())) (cons (cons 'c (cons 'd '())) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (((a)))

(cons (cons (cons 'a '()) '()) '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 2.2.3
;; Determine the values of the following expressions. Use your Scheme system to verify your answers.

;; These problems will evaluate the expressions directly from the given problem (simply to check our thinking) and the answer will use 4 semicolons (;) instead of the usual two with an arrow to indicate it is an answer (=>).

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (cons 'car 'cdr)
(cons 'car 'cdr)

;;;; => (car . cdr)

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (list 'this '(is silly))
(list 'this '(is silly))

;;;; => (this (is silly))

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (cons 'is '(this silly?))
(cons 'is '(this silly?))

;;;; => (is this silly?)

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (quote (+ 2 3))
(quote (+ 2 3))

;;;; => (+ 2 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (cons '+ '(2 3))
(cons '+ '(2 3))

;;;; => (+ 2 3)
;; same result as the previous part

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (car '(+ 2 3))
(car '(+ 2 3))

;;;; => +
;; just the plus symbol

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (cdr '(+ 2 3))
(cdr '(+ 2 3))

;;;; => (2 3)
;; just the list of 2 and 3

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cons
cons

;;;; => #<procedure:mcons>
;; prints the label of a procedure (specifically, for cons)

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (quote cons)
(quote cons)

;;;; => cons

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (quote (quote cons))
(quote (quote cons))

;;;; => 'cons
;; looks like we can stack multiple levels of quote (data)...

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (car (quote (quote cons)))
(car (quote (quote cons)))

;;;; => quote

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (+ 2 3)
(+ 2 3)

;;;; => 5

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (+ '2 '3)
(+ '2 '3)

;;;; => 5
;; seems like literal numbers and ticked numbers (as data) are treated similarly...

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (+ (car '(2 3)) (car (cdr '(2 3))))
(+ (car '(2 3)) (car (cdr '(2 3))))

;;;; => 5
;; roundabout way to do the same thing as the previous two parts...

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ((car (list + - * /)) 2 3)
((car (list + - * /)) 2 3)

;;;; => 5
;; nice way since we can choose a different operation from the list

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 2.3.1
;; Write down the steps necessary to evaluate the expression below.

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ((car (cdr (list + - * /))) 17 5)
((car (cdr (list + - * /))) 17 5)

;;;; => answer shown below in numbered steps
;; 1. find the inner-most expression in the pairs of parantheses and evaluate it
;; 2. (list + - * /) => (+ - * /)
;; 3. then recursively go up a level of parantheses until no more pair of parantheses are left
;; 4. (cdr (+ - * /)) => (- * /)
;; 5. (car (- * /)) => -
;; 6. (- 17 5) => 12

;;;; final answer: 12

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 2.4.1
;; Rewrite the following expressions, using let to remove common subexpressions and to improve the structure of the code. Do not perform any algebraic simplifications.

;; NOTE: since R5RS doesn not allow brackets (i.e., []), they will not be used in our answers

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (+ (- (* 3 a) b) (+ (* 3 a) b))

;; => answer below
;;(let ((x (* 3 a)))
;;  (+ (- x b)
;;     (+ x b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (cons (car (list a b c)) (cdr (list a b c)))

;; => answer below
;;(let ((x (list a b c)))
;;  (cons (car x) (cdr x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 2.5.1
;; Determine the values of the expressions below.

;; These problems will evaluate the expressions directly from the given problem (simply to check our thinking) and the answer will use 4 semicolons (;) instead of the usual two with an arrow to indicate it is an answer (=>).

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a
;;(let ((f (lambda (x) x)))
;;  (f 'a))
(let ((f (lambda (x) x)))
  (f 'a))

;;;; => a

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; b
;;(let ((f (lambda x x)))
;;  (f 'a))
(let ((f (lambda x x)))
  (f 'a))

;;;; => (a)

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; c
;;(let ((f (lambda (x . y) x)))
;;  (f 'a))
(let ((f (lambda (x . y) x)))
  (f 'a))

;;;; => a

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; d	
;;(let ((f (lambda (x . y) y)))
;;  (f 'a))
(let ((f (lambda (x . y) y)))
  (f 'a))

;;;; => ()

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 2.5.3
;; List the variables that occur free in each of the lambda expressions below. Do not omit variables that name primitive procedures such as + or cons.

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a
;;(lambda (f x) (f x))

;;;; => [none]

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; b
;;(lambda (x) (+ x x))

;;;; => +

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; c
;;(lambda (x y) (f x y))

;;;; => f

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; d
;;(lambda (x)
;;  (cons x (f x y)))

;;;; => cons, f, y

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; e
;;(lambda (x)
;;  (let ((z (cons x y)))
;;    (x y z)))

;;;; => let, cons, z

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; f
;;(lambda (x)
;;  (let ((y (cons x y)))
;;    (x y z)))

;;;; => let, cons, z

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 2.6.2
;; A more elegant (though possibly less efficient) way to define cadr and cddr than given in this section is to define a procedure that composes two procedures to create a third. Write the procedure compose, such that (compose p1 p2) is the composition of p1 and p2 (assuming both take one argument). That is, (compose p1 p2) should return a new procedure of one argument that applies p1 to the result of applying p2 to the argument. Use compose to define cadr and cddr.

;; Another note: to make sure the built-in procedures are not overwritten (with an exception), the prefix "my-" will be added to our procedure names below

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; compose procedure
;; takes in two procedures p1 and p2
;; returns procedure that takes in one argument that applies p1 to the result of applying p2 to the argument
(define compose
  (lambda (p1 p2)
    (lambda (arg)
      (p1 (p2 arg)))))

;; cadr procedure
;; takes in one argument (should be a list)
;; applies the car of the cdr of the argument
(define my-cadr
  (compose car cdr))

;; cddr procedure
;; takes in one argument (should be a list)
;; applies the cdr of the cdr of the argument
(define my-cddr
  (compose cdr cdr))

;; test cases (personal)
(define test-case-2-6-2
  '(a b c))

(my-cadr test-case-2-6-2)
(cadr test-case-2-6-2)
(my-cddr test-case-2-6-2)
(cddr test-case-2-6-2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 2.6.3
;; Scheme also provides caar, cdar, caaar, caadr, and so on, with any combination of up to four a's (representing car) and d's (representing cdr) between the c and the r (see Section 6.3). Define each of these with the compose procedure of the preceding exercise.

;; Note: only implement caar, cdar, caaar, caadr
;; Another note: to make sure the built-in procedures are not overwritten (with an exception), the prefix "my-" will be added to our procedure names below

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; caar procedure
;; takes in one argument (should be a list where the first element is also a list)
;; applies the car of the cdr of the argument
(define my-caar
  (compose car car))

;; cdar procedure
;; takes in one argument (should be a list where the first element is also a list)
;; applies the cdr of the car of the argument
(define my-cdar
  (compose cdr car))

;; caaar procedure
;; takes in one argument (should be a list where the first element is a list where the first element is also a list)
;; applies the car of the car of the car of the argument
(define my-caaar
  (compose car (compose car car)))

;; caadr procedure
;; takes in one argument (should be a list with at least two elements where the cdr has the first element as a list where the first element is also a list)
;; applies the car of the car of the cdr of the argument
(define my-caadr
  (compose car (compose car cdr)))

;; test cases (personal)
(define test-case-2-6-3
  '(((a b) c) ((d e) f)))

(my-caar test-case-2-6-3)
(caar test-case-2-6-3)
(my-cdar test-case-2-6-3)
(cdar test-case-2-6-3)
(my-caaar test-case-2-6-3)
(caaar test-case-2-6-3)
(my-caadr test-case-2-6-3)
(caadr test-case-2-6-3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; What is the value of the following expressions?

;; These problems will evaluate the expressions directly from the given problem (simply to check our thinking) and the answer will use 4 semicolons (;) instead of the usual two with an arrow to indicate it is an answer (=>).

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (and #t (or #t #f))
(and #t (or #t #f))

;;;; => #t

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (or #f (and (not #f) #t #t))
(or #f (and (not #f) #t #t))

;;;; => #t

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (not (or (not #t) (not #t)))
(not (or (not #t) (not #t)))

;;;; => #t

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (and (or #t #f) (not (or #t #f)))
(and (or #t #f) (not (or #t #f)))

;;;; => #f

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 2.7.1
;; Define the predicate atom?, which returns true if its argument is not a pair and false if it is.

;; Another note: to make sure the built-in procedures are not overwritten (with an exception), the prefix "my-" will be added to our procedure names below

;;;;;;;;;;;;;;;;;;;;;;;;;;

(define atom?
  (lambda (x)
    (cond
      ((not (pair? x)) #t)
      (else #f))))

;; test cases (personal)
(define test-case-2-7-1-a
  'a)
(define test-case-2-7-1-b
  '(a))

(atom? test-case-2-7-1-a)
(atom? test-case-2-7-1-b)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 2.7.2
;; The procedure length returns the length of its argument, which must be a list. For example, (length '(a b c)) is 3. Using length, define the procedure shorter, which returns the shorter of two list arguments. Have it return the first list if they have the same length.

;; Example output (given in problem):
;;(shorter '(a b) '(c d e)) <graphic> (a b)
;;(shorter '(a b) '(c d)) <graphic> (a b)
;;(shorter '(a b) '(c)) <graphic> (c)

;;;;;;;;;;;;;;;;;;;;;;;;;;

(define shorter
  (lambda (ls1 ls2)
    (cond
      ((< (length ls1) (length ls2))
       ls1)
      ((> (length ls1) (length ls2))
       ls2)
      (else
       ls1))))

;; test our procedure with the sample test cases given to us
(shorter '(a b) '(c d e)) 
(shorter '(a b) '(c d)) 
(shorter '(a b) '(c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 2.8.3
;; Define the procedure make-list, which takes a nonnegative integer n and an object and returns a new list, n long, each element of which is the object.

;; Example output (given in problem):
;; (make-list 7 '()) <graphic> (() () () () () () ())

;; [Hint: The base test should be (= n 0), and the recursion step should involve (- n 1). Whereas () is the natural base case for recursion on lists, 0 is the natural base case for recursion on nonnegative integers. Similarly, subtracting 1 is the natural way to bring a nonnegative integer closer to 0.]

;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-list
  (lambda (n obj)
    (cond
      ((= n 0)
       '())
      (else
       (cons obj
             (make-list (- n 1) obj))))))

;; test our procedure with the sample test cases given to us
(make-list 7 '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 2.8.4
;; The procedures list-ref and list-tail return the nth element and nth tail of a list ls.

;; Example output (given in problem):
;;(list-ref '(1 2 3 4) 0) <graphic> 1
;;(list-tail '(1 2 3 4) 0) <graphic> (1 2 3 4)
;;(list-ref '(a short (nested) list) 2) <graphic> (nested)
;;(list-tail '(a short (nested) list) 2) <graphic> ((nested) list)

;; Define both procedures.

;; Note: name your functions my-list-ref and my-list-tail

;;;;;;;;;;;;;;;;;;;;;;;;;

;; Original attempt, but wanted to pull out pattern
;;(define my-list-ref
;;  (lambda (ls index)
;;    (cond
;;      ((= index 0)
;;       (car ls))
;;      (else
;;       (my-list-ref (cdr ls) (- index 1))))))
;;
;;(define my-list-tail
;;  (lambda (ls index)
;;    (cond
;;      ((= index 0)
;;       ls)
;;      (else
;;       (my-list-tail (cdr ls) (- index 1))))))

;; Further attempt -- kind of clunky way (wanted proc to be optional, but the only option I found was to use lambda (ls index . proc) which made proc a list...and possible to have proc refer to multiple arguments in the list; in the end, wanted to restrain proc to just one argument)
;; Just made the empty list the "null" option (acts as without any argument given to proc)
(define my-list-helper
  (lambda (ls index proc)
    (cond
      ((= index 0)
       (if (null? proc)
           ls
           (proc ls)))
      (else
       (my-list-helper (cdr ls) (- index 1) proc)))))

(define my-list-ref
  (lambda (ls index)
    (my-list-helper ls index car)))

(define my-list-tail
  (lambda (ls index)
    (my-list-helper ls index '())))

;; test our procedure with the sample test cases given to us
(my-list-ref '(1 2 3 4) 0)
(my-list-ref '(a short (nested) list) 2)
(my-list-tail '(1 2 3 4) 0)
(my-list-tail '(a short (nested) list) 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define a function subst-first that takes the following three arguments: an
;; item new, an item old, and a list of items ls. The function should look for
;; the first top-level (do not recurse into sublists) occurrence of the item old
;; in the list and replace it with new

;; Example output (given in problem):
;;(subst-first ’dogs ’cats ’(I love cats)) => (I love dogs)
;;(subst-first ’x ’y ’(+ x y)) => (+ x x)
;;(subst-first ’x ’y ’(+ x (* y y) y)) => (+ x (* y y) x)
;;(subst-first ’(hello) ’(world) ’(hello world (world))) => (hello world
;;                                                                 (hello))
;;(subst-first ’a ’b ’()) => ()

;; Note: looks like the ticks need to be fixed when copy and pasting...

;;;;;;;;;;;;;;;;;;;;;;;;;

(define subst-first
  (lambda (item-new item-old ls)
    (cond
      ((null? ls)
       '())
      ((equal? (car ls) item-old)
       (cons item-new (subst-first item-new item-old (cdr ls))))
      (else
       (cons (car ls) (subst-first item-new item-old (cdr ls)))))))

;; test our procedure with the sample test cases given to us
(subst-first 'dogs 'cats '(I love cats))
(subst-first 'x 'y '(+ x y))
(subst-first 'x 'y '(+ x (* y y) y))
(subst-first '(hello) '(world) '(hello world (world)))
(subst-first 'a 'b '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define a function firsts that takes a list of nonempty lists. This function
;; should return a list of the first item from each sub-list.

;; Example output (given in problem):
;;(firsts ’((a) (b) (c))) => (a b c)
;;(firsts ’((a b c) (d e f) (g h i))) => (a d g)
;;(firsts ’(((uno)))) => ((uno))
;;(firsts ’()) => ()

;; Note: looks like the ticks need to be fixed when copy and pasting...
;; Another note: caar (built-in) was used since we already defined our own implementation above (in a previous problem)

;;;;;;;;;;;;;;;;;;;;;;;;;

(define firsts
  (lambda (ls)
    (cond
      ((null? ls)
       '())
      (else
       (cons (caar ls) (firsts (cdr ls)))))))

;; test our procedure with the sample test cases given to us
(firsts '((a) (b) (c)))
(firsts '((a b c) (d e f) (g h i)))
(firsts '(((uno))))
(firsts '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define a function remove-last that removes the last top-level occurrence of
; a given item in a list ls

;; Example output (given in problem):
;;(remove-last ’i ’(m i s s i s s i p p i)) => (m i s s i s s i p p)
;;(remove-last ’i ’(m i s s i s s i p p i s)) => (m i s s i s s i p p s)
;;(remove-last ’i ’()) => ()

;; Note: looks like the ticks need to be fixed when copy and pasting...

;;;;;;;;;;;;;;;;;;;;;;;;;

;; Initial attempt that removes every occurence...need some way to keep track if the first occurence was already found

;;(define remove-last
;;  (lambda (item ls)
;;    (cond
;;      ((null? ls)
;;       '())
;;      ((equal? (car ls) item)
;;       (remove-last item (cdr ls)))
;;      (else
;;       (cons (car ls) (remove-last item (cdr ls)))))))

;; Second attempt...removed the first occurence instead of the last...

;;(define remove-last-helper
;;  (lambda (item ls item-flag)
;;    (cond
;;      ((null? ls)
;;       '())
;;      ((and (equal? item-flag #t)
;;           (equal? (car ls) item))
;;           (remove-last-helper item (cdr ls) #f))
;;      (else
;;       (cons (car ls) (remove-last-helper item (cdr ls) item-flag))))))
;;
;;(define remove-last
;;  (lambda (item ls)
;;    (remove-last-helper item ls #t)))

;; Final attempt

(define remove-last-helper
  (lambda (item ls item-flag)
    (cond
      ((null? ls)
       '())
      ((and (equal? item-flag #t)
           (equal? (car ls) item))
           (remove-last-helper item (cdr ls) #f))
      (else
       (cons (car ls) (remove-last-helper item (cdr ls) item-flag))))))

;; need to reverse list before and after (from the previous attempt) so the last occurence is removed (which is searched in reverse order) and reversed back to its original order
(define remove-last
  (lambda (item ls)
    (reverse (remove-last-helper item (reverse ls) #t))))

;; test our procedure with the sample test cases given to us
(remove-last 'i '(m i s s i s s i p p i))
(remove-last 'i '(m i s s i s s i p p i s))
(remove-last 'i '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

