;; CS 357L - Homework 2
;;
;; John Tran
;; UNM ID: 101821704
;;
;; 2/18/2022

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; IMPORTANT

;; A total of 3 late days was used

;; To make the coding a little easier to read, the different procedural abstractions (e.g., fold, filter, make-deep) will be used (as discussed in lecture)
;; Also, to clearly label which procedural abstractions were used, the base code will be provided in each problem that used them (even if it was already defined in a previous problem)
;; Specifically for fold, the other name flat-recur was used
;; I didn't find a good solution to define constant-like variables in letrec (or even use other procedures within the letrec bindings -- R5RS implementation), so I had to make procedures that would take in an argument out of them (so they can be used in the body of letrec)
;; Wasn't sure if we could define constants outside of letrec, so I didn't bother
;; Though, letrec* would help with R5RS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define a function deepen-n that takes two parameters, ls and n. This function should wrap n pairs of parens around each top level element in ls

;; For example:
;;– (deepen-n ’() 10) => ()
;;– (deepen-n ’(1 2 3 4 5) 0) => (1 2 3 4 5)
;;– (deepen-n ’(1 2 3 4 5) 1) => ((1) (2) (3) (4) (5))
;;– (deepen-n ’(1 2 3 4 5) 2) => (((1)) ((2)) ((3)) ((4)) ((5)))
;;– (deepen-n ’(1 2 3 4 5) 3) => ((((1))) (((2))) (((3))) (((4))) (((5))))
;;– (deepen-n ’((1 2) (3 4) ((5) 6)) 2) => ((((1 2))) (((3 4))) ((((5) 6))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Assuming that nested empty lists can be nested, but not just an empty list => '()

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; since we only need to wrap pairs of parens around each top level element, we could use a fold (flat recursion)
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

;; here, we define one instance of wrapping a pair of parens to each top level element
(define deepen-n-once
  (flat-recur '()
              (lambda (x ys)
                (cons (cons x '())
                      ys))))

;; now, we can recursively call deepen-n-once n times (until n reaches 0 while decrementing n after each recursive call)
(define deepen-n
  (lambda (ls n)
    (cond
      ((= n 0)
       ls)
      (else
       (deepen-n (deepen-n-once ls)
                 (- n 1))))))

;; Example commented out, as requested
;(deepen-n '() 10)
;(deepen-n '(1 2 3 4 5) 0)
;(deepen-n '(1 2 3 4 5) 1)
;(deepen-n '(1 2 3 4 5) 2)
;(deepen-n '(1 2 3 4 5) 3)
;(deepen-n '((1 2) (3 4) ((5) 6)) 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define a function insert-left-all that takes three parameters, new old ls. This function should insert new to the left of every occurrence of old in ls. This function should recurse into sublists to find all occurrences of old.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;For example:
;;– (insert-left-all ‘z ’a’()) => ()
;;– (insert-left-all ‘z ’a’(a ((b a) ((a (c)))))) => (z a ((b z a) ((z a (c)))))
;;– (insert-left-all ‘z ’a’(((a)))) => (((z a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; since we need to recurse into the sublists, we could use make-deep
;; goes through all levels of the original list (bottom up)
;; since map returns a list of the resulting elements, proc will always be applied on a list
(define make-deep
  (lambda (proc)
    (letrec
        ((go
          (lambda (ls)
            (if (pair? ls)
                (proc (map go ls))
                ls))))
      go)))

;; then to actually recreate each level sublist with the specified condition (new left of old), we need to recurse through every every element of the sublist
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

;; for every level sublist, we check if the car is equal to old and create a list of new and old
;; to add that to the recursive call of the cdr, we need to use something that can combine 2 lists and keep the same level (i.e., append)
;; otherwise, we do the usual to rebuild the list => (cons x ys)
(define insert-left-level
  (lambda (new old)
    (flat-recur '() (lambda (x ys)
                    (if (eq? x old)
                        (append (list new old)
                                ys)
                        (cons x ys))))))

;; now we can apply our insert-left-level at every recursive call of make-deep (each level of the original list) to add new to the left of old at all levels
;; combining the bottom level lambdas with just (ls) as the parameter was a little tricky, but we just need to make sure the evaluation of insert-left-level also returns a lambda with (ls) as the parameter
;; the (ls) parameter for make-deep accepts the original list and feeds each level sublist into insert-left-level
(define insert-left-all
  (lambda (new old ls)
    ((make-deep (insert-left-level new old))
     ls)))


;; Example commented out, as requested
;(insert-left-all 'z 'a '())
;(insert-left-all 'z 'a '(a ((b a) ((a (c))))))
;(insert-left-all 'z 'a '(((a))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Example commented out, as requested
;(mk-asc-list-of-ints 0)
;(mk-asc-list-of-ints 1)
;(mk-asc-list-of-ints 5)
;(mk-desc-list-of-ints 0)
;(mk-desc-list-of-ints 1)
;(mk-desc-list-of-ints 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define a function calculator that takes one argument expr. This function should evaluate the infix expr and return its value. You can assume that all sub-expressions are parenthesized (no need to worry about precedence), will contain only natural numbers, and will only contain the four basic math operators, +, -, *, /.

;;For example:
;;– (calculator 42) => 42
;;– (calculator ’(1 + 2)) => 3
;;– (calculator ’(1 + (2 * 8))) => 17
;;– (calculator ’((((2 + 3) * 2) / 5) + (17 - 1))) => 18

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note: the code given below is based on the calculator section covered in lecture

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; we need to recurse into the sublists to evaluate the inner expressions
;; the problem had us assume that all sub-expressions are parenthesized, so we can parse all sub-expressions the same way
(define make-deep
  (lambda (proc)
    (letrec
        ((go
          (lambda (ls)
            (if (pair? ls)
                (proc (map go ls))
                ls))))
      go)))

;; we only need to worry about the four main math operations
;; since the operations parsed from the list (expression) are used as data, we need to convert them to symbols that Scheme can recognize and execute the expressions
;; since this will be parenthesized, we don't need to worry about precedence and evaluate multiple arguments (i.e., (1 + 2 + 3))
;; so, we just have the leftoperand, operation, and rightoperand from infix for each sub-expression and convert them to prefix
(define one-operation
  (lambda (ls)
    (letrec
        ((op (cadr ls))
         (leftoperand (car ls))
         (rightoperand (caddr ls))
         (evalExpr
          (lambda (expr)
            (cond
              ((number? expr)
               expr)
              ((eq? '+ expr)
               +)
              ((eq? '- expr)
               -)
              ((eq? '* expr)
               *)
              ((eq? '/ expr)
               /)
              (else
               ((evalExpr op) (evalExpr leftoperand) (evalExpr rightOperand)))))))
      (evalExpr ls))))

;; just apply the evaulation of one sub-expression on all levels of the original expressions
(define calculator
  (lambda (expr)
    ((make-deep one-operation) expr)))

;; Example commented out, as requested
;(calculator 42)
;(calculator '(1 + 2))
;(calculator '(1 + (2 * 8)))
;(calculator '((((2 + 3) * 2) / 5) + (17 - 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define a function infix->prefix that takes on argument expr. This function must return a prefix expression corresponding to the infix expr argument.

;;For example:
;;– (infix->prefix 42) => 42
;;– (infix->prefix ’(1 + 2)) => (+ 1 2)
;;– (infix->prefix ’(1 + (2 * 8))) => (+ 1 (* 2 8))
;;– (infix->prefix ’((((2 + 3) * 2) / 5) + (17 - 1))) => (+ (/ (* (+ 2 3) 2) 5) (- 17 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note: the code given below is based on the prefix<->infix section covered in lecture
;; assuming the same conditions as the previous problem (parenthesized sub-expressions), so we can treat all sub-expressions the same way (three arguments per sub-expression)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; we need to recurse into the sub-lists and do the conversion at all levels
(define make-deep
  (lambda (proc)
    (letrec
        ((go
          (lambda (ls)
            (if (pair? ls)
                (proc (map go ls))
                ls))))
      go)))

;; simple procedure that just swaps the first and second arguments of each sub-list
(define mid<->front
  (lambda (ls)
    (list (cadr ls)
          (car ls)
          (caddr ls))))

;; as described in lecture, this actually could be considered more generally as infix<->prefix (two way conversion) since the only arguments we swap in each sub-expression are the first two arguments (operation and one of the arguments)
;; applies the conversion for each sub-expression at all levels
(define infix->prefix
  (make-deep mid<->front))

;; Example commented out, as requested
;(infix->prefix 42)
;(infix->prefix '(1 + 2))
;(infix->prefix '(1 + (2 * 8)))
;(infix->prefix '((((2 + 3) * 2) / 5) + (17 - 1)))

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

;; Note: the code given below is based on the iota section covered in lecture
;; all helper functions are defined in the letrec of iota-iota, but there are some that just evaluated an expression for easier readability (still satisfies the tail-recursive property since these functions are one-to-one)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; find the list of all pairs generated by the Cartesian product of the iota of n with another iota of n (indexes the second element in the pair, then the first element)
(define iota-iota
  (lambda (n)
    (letrec
        (;; we need a way to recurse through an entire list of iota to generate the first part of iota-iota
         (flat-recur
          (lambda (seed proc)
            (letrec
                ((go
                  (lambda (ls)
                    (if (null? ls)
                        seed
                        (proc (car ls)
                              (go (cdr ls)))))))
              go)))

         ;; used the mk-asc-list-of-ints procedure from the previous problem to generate a list of numbers in the range [1, n] (ascending iota) -- as given in the hint
         (iota
          (lambda (m acc)
            (cond
              ((= m 0)
               acc)
              (else
               (iota (- m 1) (cons m acc))))))

         ;; applies the generation of pairs given some element with all values in another list (in this case, iota of n)
         (one-iota-iota
          (lambda (element)
            (lambda (ls)
              ((flat-recur '() (lambda (x ys)
                                (cons (cons element x) ys))) ls))))

         ;; a helper function to use with map below to apply the resulting application of one-iota-iota onto iota of n again (level higher than one-iota-iota)
         (iota-iota-helper
          (lambda (x)
             (let ((ls (iota n '())))
               (x ls)))))

      ;; now we need to apply the one-iota-iota to every element in iota of n, then formatting the list so all the resulting pairs are in the same list (i.e., same level)
      ;; so, can we just apply append to every element to remove the outer-most parens and combine all elements into the same level (matching the output given in the example cases)
      (apply append (map iota-iota-helper (map one-iota-iota (iota n '())))))))

;; Example commented out, as requested
;(iota-iota 1) 
;(iota-iota 2)
;(iota-iota 3)

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

;; Note: the built-in length function will be used instead of defining our own implementation
;; it would have been better for me if I just used letrec* to ensure the bindings were compiled in the order they were defined (top to bottom), but the instructions only mentioned letrec, so I just stuck everything in evaluation part of the letrec

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define digits->number
  (lambda (ds)
    (letrec
        (;; used the mk-asc-list-of-ints procedure from the previous problem to generate a list of numbers in the range [1, n] (ascending iota)
         (iota
          (lambda (n acc)
            (if (= n 0)
                acc
                (iota (- n 1)
                    (cons n acc)))))

         ;; in order to match the order of digits given, we actually a descending iota (so the first digit in ds has the highest power) and every element is shifted by one (i.e., subtract 1) so the last digit is just itself (i.e., 10^0 = 1)
         (myiota
          (lambda (iota-proc)
            (let ((mylength (length ds)))
              (map (lambda (x)
                     (- x 1))
                   (reverse (iota-proc mylength '()))))))

         ;; finds the list of powers by mapping 10^[element] to each element of the given iota list
         (mypowers
          (lambda (iota-list)
            (map (lambda (x)
                 (expt 10 x))
               iota-list)))

         ;; then we just map * across the two lists -- list of powers given by mypowers above and the lists of digits given to us from ds
         (my-decimal-places
          (lambda (powers)
            (map * ds powers))))

      ;; finally, we just sum all decimal values up
      (apply + (my-decimal-places (mypowers (myiota iota)))))))

;; Example commented out, as requested
;(digits->number '(1 2 3 4)) 
;(digits->number '(7 6 1 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define a function cond->if that takes a cond expression, expr, as an argument and transforms it into an equivalent if expression.

;;For example:
;;– (cond->if ’(cond ((> x y) (- x y)) ((< x y) (- y x)) (else 0))) => (if (> x y) (- x y) (if (< x y) (- y x) 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note: the code given below is based on the let to lambda section covered in lecture
;; the code below should be pretty self-explanatory

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; everything except the cond
(define body cdr)

;; takes in a list of the condition (with both the predicate and resolution) and returns the predicate
(define predicate
  (lambda (condition)
    (car condition)))

;; takes in a list of the condition (with both the predicate and resolution) and returns the resolution (body of the predicate)
(define resolution
  (lambda (condition)
    (cadr condition)))

;; we need to recurse through all the conditions in the body of the cond
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

;; changes a single condition statement into the corresponding if expression (accounts for the else clause by just returning a list of the resolution)
;; returning the list for the resolution of the else clase is important -- see next comment below
;; EDIT: disregard the comment right above this...
(define one-cond->if
  (lambda (condition)
    (if (eq? 'else (car condition))
        (cdr condition)
        (cons 'if (cons
                   (predicate condition)
                   (cons (resolution condition) '()))))))

;; this one was a little tricky to figure out since the nested pairs of parens occur at the END of the list, not at the beginning (could have used cons if that were the case)
;; however, there was a solution with appending the transformed car with a list of the cdr
;; the tricky thing was dealing with the end clause since we want the resolution of it at the same level (of parens) as the previous transformed if statement
;; so the easiest way I found was to check if the recursive cdr of the list was null
;; this avoided having to deal with the empty list at the end...actually, the else condition check above for one-cond->if was not needed at all...
(define cond->if
  (lambda (expr)
    ((flat-recur '() (lambda (x ys)
                       (cond
                         ((null? ys)
                          (cadr x))
                         (else
                          (append (one-cond->if x) (list ys))))))
                       
     (body expr))))


;; Example commented out, as requested
;(cond->if '(cond ((> x y) (- x y)) ((< x y) (- y x)) (else 0))) 
;(one-cond->if '((> x y) (- x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define a function sine (note the e at the end) which takes a number x as an argument and returns sin(x). This function must approximate sin using the first 100 terms of the Taylor series for sin. This series is given as follows: sin(x) = x^1/1! - x^3/3! + x^5/5! - x^7/7! + x^9/9! + ...
;
;;– Any helper functions should be defined within the body of sine using
;;letrec.
;;– Extra Credit: Define this function without using or reinventing expt
;;or fact.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; IMPORTANT: Extra Credit was completed below -- named sine-extra
;; Note: some hints were given in lecture (at least, for the normal version) so there were used
;; it would have been better for me if I just used letrec* to ensure the bindings were compiled in the order they were defined (top to bottom), but the instructions only mentioned letrec, so I just stuck everything in evaluation part of the letrec

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sine
  (lambda (x)
    (letrec
        ;; modified to keep track of alternating operation (both in the higher level lambda and when calling proc on the car of ls and the recursive call on the cdr of ls)
        ;; need to recurse through the terms and add them with alternating operations (switching between + and - each time)
        ;; could have found a way to just add all the terms where you add and then subtract all the terms where you would subtract, but I found this was more natural for me
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

         ;; used the mk-asc-list-of-ints procedure from the previous problem to generate a list of numbers in the range [1, n] (ascending iota)
         (iota
          (lambda (n acc)
            (if (= n 0)
                acc
                (iota (- n 1)
                    (cons n acc)))))

         ;; now, we need the series 1, 3, 5, etc. or just the odd terms, so we modified the original iota
         (iota-odds
          (lambda (n iota-proc)
            (map (lambda (x)
                   (+ (* 2 (- x 1)) 1)) (iota-proc n '()))))

         ;; defined our own factorial using our iota procedure above
         (fact
           (lambda (n iota-proc)
             (apply * (iota-proc n '()))))

         ;; assuming only + or - operations are used
         ;; returns the other when given one
         (alternate-plus-minus-op
           (lambda (op)
             (if (eq? + op)
                 -
                 +)))

         ;; add all the terms with alternating operations
         ;; had to modify the flat-recur base, though
         (add-series
           (lambda (ls)
             ((flat-recur 0 (lambda (op x ys)
                              ((alternate-plus-minus-op op) x ys))
                          +)
              ls)))
         )

      ;; general approach was to divide the corresponding power of x by the factorial of the current term, using the iota-odds list with just odd values
      (add-series (map (lambda (arg)
             (/ (expt x arg) (fact arg iota))) (iota-odds 100 iota)))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Extra Credit
;; general approach was to keep track of everything in the parameters of the recursive call (so it was tail recursive)
;; basically, we want to build off the current term with each recursive call since the only difference between successive terms is a difference of x^2 in the numerator and (curr-n + 1) * (curr-n + 2) in the denominator (curr-n is the current value of n, which is NOT the term number, but the value of the exponent/factorial)
;; since we are just working with odd values, we increment curr-n by 2 with each recursive call
;; the base case is to check the corresponding ODD value (2(n - 1) + 1) of n and make sure to add the current term when we return (with the right operation)
;; since we could not use factorial or exponent functions, this was the best way to work around it

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sine-extra
  (lambda (x)
    ((lambda (n)
      (letrec
          ;; also keeps track of an accumulator list to check if all the terms are accounted for (debugging purposes)
          ((get-term
            (lambda (curr-n curr-element op-switch-proc curr-op acc acc-list)
              (cond
                ((eq? curr-n (+ (* 2 (- n 1)) 1))
                 (curr-op acc curr-element)
                 )
                (else
                 (get-term (+ curr-n 2)
                           (/ (* curr-element x x) (* (+ curr-n 1) (+ curr-n 2)))
                           op-switch-proc
                           (op-switch-proc curr-op)
                           (curr-op acc curr-element)
                           (cons curr-element acc-list))))))

           ;; assuming only + or - operations are used
           ;; same as the normal version
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

;; Example commented out, as requested
(define pi '3.14159265358979323)
(sin (/ pi 2))
(sine (/ pi 2))
(sine-extra (/ pi 2))
(sin 2)
(sine 2)
(sine-extra 2)
 



