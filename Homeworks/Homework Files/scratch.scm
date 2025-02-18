(map cons '(1 2) '(2 3))

(define one-two
  (lambda (a b c)
    (+ a b c)))

(define one-two-new
  (lambda a
    (apply + a)))

(map one-two '(1 2) '(2 3) '(4 5))
(map one-two-new '(1 2) '(2 3) '(4 5) '(6 7))
(apply cons '((1 2) (3 4)))
(apply cons '(1 (2 5 6)))
