(defun range (a b)
  (if (> a b) nil
    (cons a (range (1+ a) b))))

(defun any (f lst)
  (cond ((null lst) nil)
        ((funcall f (car lst)) t)
        (t (any f (cdr lst)))))

(defun all (f lst)
  (cond ((null lst) t)
        ((funcall f (car lst)) (all f (cdr lst)))
        (t nil)))

(defun is-divisible (x n) (eq (mod x n) 0))

(defun is-prime (num)
    (not (any (lambda (x) (is-divisible num x)) (range 2 (truncate (sqrt num))))))

(defun next-prime (num)
  (if (eq num 2) 3
    (let ((n (+ num 2)))
      (if (is-prime n) n
        (next-prime n)))))

(defun factors (num)
  (labels ((factors-helper (n num)
                         (cond ((is-prime num) (list num))
                               ((is-divisible num n) (cons n (factors-helper n (/ num n))))
                               (t (factors-helper (next-prime n) num)))))
    (factors-helper 2 num)))


(defun co-prime (a b)
  (eq (gcd a b) 1))

