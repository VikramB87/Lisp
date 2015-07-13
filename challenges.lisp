(defun my-length (lst)
  (if (null lst) 0
    (1+ (my-length (cdr lst)))))

(defun elem-at (lst n)
  (if (= n 0) (car lst)
    (elem-at (cdr lst) (1- n))))

(defun dup (lst)
  (if (null lst) nil
    (let ((h (car lst)))
      (cons h (cons h (dup (cdr lst)))))))

(defun rep-elem (a n)
  (if (= n 0) nil
    (cons a (rep-elem a (1- n)))))


(defun replicate (lst n)
  (if (null lst) nil
    (append (rep-elem (car lst) n) (replicate (cdr lst) n))))


(defun slice (lst a b)
  (labels ((slice-helper (lst i)
                       (cond
                         ((< i a) (slice-helper (cdr lst) (1+ i)))
                         ((> i b) nil)
                         (t (cons (car lst) (slice-helper (cdr lst) (1+ i)))))))
    (slice-helper lst 0)))

