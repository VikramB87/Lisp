
(defun istree (lst)
  (cond ((null lst) t)
        ((eq (length lst) 3) (and (istree (second lst)) (istree (third lst))))
        (t nil)))

(defun insert-elem (tree x)
  (cond ((null tree) (list x nil nil))
        ((< x (car tree)) (list (car tree) (insert-elem (second tree) x) (third tree)))
        (t (list (car tree) (second tree) (insert-elem (third tree) x)))))

(defun foldl (f lst x)
  (if (null lst) x
    (foldl f (cdr lst) (funcall f x (car lst)))))

(defun make-tree (lst)
  (foldl #'insert-elem lst nil))

(defun count-leaves (tree)
  (if (and (nil (second tree) (third tree))) 1
    (+ (count-leaves (second tree)) (count-leaves (third tree)))))

(defun find-elem (tree x)
  (cond ((null tree) nil)
        ((eq x (car tree)) t)
        (t (or (find-elem (second tree) x) (find-elem (third tree) x)))))
