(defun not (x)
  (if x nil t))

(defun null (x)
  (if (eql x '()) t nil))

(defun id (x)
  x)

(defun curry (func arg1)
  (lambda (arg2) (funcall func arg1 arg2)))

(defun plusp (num) (< 0 num))

(defun minusp (num) (> 0 num))

(defun flip (func)
  (lambda (arg1 arg2) (funcall func arg2 arg1)))

(defun compose (f g)
  (lambda (arg) (f (apply g arg))))

(defun evenp (num)
  (= (rem num 2) 0))

(defun oddp (num)
  (not (evenp num)))

(defun foldl (op acc l)
  (if (null l)
    acc
    (foldl op (funcall op acc (car l)) (cdr l))))

(defun sum (l) (foldl '+ 0 l))

(defun product (l) (foldl '* 1 l))

(defun length (l)
  (foldl (lambda (x y) (1+ x))
         0
         l))

(defun reverse (l)
  (foldl (flip 'cons) '() l))

(defun foldr (op acc l)
  (if (null l)
    acc
    (funcall op (car l) (foldr op acc (cdr l)))))

(defun member-helper (pred)
  (lambda (acc curr)
          (if (id acc)
              (cons (car acc) (cons curr (cdr acc)))
              (if (funcall pred curr)
                (cons curr acc)
                acc))))

(defun member (obj l)
  ((lambda (res)
          (cons (car res) (reverse (cdr res))))
  (foldl (member-helper (curry 'eql obj)) '() l)))

(defun assoc-helper (pred)
  (lambda (acc curr)
          (if (and (not (id acc))
                   (funcall pred (car curr)))
              curr
              acc)))

(defun assoc (obj l)
  (foldl (assoc-helper (curry 'eql obj)) '() l))

(defun mapcar (f l)
  (foldr (lambda (x y) (cons (funcall f x) y)) '() l))

(defun remove (z l)
  (foldr (lambda (x y) (if (eql z x) y (cons x y))) '() l))

(defun remove-if (pred l)
  (foldr (lambda (x y) (if (funcall pred x) y (cons x y))) '() l))

(defun remove-if-not (pred l)
  (foldr (lambda (x y) (if (not (funcall pred x)) y (cons x y))) '() l))

(defun caar (x) (car (car x)))
(defun cadr (x) (car (cdr x)))
(defun cdar (x) (cdr (car x)))
(defun cddr (x) (cdr (cdr x)))

(defun caaar (x) (car (car (car x))))
(defun caadr (x) (car (car (cdr x))))
(defun cadar (x) (car (cdr (car x))))
(defun caddr (x) (car (cdr (cdr x))))
(defun cdaar (x) (cdr (car (car x))))
(defun cdadr (x) (cdr (car (cdr x))))
(defun cddar (x) (cdr (cdr (car x))))
(defun cdddr (x) (cdr (cdr (cdr x))))

(defun caaadr (x) (car (car (car (cdr x)))))
(defun caaaar (x) (car (car (car (car x)))))
(defun caadar (x) (car (car (cdr (car x)))))
(defun cadaar (x) (car (cdr (car (car x)))))
(defun caaddr (x) (car (car (cdr (cdr x)))))
(defun cadadr (x) (car (cdr (car (cdr x)))))
(defun caddar (x) (car (cdr (cdr (car x)))))
(defun cdaaar (x) (cdr (car (car (car x)))))
(defun cdaadr (x) (cdr (car (car (cdr x)))))
(defun cadddr (x) (car (cdr (cdr (cdr x)))))
(defun cdadar (x) (cdr (car (cdr (car x)))))
(defun cdaddr (x) (cdr (car (cdr (cdr x)))))
(defun cddaar (x) (cdr (cdr (car (car x)))))
(defun cddadr (x) (cdr (cdr (car (cdr x)))))
(defun cdddar (x) (cdr (cdr (cdr (car x)))))
(defun cddddr (x) (cdr (cdr (cdr (cdr x)))))
