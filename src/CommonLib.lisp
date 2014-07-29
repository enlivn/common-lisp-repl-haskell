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
