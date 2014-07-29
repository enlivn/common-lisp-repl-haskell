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
  (lambda (arg1 arg2) (func arg2 arg1)))

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

(defun foldr (op acc l)
  (if (null l)
    acc
    (funcall op (car l) (foldr op acc (cdr l)))))
