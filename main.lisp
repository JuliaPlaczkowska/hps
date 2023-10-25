(defun pluss (a b)
  (if (= b 0) a
    (pluss (1+ a) (1- b))))

(print (pluss 2 3))

(defun pluss-int (a b)
  (cond ((< b 0) (pluss-int (1- a) (1+ b)))
        (:else (if (= b 0) a 
                   (pluss (1+ a) (1- b))))))

(print (pluss-int 2 -3))

(defun minuss (a b)
  (if (= b 0) a
    (minuss (1- a) (1- b))))

(print (minuss 2 3))

(defun minuss-int (a b)
  (cond ((< b 0) (minuss-int (1+ a) (1+ b)))
    (:else (if (= b 0) a 
               (minuss (1- a) (1- b))))))

(print (minuss-int -7 -11))

(defun multiplyy (a b &optional (result 0) (count 0))
  (if (= count b) result
      (multiplyy a b (pluss-int result a) (1+ count))))

(print (multiplyy 3 14))
(print (multiplyy -3 14))


(defun multiplyy-int (a b &optional (result 0) (count 0))
  (cond ((= count b) result)
        ((< b 0) (-(multiplyy a (- b) (pluss-int result a) (1+ count))))
        (:else (multiplyy a b (pluss-int result a) (1+ count)))))

(print (multiplyy-int -3 -10))

(defun dvd (a b &optional (result a) (count 0))
  (cond ((< result b) count)
        ((= b 0) (print "you can not divide by zero!"))
        (:else (dvd a b (minuss-int result b) (1+ count)))))

(print (dvd 121 4))

(defun buttner (a &optional (result 0))
  (if (= a 0) result
      (buttner (1- a) (pluss-int result a))))

(print (buttner 100))