(defun modular-exponentiation (base exp modulus)
  (cond ((= exp 0) 1)
        ((evenp exp)
         (mod (expt (modular-exponentiation base (/ exp 2) modulus) 2) modulus))
        (t
         (mod (* base (modular-exponentiation base (- exp 1) modulus)) modulus))))

(defun fermat-test (p n)
  (loop for i from 1 to n
        always (let ((a (+ 1 (random (- p 1)))))
                 (= (modular-exponentiation a p p) (mod a p)))))

(defun probabilistic-prime-p (p)
  (and (> p 1) (fermat-test p 100))) ; 100 tests

; standard deterministic program
(defun deterministic-prime-p (n)
  (cond ((<= n 1) nil)
        ((= n 2) t)
        (t (loop for i from 2 to (isqrt n) never (zerop (mod n i))))))
