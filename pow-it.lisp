(defun pow-loop (x n)   ; x^n = (x^(2))^(n/2) = ((x^(2))^(2))^(n/(2*2)) = ... (until n/2 = 1)
  (do*
      ((n n (/ n 2))    ; syntax: (variable-name initial-value updated-value)
        (result x (* result result))
        (rest 1 (if (or (= n 1) (= (mod n 2) 0)) rest
                    (progn
                      (setf n (1- n))
                      (setf rest (* rest result))))))
  ((= n 1) (* result rest))
 (format t "~% result = ~d  rest = ~d n = ~d" result rest n)))

(defun pow-it (x n)
  (if (= (mod n 2) 0) (pow-loop x n)   ; when n is even do x^n
      (* (pow-loop x (1- n)) x)))      ; when n is odd do (x^(n-1))*x

; v2
(defun power (x n)
  (let ((result 1))
    (loop while (> n 0)
          do (progn
               (when (= (mod n 2) 1)
                 (setf result (* result x)))
               (setf x (* x x))
               (setf n (floor n 2))))
    result))
