(declaim (optimize (debug 3)))

(defun factorial (n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(factorial 8)

