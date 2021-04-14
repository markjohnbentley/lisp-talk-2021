;;; Basic Lisp functions

(defun hello-world ()
  (format t "Hello World! ~%"))

;; Can copy paste to REPL or...

(defun add (x y)
  "Clearer than cons"
  (cons x y))

;; Recursive:

(defun reverse-list (x &optional (return-list nil))
  (if (null x)
      return-list
      (reverse-list (rest x) (add (first x) return-list))))

;; Can do procedural

(loop for i from 1 to 10 do (format t "~s ~%" i))

;; Define c

(defvar c 8)
(reverse-list `(a b ,c))

;; But what *are* a and b?!

;; Encapsulation through a closure

(let ((x 0))
  (defun counter ()
    (setf x (+ x 1))))

