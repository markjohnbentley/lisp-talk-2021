;;; Some basic macro examples

;;; 1. Some things we can't do with functions

(defun do-not-run-this-code ()
  (format t "Are you sure you want to format C:\? [Y\\n] ")
  (sleep 1)
  (format t "Y ~%")
  (sleep 0.5)
  (format t "Really though? [Y\\n] ")
  (sleep 1)
  (format t "Y ~%")
  (sleep 0.5)
  (format t "Obliterating all work ~%"))

(if (= 1 1)
    "All good"
    (do-not-run-this-code))

(defun my-if (test-form then-form else-form)
  (format t "My special message!")
  (cond (test-form then-form)
        (t else-form)))

(my-if (= 1 1)
       "No worries!"
       (do-not-run-this-code))

;;; 2. But we can with macros

(defmacro my-if-2 (test-form then-form else-form)
  (format t "About to use my-if!")  
  `(cond (,test-form ,then-form)
	 (t ,else-form)))

(my-if-2 (= 1 1)
	 "No worries!"
	 (do-not-run-this-code))

;;; 3. We can fundamentally redefine the language

(defmacro backwards (expr)
  (reverse expr))

(format t "Hello World! ~%")

(backwards ("Hello World! ~%" t format))

(defmacro infix (arg-1 operation arg-2)
  `(,operation ,arg-1 ,arg-2))

(infix 1 + 8)

;; C-c ENTER shows macroexpansion (cursor on form, not symbol)

(loop for i upto 10 collect i)
