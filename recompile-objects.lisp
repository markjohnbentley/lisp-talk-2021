;;;; Recompiling on the fly

(defclass point ()
  ((x :accessor point-x :initarg :x :initform 0)
   (y :accessor point-y :initarg :y :initform 0)))

;; (defparameter *location* (make-instance 'point :x 100 :y 100))

;;; C-c C-v TAB

;; (debug)

;; (defmethod update-instance-for-redefined-class :before
;;      ((pos point) added deleted plist &key)
;;   (let ((x (getf plist 'x))
;;         (y (getf plist 'y)))
;;     (setf (point-rho pos) (sqrt (+ (* x x) (* y y)))
;;           (point-theta pos) (atan y x))))

;; (defclass point ()
;;   ((rho :initarg :rho :initform 0 :accessor point-rho)
;;    (theta :initarg :theta :initform 0 :accessor point-theta)))

;;; C-c C-v TAB
