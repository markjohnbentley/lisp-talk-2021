;;;; Looking at CLOS in the context of a European Barrier Option
;;;;
;;;; Mark Bentley, 2021

;;; Code to simulate a path

(defun uniform (&optional (N 1e9))
  "Gives approximate U(0,1) random variate"
  (let ((u (/ (random N)
	      N)))
    (if (> u 0)
	u
	(uniform N))))

(defun box-muller ()
  "Returns an independent pair of N(0,1)'s"
  (let* ((u-1 (uniform))
	 (u-2 (uniform))
	 (fact (sqrt (* (- 2) (log u-1))))
	 (arg (* 2 PI u-2)))
    (values (* fact (cos arg))
	    (* fact (sin arg)))))

(defun box-mullers (N)
  "Gets N N(0,1)'s - assuming N is even"
  (loop for i from 1 to (floor (/ N 2)) appending (multiple-value-list (box-muller))))

(defun save-numbers (x filename)
  "Saves some numbers to a file."
  (with-open-file (stream filename :direction :output
					    :if-exists :overwrite
					    :if-does-not-exist :create)
    (format stream "~{~f,~% ~}" x)))

(defun gbm-step (r sigma dt S Z)
  "Returns an increment of GBM"
  (* S
     (exp (+ (* (- r (* 0.5 sigma sigma)) dt)
	     (* sigma (sqrt dt) Z)))))

(defun gbm (r sigma maturity steps S &optional (i 0) (path nil) (Z nil) (dt nil))
  "Simulates a path of Geometric Brownian Motion"
  (cond ((= i steps) (reverse path))
	((null path) (let* ((dt (float (/ maturity steps)))
			    (Z (box-mullers steps))
			    (step (gbm-step r sigma dt (float S) (nth i Z))))
		       (gbm r sigma maturity steps step (1+ i) (push step path) Z dt)))
	(T (let ((step (gbm-step r sigma dt S (nth i Z))))
	     (gbm r sigma maturity steps step (1+ i) (push step path) Z dt)))))
	
(defun save-gbm-path (r sigma maturity steps S)
  (with-open-file (stream "cat.csv" :direction :output
  				    :if-exists :overwrite
  				    :if-does-not-exist :create)
    (let ((path (gbm r sigma maturity steps S))
	  (dt (/ maturity steps)))
      (loop for i from 1 to steps do
	(format stream "~F, ~F ~%" (* i dt) (nth i path))))))

;;; Code for B-S formula

(defun rough-integrate (f a b N)
  "Integrates f from a to b using N steps - roughly."
  (let ((dx (float (/ (- b a) N))))
    (loop for n from 1 to N summing (* (funcall f (+ a (* n dx))) dx))))

(defun Phi (x &optional (N 1e5))
  "A dodgy standard normal integral"
  (let ((low-enough -6))
    (rough-integrate (lambda (x) (* (/ 1 (sqrt (* 2 PI)))
				    (exp (- (* 0.5 x x)))))
		     (if (< x low-enough) (+ x low-enough) low-enough)
		     x N)))

(defun black-scholes-call (S-0 r sigma maturity K)
  "Returns the value of a Black-Scholes call"
  (let* ((sigma-sqrt-T (* sigma (sqrt maturity)))
	 (d1 (* (/ 1 sigma-sqrt-T)
		(+ (log (/ S-0 K))
		   (* (+ r (/ (* sigma sigma) 2)) maturity))))
	 (d2 (- d1 sigma-sqrt-T))
	 (df (exp (* (- r) maturity))))
    (- (* S-0 (Phi d1))
       (* K df (Phi d2)))))

;;; A generic function that will take in a payoff object (or payoff objects)

(defgeneric discounted-payoff (option path)
  (:documentation "Returns a payoff"))

;;; A class representing an option 

(defclass option ()
  ((risk-free
    :initarg :risk-free)
   (maturity
    :initarg :maturity)
   (S-0
    :initarg :S-0)
   (sigma
    :initarg :sigma)))

;; A class representing a european option

(defclass european-call (option)
  ((strike
    :initarg :strike)))

;; (make-instance 'european-call :risk-free 0.01 :maturity 1 :strike 10 :S-0 20 :sigma 0.2)

(defmethod discounted-payoff ((option european-call) path)
  "European Call Payoff"
  (with-slots (risk-free maturity strike) option
    (let ((S-T (first (last path)))
	  (df (exp (* (- risk-free) maturity))))
      (* df (max 0 (- S-T strike))))))

(defun calculate-average (x)
  "Calculates average"  
  ;; (save-numbers x "blerg.csv")
  (float (/ (apply #'+ x)
	    (length x))))

(defun get-several-discounted-payoffs (option number-of-paths steps)
  "Get several paths"
  (with-slots (risk-free maturity sigma S-0) option
    (let ((discounted-payoffs (loop for m from 1 to number-of-paths
				    collecting
				    (discounted-payoff option (gbm risk-free sigma maturity steps S-0)))))
      discounted-payoffs)))

(defun simulate-call-price (option number-of-paths steps)
  "Simulates a bunch of paths, then finds the discounted payoff. "
  (let* ((discounted-payoffs (get-several-discounted-payoffs option number-of-paths steps)))
    (calculate-average discounted-payoffs)))

;;; Now to test it

(defun run-example ()
  (let* ((S-0 20)
	 (r 0.01)
	 (K 15)
	 (sigma 0.2)
	 (maturity 5)
	 (call (make-instance 'european-call :risk-free r
					     :strike K
					     :S-0 S-0
					     :sigma sigma
					     :maturity maturity))
	 (simulated (simulate-call-price call 20000 100))
	 (exact (black-scholes-call S-0 r sigma maturity K)))
    (format t "Simulated is ~a, exact is ~a ~%" simulated exact)))

(run-example)
  
;;; Methods for a digital no-touch type option

(defclass no-touch-call (european-call)
  ((lower
    :initarg :lower)
   (upper
    :initarg :upper)))

(defmethod discounted-payoff ((option no-touch-call) path)
  "It's 1 if it stays in the bounds"
  (with-slots (lower upper) option
    (let ((stays-above (every (lambda (x) (> x lower)) path))
	  (stays-below (every (lambda (x) (< x upper)) path)))
      (* (if (and stays-above stays-below) 1 0)
	 (call-next-method))))) ; This is the key bit

(defun run-second-example ()
  (let* ((S-0 20)
	 (r 0.01)
	 (K 15)
	 (sigma 0.2)
	 (maturity 5)
	 (lower 15.0)
	 (upper 25.0)
	 (exact (black-scholes-call S-0 r sigma maturity K))
	 (no-touch-call (make-instance 'no-touch-call :risk-free r
						      :strike K
						      :S-0 S-0
						      :sigma sigma
						      :maturity maturity
						      :lower lower
						      :upper upper))
	 (simulated-no-touch-call (simulate-call-price no-touch-call 10000 100)))
    (format t "Call: Exact is ~a ~%" exact)
    (format t "No Touch Call: Simulated is ~a ~%" simulated-no-touch-call)))

(run-second-example)

;;; multimethods: basic idea

(defclass drum () ())
(defclass snare (drum) ())
(defclass cymbals (drum) ())

(defclass drumstick () ())
(defclass wooden-drumstick (drumstick) ())
(defclass mallet (drumstick) ())

(defgeneric play (drum drumstick))

(defmethod play ((drum snare) (drumstick wooden-drumstick)) (format t "~s ~%" "Rata-tat-tat"))
(defmethod play ((drum snare) (drumstick mallet)) (format t "~a ~%" "I think I broke it"))
(defmethod play ((drum cymbals) (drumstick wooden-drumstick)) (format t "~a ~%" "tssss"))
(defmethod play ((drum cymbals) (drumstick mallet)) (format t "~a ~%" "TSSSS"))

(let ((snare (make-instance 'snare))
      (cymbals (make-instance 'cymbals))
      (wooden-drumstick (make-instance 'wooden-drumstick))
      (mallet (make-instance 'mallet)))
  (list (play snare wooden-drumstick)
	(play cymbals wooden-drumstick)
	(play cymbals mallet)
	(play snare mallet)))

;;; Simple profiling

;; (time (run-second-example))

;;; Deterministic profiling

;; (profile uniform box-muller box-mullers gbm-step gbm rough-integrate discounted-payoff
;; 	 get-several-payoffs simulate-call-price)

;; (report)

;;; Statistical profiling

;; (require :sb-sprof)

;; (sb-sprof:with-profiling (:report :flat)
;;   (run-second-example))

;;; Type declarations

;; (defun poly (a b x)
;;   (+ (* a (expt x 2))
;;      (* b x)))

;; (defun poly (a b x)
;;   (declare (fixnum a b x))
;;   (+ (* a (expt x 2))
;;      (* b x)))

;; (defun poly (a b x)
;;   (declare (fixnum a b x))
;;   (the fixnum (+ (the fixnum (* a (the fixnum (expt x 2))))
;; 		 (the fixnum (* b x)))))

