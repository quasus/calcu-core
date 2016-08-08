;;;; calcu-core.lisp

(in-package #:calcu-core)

(defparameter *default-capacity* 10)

(defstruct (display-buffer (:constructor make-display-buffer (capacity)))
  capacity
  (buffer (make-array (1+ capacity)
		      :element-type 'base-char
		      :fill-pointer 0))
  minusp)

(defun display-buffer-empty-p (buf)
  (zerop (length (display-buffer-buffer buf))))

(defun display-buffer-reset (buf)
  (setf (fill-pointer (display-buffer-buffer buf)) 0
	(display-buffer-minusp buf) nil))

(defun display-buffer-push (char buf)
  (let ((str (display-buffer-buffer buf)))
    (cond ((digit-char-p char)
	   (cond ((zerop (length str))
		  (vector-push char str))
		 ((string= str "0")
		  (setf (char str 0) char))
		 ((< (count-if #'digit-char-p str) (display-buffer-capacity buf))
		  (vector-push char str))))
	  ((char= char #\.)
	   (cond ((zerop (length str))
		  (vector-push #\0 str)
		  (vector-push #\. str))
		 ((not (find char (display-buffer-buffer buf)))
		  (vector-push #\. str)))))))

(defun display-buffer-value (buf)
  (if (display-buffer-empty-p buf)
      0.0d0
      (let ((abs (coerce (let ((*read-default-float-format* 'double-float))
			   (read-from-string (format nil "~A" (display-buffer-buffer buf))))
			 'double-float)))
	(if (display-buffer-minusp buf)
	    (- abs)
	    abs))))

(defun display-buffer-negate (buf)
  (setf (display-buffer-minusp buf) (not (display-buffer-minusp buf))))

(defstruct (calculator (:constructor make-calculator (&optional (capacity *default-capacity*))))
  capacity
  (x 0.0d0)
  (y 0.0d0)
  flag
  flag2
  (mem 0.0d0)
  error-p
  (buffer (make-display-buffer capacity)))

(defun admissible-number-p (number calc)
  (<= (abs number) (1- (expt 10 (calculator-capacity calc)))))

(defun sync-buffer-x (calc)
  (setf (calculator-x calc) (display-buffer-value (calculator-buffer calc))))

(defun feed-char (char calc)
  (unless (calculator-error-p calc)
    (and (display-buffer-push char (calculator-buffer calc))
       (sync-buffer-x calc))))

(defun feed-pm (calc)
  (unless (calculator-error-p calc)
    (if (display-buffer-empty-p (calculator-buffer calc))
	(setf (calculator-x calc) (- (calculator-x calc)))
	(progn
	  (display-buffer-negate (calculator-buffer calc))
	  (sync-buffer-x calc)))))

(defun feed-binary-operation (operation calc)
  (unless (calculator-error-p calc)
    (compute calc nil t)
    (setf (calculator-flag calc) operation
	  (calculator-flag2 calc) operation
	  (calculator-y calc) (calculator-x calc))
    (display-buffer-reset (calculator-buffer calc))))

(defun reset-calculator (calc)
  (setf (calculator-error-p calc) nil
	(calculator-x calc) 0
	(calculator-y calc) 0
	(calculator-flag calc) nil
	(calculator-flag2 calc) nil)
  (display-buffer-reset (calculator-buffer calc)))

(defun throw-error (calc)
  (reset-calculator calc)
  (setf (calculator-error-p calc) t))

(defun unary-operation->function (operation)
  (second (assoc operation *unary-operations*)))

(defun feed-unary-operation (operation calc)
  (unless (calculator-error-p calc)
    (let ((result (ignore-errors
		    (funcall (unary-operation->function operation)
			     (calculator-x calc)))))
      (cond ((and result (admissible-number-p result calc))
	     (setf (calculator-x calc) result)
	     (reset-input calc))
	    (t (throw-error calc))))))

(defparameter *unary-operations*
  `((sqrt ,(lambda (x)
		   (assert (>= x 0))
		   (sqrt x)))))

(defparameter *normal-binary-operations*
  `((+ ,#'+)
    (- ,#'-)
    (* ,#'*)
    (/ ,#'/)))

(defparameter *percent-binary-operations*
  `((* ,(lambda (y x)
		(* x y 0.01d0)))
    (/ ,(lambda (y x)
		(/ y x 0.01d0)))
    (+ ,(lambda (y x)
		(+ y (* y x 0.01d0))))
    (- ,(lambda (y x)
		(- y (* y x 0.01d0))))))

(defun operation->function (operation percentp)
  (second (assoc operation (if percentp
			       *percent-binary-operations*
			       *normal-binary-operations*))))

(defun compute (calc repeatp save-operation-p &optional percentp)
  (unless (calculator-error-p calc)
    (let ((operation (if repeatp
			 (or (calculator-flag calc)
			     (calculator-flag2 calc))
			 (calculator-flag calc))))
      (if operation
	  (let ((result (ignore-errors
			  (funcall (operation->function operation percentp)
				   (calculator-y calc)
				   (calculator-x calc)))))
	    (cond ((and result (admissible-number-p result calc))
		   (setf (calculator-x calc) result)
		   (unless save-operation-p
		     (setf (calculator-flag calc) nil))
		   (reset-input calc))
		  (t (throw-error calc))))
	  (reset-input calc)))))

(defun reset-input (calc)
  (display-buffer-reset (calculator-buffer calc)))

(defun mr (calc)
  (unless (calculator-error-p calc)
    (setf (calculator-x calc)
	  (calculator-mem calc))
    (reset-input calc)))

(defun m (calc)
  (unless (calculator-error-p calc)
    (setf (calculator-mem calc)
	  (calculator-x calc))
    (reset-input calc)))

(defun mc (calc)
  (unless (calculator-error-p calc)
    (setf (calculator-mem calc) 0.0d0)
    (reset-input calc)))

(defun m+ (calc)
  (unless (calculator-error-p calc)
    (let ((new-memory (+ (calculator-mem calc)
			 (calculator-x calc))))
      (if (admissible-number-p new-memory calc)
	  (setf (calculator-mem calc) new-memory)
	  (throw-error calc)))
    (reset-input calc)))

(defun m- (calc)
  (unless (calculator-error-p calc)
    (let ((new-memory (- (calculator-mem calc)
			 (calculator-x calc))))
      (if (admissible-number-p new-memory calc)
	  (setf (calculator-mem calc) new-memory)
	  (throw-error calc)))
    (reset-input calc)))

(defun c (calc)
  (unless (calculator-error-p calc)
    (setf (calculator-x calc) 0)
    (display-buffer-reset (calculator-buffer calc))))

(defun displayed-value (calc)
  (if (display-buffer-empty-p (calculator-buffer calc))
      (values (string-right-trim "0" (format nil (format nil "~~~DF" (1+ (calculator-capacity calc)))
					     (abs (calculator-x calc))))
	      (minusp (calculator-x calc)))
      (values (copy-seq (display-buffer-buffer (calculator-buffer calc)))
	      (display-buffer-minusp (calculator-buffer calc)))))

(defparameter *text-commands*
  `((#\0 ,(lambda (calc) (feed-char #\0 calc)))
    (#\1 ,(lambda (calc) (feed-char #\1 calc)))
    (#\2 ,(lambda (calc) (feed-char #\2 calc)))
    (#\3 ,(lambda (calc) (feed-char #\3 calc)))
    (#\4 ,(lambda (calc) (feed-char #\4 calc)))
    (#\5 ,(lambda (calc) (feed-char #\5 calc)))
    (#\6 ,(lambda (calc) (feed-char #\6 calc)))
    (#\7 ,(lambda (calc) (feed-char #\7 calc)))
    (#\8 ,(lambda (calc) (feed-char #\8 calc)))
    (#\9 ,(lambda (calc) (feed-char #\9 calc)))
    (#\. ,(lambda (calc) (feed-char #\. calc)))
    (#\! ,(lambda (calc) (feed-pm calc)))
    (#\+ ,(lambda (calc) (feed-binary-operation '+ calc)))
    (#\- ,(lambda (calc) (feed-binary-operation '- calc)))
    (#\* ,(lambda (calc) (feed-binary-operation '* calc)))
    (#\/ ,(lambda (calc) (feed-binary-operation '/ calc)))
    (#\= ,(lambda (calc) (compute calc t nil)))
    (#\% ,(lambda (calc) (compute calc t nil t)))
    (#\a ,(lambda (calc) (reset-calculator calc)))
    (#\c ,(lambda (calc)
		  (setf (calculator-x calc) 0)
		  (display-buffer-reset (calculator-buffer calc))))
    (#\y ,(lambda (calc) (m calc)))
    (#\p ,(lambda (calc) (mr calc)))
    (#\u ,(lambda (calc) (m+ calc)))
    (#\i ,(lambda (calc) (m- calc)))
    (#\o ,(lambda (calc) (mc calc)))
    (#\s ,(lambda (calc) (feed-unary-operation 'sqrt calc)))))

(defun display-string (calc)
  (let ((hline (make-string (+ 6 (calculator-capacity calc)) :initial-element #\-))
	(body-line (make-string (+ 6 (calculator-capacity calc)) :initial-element #\Space)))
    (multiple-value-bind (number minusp) (displayed-value calc)
      (unless (zerop (calculator-mem calc))
	(setf (char body-line 0) #\M))
      (when (calculator-error-p calc)
	(setf (char body-line 1) #\E))
      (when minusp
	(setf (char body-line 2) #\-))
      (if (find #\. number)
	  (replace body-line number :start1 (- (length body-line) (length number)))
	  (progn
	    (replace body-line number :start1 (- (length body-line) (length number) 1))
	    (setf (char body-line (1- (length body-line))) #\.)))
      (format nil "~&~A~%~A~%~A~%" hline body-line hline))))

(defun text-calculator (&optional (capacity *default-capacity*))
  (let ((calc (make-calculator capacity)))
    (write-line (display-string calc) *query-io*)
    (finish-output *query-io*)
    (loop for line = (read-line *query-io* nil)
       while (and line (plusp (length line)))
       do (loop for c across line
	     for fun = (second (assoc c *text-commands*))
	     when fun
	     do (funcall fun calc)
	     finally (write-line (display-string calc) *query-io*)
	     finally (finish-output *query-io*)))))
