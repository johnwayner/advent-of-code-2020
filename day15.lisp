(defpackage :com.johnwayner.aoc-20.day-15
  (:use
   :cl))

(in-package :com.johnwayner.aoc-20.day-15)

(declaim (optimize (safety 3) (debug 3)))

(defparameter +input-sample-1+ '(1 3 2))
(defparameter +input-part-1+ '(12 20 0 6 1 17 7))

(defun play-game-part-1 (start-list num-iterations)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type fixnum num-iterations))
  (let ((prev-iter-for-num (make-hash-table)))
    (loop for (s but-one) on start-list
	  for i from 1
	  while but-one
	  do (setf (gethash s prev-iter-for-num) i))
    (loop with prev = (car (last start-list))
	  for iter from (length start-list) below num-iterations
	  for iter-for-prev = (gethash prev prev-iter-for-num 0)
	  do (setf (gethash prev prev-iter-for-num) iter)
;	  do (format t "~a ~a ~a~%" iter prev iter-for-prev)
	  do (setf prev (if (> (the fixnum iter-for-prev) 0)
			    (- iter iter-for-prev)
			    0))
	  finally (return prev))))

(defun run-part-1 ()
  (play-game-part-1 +input-part-1+ 2020))

(defun run-part-2 ()
  (play-game-part-1 +input-part-1+ 30000000))
