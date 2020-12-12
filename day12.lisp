(defpackage :com.johnwayner.aoc-20.day-12
  (:use
   :cl))

(in-package :com.johnwayner.aoc-20.day-12)

(declaim (optimize (safety 3) (debug 3)))

(defparameter +input-sample-1+ "F10
N3
F7
R90
F11")

;; Action N means to move north by the given value.
;; Action S means to move south by the given value.
;; Action E means to move east by the given value.
;; Action W means to move west by the given value.
;; Action L means to turn left the given number of degrees.
;; Action R means to turn right the given number of degrees.
;; Action F means to move forward by the given value in the direction the ship is currently facing.

(defun read-eval-instructions-1 (stream)
  (let ((facing 90)
        (ns-delta 0)
        (ew-delta 0))
    (labels ((eval-instruction (instruction magnitude)
               (case instruction
                 (#\N (incf ns-delta magnitude))
                 (#\S (decf ns-delta magnitude))
                 (#\E (incf ew-delta magnitude))
                 (#\W (decf ew-delta magnitude))
                 (#\L (decf facing magnitude))
                 (#\R (incf facing magnitude))
                 (#\F (eval-instruction
                       (case (mod facing 360)
                         (0 #\N)
                         (90 #\E)
                         (180 #\S)
                         (270 #\W))
                       magnitude)))))
      (loop for line = (read-line stream nil nil)
            while line
            for instruction = (elt line 0)
            for magnitude = (parse-integer line :start 1)
            do (eval-instruction instruction magnitude))
      (+ (abs ns-delta) (abs ew-delta)))))

(defun run-part-1 ()
  (with-open-file (input #P"input-day12.txt")
    (read-eval-instructions-1 input)))

;; Action N means to move the waypoint north by the given value.
;; Action S means to move the waypoint south by the given value.
;; Action E means to move the waypoint east by the given value.
;; Action W means to move the waypoint west by the given value.
;; Action L means to rotate the waypoint around the ship left (counter-clockwise) the given number of degrees.
;; Action R means to rotate the waypoint around the ship right (clockwise) the given number of degrees.
;; Action F means to move forward to the waypoint a number of times equal to the given value.

(defun read-eval-instructions-2 (stream)
  (let ((wp-ns-delta 1)
        (wp-ew-delta 10)
        (ns-delta 0)
        (ew-delta 0))
    (labels ((negate (x) (* -1 x))

             (set-wp (ns ew)
               (setf wp-ns-delta ns)
               (setf wp-ew-delta ew))

             (rotate (magnitude dir)
               (dotimes (ignored (abs (truncate (/ magnitude 90))))
                 (case dir
                   (:cw (set-wp (negate wp-ew-delta) wp-ns-delta))
                   (:ccw (set-wp wp-ew-delta (negate wp-ns-delta))))))

             (forward (magnitude)
               (incf ns-delta (* magnitude wp-ns-delta))
               (incf ew-delta (* magnitude wp-ew-delta)))

             (eval-instruction (instruction magnitude)
               (case instruction
                 (#\N (incf wp-ns-delta magnitude))
                 (#\S (decf wp-ns-delta magnitude))
                 (#\E (incf wp-ew-delta magnitude))
                 (#\W (decf wp-ew-delta magnitude))
                 (#\L (rotate magnitude :ccw))
                 (#\R (rotate magnitude :cw))
                 (#\F (forward magnitude)))))

      (loop for line = (read-line stream nil nil)
            while line
            for instruction = (elt line 0)
            for magnitude = (parse-integer line :start 1)
            do (eval-instruction instruction magnitude))
      (+ (abs ns-delta) (abs ew-delta)))))

(defun run-part-2 ()
  (with-open-file (input #P"input-day12.txt")
    (read-eval-instructions-2 input)))
