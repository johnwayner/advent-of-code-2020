(defpackage :com.johnwayner.aoc-20.day-13
  (:use
   :cl))

(in-package :com.johnwayner.aoc-20.day-13)

(declaim (optimize (safety 3) (debug 3)))


(defparameter +input-sample-1+ "939
7,13,x,x,59,x,31,19")

(defparameter +input-part-1+ "1002561
17,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,409,x,29,x,x,x,x,x,x,x,x,x,x,13,x,x,x,x,x,x,x,x,x,23,x,x,x,x,x,x,x,373,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,19")

(defun find-earliest-train (ready-time trains)
  (loop with earliest-train = nil
        for train in trains
        for wait-time = (- train (mod ready-time train))
        minimize wait-time into min-wait-time
        when (= wait-time min-wait-time)
          do (setf earliest-train train)
        finally (return (list earliest-train min-wait-time))))

(defun parse-trains (train-string &key ((:allow-nil allow-nil) nil))
  (loop for start-index = 0 then (1+ next-comma-index)
        for next-comma-index = (position #\, train-string :start start-index)
        for train = (parse-integer train-string :start start-index
                                                :end (or next-comma-index (length train-string))
                                                :junk-allowed t)
        when (or train allow-nil)
          collect train
        while next-comma-index))

(defun run-part-1 ()
  (with-input-from-string (input +input-part-1+)
    (let* ((ready-time (read input))
           (train-string (read-line input))
           (trains (parse-trains train-string)))
      (apply #'* (find-earliest-train ready-time trains)))))

(defun chinese-remainder (remainders divisors)
  (let* ((big-n (apply #'* divisors))
         (big-x (loop for bi in remainders
                      for di in divisors
                      for ni = (/ big-n di)
                      for xi = (loop for x from 1 when (= 1 (mod (* x ni) di)) return x)
                      sum (* bi ni xi))))
    (mod big-x big-n)))

(defun run-part-2 (&optional (input-string +input-part-1+))
  (with-input-from-string (input input-string)
    (read-line input) ;eat unused line in second part
    (let* ((trains (parse-trains (read-line input) :allow-nil t))
           (rs-and-ds (loop for train in trains
                            for i from 0
                            when train
                              collect train into ds
                              and collect (- train i) into rs
                            finally (return (list rs ds)))))
      (chinese-remainder (first rs-and-ds)
                         (second rs-and-ds)))))
