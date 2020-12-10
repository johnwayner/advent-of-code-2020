(defpackage :com.johnwayner.aoc-20.day-6
  (:use :cl))

(in-package :com.johnwayner.aoc-20.day-6)

(declaim (optimize (safety 3)))


(defvar *input-sample-1* "abc

a
b
c

ab
ac

a
a
a
a

b")

(defun run-part-1 (&optional (input (make-string-input-stream *input-sample-1*)))
  (loop with h = (make-hash-table)
        for line = (read-line input nil)
        if (= 0 (length line))
          sum (hash-table-count h)
          and do (setf h (make-hash-table))
        else
          do (loop for c across line do (setf (gethash c h) t))
        while line))


(defun run-part-2 (&optional (input (make-string-input-stream *input-sample-1*)))
  (loop with h = (make-hash-table)
        for line-count from 0
        for line = (read-line input nil)
        if (= 0 (length line))
          sum (loop for v being each hash-value in h when (= line-count v) sum 1)
          and do (setf h (make-hash-table))
                 (setf line-count -1)
        else
          do (loop for c across line do (incf (gethash c h 0)))
        while line))
