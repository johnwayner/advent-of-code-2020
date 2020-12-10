(defpackage :com.johnwayner.aoc-20.day-10
  (:use
   :cl))

(in-package :com.johnwayner.aoc-20.day-10)

(declaim (optimize (safety 3) (debug 3)))

(defparameter +input-sample-1-1+ '(16 10 15 5 1 11 7 19 6 12 4))
(defparameter +input-sample-1-2+ '(28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1
                                   32 25 35 8 17 7 9 4 2 34 10 3))

(defparameter +input-part-1+
  (with-open-file (input "input-day10.txt")
    (loop for line = (read input nil nil)
          while line
          collect line)))

(defun run-part-1 (input-seq)
  (loop for (a b) on (append '(0) (sort (copy-seq input-seq) #'<))
        while (and a b)
        for diff = (- b a)
        count (= 1 diff) into single-diffs
        count (= 3 diff) into triple-diffs
        finally (return (* single-diffs (1+ triple-diffs)))))

(defun run-part-2 (input-seq)
  (loop with cur-mult = 1
        with total = 1
        for (a b c) on (sort (copy-seq input-seq) #'<)
        while b
        if (= a (1+ b) (+ 2 c))
          do (incf cur-mult cur-mult)
        else
          do (setf total (* total cur-mult))
          and do (setf cur-mult 1)
        finally (return (* cur-mult total))))

(defun run-part-2-d ()
  (loop for (a b c d) on (sort (copy-seq +input-sample-1-1+) #'>)
        when (and b (<= (- a b) 2))
          collect (list a b)
        when (and c (<= (- a c) 2))
          collect (list a c)
        when (and d (<= (- a d) 2))
          collect (list a d)))
