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
  (apply #'*
         (loop for (a b) on (sort (copy-seq input-seq) #'<)
               with ones = 1
               while a
               if (and b (= 1 (- b a)))
                 do (incf ones)
               else
                 collect (case ones
                           (0 1)
                           (1 1)
                           (2 2)
                           (3 4)
                           (4 7)
                           (t :x))
                 and do (setf ones 0))))

(defun validp (seq)
  (not
   (loop for (a b) on seq
         while b
           thereis (> (- b a) 3))))
