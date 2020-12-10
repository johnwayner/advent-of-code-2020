(defpackage :com.johnwayner.aoc-20.day-9
  (:use
   :cl))

(in-package :com.johnwayner.aoc-20.day-9)

(declaim (optimize (safety 3) (debug 3)))

(defparameter *input-sample-part-1* "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")

(defun find-first-non-sum (input-stream window-size)
  "Read stream of numbers returning the first not having two previous numbers within window-size summing to it."
  (loop with prevs = nil
        for n = (read input-stream nil nil)
        while n
        if (and (= (length prevs) window-size)
                (not (loop for pn in prevs
                             thereis (member (- n pn) (remove pn prevs)))))
          return n
        do (setf prevs (subseq (cons n prevs) 0 (min (1+ (length prevs)) window-size)))))

(defun run-part-1 ()
  (with-open-file (input #P"input-day9.txt") (find-first-non-sum input 25)))

(defclass queue ()
  ((list :initform nil :reader get-list)
   (tail :initform nil)))

(defmethod print-object ((queue queue) stream)
  (print-unreadable-object (queue stream)
    (format stream "~{~a~^, ~}" (get-list queue))))

(defmethod enqueue ((queue queue) item)
  (with-slots (list tail) queue
    (let ((new-tail (cons item nil)))
      (if tail
          (setf (cdr tail) new-tail)
          (setf list new-tail))
      (setf tail new-tail))))

(defmethod dequeue ((queue queue))
  (with-slots (list tail) queue
      (when (eq list tail)
        (setf tail nil))
    (pop list)))

(defmethod peek ((queue queue))
  (car (get-list queue)))

(defclass summing-queue (queue)
  ((sum :initform 0 :reader sum)))

(defmethod enqueue :before ((queue summing-queue) item)
  (when (numberp item)
    (incf (slot-value queue 'sum) item)))

(defmethod dequeue :before ((queue summing-queue))
  (with-slots (sum) queue
    (let ((item (peek queue)))
      (when (numberp item)
        (decf sum item)))))


(defun find-contiguous-sums (input-stream target-number)
  "Read stream of numbers returning list contianing the first consecutive numbers summing to target-number."
  (loop with prev-q = (make-instance 'summing-queue)
        for n = (read input-stream nil nil)
        while n
        do (enqueue prev-q n)
        if (> (sum prev-q) target-number)
          do (loop do (dequeue prev-q)
                   while (> (sum prev-q) target-number))
        if (and (< 1 (length (get-list prev-q)))
                (= target-number (sum prev-q)))
          return (get-list prev-q)))

(defun run-part-2 ()
  (with-open-file (input #P"input-day9.txt")
    (let* ((contiguous-sums (find-contiguous-sums input (run-part-1)))
           (min-max (loop for n in contiguous-sums
                          maximizing n into max
                          minimizing n into min
                          finally (return (list min max)))))
      (apply #'+ min-max))))
