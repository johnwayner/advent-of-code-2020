(defpackage :com.johnwayner.aoc-20.day-11
  (:use
   :cl))

(in-package :com.johnwayner.aoc-20.day-11)

(declaim (optimize (safety 3) (debug 3)))

(defparameter +input-sample-1+
  "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")


(defun read-seat-map (stream)
  (loop with seat-array = (make-array '(1 1) :initial-element :floor :adjustable t)
        for line = (read-line stream nil nil)
        for row-index from 0
        while line
        do (adjust-array seat-array (list (1+ row-index) (length line)))
        do (loop for seat across line
                 for col-index from 0
                 do (setf (aref seat-array row-index col-index)
                          (case seat
                            (#\L :empty)
                            (#\# :occupied)
                            (t :floor))))
        finally (return seat-array)))

(defun seat-neighbors (seat-array row col)
  (loop with (max-row max-col) = (array-dimensions seat-array)
        for r from (1- row) to (1+ row)
        nconc (loop for c from (1- col) to (1+ col)
                    when (and (>= r 0)
                              (< r max-row)
                              (>= c 0)
                              (< c max-col)
                              (not (and (= r row)
                                        (= c col))))
                      collect (aref seat-array r c))))

(defun seat-neighbors-los (seat-array row col)
  (loop with (max-row max-col) = (array-dimensions seat-array)
        for rdelta from -1 to 1
        nconc (loop for cdelta from -1 to 1
                    unless (= 0 rdelta cdelta)
                      nconc (loop for r = row then (+ r rdelta)
                                  for c = col then (+ c cdelta)
                                  while (and (< r max-row)
                                             (< c max-col)
                                             (>= r 0)
                                             (>= c 0))
                                  when (and
                                        (not (and (= r row)
                                                  (= c col)))
                                        (not (eql :floor (aref seat-array r c))))
                                    return (list (aref seat-array r c))))))


;; Part 1 rules
;; If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
;; If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
;; Otherwise, the seat's state does not change.
(defvar *NEIGHBOR-FN* #'seat-neighbors)
(defvar *NUM-TOLERATED* 4)
(defun next-seat (seat seat-array row col &optional
                                            (neighbor-fn (or *NEIGHBOR-FN* #'seat-neighbors))
                                            (num-tolerated (or *NUM-TOLERATED* 4)))
  (let* ((adjacent-seats (funcall neighbor-fn seat-array row col))
         (num-occupied (count :occupied adjacent-seats)))
   (case seat
    (:floor :floor)
    (:empty (if (= 0 num-occupied)
                :occupied
                :empty))
    (:occupied (if (>= num-occupied num-tolerated)
                   :empty
                   :occupied)))))

(defun step-map (seat-array)
  (loop with new-seat-array = (make-array (array-dimensions seat-array))
        with change-count = 0
        for row from 0 to (1- (array-dimension seat-array 0))
        do (loop for col from 0 to (1- (array-dimension seat-array 1))
                 for seat = (aref seat-array row col)
                 for next-value = (next-seat seat seat-array row col)
                 do (setf (aref new-seat-array row col)
                          next-value)
                 when (not (eql seat next-value))
                   do (incf change-count))
        finally (return (list new-seat-array change-count))))

(defun step-map-stable (stream)
  (loop for (seat-array num-changes) = (list (read-seat-map stream) 1)
          then (step-map seat-array)
        until (= 0 num-changes)
        finally (return (loop with (max-row max-col) = (array-dimensions seat-array)
                              for r from 0 below max-row
                              sum (loop for c from 0 below max-col
                                        for seat = (aref seat-array r c)
                                        count (eql seat :occupied))))))

(defun run-part-1 ()
  (with-open-file (input #P"input-day11.txt")
    (step-map-stable input)))

(defun run-part-2 ()
  (with-open-file (input #P"input-day11.txt")
    (let ((*NEIGHBOR-FN* #'seat-neighbors-los)
          (*NUM-TOLERATED* 5))
      (step-map-stable input))))
