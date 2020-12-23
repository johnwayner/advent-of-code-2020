(defpackage :com.johnwayner.aoc-20.day-22
  (:use :cl))

(in-package :com.johnwayner.aoc-20.day-22)
;;;(declaim (optimize (speed 3)))
(declaim (optimize (safety 3) (debug 3)))


(defparameter +input-part-1-player-1+ '(27 29 30 44 50 5 33 47 34 38 36 4 2 18 24 16 32 21 17 9 3 22 41 31 23))
(defparameter +input-part-1-player-2+ '(25 1 15 46 6 13 20 12 10 14 19 37 40 26 43 11 48 45 49 28 35 7 42 39 8))

(defclass queue ()
  ((list :initform nil :reader get-list)
   (tail :initform nil)
   (size :initform 0 :reader size)))

(defmethod print-object ((queue queue) stream)
  (print-unreadable-object (queue stream)
    (format stream "Count: ~a:~{ ~a~^,~}" (size queue) (get-list queue))))

(defmethod enqueue ((queue queue) &rest items)
  (dolist (item items queue)
    (with-slots (list tail size) queue
      (let ((new-tail (cons item nil)))
        (if tail
            (setf (cdr tail) new-tail)
            (setf list new-tail))
        (setf tail new-tail)
        (incf size)))))

(defmethod dequeue ((queue queue))
  (with-slots (list tail size) queue
      (when (eq list tail)
        (setf tail nil))
    (when list
      (decf size))
    (pop list)))

(defmethod peek ((queue queue))
  (car (get-list queue)))

(defmethod emptyp ((queue queue))
  (= 0 (size queue)))

(defun make-queue (&optional initial-contents)
  (let ((q (make-instance 'queue)))
    (dolist (item initial-contents q)
      (enqueue q item))))

(defun play-game (player-1 player-2)
  (let ((deck-1 (make-queue player-1))
        (deck-2 (make-queue player-2)))
    (loop until (or (emptyp deck-1)
                    (emptyp deck-2))
          for card-1 = (dequeue deck-1)
          for card-2 = (dequeue deck-2)
          if (>= card-1 card-2)
            do (enqueue deck-1 card-1 card-2)
          else
            do (enqueue deck-2 card-2 card-1))
    (get-list
     (if (emptyp deck-1)
         deck-2
         deck-1))))

(defun score-deck (deck)
  (loop for card in deck
        for value downfrom (length deck)
        sum (* value card)))

;;;(score-deck (play-game '(9 2 6 3 1) '(5 8 4 7 10)))

(defun run-part-1 ()
  (score-deck (play-game +input-part-1-player-1+
                         +input-part-1-player-2+)))

;;;(run-part-1)  ; => 32162 (15 bits, #x7DA2)

(defclass repeat-checking-queue (queue)
  ((hash-history :initform nil)))

(defmethod check-repeat ((queue repeat-checking-queue))
  (with-slots (list hash-history) queue
  (let ((current-hash (sxhash list)))
    (if (member current-hash hash-history)
        t
        (progn
          (push current-hash hash-history)
          nil)))))

(defun make-repeat-checking-queue (&optional initial-contents)
  (let ((q (make-instance 'repeat-checking-queue)))
    (dolist (item initial-contents q)
      (enqueue q item))))


(defun play-recursive-game (player-1 player-2)
  (let ((deck-1 (make-repeat-checking-queue player-1))
        (deck-2 (make-repeat-checking-queue player-2)))

    (flet ((winner (card-1 card-2 deck-1 deck-2)
             (if (and (>= (size deck-1) card-1)
                      (>= (size deck-2) card-2))
                 (play-recursive-game (subseq (get-list deck-1) 0 card-1)
                                      (subseq (get-list deck-2) 0 card-2))
                 (if (>= card-1 card-2)
                     :player-1
                     :player-2))))

      (loop until (or (emptyp deck-1)
                      (emptyp deck-2))

            for card-1 = (dequeue deck-1)
            for card-2 = (dequeue deck-2)

            when (or (check-repeat deck-1)
                     (check-repeat deck-2))
              return :player-1

            if (eql :player-1 (winner card-1 card-2 deck-1 deck-2))
              do (enqueue deck-1 card-1 card-2)
            else
              do (enqueue deck-2 card-2 card-1)))
    (values (if (emptyp deck-1)
                :player-2
                :player-1)
            (get-list
             (if (emptyp deck-1)
                 deck-2
                 deck-1)))))

(defun run-part-2 ()
  (multiple-value-bind (winner final-deck)
      (play-recursive-game +input-part-1-player-1+
                           +input-part-1-player-2+)
    
    (declare (ignore winner))
    (score-deck final-deck)))

;;;(run-part-2)   ; => 32534 (15 bits, #x7F16)
