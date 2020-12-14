(defpackage :com.johnwayner.aoc-20.day-14
  (:use
   :cl))

(in-package :com.johnwayner.aoc-20.day-14)

(declaim (optimize (safety 3) (debug 3)))

(defparameter +input-sample-1+ "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0")

(defun read-string (string)
  (with-input-from-string (input string)
    (read input)))

(defun read-eval-part-1 (&optional input-stream)
  (loop with memory = (make-hash-table)
        with and-mask = #xFFFFFFFFF
        with or-mask = 0
        for line = (read-line input-stream nil nil)
        while line
        for space-index = (position #\Space line)
        for instruction = (subseq line 0 space-index)
        for value = (subseq line (+ 3 space-index))
        if (search "mask" instruction)
          do (setf and-mask (read-string (concatenate 'string "#b" (substitute #\1 #\X value)))
                   or-mask (read-string (concatenate 'string "#b" (substitute #\0 #\X value))))
        else
          do (let* ((mem-loc (parse-integer line :start 4 :end (position #\] line)))
                    (value-int (parse-integer value))
                    (and-masked-value (logand and-mask value-int))
                    (masked-value (logior or-mask and-masked-value)))
               (setf (gethash mem-loc memory) masked-value))
        finally (return (loop for v being each hash-value in memory sum v))))

(defun run-part-1 ()
  (with-open-file (input #P"input-day14.txt")
    (read-eval-part-1 input)))

(defun masked-addresses (mask address)
  "Returns list of integer addresses after apply mask"
  (declare (type string mask)
           (type fixnum address))
  (loop with addresses = (list address)
        for mask-char across (reverse mask)
        for bit-value = 1 then (* 2 bit-value)
        do (case mask-char
             (#\1 (setf addresses (mapcar (lambda (a) (logior bit-value a))
                                          addresses)))
             (#\X (setf addresses
                        (loop for a in addresses
                              collect (logior a bit-value)
                              collect (logand a (logxor bit-value #xFFFFFFFFF))))))
        finally (return addresses)))



(defun read-eval-part-2 (&optional input-stream)
  (loop with memory = (make-hash-table)
        with mask = ""
        for line = (read-line input-stream nil nil)
        while line
        for space-index = (position #\Space line)
        for instruction = (subseq line 0 space-index)
        for value = (subseq line (+ 3 space-index))
        if (search "mask" instruction)
          do (setf mask value)
        else
          do (let* ((mem-loc (parse-integer line :start 4 :end (position #\] line)))
                    (mem-locs (masked-addresses mask mem-loc))
                    (value-int (parse-integer value)))
               (dolist (masked-mem-loc mem-locs)
                 (setf (gethash masked-mem-loc memory) value-int)))
        finally (return (loop for v being each hash-value in memory sum v))))

(defun run-part-2 ()
  (with-open-file (input #P"input-day14.txt")
    (read-eval-part-2 input)))
