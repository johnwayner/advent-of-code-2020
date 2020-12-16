(defpackage :com.johnwayner.aoc-20.day-16
  (:use :cl))

(in-package :com.johnwayner.aoc-20.day-16)

(declaim (optimize (safety 3) (debug 3)))

(defclass field-range ()
  ((lowest :initarg :lowest :reader lowest)
   (highest :initarg :highest :reader highest)))

(defun make-field-range (lowest highest)
  (make-instance 'field-range
                 :lowest lowest
                 :highest highest))

(defclass field ()
  ((name :initarg :name :reader name)
   (ranges :initarg :ranges :reader ranges :type list)))

(defun make-field (name ranges)
  (make-instance 'field
                 :name name
                 :ranges ranges))

(defclass ticket ()
  ((fields :initarg :fields :reader fields)
   (field-values :initarg :field-values :reader field-values)))

(defun make-ticket (fields values)
  (make-instance 'ticket
                 :fields fields
                 :field-values values))

(defmethod valid-value-p ((field-range field-range) value)
  (with-slots (lowest highest) field-range
    (<= lowest value highest)))

(defmethod valid-value-p ((field field) value)
  (loop for r in (ranges field)
	when (valid-value-p r value)
	  return t))

(defmethod collect-invalid-field-values ((ticket ticket))
  (with-slots (fields field-values) ticket
    (loop for v in field-values
          unless (some (lambda (field) (valid-value-p field v))
                       fields)
            collect v)))


(defparameter +input-sample-1+ "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12")

(defun parse-range (string)
  (let* ((dash-pos (position #\- string))
         (lowest (parse-integer string :end dash-pos))
         (highest (parse-integer string :start (1+ dash-pos))))
    (make-field-range lowest highest)))

(defun read-fields (stream)
  (loop for line = (read-line stream nil nil)
        while line
        until (= 0 (length line))
        for colon-pos = (position #\: line)
        for or-pos = (search " or " line)
        for name = (subseq line 0 colon-pos)
        for range-1 = (parse-range (subseq line (1+ colon-pos) or-pos))
        for range-2 = (parse-range (subseq line (+ 4 or-pos)))
        collect (make-field name (list range-1 range-2))))

(defun parse-csv-to-list (csv)
  (let* ((space-sep-string (substitute #\Space #\, csv))
         (listified-string (concatenate 'string "(" space-sep-string ")")))
    (with-input-from-string (input listified-string)
      (read input))))

;(parse-csv-to-list "1,2,4")

(defun read-tickets (stream fields)
  (loop for line = (read-line stream nil nil)
        while line
        when (not (or (= 0 (length line))
                      (search "ticket" line)))
          collect (make-ticket fields
                               (parse-csv-to-list line))))

(defun read-input (stream)
  (let* ((fields (read-fields stream))
         (tickets (read-tickets stream fields)))
    tickets))

(defun read-invalid-field-values (stream)
  (let ((tickets (read-input stream)))
    (loop for ticket in (cdr tickets) ;skip our ticket
          nconc (collect-invalid-field-values ticket))))

(defun run-part-1-test ()
  (with-input-from-string (input +input-sample-1+)
    (read-invalid-field-values input)))

(defun run-part-1 ()
  (with-open-file (input #P"input-day16.txt")
    (loop for error in (read-invalid-field-values input)
          sum error)))

;;;(run-part-1) ; => 25961 (15 bits, #x6569)

(defun all-values-match-field (field tickets index)
  (every (lambda (ticket)
           (valid-value-p field
                          (nth index (field-values ticket))))
         tickets))

(defun find-valid-value-indexes (field tickets)
  (loop for index from 0 below (length (field-values (car tickets)))
        when (all-values-match-field field tickets index)
          collect index))

(defun determine-field-order (tickets)
  (let* ((valid-indexes (loop with valid-tickets = (remove-if #'collect-invalid-field-values tickets)
                              for field in (fields (car valid-tickets))
                              collect (cons field (find-valid-value-indexes field valid-tickets))))
         (sorted-valid-indexes (sort valid-indexes #'< :key (lambda (x) (length (cdr x))))))
    (loop with assigned-indexes = '()
          for (field . indexes) in sorted-valid-indexes
          for assignment = (car (set-difference indexes assigned-indexes))
          collect (cons field assignment)
          do (push assignment assigned-indexes))))
    
(defparameter +input-sample-part-2+ "class: 0-1 or 4-19
departure: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9")

(defun read-departure-field-product (stream)
  (let* ((tickets (read-input stream))
           (my-ticket (car tickets))
           (field-index-alist (determine-field-order tickets))
           (my-departure-values (loop with my-field-values = (field-values my-ticket)
                                      for field in (fields my-ticket)
                                      for field-value = (nth (cdr (assoc field field-index-alist))
                                                             my-field-values)
                                      when (search "departure" (name field)) ;technically cheating -- should be starts-with
                                        collect field-value)))
      (when my-departure-values
        (apply #'* my-departure-values))))

(defun run-part-2-test ()
  (with-input-from-string (input +input-sample-part-2+)
    (read-departure-field-product input)))

(defun run-part-2 ()
  (with-open-file (input #P"input-day16.txt")
    (read-departure-field-product input)))

;;;(run-part-2) ; => 603409823791 (40 bits, #x8C7E07382F)
