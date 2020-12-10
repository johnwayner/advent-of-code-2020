(defpackage :com.johnwayner.aoc-20.day-7
  (:use :cl))

(in-package :com.johnwayner.aoc-20.day-7)

(declaim (optimize (safety 3)))

(defvar *input-sample-part-1* "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(defvar *input-sample-part-2* "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.")

(defun parse-constraints (constraints-string)
  (loop with start = 0
        for i = (when start (search " bag" constraints-string :start2 start))
        while i
        when (equalp "no" (subseq constraints-string start (+ 2 start)))
          return nil
        collect (list (subseq constraints-string (+ 2 start) i)
                      (parse-integer constraints-string :start start :end (1+ start)))
        do (setf start (search " " constraints-string :start2 (1+ i)))
        when start do (incf start)))

(defun parse-rule (rule-string)
  (let* ((contain-index (search " bags contain " rule-string))
         (bag-name (subseq rule-string 0 contain-index))
         (rules (parse-constraints (subseq rule-string (+ (length " bags contain ") contain-index)))))
    (list bag-name rules)))

(defun parse-input-for-part-1 (input)
  (loop with h = (make-hash-table :test #'equalp)
        for line = (read-line input nil)
        for rule = (when line (parse-rule line))
        for rule-name = (car rule)
        while rule
        do (loop for c in (mapcar #'car (cadr rule))
                 do (push rule-name (gethash c h)))
        finally (return h)))

(defun collect-paths-for-bag (bag-source-ht bag-name &optional visited-bags)
  (when (not (member bag-name visited-bags :test #'equalp))
    (cons bag-name
           (mapcan (lambda (parent)
                     (collect-paths-for-bag bag-source-ht parent (cons bag-name visited-bags)))
                   (set-difference (gethash bag-name bag-source-ht) visited-bags)))))

(defun run-part-1 (&optional (input (make-string-input-stream *input-sample-part-1*)))
  (- (length (remove-duplicates (collect-paths-for-bag (parse-input-for-part-1 input)
                                                       "shiny gold")))
     1))

(defun parse-input-for-part-2 (input)
  (loop with h = (make-hash-table :test #'equalp)
        for line = (read-line input nil)
        for rule = (when line (parse-rule line))
        for rule-name = (car rule)
        while rule
        do (setf (gethash rule-name h) (cadr rule))
        finally (return h)))

(defun count-required-bags (bag-source-ht bag-name count)
  (loop for sub-constraint in (gethash bag-name bag-source-ht)
        for new-count = (* count (or (cadr sub-constraint) 1))
        sum new-count
        sum (count-required-bags bag-source-ht (car sub-constraint) new-count)))


(defun run-part-2 (&optional (input (make-string-input-stream *input-sample-part-1*)))
  (count-required-bags (parse-input-for-part-2 input) "shiny gold" 1))
