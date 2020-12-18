(defpackage :com.johnwayner.aoc-20.day-18
  (:use :cl))

(in-package :com.johnwayner.aoc-20.day-18)
(declaim (optimize (safety 3) (debug 3)))

(defparameter +input-sample-1+ "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")

(defun read-all (stream)
  (loop for token = (read stream nil nil)
        while token
        collect token))

(defvar *operator-precedence* '((+ *)))

(defun build-ast (tokens)
  (cond
    ((null tokens) (error "Shouldn't get here."))
    ((not (listp tokens)) tokens)
    ((= 1 (length tokens)) (build-ast (car tokens)))
    ((cdr tokens)
     (loop for operator-list in *operator-precedence*
           for op-index = (position-if (lambda (token)
                                         (member token operator-list))
                                       tokens
                                       :from-end t)
           when op-index
             return (list (elt tokens op-index)
                          (build-ast (subseq tokens 0 op-index))
                          (build-ast (subseq tokens (+ 1 op-index))))
           finally (return (build-ast (car tokens)))))
    (t tokens)))

(defun run-part-1-sample ()
  (with-input-from-string (input +input-sample-1+)
    ;;both operators have the same precedence..so in same list.
          (let ((*operator-precedence* '((+ *))))
            (eval (build-ast (read-all input))))))


;;; (run-part-1-sample) ;13632 (14 bits, #x3540)
  
(defun sum-of-all-lines-evaled (stream)
    (loop for line = (read-line stream nil nil)
          while line
          sum (with-input-from-string (line-input line)
                (eval (build-ast (read-all line-input))))))

(defun run-part-1 ()
  (with-open-file (input #P"input-day18.txt")
    ;;both operators have the same precedence..so in same list.
    (let ((*operator-precedence* '((+ *))))
      (sum-of-all-lines-evaled input))))

;;;(run-part-1) 650217205854 (40 bits, #x9763F7305E)

(defun run-part-2 ()
  (with-open-file (input #P"input-day18.txt")
    ;; '* should be higher up the ast (eval'd later)
    (let ((*operator-precedence* '((*) (+))))
      (sum-of-all-lines-evaled input))))
  ;;;(run-part-2) 20394514442037 (45 bits, #x128C77C9E735)
