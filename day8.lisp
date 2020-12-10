(defpackage :com.johnwayner.aoc-20.day-8
  (:use
   :cl
   :com.johnwayner.aoc-20.game-console))

(in-package :com.johnwayner.aoc-20.day-8)

(declaim (optimize (safety 3) (debug 3)))

(defun run-part-1 (&optional instruction-stream)
  (with-open-file (input-file #P"input-day8.txt")
    (run-console
     (make-game-console
      (read-instructions (or instruction-stream
                             input-file))))))

(defun test-console (new-instruction-type old-instruction instructions)
  (let* ((new-instruction (apply #'make-instruction new-instruction-type (arguments old-instruction)))
         (console (make-game-console (substitute new-instruction old-instruction instructions))))
    (run-console console)
    console))

(defun run-part-2 (&optional instruction-stream (instruction-subs '((jmp . nop) (nop . jmp))))
  (with-open-file (input-file #P"input-day8.txt")
    (loop with instructions = (read-instructions (or instruction-stream input-file))
          for instruction in instructions
          for sub = (cdr (assoc (type-of instruction) instruction-subs))
          when sub
            do (let ((c (test-console sub instruction instructions)))
                 (when (eql :halted-exhausted (status c))
                   (return (accumulator c)))))))



(defvar *input-sample-part-1* "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")
