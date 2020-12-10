(defpackage :com.johnwayner.aoc-20.game-console
  (:use :cl)
  (:export reset
           status
           arguments
           accumulator
           jmp
           nop
           acc
           make-instruction
           read-instructions
           make-game-console
           run-console))

(in-package :com.johnwayner.aoc-20.game-console)

(declaim (optimize (safety 3) (debug 3)))

(defclass instruction ()
  ((arguments :initform nil :initarg :arguments :reader arguments)))

(defmethod print-object ((object instruction) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "args:~{~a~^, ~}" (arguments object))))

(defclass nop (instruction) ())
(defclass acc (instruction) ())
(defclass jmp (instruction) ())

(defclass storage ()
  ((data :initarg :data :accessor data)))
(defgeneric read-storage (storage address))
(defgeneric write-storage (storage address value))

(defclass list-storage (storage) ())
(defmethod peek ((storage list-storage) address)
  (nth address (data storage)))
(defmethod (setf peek) (new-value (storage list-storage) address)
  (setf (nth address (data storage)) new-value))

(defclass game-console ()
  ((accumulator :initform 0 :accessor accumulator)
   (pc :initform 0 :accessor pc)
   (storage :initarg :storage :accessor storage :type storage)
   (status :initform :running :accessor status)))

(defmethod print-object ((object game-console) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (pc accumulator storage) object
      (format stream "pc: ~a, acc: ~a, next: ~a" pc accumulator (peek storage pc)))))

(defmethod reset ((game-console game-console))
  (setf (pc game-console) 0)
  (setf (accumulator game-console) 0)
  (setf (status game-console) :running)
  game-console)

(defgeneric execute (instruction game-console))
(defmethod execute ((instruction t) game-console)
  (setf (status game-console) :halted-exhausted)
  game-console)
(defmethod execute ((instruction nop) (game-console game-console))
  nil)
(defmethod execute ((instruction acc) (game-console game-console))
  (incf (accumulator game-console) (first (arguments instruction)))
  nil)
(defmethod execute ((instruction jmp) (game-console game-console))
  (first (arguments instruction)))

(defmethod step-instruction ((game-console game-console))
  (with-slots (pc storage accumulator status) game-console
    (let ((pc-delta (funcall #'execute (peek storage pc) game-console)))
      (when (eql :running status)
        (incf (pc game-console) (or pc-delta 1)))
      game-console)))

(defvar *AUDIT-DATA* nil)
(defvar *HALT-ON-SECOND-EXECUTION-OF-INS* nil)
(defmethod step-instruction :around ((game-console game-console))
  (if (and *HALT-ON-SECOND-EXECUTION-OF-INS*
           *AUDIT-DATA*
           (member (pc game-console) *AUDIT-DATA*))
      (progn
        (setf (status game-console) :halted-repeat)
        game-console)
      (progn
        (push (pc game-console) *AUDIT-DATA*)
        (call-next-method game-console))))

(defun make-instruction (instruction-symbol &rest arguments)
  (make-instance (find-symbol (symbol-name instruction-symbol) :com.johnwayner.aoc-20.game-console)
                 :arguments arguments))

(defun read-instructions (input)
  (loop
    for x = (read input nil nil)
    while x
    if (symbolp x)
      collect (make-instruction x (read input))))

(defun make-game-console (instructions)
  (make-instance 'game-console
                 :storage (make-instance 'list-storage
                                         :data instructions)))

(defun run-console (game-console)
  (reset game-console)
  (let ((*halt-on-second-execution-of-ins* t)
        (*audit-data* '()))
    (loop for stepped-console = (step-instruction game-console)
          while (eql :running (status stepped-console)))
    game-console))
