
;;; 从文件读取*input*，作为defparameter
;; 第1部分
(defun read-input ()
  (with-open-file (stream "./day17-program.txt")
    (read stream)))

(defparameter *program* (read-input))
; Register A: 64012472
; Register B: 0
; Register C: 0

(defun machine (program &optional (A-reg 64012472) (B-reg 0) (C-reg 0))
  (labels ((combo (operand A B C)
               (case operand
                 (4 A) (5 B) (6 C)
                 (otherwise operand))))
    (let ((program-ls (apply #'vector program))
          (prog-len (length program))
          (out '()))
      (loop with instr-ptr = 0
            with A = A-reg
            with B = B-reg
            with C = C-reg
            while (< instr-ptr prog-len) ; 停机条件
            do (let ((operator (aref program-ls instr-ptr))
                     (operand (aref program-ls (1+ instr-ptr))))
                 (format t "~a,~a~%" operator operand)
                 (case operator
                   (0 (setf A (ash A (- (combo operand A B C)))) ; adv
                    (incf instr-ptr 2))
                   (1 (setf B (logxor B operand)) ; bxl
                    (incf instr-ptr 2))
                   (2 (setf B (logand (combo (logand operand #b111) A B C) #b111)) ; bst
                    (incf instr-ptr 2))
                   (3 (if (eql 0 A) (incf instr-ptr 2) ; jnz
                          (setf instr-ptr operand)))
                   (4 (setf B (logxor B C)) ;bxc
                    (incf instr-ptr 2))
                   (5 (push (logand (combo (logand operand #b111) A B C) #b111) out) ; out
                    (incf instr-ptr 2))
                   (6 (setf B (ash A (- (combo operand A B C)))) ; bdv
                    (incf instr-ptr 2))
                   (7 (setf C (ash A (- (combo operand A B C)))) ; cdv
                    (incf instr-ptr 2)))
                 (format t "A:~a,B:~a,C:~a~%" A B C)))
      (reverse out))))

;; 第2部分


