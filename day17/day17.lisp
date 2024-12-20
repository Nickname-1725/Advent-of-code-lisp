
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

; 思路
; 1. 代码2 4| 1 7| 7 5| 0 3| 1 7| 4 1| 5 5| 3 0|实际上可以直接读出来
; 2. 这是一个循环，每次消耗A-reg，并且输出一次结果
; 3. 翻译: loop with A = ???
;               with out = nil
;               while (not (eql A #b0))
;               do (let* ((B (logand A #b111))
;                         (C (logand (ash A (- (- #b111 B))) #b111)
;                         (B (logxor C B)))
;                    (push B out)
;                    (ash A -3))

(defun oct-ls () '(0 1 2 3 4 5 6 7))
(defun guess-code (reg code &optional (non-0 nil))
  "给定寄存器值，给定预期输出code，给出可行的最小的oct"
  (let* ((oct-ls (oct-ls))
         (out-ls (mapcar #'(lambda (oct)
                             (let ((reg (logior reg oct)))
                               `(,(logxor (logand (ash reg (- oct 7)) #b111) oct) ,oct)))
                         oct-ls))
         (valid-ls (if non-0 (remove 0 out-ls :test
                                     #'(lambda (item x) (eql (cadr x) item)))
                       out-ls))
         (valid-ls (remove code valid-ls :test-not
                           #'(lambda (item x) (eql item (car x)))))
         (reg-ls (mapcar #'(lambda (x) (logior (cadr x) reg)) valid-ls)))
    reg-ls))

(defun guess-A-reg (out-ls-rev &optional (A-init 0) (head-p nil))
  (cond
    ((null out-ls-rev) (list A-init))
    (t
     (let* ((A A-init))
       (setf A (ash A 3))
       (let ((reg-ls (guess-code A (car out-ls-rev) head-p)))
         (if head-p (setf head-p nil))
         (mapcan #'(lambda (reg) (guess-A-reg (cdr out-ls-rev) reg))
                 reg-ls))))))

(format t "~a~%" (apply #'min (guess-A-reg (reverse *program*) 0 t)))
