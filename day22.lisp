
;;; 从文件读取*input*，作为defparameter
;; 第1部分
(defun read-input ()
  (with-open-file (stream "./day22-process.txt")
    (read stream)))

(defparameter *input* (read-input))

(defun generate (secret-num)
  (labels ((mix (a b) (logxor a b))
           (prune (a) (logand a (1- (ash 1 24))))
           (step-1 (num) (prune (mix (ash num 6) num)))
           (step-2 (num) (prune (mix (ash num -5) num)))
           (step-3 (num) (prune (mix (ash num 11) num))))
    (step-3 (step-2 (step-1 secret-num)))))

(defun process-secret-ls (num-ls &optional (times 1))
  (cond
    ((<= times 0) num-ls)
    (t (let ((next-ls (mapcar #'generate num-ls)))
         (process-secret-ls next-ls (1- times))))))

(defun solve (input)
  (reduce #'+ (process-secret-ls input 2000)))

;; 第2部分

