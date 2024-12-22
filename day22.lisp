
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

;(defun price-monkey-table (len)
;  "（对某一序列而言），记录每个猴子的出价，产生该表"
;  (make-array len :initial-element 0))

(defparameter *series-price-table* (make-hash-table :test #'equal))

(defun secret->price-ls (num &optional (times 2000))
  (let* ((price-ls nil)
         (price-ls (dotimes (i times price-ls)
                     (push (mod num 10) price-ls)
                     (setf num (generate num))))
         (rev-price-ls (reverse price-ls)))
    rev-price-ls))

(defun price-ls->change-ls (price-ls)
  (mapcar #'- (cdr price-ls) price-ls ))

(defun change-ls->series-ls (change-ls)
  (mapcar #'list change-ls (cdr change-ls)
          (cddr change-ls) (cdddr change-ls)))

(defun price-ls->series.price (price-ls)
  (mapcar #'cons (change-ls->series-ls
                  (price-ls->change-ls price-ls))
          (cddddr price-ls)))

(defun eval-secret-ls (price-ls &optional (hash *series-price-table*))
  "靠副作用完成功能" ; 重新运行之前必须见重置hash表
  (let* ((series.price (price-ls->series.price price-ls))
         (series.price* (remove-duplicates
                         series.price :test #'(lambda (x y) (equal (car x) (car y)))
                                      :from-end t)))
    (map nil #'(lambda (se.pri)
                 (let ((series (car se.pri)) (price (cdr se.pri)))
                 (cond
                   ((null (gethash series hash))
                    (setf (gethash series hash) price))
                   (t (setf (gethash series hash) (+ (gethash series hash) price))))))
         series.price*)))

(defun eval-input (input &optional (hash *series-price-table*))
  "靠副作用完成功能" ; 重新运行之前必须见重置hash表
  (loop for secret in input
        for i from 0
        do (format t "~f%~%" (* 100 (/ i (length input)))) ; 如果嫌慢可以观看进度
           (let ((price-ls (secret->price-ls secret)))
             (eval-secret-ls price-ls hash))))

(defun find-max-hash (hash) ; 运行完eval-input之后再运行find-max-hash
  (let ((max-value -1))
    (maphash #'(lambda (key value)
                 (declare (ignore key))
                 (when (> value max-value)
                   (setf max-value value)))
             hash)
    max-value))
