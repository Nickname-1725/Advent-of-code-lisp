
;;; 从文件读取*input*，作为defparameter
;; 第1部分
(defun read-input ()
  (with-open-file (stream "./day7-process.txt")
    (read stream)))

(defparameter *input* (read-input))

(defun mul-plus (target oprand-lst &optional (acc 0))
  (cond
    ((> acc target) nil)
    ((eql nil oprand-lst)
     (cond
       ((eql target acc) t)
       (t nil)))
    (t (let ((head (car oprand-lst))
             (rest-ls (cdr oprand-lst)))
         (or (mul-plus target rest-ls (* acc head))
             (mul-plus target rest-ls (+ acc head)))))))

(defun process (input)
  (let* ((valid-ls (remove-if-not #'(lambda (ls)
                                      (mul-plus (car ls) (cadr ls)))
                                  input))
         (calibration-ls (mapcar #'car valid-ls)))
    (reduce #'+ calibration-ls)))

;; 第2部分

(defun || (x y) ; 有效率问题，不过不影响结果
  (read-from-string (format nil "~a~a" x y)))

(defun mul-plus-|| (target oprand-lst &optional (acc 0))
  (cond
    ((> acc target) nil)
    ((eql nil oprand-lst)
     (cond
       ((eql target acc) t)
       (t nil)))
    (t (let ((head (car oprand-lst))
             (rest-ls (cdr oprand-lst)))
         (or (mul-plus-|| target rest-ls (|| acc head))
             (mul-plus-|| target rest-ls (* acc head))
             (mul-plus-|| target rest-ls (+ acc head)))))))

(defun process* (input)
  (let* ((valid-ls (remove-if-not #'(lambda (ls)
                                      (mul-plus-|| (car ls) (cadr ls)))
                                  input))
         (calibration-ls (mapcar #'car valid-ls)))
    (reduce #'+ calibration-ls)))
