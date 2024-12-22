
;;; 从文件读取*input*，作为defparameter
;; 第1部分
(defun read-input ()
  (with-open-file (stream "./day16-process.txt")
    (read stream)))

(defparameter *input* (read-input))
(defparameter *traff-p* (make-hash-table :test #'equal)) ; 用列表保存通过性

;; 第2部分

