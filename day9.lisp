
;;; 从文件读取*input*，作为defparameter
;; 第1部分
(defun read-input ()
  (with-open-file (stream "./day9-process.txt")
    (read stream)))

(defparameter *input* (read-input))

(defun indx-to-ID (indx)
  (ash indx -1))

(defun process (input) ;; 看起来像是机器
  (let ((len-input (length input))
        (disk-map (coerce input 'vector))
        (total-space (reduce #'+ input))
        (acc 0))
    (loop with indx-head = 0
          with indx-tail = (1- len-input)
          with ptr-head = 0
          with ptr-tail = (1- total-space)
          with head-remain = (aref disk-map indx-head)
          with tail-remain = (aref disk-map indx-tail)
          while (<= ptr-head ptr-tail)
          do (cond
               ((evenp indx-head)
                (setf acc (+ acc (* ptr-head (indx-to-ID indx-head))))
                ;; 改变状态，准备下一次状态
                (incf ptr-head) (decf head-remain)
                (when (eql head-remain 0)
                  (incf indx-head) (setf head-remain (aref disk-map indx-head)))                
                (when (eql head-remain 0)
                  (incf indx-head) (setf head-remain (aref disk-map indx-head))))
               (t
                (setf acc (+ acc (* ptr-head (indx-to-ID indx-tail))))
                ;; 改变状态, 准备下一次状态
                (incf ptr-head) (decf head-remain)
                (decf ptr-tail) (decf tail-remain)
                (when (eql head-remain 0)
                  (incf indx-head) (setf head-remain (aref disk-map indx-head)))
                (when (eql head-remain 0)
                  (incf indx-head) (setf head-remain (aref disk-map indx-head)))
                (when (eql tail-remain 0)
                  (decf indx-tail) (setf ptr-tail (- ptr-tail (aref disk-map indx-tail)))
                  (decf indx-tail) (setf tail-remain (aref disk-map indx-tail)))
                (when (eql tail-remain 0)
                  (decf indx-tail) (setf ptr-tail (- ptr-tail (aref disk-map indx-tail)))
                  (decf indx-tail) (setf tail-remain (aref disk-map indx-tail))))))
    acc))

;; 第2部分
