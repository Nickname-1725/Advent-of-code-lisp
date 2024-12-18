
;;; 从文件读取*input*，作为defparameter
;; 第1部分
(defun read-input ()
  (with-open-file (stream "./day13-process.txt")
    (read stream)))

(defparameter *input* (read-input))

; ((Xa . Ya)
;  (Xb . Yb)
;  (bx . by)
(defun solve-machine (data)
  (let ((Xa (caar data))
        (Ya (cdar data))
        (Xb (caadr data))
        (Yb (cdadr data))
        (bx (caaddr data))
        (by (cdaddr data)))
    (let* ((X-b (/ (- (* by Xa) (* bx Ya)) (- (* Xa Yb) (* Xb Ya))))
           (X-a (/ (- bx (* Xb X-b)) Xa)))
      (values X-a X-b))))

(defun valid-p (X-a X-b)
  (and (integerp X-a) (integerp X-b)
       (<= X-a 100) (<= X-b 100)
       (>= X-a 0) (>= X-b 0)))

(defun process (input)
  (let ((acc 0))
    (map nil
         #'(lambda (data)
             (multiple-value-bind (X-a X-b)
                 (solve-machine data)
               (when
                   (valid-p X-a X-b) 
                 (setf acc (+ acc (* 3 X-a) X-b)))))
         input)
    acc))

;; 第2部分
(defun solve-machine* (data)
  (let ((Xa (caar data))
        (Ya (cdar data))
        (Xb (caadr data))
        (Yb (cdadr data))
        (bx (+ (caaddr data) 10000000000000))
        (by (+ (cdaddr data) 10000000000000)))
    (let* ((X-b (/ (- (* by Xa) (* bx Ya)) (- (* Xa Yb) (* Xb Ya))))
           (X-a (/ (- bx (* Xb X-b)) Xa)))
      (values X-a X-b))))

(defun valid-p* (X-a X-b)
  (and (integerp X-a) (integerp X-b)))

(defun process* (input)
  (let ((acc 0))
    (map nil
         #'(lambda (data)
             (multiple-value-bind (X-a X-b)
                 (solve-machine* data)
               (when
                   (valid-p* X-a X-b) 
                 (setf acc (+ acc (* 3 X-a) X-b)))))
         input)
    acc))
