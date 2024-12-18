
;;; 已经从REPL输入了*obstacles*, *guard*，作为defparameter
(defparameter *width* 130)
(defparameter *height* 130)

; x -- o
;      |
;      y

(defparameter *guard-init-y-indx* (position-if-not #'(lambda (x) (equal x 0)) *guard*))
(defparameter *guard-init-x-code* (nth *guard-init-y-indx* *guard*))

(defun make-map ()
  (make-list *height* :initial-element #x0))
(defparameter *map* (make-list *height* :initial-element #x0))
;; 第1部分

(defmacro generate-wards (out-bound-p update de-update)
  `(labels ((handler (obs map y-indx x-code)
              (cond
                (,out-bound-p (values map -1 -1))
                (t (let* (,update
                          (line-fetch (nth y-indx obs))
                          (scan (logand x-code line-fetch)))
                     (cond
                       ((eql scan 0)
                        ;(format t "~a,~a~%" y-indx (integer-length x-code))
                        (let ((target (nth y-indx map)))
                          (setf (nth y-indx map) (logior target x-code))
                          (handler obs map y-indx x-code)))
                       (t (values map ,@de-update))))))))
     (handler obstacles map guard-y-indx guard-x-code)))

(defun upwards (obstacles map guard-y-indx guard-x-code)
  (generate-wards (eql y-indx 0)
                  (y-indx (1- y-indx))
                  ((1+ y-indx) x-code)))

(defun right-wards (obstacles map guard-y-indx guard-x-code)
  (generate-wards (eql x-code 0)
                  (x-code (ash x-code -1))
                  (y-indx (ash x-code 1))))

(defun downwards (obstacles map guard-y-indx guard-x-code)
  (generate-wards (eql y-indx (1- *height*))
                  (y-indx (1+ y-indx))
                  ((1- y-indx) x-code)))

(defun left-wards (obstacles map guard-y-indx guard-x-code)
  (generate-wards (eql x-code (ash 1 (1- *width*)))
                  (x-code (ash x-code 1))
                  (y-indx (ash x-code -1))))

(defun solve-map (obstacles map y-indx x-code &optional (dir 'up))
  (multiple-value-bind (map y-indx x-code)
      (case dir
        (up (upwards obstacles map y-indx x-code))
        (right (right-wards obstacles map y-indx x-code))
        (down (downwards obstacles map y-indx x-code))
        (left (left-wards obstacles map y-indx x-code)))
    (cond
      ((eql y-indx -1) map)
      (t (solve-map obstacles map y-indx x-code
                (case dir
                  (up 'right)
                  (right 'down)
                  (down 'left)
                  (left 'up)))))))
(defun process (map)
  (let* ((count-list (mapcar #'logcount map))
         (sum (reduce #'+ count-list)))
    sum))
(format t "~a~%" ; 增加1个点是守卫初始位置
        (1+ (process (solve-map *obstacles* *map* *guard-init-y-indx* *guard-init-x-code*))))

;; 第2部分

(defun loop-obstacles-p (obs y-indx x-code)
  (let ((map (make-map))
        (count-steps 0)
        (count-no-new 0))
    (labels ((walk (obs map y-indx x-code &optional (dir 'up))
               (multiple-value-bind (map y-indx x-code)
                   (case dir
                     (up    (upwards obs map y-indx x-code))
                     (right (right-wards obs map y-indx x-code))
                     (down  (downwards obs map y-indx x-code))
                     (left  (left-wards obs map y-indx x-code)))
                 (let ((process-map (process map)))
                   (cond
                     ((eql y-indx -1) nil) ; 退出场景，为假
                     ((eql process-map count-steps) ; 循环且足迹不再增加，为真
                      (setf count-no-new (1+ count-no-new))
                      (if (>= count-no-new 4) t ; 逐渐增大限制次数，从3增加到4，结果不再变化
                          (walk obs map y-indx x-code
                                (case dir
                                  (up 'right)
                                  (right 'down)
                                  (down 'left)
                                  (left 'up)))))
                     (t (setf count-steps process-map)
                        (walk obs map y-indx x-code
                              (case dir
                                (up 'right)
                                (right 'down)
                                (down 'left)
                                (left 'up)))))))))
      (walk obs map y-indx x-code))))
(defun decompose-binary (n)
  (let ((result '()))
    (loop for i from 0 below (integer-length n)
          when (logbitp i n)
            do (push (ash 1 i) result))
    (nreverse result)))
(defun modify-obs (obs indx code)
  (let ((obs (copy-list obs)))
    (setf (nth indx obs) (logior code (nth indx obs)))
    obs))
(defun find-loops (obs y-indx x-code)
  (let* ((map (make-map))
         (gather-steps (solve-map obs map y-indx x-code))
         (counts(loop for line in gather-steps
                      for indx from 0
                      collect
                      (let* ((decom-lst (decompose-binary line))
                             (obs-lst (mapcar #'(lambda (code) (modify-obs obs indx code))
                                              decom-lst))
                             (loop-p-lst
                               (remove-if-not #'(lambda (obs)
                                                  (loop-obstacles-p obs y-indx x-code))
                                              obs-lst)))
                        (length loop-p-lst)))))
    (reduce #'+ counts)))
