
;;; 从文件读取*input*，作为defparameter
;; 第1部分
(defun read-input ()
  (with-open-file (stream "./day14-process.txt")
    (read stream)))
(defun read-test ()
  (with-open-file (stream "./day14-test.txt")
    (read stream)))

(defparameter *input* (read-input))
(defparameter *test* (read-test))
(defparameter *width* 101)
(defparameter *height* 103)

(defun revolve (robot width height)
  (let ((pos-x (caar robot))
        (pos-y (cdar robot))
        (vel-x (caadr robot))
        (vel-y (cdadr robot)))
    `((,(mod (+ pos-x vel-x) width) . ,(mod (+ pos-y vel-y) height))
      ,(cadr robot))))

(defun revolve-time (input width height &optional (time 100))
  (let ((revol input))
    (dotimes (i time revol)
      (setf revol (mapcar #'(lambda (robot) (revolve robot width height)) revol)))))

; 象限
; 0 | 1
; 2 | 3

(defun filt-quadrant-gen (width height &optional (quadrant-which 0))
  (let ((width-1-no-more-than (ash width -1))
        (height-1-no-more-than (ash height -1))
        (width-2-min (- width (ash width -1)))
        (height-2-min (- height (ash height -1))))
    `(lambda (robot)
       (let ((x (caar robot))
             (y (cdar robot)))
         ,(case quadrant-which
            (0 `(and (< x ,width-1-no-more-than) (< y ,height-1-no-more-than)))
            (1 `(and (>= x ,width-2-min) (< y ,height-1-no-more-than)))
            (2 `(and (< x ,width-1-no-more-than) (>= y ,height-2-min)))
            (3 `(and (>= x ,width-2-min) (>= y ,height-2-min))))))))

(defun process (input width height)
  (let* ((elapsed-100s (revolve-time input width height 100))
         (quadrant-filt (mapcar #'(lambda (x) (eval (filt-quadrant-gen width height x)))
                                '(0 1 2 3)))
         (quadrant-robot (mapcar #'(lambda (fn) (remove-if-not fn elapsed-100s))
                                 quadrant-filt))
         (quadrant-count (mapcar #'length quadrant-robot))
         (safety-factor (reduce #'* quadrant-count)))
    safety-factor))

;; 第2部分

(defun draw-line (code width)
  (let ((char-ls '())
        (remain-width width))
    (loop with remain = code
          while (not (eql remain-width 0))
          do (push (if (eql #b1 (logand remain #b1)) #\* #\ ) char-ls)
             (setf remain (ash remain -1)) (decf remain-width))
    (coerce char-ls 'string)))

(defun draw.input->map (input height)
  ;(format t "~c[2J~c[H" #\escape #\escape)
  ;(format t "===========================================================================")
  (let ((map (make-list height :initial-element #b0)))
    (map nil
         #'(lambda (robot)
             (let ((x (caar robot)) (y (cdar robot)))
               (setf (nth y map) (logior (nth y map) (ash 1 x)))))
         input)
    map))

(defun draw (input width height)
  ;(format t "~c[2J~c[H" #\escape #\escape)
  ;(format t "===========================================================================")
  (let ((map (make-list height :initial-element #b0)))
    (map nil
         #'(lambda (robot)
             (let ((x (caar robot)) (y (cdar robot)))
               (setf (nth y map) (logior (nth y map) (ash 1 x)))))
         input)
    (let ((line-ls (mapcar #'(lambda (code) (draw-line code width)) map)))
      line-ls
      (map nil #'print line-ls))))

(defun search-line (code &optional (search-len 5))
  "给定代表一行的code，判断是否含有长条"
  (let ((find-p nil)
        (match (1- (ash 1 search-len))))
    (loop with remain = code
          while (not (eql remain #b0))
          do (when (eql #b0 (logxor (logand code match) match))
               (setf find-p t))
             (setf remain (ash remain -1))
          )
    find-p))

(defun search-map (map &optional (search-len 5))
  (let ((find-p nil))
    (map nil #'(lambda (code)
                 (when (search-line code search-len) (setf find-p t)))
         map)
    find-p))

(defun search-revolve (input width height &optional (search-len 5))
  (let (map (current-input input) (search-p nil) (num 0))
    (loop while (eql search-p nil)
          do (incf num)
             (setf map (draw.input->map current-input height))
             ;(format  t "~a~%" map )
             (setf search-p (search-map map search-len))
             (setf current-input (revolve-time current-input width height 1))
          )
    num))

(defun anime (input width height &optional (interval 0.5) (step 1))
  (loop do (draw input width height)
           (setf input (revolve-time input width height step))
           (sleep interval)))

(defun safety-factor (input width height)
  (let* ((quadrant-filt (mapcar #'(lambda (x) (eval (filt-quadrant-gen width height x)))
                                '(0 1 2 3)))
         (quadrant-robot (mapcar #'(lambda (fn) (remove-if-not fn input))
                                 quadrant-filt))
         (quadrant-count (mapcar #'length quadrant-robot))
         (safety-factor (reduce #'* quadrant-count)))
    safety-factor))

(defun safty-revolve (input width height &optional (max-time 15000)) ; 大约在8000次出现一次
  (let ((current-input input))
    (loop for i from 1 below (1+ max-time)
          do (setf current-input (revolve-time current-input width height 1))
          collect `(,(safety-factor current-input width height) ,i))))

(format t "~a~%" (sort (safty-revolve *input* *width* *height*) ; 筛选很缓慢
                       #'(lambda (x y) (< (car x) (car y)))))

;; (draw (revolve-time *input* *width* *height* xx时间) *width* *height*) ; 可以直接查看结果
