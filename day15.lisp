
;;; 从文件读取*input*，作为defparameter
;; 第1部分
(defun read-input ()
  (with-open-file (stream "./day15-process(maze).txt")
    (read stream)))
(defun read-movements ()
  (with-open-file (stream "./day15-process(movements).txt")
    (read stream)))


(defparameter *maze* (read-input))
(defparameter *movements* (apply #'concatenate 'string (read-movements)))

(defun str->code (str target-char &optional (match-> #\1) (unmatch-> #\0))
  (let* ((str-replace
           (coerce (map 'string
                        (lambda (char)
                          (if (char= char target-char) match-> unmatch->))
                        str)
                   'string))
         (str-bin (concatenate 'string "#b" str-replace))
         (code (read-from-string str-bin)))
    code))

(defun str-ls->code (str-ls target-char)
  "给定迷宫字符列表，将特定字符替换成二进制表达的数字形式"
  (let* ((width (length (car str-ls)))
         (rev-str-ls (mapcar #'reverse str-ls))
         (code-ls (mapcar #'(lambda (str) (str->code str target-char)) rev-str-ls))
         (rev-code-ls (reverse code-ls))
         (code (reduce #'(lambda (acc code) (logior (ash acc width) code)) rev-code-ls)))
    code))

; O -- x
; |
; y
; 方向 ^ v < >
(defun vert-bar (width &optional (bar-len 1))
  "生成一个竖直的条"
  (let ((acc #b0))
    (loop repeat bar-len
          do (setf acc (ash acc width))
             (setf acc (logior acc #b1)))
    acc))
(defun bar-code (y x width &optional (bar-len 1) (dir #\^))
  "位于y,x的点，向指定方向延伸bar-len长度"
  (case dir
    (#\> (ash (ash (1- (ash 1 bar-len)) x) (* width y)))
    (#\< (ash (ash (1- (ash 1 bar-len)) (- x bar-len -1)) (* width y)))
    (#\^ (ash (ash (vert-bar width bar-len) x) (* width (- y bar-len -1))))
    (#\v (ash (ash (vert-bar width bar-len) x) (* width y)))
    ))

; 思路
; 1. 对于一个指令，获得一个预期坐标
; 2. 从这个坐标出发，构造一个可生长的直条，首先考察其是否能够吸收O方块
;    1) 若能吸收新的O方块，则直条生长
;    2) 若不能吸收新的O方块，则停止生长
; 3. 用上一步构造的直条考察其是否与障碍物#重合
;    1) 若不重合，则抹除原有O方块，重叠新的O方块
;    2) 若重合，则对状态不进行更改

(defun new-pair (y-x-pair &optional (dir #\^))
  (let ((x (cdr y-x-pair)) (y (car y-x-pair)))
  (case dir
    (#\^ `(,(1- y) . ,x)) (#\v `(,(1+ y) . ,x))
    (#\< `(,y . ,(1- x))) (#\> `(,y . ,(1+ x))))))

(defun box-grow (y-new x-new box-code width &optional (bar-len 1) (dir #\^))
  "返回已经生长的长度"
  (let* ((grow-bar (bar-code y-new x-new width bar-len dir))
         (not-overlap-p (eql #b0 (logand box-code grow-bar))))
    (cond
      (not-overlap-p (1- bar-len))
      (t (box-grow y-new x-new (logand box-code (lognot grow-bar))
                   width (1+ bar-len) dir)))))

(defun not-bump-p (y-new x-new obs-code width &optional (bar-len 1) (dir #\^))
  (let* ((bar-code (bar-code y-new x-new width (1+ bar-len) dir))
         (not-overlap-p (eql #b0 (logand bar-code obs-code))))
    not-overlap-p))

(defun walk (y-x-pair obstacle-code box-code width &optional (dir #\^))
  "单步行走，返回新的机器人坐标、box-code"
  (let* ((expected-y-x (new-pair y-x-pair dir))
         (|x'| (cdr expected-y-x)) (|y'| (car expected-y-x))
         (box-mov-len (box-grow |y'| |x'| box-code width 1 dir))
         (not-bump-p (not-bump-p |y'| |x'| obstacle-code width box-mov-len dir)))
    (cond
      (not-bump-p
       (let* ((|y-x''| (new-pair expected-y-x dir)) ; 为了将新坐标(y' . x')本身去除
              (|y''| (car |y-x''|))
              (|x''| (cdr |y-x''|))
              (bar-code-remove (bar-code |y'| |x'| width box-mov-len dir))
              (bar-code-add (bar-code |y''| |x''| width box-mov-len dir))
              (box-remove (logxor (logior box-code bar-code-remove) bar-code-remove))
              (box-add (logior box-remove bar-code-add)))
         (values expected-y-x box-add)))
      (t (values y-x-pair box-code))))) ; 障碍，无法进行，只能返回原值

(defun exec-mov-ls (y-x obstacle-code box-code width mov-ls)
  "依据指令和障碍图、盒子图执行运动，最终返回盒子的分布状态"
  (cond
    ((null mov-ls) box-code)
    (t (multiple-value-bind (y-x-new box-code-new)
           (walk y-x obstacle-code box-code width (car mov-ls))
         (exec-mov-ls y-x-new obstacle-code box-code-new width (cdr mov-ls))))))
(defun code->y-x-pair (code width)
  (multiple-value-bind (y x)
      (truncate (1- (integer-length code)) width)
    `(,y . ,x)))

(defun GPS (pair)
  (let ((y (car pair))
        (x (cdr pair)))
    (+ (* 100 y) x)))

(defun decompose-binary (n)
  (let ((result '()))
    (loop for i from 0 below (integer-length n)
          when (logbitp i n)
            do (push (ash 1 i) result))
    (nreverse result)))

(defun process (maze movs)
  "给定输入地图，以及输入动作指令，输出答案"
  (let* ((width (length (car maze)))
         (mov-ls (coerce movs 'list))
         (obs-code (str-ls->code maze #\#))
         (box-code (str-ls->code maze #\O))
         (robot-code (str-ls->code maze #\@))
         (robot-y-x (code->y-x-pair robot-code width))
         (box-code-final (exec-mov-ls robot-y-x obs-code box-code width mov-ls))
         (box-code-ls (decompose-binary box-code-final))
         (box-pair-ls (mapcar #'(lambda (code) (code->y-x-pair code width)) box-code-ls))
         (GPS-ls (mapcar #'GPS box-pair-ls))
         (GPS-sum (reduce #'+ GPS-ls)))
    GPS-sum))

;; 第2部分

