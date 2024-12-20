
;;; 从文件读取*input*，作为defparameter
;; 第1部分
(defun read-input ()
  (with-open-file (stream "./day12-process.txt")
    (read stream)))

(defparameter *input* (read-input))

(defun replace-chars (str target-char &optional (match-> #\1) (unmatch-> #\0))
  (coerce (map 'string
               (lambda (char)
                 (if (char= char target-char) match-> unmatch->))
               str)
          'string))
(defun decompose-binary (n)
  (let ((result '()))
    (loop for i from 0 below (integer-length n)
          when (logbitp i n)
            do (push (ash 1 i) result))
    (nreverse result)))
(defun seperate-bin (bin)
  "将整数按照二进制数位中1相邻情况分组分隔"
  (let ((fringe (logxor (ash bin 1) bin))
        (bin-ls nil))
    (loop for i from 0 below (integer-length fringe)
          with acc = 0
          with scanning = nil
          do (cond
               (scanning
                (setf acc (+ acc (ash 1 (1- i))))
                (when (logbitp i fringe) (setf scanning nil)
                      (push acc bin-ls) (setf acc 0)))
               (t
                (when (logbitp i fringe) (setf scanning t)))))
    bin-ls))

(defun compose-p (bin-1 bin-2) (eql 0 (logand bin-1 bin-2)))
(defun bin-compose (bin-1 bin-2 width)
  (declare (integer bin-1 bin-2 width))
  "bin-1和bin-2尝试拼接，bin-1提升数位，若其在纵向相邻，则拼接"
  (cond
    ((eql 0 (logand bin-1 bin-2)) ; 不相邻
     (ash bin-1 width)) ; 拼接失败，仍然提升bin-1的数位 ; 曾为"保留bin-2"
    (t ; 相邻
     (logior bin-2 (ash bin-1 width)))))
(defun ash-bin-ls (bin-ls)
  (mapcar #'(lambda (bin) (declare (integer bin)) (ash bin 1)) bin-ls))

(defun merge-p (bin-1 bin-2)
  (declare (integer bin-1 bin-2)) (not (eql 0 (logand bin-1 bin-2))))
(defun bin-merge (bin-1 bin-2)
  "bin-1和bin-2融合，不涉及移位操作；若融合失败，则仍然保留bin-2"
  (declare (integer bin-1 bin-2))
  (if (eql 0 (logand bin-1 bin-2)) bin-2 (logior bin-1 bin-2)))
(defun bin-ls-merge (bin-ls)
  "bin-ls列表内部融合"
  (labels ((handler (head rest-ls &optional (acc nil))
             (cond
               ((eql nil rest-ls) (push head acc))
               (t (let* ((merge-p (mapcar #'(lambda (bin-2)
                                              (merge-p head bin-2))
                                          rest-ls))
                         (merge-p* (remove-if #'null merge-p))
                         (merge-ls (mapcar #'(lambda (bin-2)
                                              (bin-merge head bin-2))
                                          rest-ls)))
                    (cond
                      ((null merge-p*) ; 不可消耗
                       (push head acc)
                       (handler (car rest-ls) (cdr rest-ls) acc))
                      (t (let ((rest merge-ls)) ; 可以消耗
                           (handler (car rest) (cdr rest) acc)))))))))
    (if (null bin-ls) nil (handler (car bin-ls) (cdr bin-ls)))))

(defun bin-ls-compose (bin-ls-1 bin-ls-2 width) ; 需要和bin-ls-merge类似
  "bin-ls-1和bin-ls-2拼接，返回结果"
  (declare (integer width))
  (if (null bin-ls-1) (setf bin-ls-1 '(0)))
  (if (null bin-ls-2) (setf bin-ls-2 '(0)))
  (let* ((bin-2-ls-ls
           (mapcar
            #'(lambda (bin-2)
                (mapcar
                 #'(lambda (bin-1) (bin-compose bin-1 bin-2 width))
                 bin-ls-1))
            bin-ls-2))
         (bin-2-ls (remove-duplicates (reduce #'append bin-2-ls-ls
                                              :initial-value bin-ls-2)))
         (bin-ls (bin-ls-merge bin-2-ls)))
    bin-ls))

(defun input->code (input char) ;改成input->code-ls
  (let* ((str-whole (reduce #'(lambda (acc str) (concatenate 'string acc "." str))
                            input))
         (str-replace (replace-chars str-whole char))
         (str-bin (concatenate 'string "#b" str-replace))
         (code (read-from-string str-bin)))
    code))

(defun input->code-ls (input char)
  (let* ((width (length (car input)))
         (str-ls-replace (mapcar #'(lambda (str) (replace-chars str char))
                                 input))
         (str-ls-bin (mapcar #'(lambda (str) (concatenate 'string "#b" str))
                             str-ls-replace))
         (code-ls (mapcar #'(lambda (str) (read-from-string str))
                              str-ls-bin))
         (code-sep-ls (mapcar #'seperate-bin code-ls))
         (block-ls (reduce #'(lambda (acc bin-ls)
                               (bin-ls-compose acc bin-ls width))
                           code-sep-ls)))
    (remove 0 block-ls)))

(defun input->char-ls (input)
  (let* ((str-whole (reduce #'(lambda (acc str) (concatenate 'string acc str)) input))
         (char-ls* (coerce str-whole 'list))
         (char-ls (remove-duplicates char-ls*)))
    char-ls))

(defun code-area (code)
  (logcount code))

(defun code-perimeter (code width)
  (let ((y-measure (logcount (logxor (ash code 1) code)))
        (x-measure (logcount (logxor (ash code width) code))))
    (+ x-measure y-measure)))

(defun process (input)
  (let* ((width (length (car input)))
         (char-ls (input->char-ls input))
         (code-ls (mapcar #'(lambda (char) (input->code-ls input char))
                          char-ls))
         (code-ls* (reduce #'append code-ls))
         (area-ls (mapcar #'code-area code-ls*))
         (peri-ls (mapcar #'(lambda (code) (code-perimeter code width)) code-ls*))
         (price-ls (mapcar #'* area-ls peri-ls))
         (price (reduce #'+ price-ls)))
    price))

;; 第2部分

; 思路: 
; 1. 先把图放大2倍
; 2. 然后再x、y轴同时平移一个单位，与原图案异或运算
; 3. 原图案x、y轴分别平移一个单位，两者异或运算
; 4. 两重影进行异或运算
; 5. 最后识别出直线的模式（与分离出斑渍相同），两种直线同时存在

(defun repeat-elem (lst)
  (mapcan (lambda (x) (list x x)) lst))
(defun repeat-chars (str)
  (coerce (repeat-elem (coerce str 'list)) 'string))

(defun scale-input-2x2 (input)
  (let* ((input-scale-width (mapcar #'repeat-chars input))
         (input-scale-height (repeat-elem input-scale-width))) ; 没有复制字符串的环节,谨慎使用
    input-scale-height))

(defun code->input (code width) ; 性能绝对有损耗，但是没办法理解之前的代码了
  (let ((str-ls nil) (remain code))
    (loop while (> remain 0)
          do (push (format nil (format nil "~a~db" #\~ width)
                           (logand remain (1- (ash 1 width))))
                   str-ls)
             (setf remain (ash remain (- width))))
    str-ls))

(defun code-sides (code width) ; 如果瘢痕贯穿左右边界，则会过少估计
  "仅针对输入被放大2倍处理后的数据"
  (labels ((hori-ash (code) (ash code width))
           (vert-ash (code) (ash code 1)))
    (let* ((fringe-1 (logxor (vert-ash (hori-ash code)) code))
           (fringe-2 (logxor (vert-ash code) (hori-ash code)))
           (edges (logand fringe-1 fringe-2))
           (fake-input (code->input edges width))
           (code-ls (input->code-ls fake-input #\1))
           )
      ;(+ y-sides x-sides)
      (length code-ls)
      ;(values fringe-1 fringe-2)
      ;edges
      ;(vert-ash hori-ash code))
      )))

(defun process* (input) ; 超级慢，不过最后结果还是出来了，耶
  (let* ((input (mapcar #'(lambda (str) (concatenate 'string str ".")) input))
         (width (length (car input)))
         (char-ls (remove #\. (input->char-ls input))) ; 增加padding之后去除
         (code-ls (mapcar #'(lambda (char) (input->code-ls input char))
                          char-ls))
         (code-ls* (reduce #'append code-ls))
         (area-ls (mapcar #'code-area code-ls*))
         (sides-ls (mapcar #'(lambda (code) (code-sides ; 垃圾代码，转换环节太多
                                             (car(input->code-ls
                                                  (scale-input-2x2 (code->input code width))
                                                  #\1))
                                             (* 2 width)))
                           code-ls*))
         (price-ls (mapcar #'* area-ls sides-ls))
         (price (reduce #'+ price-ls)))
    price))
