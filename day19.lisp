
;;; 从文件读取*input*，作为defparameter
;; 第1部分
(defun read-input ()
  (with-open-file (stream "./day19-process1.txt")
    (read stream)))
(defun read-design ()
  (with-open-file (stream "./day19-process2.txt")
    (read stream)))

(defparameter *pattern-ls* (read-input))
(defparameter *design-ls* (read-design))
(defparameter *match-table* (make-hash-table :test #'equal)) ; 记忆化优化

(defun match-char-ls (char-ls target)
  "给定一长串字符char-ls，给定匹配目标，查看是否从头部匹配；~@
  若不能匹配返回'no；若能匹配返回剩余字符列表"
  (cond
    ((null target) char-ls)
    ((null char-ls) 'no)
    ((eql (car char-ls) (car target))
     (match-char-ls (cdr char-ls) (cdr target)))
    (t 'no)))

(defun match-design-pattern (design-char pattern-char-ls)
  "给定一字符列表design-char，给定字符列表列表pattern-char-ls，进行匹配~@
  查看是否能够完全消耗design-char"
  (let ((remain-ls (remove 'no
                           (mapcar #'(lambda (target) (match-char-ls design-char target))
                                   pattern-char-ls))))
    (cond
      ((eql (gethash design-char *match-table*) 'match) t)
      ((eql (gethash design-char *match-table*) 'no) nil)
      ((null design-char) t) ; 已经消耗完毕
      ((null remain-ls) nil) ; 无法进一步消耗
      (t (let* ((match-p (reduce #'(lambda (acc char-ls)
                                     (or acc (match-design-pattern char-ls pattern-char-ls)))
                                 remain-ls :initial-value nil)))
           (if match-p
               (setf (gethash design-char *match-table*) 'match)
               (setf (gethash design-char *match-table*) 'no))
           match-p)))))

(defun compress-pattern (pattern-char-ls &optional (acc nil))
  (cond
    ((null pattern-char-ls) acc)
    (t
     (let* ((head (car pattern-char-ls))
            (rest (cdr pattern-char-ls))
            (match-p (match-design-pattern head rest)))
       (cond
         (match-p (compress-pattern rest acc))
         (t (compress-pattern rest (cons head acc))))))))


(defun solve (design-ls pattern-ls) ; 如果使用字典树可能更加内存友好一些，不过我内存够用
  (let* ((design-char-ls (mapcar #'(lambda (str) (coerce str 'list)) design-ls))
         (pattern-char-ls (mapcar #'(lambda (str) (coerce str 'list)) pattern-ls))
         (match-p-ls (mapcar #'(lambda (char-ls)
                                 (match-design-pattern char-ls pattern-char-ls))
                             design-char-ls))
         (match-count (length (remove nil match-p-ls))))
    match-count))

;; 第2部分
(defparameter *arrange-table* (make-hash-table :test #'equal)) ; 记忆化优化

(defun match-design-pattern-count* (design-char pattern-char-ls)
  "给定一字符列表design-char，给定字符列表列表pattern-char-ls，进行匹配~@
  查看是否能够完全消耗design-char"
  (let ((remain-ls (remove 'no
                           (mapcar #'(lambda (target) (match-char-ls design-char target))
                                   pattern-char-ls))))
    (cond
      ((gethash design-char *arrange-table*)
       (gethash design-char *arrange-table*))
      ((null design-char) 1) ; 已经消耗完毕
      ((null remain-ls) 0) ; 无法进一步消耗
      (t (let* ((match-count (reduce #'(lambda (acc char-ls)
                                         (+ acc (match-design-pattern-count*
                                                 char-ls pattern-char-ls)))
                                     remain-ls :initial-value 0)))
           (setf (gethash design-char *arrange-table*) match-count)
           match-count)))))

(defun solve* (design-ls pattern-ls) ; 结果超大，别怀疑
  (let* ((design-char-ls (mapcar #'(lambda (str) (coerce str 'list)) design-ls))
         (pattern-char-ls (mapcar #'(lambda (str) (coerce str 'list)) pattern-ls))
         (match-count-ls (mapcar #'(lambda (char-ls)
                                     (match-design-pattern-count* char-ls pattern-char-ls))
                                 design-char-ls))
         (match-count (reduce #'+ match-count-ls)))
    match-count))
