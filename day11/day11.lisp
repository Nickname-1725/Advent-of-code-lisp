
;;; 从文件读取*input*，作为defparameter
;; 第1部分
(defun read-input ()
  (with-open-file (stream "./day11-process.txt")
    (read stream)))

(defparameter *input* (read-input))

(defun split (num)
  (let* ((str (format nil "~a" num))
         (len-digit (length str)))
    (if (eql (mod len-digit 2) 0)
        (let ((sub-1 (subseq str 0 (ash len-digit -1)))
              (sub-2 (subseq str (ash len-digit -1))))
          (values (read-from-string sub-1) (read-from-string sub-2)))
        (values nil nil))))
(defun rules (num)
  (if (eql num 0) (list 1)
      (multiple-value-bind (sub-1 sub-2)
          (split num)
        (cond
          (sub-1 (list sub-1 sub-2))
          (t (list (* 2024 num)))))))

(defun blink (ls)
  (reduce #'(lambda (acc num)
              (append acc (rules num)))
          ls :initial-value nil))

(defun solve (ls &optional (count 25)) ; 消耗的时间极长，应该跟字符串打印、读取，以及append有关
  (cond
    ((eql count 0) (length ls))
    (t (solve (blink ls) (1- count)))))

;; 第2部分
(defun split* (num)
  (labels ((handler (remain &optional (acc-ls nil))
             (if (eql 0 remain) acc-ls
                 (multiple-value-bind (remain digit)
                     (truncate remain 10)
                   (handler remain (cons digit acc-ls)))))
           (compose (ls &optional (acc 0))
             (if (eql nil ls) acc
                 (let ((acc (+ (* 10 acc) (car ls))))
                   (compose (cdr ls) acc)))))
    (let* ((digit-ls (handler num))
           (len (length digit-ls)))
      (if (eql 0 (mod len 2))
          (let ((sub-ls-1 (subseq digit-ls 0 (ash len -1)))
                (sub-ls-2 (subseq digit-ls (ash len -1))))
            (values (compose sub-ls-1) (compose sub-ls-2)))
          (values nil nil)))))
(defun rules* (num)
  (if (eql num 0) 1
      (multiple-value-bind (sub-1 sub-2)
          (split* num)
        (cond
          (sub-1 (values sub-1 sub-2))
          (t (* 2024 num))))))

(defun blink* (ls)
  (reverse (reduce #'(lambda (acc num)
                       (multiple-value-bind (sub-1 sub-2)
                           (rules* num)
                         (push sub-1 acc)
                         (when sub-2 (push sub-2 acc))
                         acc))
                   ls :initial-value nil)))

(defun solve* (ls &optional (count 75)) ; 需要根据固定模式讨论, 0, 1, 2, 4, 8等等
  (cond
    ((eql count 0) (length ls) ls)
    (t (solve* (blink* ls) (1- count)))))

;; 查表优化
(defparameter *revol-table*
  '((0 (1) ;V
     (1) (1)) ; (revolve X) x times
    (1 (1 2 4) ;V
     (0 2 4) (1 2 1))
    (2 (1 2 4) ;V
     (0 4 8) (1 2 1))
    (3 (1 2 4) ;V
     (0 2 6 7) (1 1 1 1))
    (4 (1 2 4) ;V
     (0 6 8 9) (1 1 1 1))
    (5 (1 1 2 4 8) ;V
     (0 2 4 8) (2 2 1 3))
    (6 (1 1 2 4 8) ;V
     (2 4 5 6 7 9) (1 2 2 1 1 1))
    (7 (1 1 2 4 8) ;V 
     (0 2 3 6 7 8) (1 2 1 2 1 1))
    (8 (1 1 2 4) ;V
     (32 77 26 8) (1 1 1 1))
    (32 (2) (2 3) (1 1))
    (77 (2) (7) (2))
    (26 (2) (2 6) (1 1))
    (9 (1 1 2 4 8) ;V
     (1 3 4 6 8 9) (1 1 1 2 2 1))
    (4979 (2 4) ;V
     (4 7 9) (1 1 2))
    (24 (2) ;V
     (2 4) (1 1))
    (4356199 (1 1 1 2 4 7 7 12 16 21)
     (88167 84856) (1 1))
    (914 (1 1 2)
     (37442 70464) (1 1))
    (85734 (1 1 2 4)
     (351 215 846 784) (1 1 1 1))
    (698829 (2)
     (698 829) (1 1))
    )) ;或许能够自动生成这张表？或许改用Prolog+记忆化？

(defun range (len &optional (begin 0))
  (loop for i from begin below (+ len begin) collect i))
(defun generate-revolve (table) ; 自动生成函数
  (let* ((x-ls (mapcar #'car table))
         (case-ls (mapcar #'cadr table))
         (otherwise-x-ls (mapcar #'caddr table))
         (otherwise-times-ls (mapcar #'cadddr table))
         (otherwise-expr-ls
           (mapcar #'(lambda (x-ls times-ls)
                       (reverse
                        (reduce #'(lambda (acc x) (push x acc) acc)
                                (mapcar #'(lambda (x times) `(* ,times (revolve ,x (1- num))))
                                        x-ls times-ls)
                                :initial-value '(+))))
                   otherwise-x-ls otherwise-times-ls))
         (case-expr-ls
           (mapcar #'(lambda (ls)
                       (let* ((range (range (length ls) 1))
                              (pair-ls (mapcar #'list range ls)))
                         `(case num ,@pair-ls)))
                   case-ls))
         (sub-case-ls
           (mapcar #'(lambda (case-num other) (append case-num `((otherwise ,other))))
                   case-expr-ls otherwise-expr-ls))
         (main-case-ls
           (mapcar #'(lambda (x sub-case) `(,x ,sub-case)) x-ls sub-case-ls))
         (main-case-expr
           (reverse (reduce #'(lambda (acc x) (push x acc))
                            main-case-ls :initial-value '(x case)))))
    `(defun revolve (x num) ,main-case-expr)))

(eval (generate-revolve *revol-table*)) ; 生成revolve函数

(defun solve** (ls &optional (count 75))
  (cond
    ((eql count 0) (length ls))
    (t (let* ((revolution (mapcar #'(lambda (x)
                                      (cond
                                        ((and (>= x 0) (<= x 9))
                                         (revolve x count))
                                        (t
                                        (solve** (blink* (list x)) (1- count))
                                        )))
                                  ls))
              (sum (reduce #'+ revolution)))
         sum)
     ;(solve* (blink* ls) (1- count))
       )))
