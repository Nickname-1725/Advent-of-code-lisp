
;;; 从文件读取*input*，作为defparameter
;; 第1部分
(defun read-input ()
  (with-open-file (stream "./day24-process-1.txt")
    (read stream)))
(defun read-operations ()
  (with-open-file (stream "./day24-process-2.txt")
    (read stream)))

(defun read-test-in ()
  (with-open-file (stream "./day24-test-1.txt")
    (read stream)))
(defun read-test-ops ()
  (with-open-file (stream "./day24-test-2.txt")
    (read stream)))

(defparameter *input* (read-input))
(defparameter *operations* (read-operations))

(defparameter *test-in* (read-test-in))
(defparameter *test-ops* (read-test-ops))

(defun input->let-form (input &rest body)
  (let ((let-ls (mapcar (lambda (ls) (list (car ls) (cadr ls))) input)))
    `(let ,let-ls ,@body)))

;(defun xor (x y)
;  (and (or x y) (not (and x y))))

(defun sym->num (sym)
  (read-from-string (coerce (cdr (coerce (format nil "~a" sym) 'list)) 'string)))

(defun ops->z-sym-ls (ops)
  (let* ((sym-ls (mapcar #'cadr ops))
         (z-sym-ls (remove-if-not
                    #'(lambda (sym)
                        (eql #\Z (car (coerce (format nil "~a" sym) 'list))))
                    sym-ls))
         (z-sym-ls-sort (sort z-sym-ls #'(lambda (x y)
                                           (< (sym->num x) (sym->num y))))))
    z-sym-ls-sort))

(defun depends-on (ls-1 ls-2)
  "ls-2是否依赖于ls-2"
  (let ((1-dep (cadr ls-1)) (2-dep (car ls-2)))
    (member 1-dep 2-dep :test #'equal)))

(defun topo-sort (lst)
  "实际上是要做一个拓扑排序（不可用sort函数替代）"
  (let ((sorted-ls nil)
        (remain-ls lst))
    (loop while remain-ls
          do (let ((current (car remain-ls))
                   (rest (cdr remain-ls)))
               (if (every (lambda (item) (not (depends-on current item))) rest)
                   (progn
                     (push current sorted-ls)
                     (setf remain-ls rest))
                   (setf remain-ls (append rest (list current))))
               ))
    sorted-ls))

(defun ops->let-form (ops &rest body)
  (let* ((sort-ops (topo-sort ops))
         (prefix-form (mapcar
                       #'(lambda (ls)
                           (let* ((expr (car ls))
                                  (oprand-1 (car expr))
                                  (oprand-2 (caddr expr))
                                  (op (case (cadr expr)
                                        (and 'logand) (or 'logior) (xor 'logxor)))
                                  (name (cadr ls)))
                             `(,name (,op ,oprand-1 ,oprand-2))))
                       sort-ops)))
    `(let* ,prefix-form ,@body)))

(defun z-sym-ls->ls-ret (z-sym-ls)
  (let ((z-eval-ls (mapcar #'(lambda (sym) sym) z-sym-ls)))
    `(reverse (list ,@z-eval-ls))))
(defun in-ops->eval-form (input ops)
  (input->let-form input (ops->let-form ops (z-sym-ls->ls-ret (ops->z-sym-ls ops)))))

(defun bin-list->num (lst &optional (acc 0))
  (cond ((null lst) acc)
        (t (bin-list->num (cdr lst) (logior (ash acc 1) (car lst))))))

(format t "~a~%" (bin-list->num (eval (in-ops->eval-form *input* *operations*))))

;; 第2部分

;; 统计发现
;; 1. AND逻辑门存在富余，其中必定包含无用的节点；除此之外，XOR和OR均无过度使用的迹象
;; 2. Zxx必定通过XOR得出，通过非这种手段获得的有问题（获得XOR门需要消耗AND、OR或者NOT门（没有））
;;    分别是: 
;;    1) Z07(AND): Y07 X07
;;    2) Z20(IOR): JBQ HDS
;;    3) Z28(AND): VWH JSG
;;    4) Z45(IOR): VFW BVD
;; 3. 只有四对逻辑门输出有反接的问题
