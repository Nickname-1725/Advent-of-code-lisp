
;;; 从文件读取*input*，作为defparameter
;; 第1部分
(defun read-input ()
  (with-open-file (stream "./day21-process.txt")
    (read stream)))

(defparameter *input* (read-input))

;      y
;      |
; x -- O
(defun num-keypad->y-x (num-key)
  (case num-key
    (7 '(3 . 2)) (8 '(3 . 1)) (9 '(3 . 0))
    (4 '(2 . 2)) (5 '(2 . 1)) (6 '(2 . 0))
    (1 '(1 . 2)) (2 '(1 . 1)) (3 '(1 . 0))
    (0 '(0 . 1) ) (A '(0 . 0))))

; x -- O
;      |
;      y
(defun dir-keypad->y-x (dir-key)
  (case dir-key
    (^ '(0 . 1)) (A '(0 . 0))
    (< '(1 . 2)) (v '(1 . 1)) (> '(1 . 0))))

(defun parse-y-x-ls (y-x-ls &optional (num-pad-p nil))
  (mapcar #'(lambda (pair-1 pair-2)
              (let ((y-diff (- (car pair-2) (car pair-1)))
                    (x-diff (- (cdr pair-2) (cdr pair-1))))
                `(,(cond
                     (num-pad-p
                      (cond ((> y-diff 0) `(^ . ,y-diff))
                            (t `(v . ,(- y-diff)))))
                     (t (cond ((> y-diff 0) `(v . ,y-diff))
                              (t `(^ . ,(- y-diff))))))
                  ,(cond
                     ((> x-diff 0) `(< . ,x-diff))
                     (t `(> . ,(- x-diff)))))))
          (cons '(0 . 0) y-x-ls) y-x-ls))

(defun repeat-sym (sym &optional (num 0) (acc nil))
  ;(format t  "~a~%" acc)
  (cond
    ((<= num 0) acc)
    (t (repeat-sym sym (1- num) (push sym acc)))))

(defun parse-ls->key-ls (parse-ls)
  (let* ((trans-1 (mapcan #'(lambda (pairs)
                              (list (repeat-sym (caar pairs) (cdar pairs))
                                    (repeat-sym (caadr pairs) (cdadr pairs))
                                    '(A)))
                          parse-ls))
         (trans-2 (remove nil trans-1))
         (trans-3 (reduce #'append trans-2)))
    trans-3))

(defun process-num (num-ls)
  (let* ((step-1 (parse-ls->key-ls (parse-y-x-ls
                                    (mapcar #'num-keypad->y-x num-ls) t)))
         (step-2 (parse-ls->key-ls (parse-y-x-ls
                                    (mapcar #'dir-keypad->y-x step-1))))
         (step-3 (parse-ls->key-ls (parse-y-x-ls
                                    (mapcar #'dir-keypad->y-x step-2)))))
    step-3))

(defun num-code->num (ls)
  (+ (* 100 (car ls)) (* 10 (cadr ls)) (caddr ls)))

(defun solve (input)
  (let* ((num-ls (mapcar #'num-code->num input))
         (inst-ls (mapcar #'process-num input))
         (inst-len-ls (mapcar #'length inst-ls))
         (prod-ls (mapcar #'* num-ls inst-len-ls))
         (add-prod (reduce #'+ prod-ls)))
    add-prod
    (values num-ls inst-len-ls)))

;; 第2部分

