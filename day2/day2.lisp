
;;; 已经从REPL输入了*input*，作为defparameter

;; 第1部分
(defun diff-lst (lst)
  (let* ((rest-list (rest lst))
         (diff (mapcar #'(lambda (lst-item next-item)
                           (- next-item lst-item))
                       lst rest-list)))
    diff))

(defun positive-p (lst)
  (reduce #'(lambda (acc x) (and acc (> x 0))) lst :initial-value t))
(defun negative-p (lst)
  (reduce #'(lambda (acc x) (and acc (< x 0))) lst :initial-value t))

(defun same-sign-p (lst)
  (or (positive-p lst) (negative-p lst)))

(defun revert-sign (lst)
  (mapcar #'- lst))

(defun valid-abs-diff-p (lst)
  (let ((invalid-list (remove-if #'(lambda (x) (and (>= x 1) (<= x 3))) lst)))
    (not invalid-list)))

(defun safe-lst-p (lst)
  (let ((diff-list (diff-lst lst)))
    (cond
      ((same-sign-p diff-list)
       (let ((checked-diff (if (negative-p diff-list)
                               (revert-sign diff-list) diff-list)))
         (valid-abs-diff-p checked-diff)))
      (t nil))))

(defun safe-count (input)
  (let ((safe-data (remove-if-not #'safe-lst-p input)))
    (length safe-data)))

;; 第二部分
(defun ascending-guess-p (diff-lst)
  (let ((pos-count (length (remove-if-not #'(lambda (x) (> x 0)) diff-lst)))
        (neg-count (length (remove-if-not #'(lambda (x) (< x 0)) diff-lst))))
    (> pos-count neg-count)))

;(defun valid-abs-diff-dampener (lst)
;  (let ((result (reduce
;                 #'(lambda (acc x)
;                     (let ((p-valid (car acc))
;                           (rest-list (cadr acc))
;                           (last-item (caddr acc)))
;                       (cond
;                         ((eql rest-list nil) acc)
;                         (p-valid (cond
;                                    ((and (>= x 1) (<= x 3)) (list t (cdr rest-list) x))
;                                    (t
                                     ;(format t "排除一个，x为~a~%=>此时剩下的列表~a;~%" x rest-list)
;                                     (let* ((rest-list-1 (rest rest-list))
;                                            (rest-list-1 (if rest-list-1
;                                                             (cons (+ x (car rest-list-1))
;                                                                   (cdr rest-list-1))
;                                                             nil))
;                                            (rest-list-2 (cons (+ x last-item) rest-list)))
;                                       (if (valid-abs-diff-p rest-list-2)
;                                           '(t nil x)
;                                           (list nil rest-list-1 x))))))
;                         (t (if (valid-abs-diff-p rest-list)
;                                '(t nil x) '(nil nil x))))))
;                 lst :initial-value (list t lst 0))))
;    (and (eql (car result) t)
;         (eql (cadr result) nil))))

(defun valid-diff-item-p (x)
  (and (>= x 1) (<= x 3)))

(defun valid-abs-diff-dampener (lst)
  (labels ((check (his-head cur-ls fut-head)
             (cond
               ((eql (length cur-ls) 1) t)
               ((valid-diff-item-p (car cur-ls))
                (check (car cur-ls) (cdr cur-ls) (caddr cur-ls)))
               (t (format t "==>~%lst:~a~%his:~a,~% cur-list:~a,~% fut-head:~a~%accepted:~a~%"
                          lst
                          (cons (+ his-head (car cur-ls)) (cdr cur-ls))
                          cur-ls
                          (cons (+ (car cur-ls) fut-head) (cddr cur-ls))
                          (or (valid-abs-diff-p (cons (+ his-head (car cur-ls)) (cdr cur-ls)))
                              (valid-abs-diff-p (cons (+ (car cur-ls) fut-head) (cddr cur-ls)))))
                  (or (valid-abs-diff-p (cons (+ his-head (car cur-ls)) (cdr cur-ls)))
                      (valid-abs-diff-p (cons (+ (car cur-ls) fut-head) (cddr cur-ls))))))))
    (if (valid-diff-item-p (car lst))
        (check (car lst) (cdr lst) (caddr lst))
        (or (valid-abs-diff-p (cdr lst))
            (valid-abs-diff-p (cons (+ (car lst) (cadr lst)) (cddr lst)))))))

(defun dampener-safe-lst-p (lst)
  (let* ((diff-list (diff-lst lst))
         (ascending-p (ascending-guess-p diff-list))
         (normalized-diff (if ascending-p
                              diff-list (revert-sign diff-list))))
    (valid-abs-diff-dampener normalized-diff)))
(defun dampener-count (input)
  (let ((safe-data (remove-if-not #'dampener-safe-lst-p input)))
    (length safe-data)))
