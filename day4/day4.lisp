
;;; 已经从REPL输入了*input*，作为defparameter
(ql:quickload :cl-ppcre)

;; 第1部分
;(defparameter *regex* "mul\\(\\d{1,3},\\d{1,3}\\)")

(defun combine-lists (lists)
  (apply #'mapcar #'list lists))
(defun count-hori (input)
  (let* ((matches-count-1 (mapcar #'(lambda (str)
                                      (length (cl-ppcre:all-matches-as-strings "XMAS" str)))
                                  input))
         (matches-count-2 (mapcar #'(lambda (str)
                                      (length (cl-ppcre:all-matches-as-strings "SAMX" str)))
                                  input))
         (matches-sum-1 (reduce #'+ matches-count-1 :initial-value 0))
         (matches-sum-2 (reduce #'+ matches-count-2 :initial-value 0)))
    (+ matches-sum-1 matches-sum-2)))
(defun transpos (input)
  (let* ((split-list (mapcar #'(lambda (str) (coerce str 'list)) input))
         (recompose-list (combine-lists split-list))
         (output (mapcar #'(lambda (lst) (coerce lst 'string)) recompose-list)))
    output))

(defun count-vert (input)
  (count-hori (transpos input)))

(defun shear (input)
  (let* ((split-list (mapcar #'(lambda (str) (coerce str 'list)) input))
         (len (length input))
         (ordered-list (loop for i from 0 below len collect i))
         (recompose-list
           (mapcar #'(lambda (lst i)
                       (append (make-list i :initial-element #\.)
                               lst
                               (make-list (- len 1 i) :initial-element #\.)))
                   split-list ordered-list))
         (output (mapcar #'(lambda (lst) (coerce lst 'string)) recompose-list)))
    output))

(defun count-back-diag (input)
  (count-hori (transpos (shear input))))
(defun count-diag (input)
  (count-hori (transpos (shear (reverse input)))))

(defun count-all (input)
  (+ (count-vert input) (count-hori input)
     (count-back-diag input) (count-diag input)))

(count-all *input*)

;; 第2部分
; 思路
; 1. 首先在每一行末尾填充"..."
; 2. 然后拼接所有行
; 3. 搜索"M.S.{多少}A{多少}M.S"，穷举所有的方向即可
; 4. 需要保证子字符串之间允许重叠
; 5. 按照copilot的方式，可以用cl-ppcre:scan和loop来做

(defparameter *len* (length (car *input*)))
(defun join-strings (strings seperator)
  (reduce #'(lambda (acc x) (concatenate 'string acc seperator x))
          strings))
(defun generate-regex (str-lst len &optional (augment 3))
  (join-strings str-lst (format nil ".{~a}" (+ (- len (length (car str-lst)))
                                               augment))))

(defparameter *regex* "M.M.{10}.A..{10}S.S")
(defun find-overlapping-matches (regex string)
  (loop with start = 0
        for match = (cl-ppcre:scan regex string :start start)
        while match
        collect `(,match ,(cl-ppcre:scan-to-strings regex string :start start))
        do (setf start (1+ match))))

(defun search-x (input)
  (let* ((split-list (mapcar #'(lambda (str)
                                 (coerce str 'list))
                             input))
         (augmented-list (mapcar #'(lambda (ls) (append ls (make-list 3 :initial-element #\.)))
                                 split-list))
         (flatten (apply #'append augmented-list))
         (cat-string (concatenate 'string flatten))
         (regex-ls (mapcar #'(lambda (lst) (generate-regex lst *len* 3))
                        '(("M.M" ".A." "S.S") ("M.S" ".A." "M.S")
                          ("S.S" ".A." "M.M") ("S.M" ".A." "S.M"))))
         (regex (join-strings regex-ls "|"))
         (all-matches (find-overlapping-matches regex cat-string)))
    (length all-matches)))
