
;;; 已经从REPL输入了*input*，作为defparameter
(ql:quickload :cl-ppcre)

;; 第1部分
(defparameter *regex* "mul\\(\\d{1,3},\\d{1,3}\\)")

(defun catch-instru (regex input)
  (let* ((all-match (cl-ppcre:all-matches-as-strings regex input))
         (pairs (mapcar #'(lambda (str)
                            (let* ((str-pair (cl-ppcre:all-matches-as-strings "\\d{1,3}" str))
                                   (num-pair (mapcar #'parse-integer str-pair)))
                              num-pair))
                        all-match)))
    pairs))

(defun calc (instr)
  (let* ((mult-exec (mapcar #'(lambda (pair) (* (car pair) (cadr pair))) instr))
         (sum (reduce #'+ mult-exec :initial-value 0)))
    sum))

(calc (catch-instru *regex* *input*))

;; 第2部分
(defparameter *regex-new* "mul\\(\\d{1,3},\\d{1,3}\\)|do\\(\\)|don't\\(\\)")
(defun catch-instru* (regex input)
  (let* ((all-match (cl-ppcre:all-matches-as-strings regex input))
         (pairs (mapcar #'(lambda (str)
                            (let* ((str-pair (cl-ppcre:all-matches-as-strings "\\d{1,3}" str))
                                   (num-pair (mapcar #'parse-integer str-pair)))
                              (cond
                                (num-pair num-pair)
                                ((equal str "do()") 'do)
                                ((equal str "don't()") 'dont)
                                ;(t str)
                                )
                              ))
                        all-match)))
    pairs))
(defun filt-instru (instr)
  (let* ((stat-do t)
         (filted (reduce #'(lambda (acc x)
                             (cond
                               ((equal x 'do) (setf stat-do t) acc)
                               ((equal x 'dont) (setf stat-do nil) acc)
                               (t (if stat-do (cons x acc) acc))))
                         instr :initial-value nil))
         (filted (reverse filted)))
    filted))

(calc (filt-instru (catch-instru* *regex-new* *input*)))
