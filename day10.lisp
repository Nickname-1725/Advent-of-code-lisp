
;;; 从文件读取*input*，作为defparameter
;; 第1部分
(defun read-input ()
  (with-open-file (stream "./day10-process.txt")
    (read stream)))

(defun add-padding (input &optional (elem -1))
  "增加衬套，防止越界"
  (let* ((width (length (car input)))
         (add-width (mapcar #'(lambda (ls) (append (list elem) ls (list elem)))
                            input))
         (hori-bar (make-list (+ 2 width) :initial-element elem))
         (add-height (append (list hori-bar)
                             add-width (list hori-bar)))) ; 不涉及列表复制，谨慎使用
    add-height))

(defun list->array (input)
  (let* ((height (length input))
         (width (length (car input)))
         (array (make-array `(,height ,width))))
    (loop for j from 0 below height
          do (loop for i from 0 below width
                   do (setf (aref array j i) (nth i (nth j input)))))
    array))

(defparameter *input* (read-input))
(defparameter *map* (list->array *input*))

(defun collect-trailhead-ls (input)
  (let ((height (length input))
        (width (length (car input)))
        (trailhead-ls nil))
    (loop for j from 0 below height
          do (loop for i from 0 below width
                   when (eql 0 (nth i (nth j input)))
                     do (push `(,j . ,i) trailhead-ls)))
    (reverse trailhead-ls)))

(defun neighbour-ls (j-i-pair)
  "给定坐标，返回相邻坐标"
  (let ((j (car j-i-pair)) (i (cdr j-i-pair)))
  `((,(1- j) . ,i) (,(1+ j) . ,i) (,j . ,(1- i)) (,j . ,(1+ i)))))

(defun collect-score (j-i-pair map &optional (cur-num 0) (target-num 9))
  (if (eql cur-num target-num) (list j-i-pair)
      (let* ((neigh-ls (neighbour-ls j-i-pair))
             (valid-ls (remove-if-not
                        #'(lambda (j-i)
                            (let ((j (car j-i)) (i (cdr j-i)))
                              (eql 1 (- (aref map j i) cur-num))))
                        neigh-ls))
             (score-ls (mapcan
                        #'(lambda (j-i)
                            (let ((j (car j-i)) (i (cdr j-i)))
                              (collect-score j-i map (aref map j i))))
                        valid-ls))
             (score-ls* (remove-duplicates score-ls :test #'equal)))
        score-ls*)))

(defun process (input)
  (let* ((input (add-padding input))
         (map (list->array input))
         (trailhead-ls (collect-trailhead-ls input))
         (score-ls (mapcan #'(lambda (j-i) (collect-score j-i map)) trailhead-ls))
         (score-sum (length score-ls)))
    score-sum))

;; 第2部分

(defun collect-rating (j-i-pair map &optional (cur-num 0) (target-num 9))
  (if (eql cur-num target-num) (list j-i-pair)
      (let* ((neigh-ls (neighbour-ls j-i-pair))
             (valid-ls (remove-if-not
                        #'(lambda (j-i)
                            (let ((j (car j-i)) (i (cdr j-i)))
                              (eql 1 (- (aref map j i) cur-num))))
                        neigh-ls))
             (score-ls (mapcan
                        #'(lambda (j-i)
                            (let ((j (car j-i)) (i (cdr j-i)))
                              (collect-rating j-i map (aref map j i))))
                        valid-ls))) ; 比第1部分还要简单一点，不需要去重了
        score-ls)))

(defun process* (input)
  (let* ((input (add-padding input))
         (map (list->array input))
         (trailhead-ls (collect-trailhead-ls input))
         (score-ls (mapcan #'(lambda (j-i) (collect-rating j-i map)) trailhead-ls))
         (score-sum (length score-ls)))
    score-sum))
