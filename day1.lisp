
;;; 已经从REPL输入了*input*，作为defparameter
;; 第1部分可以直接用Excel解决，并且直接作为第2部分的输入

;; 第2部分
(defun process (input)
  "处理*input*"
  (labels ((duplicates (olst)
             "olst是升序排列的已经排序的列表"
             (let* ((group (reduce #'(lambda (acc item)
                                       (let* ((look-up (assoc item acc))
                                              (count (cadr look-up)))
                                         (cond
                                           (look-up
                                            (setf (cadr (assoc item acc)) (1+ count)))
                                           (t
                                            (push (list item 1) acc)))
                                         acc))
                                   olst
                                   :initial-value nil))
                    (group-list (reverse group)))
               group-list)))
    (let* ((left-list (mapcar #'car input))
           (right-list (mapcar #'cadr input))
           (group-list (duplicates right-list))
           (filt-list (remove-if-not #'(lambda(item)
                                         (let ((id (car item)))
                                           (find id left-list)))
                                     group-list))
           (mult-result (reduce (lambda (acc pair)
                                  (let ((id (car pair))
                                        (count (cadr pair)))
                                    (+ acc (* id count))))
                                filt-list :initial-value 0)))
      mult-result)))

