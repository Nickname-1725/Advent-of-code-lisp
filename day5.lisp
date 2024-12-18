
;;; 已经从REPL输入了*rules*, *update-list*，作为defparameter

;; 第1部分

(defun update-valid-p (rules update)
  (labels ((process (head rest-ls)
             (cond
               ((eql rest-ls nil) t)
               (t
                (let* ((fetch-head (remove-if-not
                                    #'(lambda(x) (equal (car x) head))
                                    rules))
                       (after-pages (mapcar #'cdr fetch-head))
                       (pages-check (mapcar #'(lambda (page) (page-check page rest-ls))
                                     after-pages)))
                  (cond
                    ((reduce (lambda (acc x) (and acc x)) pages-check)
                     (process (car rest-ls) (cdr rest-ls)))
                    (t nil))))
               ))
           (page-check (page ls)
             (cond
               ((eql (find page update) nil) t)
               ((find page ls) t)
               (t nil))))
    (process (car update) (cdr update))
    ))

(defun mid-page (update)
  (let* ((len (length update))
         (mid-indx (ash len -1))
         (mid-page (nth mid-indx update)))
    mid-page))

(defun process (rules update-list)
  (let* ((valid-updates (remove-if-not #'(lambda (x) (update-valid-p rules x)) update-list))
         (mid-pages (mapcar #'mid-page valid-updates))
         (mid-page-sum (reduce #'+ mid-pages)))
    mid-page-sum))

;; 第2部分
(defun reorder (rules update)
  (sort (copy-list update) #'(lambda (x y)
                               (find `(,x . ,y) rules :test #'equal))))
(defun process* (rules update-list)
  (let* ((incorrect-updates (remove-if #'(lambda (x) (update-valid-p rules x))  update-list))
         (reordered-updates (mapcar #'(lambda (x) (reorder rules x)) incorrect-updates))
         (mid-pages (mapcar #'mid-page reordered-updates))
         (mid-page-sum (reduce #'+ mid-pages)))
    mid-page-sum))
