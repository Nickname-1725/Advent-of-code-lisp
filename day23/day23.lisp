
;;; 从文件读取*input*，作为defparameter
;; 第1部分
(defun read-input ()
  (with-open-file (stream "./day23-process.txt")
    (read stream)))

(defun read-test ()
  (with-open-file (stream "./day23-test.txt")
    (read stream)))

(defparameter *input* (read-input))
(defparameter *context* (make-hash-table :test #'equal))

(defun write-context (contxt input)
  (declare (hash-table contxt) (list input))
  (map nil #'(lambda (pair)
               (setf (gethash pair contxt) t)
               (setf (gethash (cons (cdr pair) (car pair)) contxt) t))
       input))

(defun connect-p (contxt com-1 com-2)
  (declare (hash-table contxt) (symbol com-1 com-2))
  (gethash (cons com-1 com-2) contxt))

(defun LAN-party-p (contxt com-1 com-2 com-3)
  (declare (hash-table contxt) (symbol com-1 com-2 com-3))
  (and (connect-p contxt com-1 com-2)
       (connect-p contxt com-1 com-3)
       (connect-p contxt com-2 com-3)))

(defun filt-pairs (input &optional (begin-char #\T))
  (declare (list input) (standard-char begin-char))
  (labels ((to-string (sym)
             (format nil "~a" sym))
           (first-char-p (string char)
             (eql char (car (coerce string 'list)))))
    (remove-if-not #'(lambda (pair)
                       (or (first-char-p (to-string (car pair)) begin-char)
                           (first-char-p (to-string (cdr pair)) begin-char)))
                   input)))

(defun input->name-ls (input)
  (declare (list input))
  (let* ((ls (mapcan #'(lambda (pair) (list (car pair) (cdr pair))) input))
         (name-ls (remove-duplicates ls)))
    name-ls))

(defun same-group (group-1 group-2)
  (declare (list group-1 group-2))
  (and (eql (length group-1) (length group-2))
       (every #'(lambda (com) (member com group-2)) group-1)))

(defun process (context input)
  (declare (hash-table context) (list input))
  (let* ((valid-pairs (filt-pairs input))
         (name-ls (input->name-ls input))
         (group-ls (mapcan
                    #'(lambda (pair)
                        (let* ((ls-s (mapcar
                                      (lambda (name) (list (car pair) (cdr pair) name))
                                      name-ls))
                               (ls-s* (remove-if-not
                                       (lambda (ls) (LAN-party-p context (car ls)
                                                                 (cadr ls) (caddr ls)))
                                       ls-s)))
                          ls-s*))
                    valid-pairs)))
    (remove-duplicates group-ls :test #'same-group)))

;; 第2部分
(defun group-p (contxt ls)
  (declare (list ls))
  (cond
    ((< (length ls) 3) nil)
    ((eql (length ls) 3) (LAN-party-p contxt (car ls) (cadr ls) (caddr ls)))
    (t (and (group-p contxt (cdr ls))
            (every #'(lambda (com) (connect-p contxt (car ls) com)) (cdr ls))))))

(defun group-grow (context group com)
  (declare (hash-table context) (list group) (symbol com))
  (cond
    ((every #'(lambda (com*) (connect-p context com* com)) group)
     (cons com group))
    (t group)))

(defun name-ls->group-ls (context name-ls ls)
  (declare (list name-ls ls))
  (cond
    ((null name-ls) ls)
    (t
     (let* ((head-com (car name-ls))
            (group-ls (mapcar #'(lambda (group) (group-grow context group head-com)) ls))
            (group-ls* (remove-duplicates group-ls :test #'same-group)))
       (name-ls->group-ls context (cdr name-ls) group-ls*)))))

(defun solve* (context input)
  (let* ((name-ls (input->name-ls input))
         (group-ls (name-ls->group-ls context name-ls (mapcar #'list name-ls))))
    (sort (copy-list group-ls) #'(lambda (x y) (> (length x) (length y))))))

;; 过程：
(write-context *context* *input*)
(string-downcase (format nil "~{~a,~}" (sort (copy-list (car (solve* *context* *input*)))
                                            #'(lambda (x y) (string< (symbol-name x)
                                                                     (symbol-name y))))))
; 根本就是骗人吧，真正的结果里面没有哪一台电脑是T开头的，跟第1部分完全没关系了都
