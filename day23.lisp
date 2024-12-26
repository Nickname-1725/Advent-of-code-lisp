
;;; 从文件读取*input*，作为defparameter
;; 第1部分
(defun read-input ()
  (with-open-file (stream "./day23-process.txt")
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
  (every #'(lambda (com) (member com group-2)) group-1))

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

