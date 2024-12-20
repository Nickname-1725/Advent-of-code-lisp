
;;; 从文件读取*input*，作为defparameter
;; 第1部分
(defun read-input ()
  (with-open-file (stream "./day8-process.txt")
    (read stream)))

(defparameter *input* (read-input))
(defparameter *height* (length *input*))
(defparameter *width* (length (car *input*)))

(defun reform (str-ls)
  (let* ((str (reduce #'(lambda (acc str)
                          (concatenate 'string acc (make-string (length (car str-ls))
                                                                :initial-element #\.) str))
                      str-ls))
         (char-ls (coerce str 'list)))
    char-ls))

(defun find-all-indices (char-ls char)
  (loop for item in char-ls
        for indx from 0
        when (eql item char)
          collect indx))

(defun make-map (len) `(#x0 ,len))
(defun write-map (map indx)
  (let ((len-max (cadr map)))
    (macrolet ((map-code () `(car map)))
      (labels ((valid-p (indx)
                 (and (>= indx 0) (< indx len-max))))
        (if (valid-p indx)
            (setf (map-code) (logior (map-code) (ash 1 indx))))
        map))))

(defun find-antinodes (indx-ls map)
  (labels ((handler (head rest-ls)
             (cond
               ((eql nil rest-ls) map)
               (t
                (mapcar #'(lambda (x)
                            (write-map map (- (* 2 head) x))
                            (write-map map (- (* 2 x) head)))
                        rest-ls)
                (handler (car rest-ls) (cdr rest-ls))))))
    (handler (car indx-ls) (cdr indx-ls))))

(defun process (input)
  (let* ((char-ls (reform input))
         (len-max (length char-ls))
         (antennas (remove #\. (remove-duplicates char-ls)))
         (antenna-lst (mapcar #'(lambda (char) (find-all-indices char-ls char))
                              antennas))
         (map (make-map len-max)))
    (map nil #'(lambda (antena) (find-antinodes antena map))
         antenna-lst)
    map))

(defun generate-full-map (width height)
  (let* ((line-full (1- (ash 1 width)))
         (acc line-full))
    (loop repeat (1- height)
          do (setf acc (logior (ash acc (* 2 width)) line-full)))
    acc))

(defun count-map (map width height)
  (let* ((full-map (generate-full-map width height))
         (capture-map (logand full-map (car map))))
    (logcount capture-map)))

(format t "~a~%" (count-map (process *input*) *width* *height*))

;; 第2部分
(defun valid-indx-p-generate (width height)
  (eval`(lambda (indx)
          (let ((len-max (* ,width (1- (* ,height 2)))))
            (and (>= indx 0) (< indx len-max)
                 (< (mod indx (* 2 ,width)) ,width))))))

(defun valid-map-indx (indx)
  (funcall (eval (valid-code-p-generate *width* *height*)) indx))

(defun generate-seq (x y &optional (valid-p #'(lambda (x) (declare (ignore x)) nil)))
  "假设y比x大"
  (let* ((diff (- y x))
         (grow-to-y (loop with a = y
                          while (funcall valid-p a)
                          collect a
                          do (setf a (+ a diff))))
         (grow-to-x (loop with a = x
                          while (funcall valid-p a)
                          collect a
                          do (setf a (- a diff))))
         (seq (append grow-to-x grow-to-y))) ; 拼接
    seq))

(defun find-antinodes* (indx-ls map width height)
  (labels ((handler (head rest-ls)
             (cond
               ((eql nil rest-ls) map)
               (t
                (mapcar #'(lambda (x)
                            (map nil #'(lambda (indx) (write-map map indx))
                                 (generate-seq x head ;#'valid-map-indx
                                               (valid-indx-p-generate width height))))
                        rest-ls)
                (handler (car rest-ls) (cdr rest-ls))))))
    (handler (car indx-ls) (cdr indx-ls))))

(defun process* (input)
  (let* ((height (length input))
         (width (length (car input)))
         (char-ls (reform input))
         (len-max (length char-ls))
         (antennas (remove #\. (remove-duplicates char-ls)))
         (antenna-lst (mapcar #'(lambda (char) (find-all-indices char-ls char))
                              antennas))
         (map (make-map len-max)))
    (map nil #'(lambda (antena) (find-antinodes* antena map width height))
         antenna-lst)
    map))

(format t "~a~%" (logcount (car(process* *input*))))
