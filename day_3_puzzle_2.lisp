(defun calc-priority (item)
  (if (upper-case-p item)
      (- (char-code item) 38)
      (- (char-code item) 96)))

(defun trim-and-coerce (line)
  (if (null line)
      nil
      (coerce (string-trim '(#\newline #\return) line) 'list)))

(defun get-item-badge (file)
  (let* ((inv-1 (trim-and-coerce (read-line file nil)))
	 (inv-2 (trim-and-coerce (read-line file nil)))
	 (inv-3 (trim-and-coerce (read-line file nil)))
	 (shared (intersection inv-3 (intersection inv-1 inv-2))))
    (if (null shared)
	0
	(calc-priority (car shared)))))

(with-open-file (file "day_3_puzzle_1.txt" :direction :input)
  (loop :for badge = (get-item-badge file)
	:while (/= badge 0)
	:sum badge :into total
	:finally (format t "total: ~a~%" total)))
