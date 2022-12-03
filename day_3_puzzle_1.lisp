(defun calc-priority (item)
  (if (upper-case-p item)
      (- (char-code item) 38)
      (- (char-code item) 96)))

(defun get-shared-items (inventory)
  (let* ((trimmed (string-trim '(#\newline #\return) inventory))
	 (length (length trimmed))
	 (half-one (coerce (subseq trimmed 0 (/ length 2)) 'list))
	 (half-two (coerce (subseq trimmed (/ length 2)) 'list))
	 (intersection (intersection half-one half-two)))
    (format t "half one: ~{~C ~}~%" half-one)
    (format t "half two: ~{~C ~}~%" half-two)
    (format t "intersection: ~{~C ~}~%" intersection)
    (remove-duplicates intersection)))

(defun get-inventory-shared-sum (shared-items)
  (loop :for item :in shared-items
	:sum (calc-priority item) :into priority
	:finally (return priority)))

(with-open-file (file "day_3_puzzle_1.txt" :direction :input)
  (loop :for line = (read-line file nil)
	:while (not (null line))
	:sum (get-inventory-shared-sum (get-shared-items line)) :into total
	:finally (format t "total: ~a~%" total)))
