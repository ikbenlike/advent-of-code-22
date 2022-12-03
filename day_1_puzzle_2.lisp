(defun read-elf-calories (file)
  (loop :for line = (read-line file nil)
	:while (and (not (null line))
		    (/= (length line) 1))
	:sum (parse-integer line) :into total
	:finally (return total)))

(defun read-total-calories (file)
  (loop :for total = (read-elf-calories file)
	:while (/= total 0)
	:collect total :into totals
	:finally (return totals)))

(with-open-file (file "day_1_puzzle_1.txt" :direction :input)
  (let* ((totals (read-total-calories file))
	 (sorted (sort totals #'>))
	 (top-3 (subseq sorted 0 3))
	 (sum (apply #'+ top-3)))
    (format t "top 3 summed: ~a~%" sum)))
