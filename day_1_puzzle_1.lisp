(defun read-elf-calories (file)
  (loop :for line = (read-line file nil)
	:while (and (not (null line))
		    (/= (length line) 1))
	:sum (parse-integer line) :into total
	:finally (return total)))

(with-open-file (file "day_1_puzzle_1.txt" :direction :input)
  (let ((highest-total 0))
    (loop :for total = (read-elf-calories file)
	  :while (/= total 0)
	  :do (if (> total highest-total)
		  (setq highest-total total)))
    (format t "highest total: ~a~%" highest-total)))
