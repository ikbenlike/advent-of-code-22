(defun translate-move (move)
  (cond ((or (char= move #\A)
	     (char= move #\X))
	 :rock)
	((or (char= move #\B)
	     (char= move #\Y))
	 :paper)
	((or (char= move #\C)
	     (char= move #\Z))
	 :scissors)))

(defun calc-move-score (move)
  (case move
    (:rock 1)
    (:paper 2)
    (:scissors 3)))

(defun get-win-move (move)
  (case move
    (:rock :paper)
    (:paper :scissors)
    (:scissors :rock)))

(defun calc-win-score (theirs yours)
  (let ((win-move (get-win-move theirs)))
    (cond ((eq win-move yours) 6)
	  ((eq theirs yours) 3)
	  (t 0))))

(defun calc-round-score (round)
  (let* ((theirs (translate-move (char round 0)))
	 (yours (translate-move (char round 2)))
	 (win-score (calc-win-score theirs yours))
	 (move-score (calc-move-score yours))
	 (total-score (+ win-score move-score)))
    total-score))

(with-open-file (file "day_2_puzzle_1.txt" :direction :input)
  (loop :for round = (read-line file nil)
	:while (not (null round))
	:sum (calc-round-score round) :into total-score
	:finally (format t "total score: ~a~%" total-score)))
