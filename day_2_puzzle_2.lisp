(defun translate-move (move)
  (cond ((char= move #\A) :rock)
	((char= move #\B) :paper)
	((char= move #\C) :scissors)))

(defun translate-response (resp)
  (cond ((char= resp #\X) :lose)
	((char= resp #\Y) :draw)
	((char= resp #\Z) :win)))

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

(defun get-lose-move (move)
  (case move
    (:rock :scissors)
    (:paper :rock)
    (:scissors :paper)))

(defun get-required-move (move resp)
  (cond ((eq resp :lose) (get-lose-move move))
	((eq resp :draw) move)
	((eq resp :win) (get-win-move move))))

(defun calc-win-score (theirs yours)
  (let ((win-move (get-win-move theirs)))
    (cond ((eq win-move yours) 6)
	  ((eq theirs yours) 3)
	  (t 0))))

(defun calc-round-score (round)
  (let* ((theirs (translate-move (char round 0)))
	 (resp (translate-response (char round 2)))
	 (yours (get-required-move theirs resp))
	 (win-score (calc-win-score theirs yours))
	 (move-score (calc-move-score yours))
	 (total-score (+ win-score move-score)))
    total-score))

(with-open-file (file "day_2_puzzle_1.txt" :direction :input)
  (loop :for round = (read-line file nil)
	:while (not (null round))
	:sum (calc-round-score round) :into total-score
	:finally (format t "total score: ~a~%" total-score)))
