;;Artificial Intelligence for Robotics
;;Robotics Group - LINA - UFU

;;File: sense-move.lisp

(defparameter sensor-color 0.8)
(defparameter sensor-move  0.8)

(defun sense (p map1 sensor-read sensor-color)
    (let* ((color-sensor-right sensor-color)
	   (color-sensor-wrong (- 1 color-sensor-right)))
      (loop
	 for p-row     in p
	 for color-row in map1
	 collect
	   (loop
	      for p-i  in p-row
	      for cor  in color-row
	      collect
		(let ((hit (if (equalp cor sensor-read) 1 0)))
		  (* pi
		     (+ (* hit       color-sensor-right)
			(* (- 1 hit) color-sensor-wrong))))) into q
	 finally (return
		   (let ((sum (apply #'+ (flatten q))))
		     (map 'list #'(lambda(a) (map 'list #'(lambda(b) (/ b sum)) a)) q)))
	   ) ))

(defun move (p motion sensor-move)
  (let ((sensor-stay (- 1 sensor-move))
	(m-hor       (car  motion))
	(m-ver       (cadr motion))
	(tam-row     (list-length p))
	(tam-col     (list-length (car p))))
    (loop
       for p-row in p
       for i below tam-row
       collect
	 (loop
	    for p-ele in p-row
	    for j below tam-col
	    collect
	      (let* ((i-pos (mod (- i m-hor) tam-row))
		     (j-pos (mod (- j m-ver) tam-col))
		     (p-ij  (nth j-pos (nth i-pos p))))
		(+ (* sensor-move p-ij) (* sensor-stay p-ele)))) )))
    
(defun flatten (ls)
  (labels ((mklist (x) (if (listp x) x (list x))))
        (mapcan #'(lambda (x) (if (atom x) (mklist x) (flatten x))) ls)))
