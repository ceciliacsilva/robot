;;Artificial Intelligence for Robotics
;;Robotics Group - LINA - UFU

;;File: sense-move.lisp

(defun sense (p map1 sensor-read sensor-color)
    (let* ((color-sensor-right sensor-color)
	   (color-sensor-wrong (/ color-sensor-right 3)))
      (loop
	 for p-row     in p
	 for color-row in map1
	 collect
	   (loop
	      for p-i   in p-row
	      for cor-i in color-row
	      collect
		(let ((hit (if
			    (and
			     (equalp (r cor-i) (r sensor-read))
			     (equalp (g cor-i) (g sensor-read))
			     (equalp (b cor-i) (b sensor-read)))
			    1 0)))
		  (* p-i
		     (+ (* hit       color-sensor-right)
			(* (- 1 hit) color-sensor-wrong))))) into q
	 finally (return
		   (let ((sum (apply #'+ (flatten q))))
		     (map 'list #'(lambda(a) (map 'list #'(lambda(b) (/ b sum)) a)) q)))
	   ) ))

(defun move (p motion sensor-move)
  (let ((sensor-stay (- 1 sensor-move))
	(m-ver       (car  motion)) ;;y
	(m-hor       (cadr motion)) ;;x
	(tam-row     (list-length p))
	(tam-col     (list-length (car p))))
    (loop
       for p-row in p
       for i below tam-row ;;y
       collect
	 (loop
	    for p-ele in p-row
	    for j below tam-col ;;x
	    collect
	      (let* ((i-pos (mod (- i m-ver) tam-row))
		     (j-pos (mod (- j m-hor) tam-col))
		     (p-ij  (nth j-pos (nth i-pos p))))
		(+ (* sensor-move p-ij) (* sensor-stay p-ele)))) )))
    
(defun flatten (ls)
  (labels ((mklist (x) (if (listp x) x (list x))))
        (mapcan #'(lambda (x) (if (atom x) (mklist x) (flatten x))) ls)))

(defun my-filter  (f args)
  (cond ((null args) nil)
	((if (funcall f (car args))
	     (cons (car args) (my-filter  f (cdr args)))
	     (my-filter  f (cdr args)))))) 
