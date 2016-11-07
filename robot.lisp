;;Artificial Intelligence for Robotics
;;Robotics Group - LINA - UFU

;;File: robot.lisp

(ql:quickload 'opticl)

(defpackage "ROBOT"
  (:use "COMMON-LISP" "OPTICL"))

(in-package :robot)

(load "/home/cecilia/robot/sense-move.lisp")

(defclass rgb ()
  ((r :accessor r :initarg :r)
   (g :accessor g :initarg :g)
   (b :accessor b :initarg :b)))

(defclass cor-posicao()
  ((cor      :accessor cor   :initarg :cor)
   (limite-x :accessor lim-x :initarg :x)
   (limite-y :accessor lim-y :initarg :y)))

(defparameter azul     (make-instance 'rgb :r 0   :g 0   :b 128))
(defparameter amarelo  (make-instance 'rgb :r 255 :g 255 :b 0))
(defparameter verde    (make-instance 'rgb :r 0   :g 100 :b 0))
(defparameter vermelho (make-instance 'rgb :r 139 :g 0   :b 0))

(defvar *possible-move* '((1 0) (-1 0) (0 1) (0 -1)))
(defvar *square* 100)

(defun map->p (tam-y tam-x)
  (let* ((qndd (* tam-x tam-y))
	 (p-i  (/ 1 qndd)))
    (loop repeat tam-y
       collect
	 (loop repeat tam-x
	    collect p-i)) ))

(defun map-complete (map1)
  (loop
     for linha in map1
     summing *square* into lim-y
     collect
       (loop
	  for elemento in linha
	  summing *square* into lim-x
	  collect (make-instance 'cor-posicao :cor elemento :x lim-x :y lim-y) )
    ))

(defun draw-map (map1 name-project cont x y tam-y tam-x)
  (let ((map1-completo (map-complete map1)))
    (let* ((height (* tam-y *square*))
	   (width  (* tam-x *square*)))
      (let ((result (make-8-bit-rgb-image height width)))
	(loop
	   for i below height
	   collect
	     (loop
		for j below width 
		do (setf (pixel* result i j) (rgb->list (matrix->pixel map1-completo i j))) ))

	;;caso saia da figura
	(let ((raio-desenhado-y (- height y))
	      (raio-desenhado-x (- width x)))
	  (if (< raio-desenhado-y 30)
	      (progn
		(fill-circle* result (- raio-desenhado-y) x 30 '(0 0 0))
		(fill-circle* result (- raio-desenhado-y) x 10 '(255 255 255)) )) 
	  (if (< y 30)
	      (progn
		(fill-circle* result (+ height y) x 30 '(0 0 0))
		(fill-circle* result (+ height y) x 10 '(255 255 255)) ))
	  (if (< raio-desenhado-x 30)
	      (progn
		(fill-circle* result y (- raio-desenhado-x) 30 '(0 0 0))
		(fill-circle* result y (- raio-desenhado-x) 10 '(255 255 255)) ))
	  (if (< x 30)
	      (progn
		(fill-circle* result y (+ width x) 30 '(0 0 0))
		(fill-circle* result y (+ width x) 10 '(255 255 255)) )) )
	(fill-circle* result y x 30 '(0 0 0))
	(fill-circle* result y x 10 '(255 255 255))
	(write-jpeg-file (concatenate 'string "maps/" name-project "/map" cont ".jpeg") result)
	))
    ))

(defun draw-gradient (p name-project cont tam-y tam-x)
  (let ((p-position (map-complete p)))
    (let* ((height (* tam-y *square*))
	   (width  (* tam-x *square*)))
      (let ((result (make-8-bit-rgb-image height width)))
	(loop
	   for i below height
	   collect
	     (loop
		for j below width
		do (setf (pixel* result i j) (p->rgb (matrix->pixel p-position i j))) ))
	(write-jpeg-file (concatenate 'string "maps/" name-project "/gradient" cont ".jpeg") result)
	))
    ))

(defun random-nth (lista)
  (nth (random (list-length lista)) lista) )

(defun localization (map1 name-project sensor-color sensor-move)
  (let* ((tam-y (list-length map1))
	 (tam-x (list-length (car map1)))
	 (p (map->p tam-y tam-x))
	 (height (* tam-y *square*))
	 (width  (* tam-x *square*))
	 (x (random width))
	 (y (random height))
	 (move-wrong (- 1 sensor-move))
	 (q p)
	 (possible-move *possible-move*)
	 (cont 0))
    (sb-ext:run-program "/bin/sh"
			(list "-c" (concatenate 'string "rm -rf /home/cecilia/robot/maps/"
						name-project " && mkdir /home/cecilia/robot/maps/" name-project))
			:input nil
			:output nil)
    (lambda()
      (let* ((move (random-nth possible-move))
	     (move-y (car move))
	     (move-x (cadr move))
	     (prob-move-wrong (if (zerop move-wrong) 0 (random move-wrong)))
	     (move-final (+ sensor-move prob-move-wrong))
	     (cor-read (sensor-read (map-complete map1) x y)))
	(setf q (sense q map1 cor-read sensor-color))
	(setf q (move q move sensor-move))
	(setf x (mod (+ x (truncate (* move-final *square* move-x))) width))
	(setf y (mod (+ y (truncate (* move-final *square* move-y))) height))
	(draw-map map1 name-project (write-to-string cont) x y tam-y tam-x)
	(draw-gradient q name-project (write-to-string cont) tam-y tam-x)
	(sb-ext:run-program "/bin/sh"
			    (list "-c" (concatenate 'string "shotwell /home/cecilia/robot/maps/" name-project
						    "/gradient" (write-to-string cont) ".jpeg"))
			    :input  nil
			    :output nil)
	(setf cont (+ cont 1))
	)) ))
  
(defun sensor-read (map1 x y)
  (matrix->pixel map1 x y))

(defun matrix->pixel (map1 i j)
  (car
   (my-filter
    #'(lambda(b) (not (equalp b nil)))
    (flatten (map 'list
		  #'(lambda(a) (map 'list
				    #'(lambda(b)
					(and (< j (lim-x b)) (< i (lim-y b)) (cor b))) a))
		  map1))) ))

(defun rgb->list (cor)
  (list (r cor) (g cor) (b cor)))

(defun p->rgb (p-i)
  (list (r-gradient p-i) (g-gradient p-i) (b-gradient p-i)) )

(defun r-gradient (p)
  (truncate (* 255 p)))

(defun g-gradient (p)
  (if (< p 0.378)
      0
      (truncate (* 255 (/ (- p 0.378) (- 1 0.378)))) ))

(defun b-gradient (p)
  (if (< p 0.378)
      (truncate (+ (* (/ 155 0.378) p) 100))
      (truncate (+ (* (- 255) (/ (- p 0.378)
				 (- 1 0.378)))
		   255)) ))

(defun p->gradient ()
  (let ((result (make-8-bit-rgb-image 400 400)))
    (loop
       for i from 0 below 400 by 10
       for p from 0 to 1 by (/ 1 40)
       do (fill-rectangle* result i 0 (+ i 10) 50 (p->rgb p)) )
    (fill-rectangle* result 0 50 400 72 '(190 190 190))
    (fill-rectangle* result 0 60 400 60 '(0 0 0))
    (fill-rectangle* result 0 55 2 65 '(0 0 0))
    (fill-rectangle* result 398 55 400 65 '(0 0 0))

    ;;1
    (fill-rectangle* result 392 68 398 68 '(0 0 0))
    ;;0
    (draw-circle*    result 3 69 3 '(0 0 0))

    (write-jpeg-file "grad.jpeg" result)
    ) )
