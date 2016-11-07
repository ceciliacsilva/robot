;;Artificial Intelligence for Robotics
;;Robotics Group - LINA - UFU

;;File: robot.lisp

(ql:quickload 'opticl)

(defpackage "ROBOT"
  (:use "COMMON-LISP" "OPTICL"))

(in-package :robot)

(load "sense-move.lisp")

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

(defvar *tam-y*  3)
(defvar *tam-x*  3)
(defvar *square* 100)
(defvar *map1*
  (list (list vermelho verde azul amarelo)
	(list amarelo amarelo verde azul)
	(list verde vermelho amarelo verde)
	(list azul verde vermelho azul)))
(defvar *p*
  (let* ((qndd (* *tam-x* *tam-y*))
	 (p-i  (/ 1 qndd)))
    (loop repeat *tam-y*
       collect
	 (loop repeat *tam-x*
	    collect p-i)) ))
(defvar *possible-move* '((1 0) (-1 0) (0 1) (0 -1)))

(defvar *map2*
  (list (list verde verde verde)
	(list verde vermelho verde)
	(list verde verde verde)))
(defvar *p2*
  (let ((a (/ 1 9)))
    (list (list a a a)
	  (list a a a)
	  (list a a a))))

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

(defun draw-map (map1 x y)
  (let ((map1-completo (map-complete map1)))
    (let* ((height (* *tam-y* *square*))
	   (width  (* *tam-x* *square*)))
      (let ((result (make-8-bit-rgb-image height width)))
	(loop
	   for i below height
	   collect
	     (loop
		for j below width 
		do (setf (pixel* result i j) (rgb->list (matrix->pixel map1-completo i j))) ))
	(fill-circle* result y x 30 '(0 0 0))
	(write-jpeg-file "maps/map.jpeg" result)
	))
    ))

(defun draw-gradient (p)
  (let ((p-position (map-complete p)))
    (let* ((height (* *tam-y* *square*))
	   (width  (* *tam-x* *square*)))
      (let ((result (make-8-bit-rgb-image height width)))
	(loop
	   for i below height
	   collect
	     (loop
		for j below width
		do (setf (pixel* result i j) (p->rgb (matrix->pixel p-position i j))) ))
	(write-jpeg-file "maps/gradient.jpeg" result)
	))
    ))

(defun random-nth (lista)
  (nth (random (list-length lista)) lista) )

(defun localization (map1 p possible-move sensor-color sensor-move)
  (let* ((height (* *tam-y* *square*))
	 (width  (* *tam-x* *square*))
	 (x (random width))
	 (y (random height))
	 (move-wrong (- 1 sensor-move))
	 (q p))
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
	(draw-map map1 x y)
	(draw-gradient q)
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
  (let ((value (truncate (* p-i 255))))
    (list (- 255 value) 0 0)))
