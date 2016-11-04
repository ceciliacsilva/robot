;;Artificial Intelligence for Robotics
;;Robotics Group - LINA - UFU

(ql:quickload 'opticl)

(defpackage "ROBOT"
  (:use "COMMON-LISP" "OPTICL"))

(in-package :robot)

(defclass rgb ()
  ((r :accessor r :initarg :r)
   (g :accessor g :initarg :g)
   (b :accessor b :initarg :b)))

(defclass cor-posicao()
  ((cor      :accessor cor   :initarg :cor)
   (limite-x :accessor lim-x :initarg :x)
   (limite-y :accessor lim-y :initarg :y)))

(defvar azul     (make-instance 'rgb :r 0   :g 0   :b 128))
(defvar amarelo  (make-instance 'rgb :r 255 :g 255 :b 0))
(defvar verde    (make-instance 'rgb :r 0   :g 100 :b 0))
(defvar vermelho (make-instance 'rgb :r 139 :g 0   :b 0))

(defvar *tam-y*  4)
(defvar *tam-x*  4)
(defvar *square* 100)
(defvar *map1*
  (list (list vermelho verde azul amarelo)
	(list amarelo amarelo verde azul)
	(list verde vermelho amarelo verde)
	(list azul verde vermelho azul)))

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

(defun draw-map (map1)
  (let ((map1-completo (map-complete map1)))
    (let* ((height (* *tam-y* *square*))
	   (width  (* *tam-x* *square*))
	   (x-init (random width))
	   (y-init (random height)))
      (let ((result (make-8-bit-rgb-image height width)))
	(loop
	   for i below height
	   collect
	     (loop
		for j below width 
		do (setf (pixel* result i j) (rgb->list (matrix->pixel map1-completo i j))) ))
	(fill-circle* result y-init x-init 30 '(0 0 0))
	(write-jpeg-file "maps/map.jpeg" result)
	))
    ))

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

(defun flatten (ls)
  (labels ((mklist (x) (if (listp x) x (list x))))
        (mapcan #'(lambda (x) (if (atom x) (mklist x) (flatten x))) ls)))

(defun my-filter  (f args)
  (cond ((null args) nil)
	((if (funcall f (car args))
	     (cons (car args) (my-filter  f (cdr args)))
	     (my-filter  f (cdr args)))))) 
