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

(defparameter azul     (make-instance 'rgb :r 0   :g 0   :b 128))
(defparameter amarelo  (make-instance 'rgb :r 255 :g 255 :b 0))
(defparameter verde    (make-instance 'rgb :r 0   :g 100 :b 0))
(defparameter vermelho (make-instance 'rgb :r 139 :g 0   :b 0))

(defvar *tam-y*  4)
(defvar *tam-x*  4)
(defvar *square* 100)
(defvar *map1*
  (make-array (list *tam-y* *tam-x*)
	      :initial-contents
	      '((vermelho verde azul amarelo)
		(amarelo amarelo verde azul)
		(verde vermelho amarelo azul)
		(azul verde vermelho azul))))

(defun map-complete (map1)
  (loop
     for i from 0 to (- *tam-y* 1)
     summing *square* into lim-y
     collect
       (loop
	  for j from 0 to (- *tam-x* 1)
	  summing *square* into lim-x
	  collect (make-instance 'cor-posicao :cor (aref map1 i j) :x lim-x :y lim-y) )
    ))

