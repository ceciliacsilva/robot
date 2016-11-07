;;Artificial Intelligence for Robotics
;;Robotics Group - LINA - UFU

;;File: ex1.lisp

(load "/home/cecilia/robot/robot.lisp")
(in-package :robot)

(defvar *map1*
  (list (list vermelho verde azul amarelo)
	(list amarelo amarelo verde azul)
	(list verde vermelho amarelo verde)
	(list azul verde vermelho azul)))

(defvar run1 (localization *map1* "mundo4cores" 0.9 0.9))
