;;Artificial Intelligence for Robotics
;;Robotics Group - LINA - UFU

;;File: ex2.lisp

(load "/home/cecilia/robot/robot.lisp")
(in-package :robot)

(defvar *map2*
  (list (list verde verde verde)
	(list verde vermelho verde)
	(list verde verde verde)))

(defvar run2 (localization *map2* "mundo2cores" 0.9 0.8))
