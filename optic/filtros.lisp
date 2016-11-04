;;Lê imagem em jpg
;;Ponha a imagem no mesmo diretório do programa.

(in-package :opticl)

(defparameter *edge-kernel* #2A((0 1 0)
                                (1 -4 1)
                                (0 1 0)))

(defparameter *faler-3* #2A((-1 -1 -1)
                            (-1  8 -1)
                            (-1 -1 -1)))

(defparameter sobel1 #2A((1 2 1)
                            (0 0 0)
                            (-1 -2 -1)))

(defparameter prewitt-x #2A((-1 0 1)
                            (-1 0 1)
                            (-1 0 1)))

(defparameter prewitt-y #2A(( 1  1  1)
                            ( 0  0  0)
                            (-1 -1 -1)))



(defun filtra (entrada filtro)
   (let* ( (img (read-jpeg-file entrada))
	   (nome (pathname-name entrada))
           (simg  img);(sharpen-image img))
           (edge (discrete-convolve simg (symbol-value filtro)))
	   (saida (make-pathname :directory (pathname-directory entrada) 
                     :name (format nil "~a-~a" nome (symbol-name filtro)) 
                     :type "jpeg")))  
       (write-jpeg-file saida edge)))

(defmacro filtrar(xxentrada xxfiltro)
   `(filtra ,xxentrada (quote ,xxfiltro)))

(defun prewitt (entrada)
  (let* ( (img (read-jpeg-file entrada))
	  (nome (pathname-name entrada))
	  (simg (sharpen-image img))
	  (fx (discrete-convolve simg prewitt-x))
	  (fy (discrete-convolve simg prewitt-y))
	  (saida (make-pathname :directory (pathname-directory entrada) 
				:name (format nil "~a-prewitt" nome) 
				:type "jpeg")))
    (with-image-bounds 
     (height width) img
     (loop for i below height do 
	   (loop for j below width do
		 (multiple-value-bind 
		  (rx gx bx) (pixel fx i j)
		  (multiple-value-bind 
		   (ry gy by) (pixel fy i j)
		   (setf (pixel img i j) 
			 (values (min 255 (+ (abs rx) (abs ry)))
				 (min 255 (+ (abs gx) (abs gy)))
				 (min 255 (+ (abs bx) (abs by)))))
		   )
		  )
		 )
	   ))
    (write-image-file saida img)))

