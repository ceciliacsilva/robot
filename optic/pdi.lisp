

;(load "/home/arthurprs/quicklisp/setup.lisp")
;(ql:quickload 'opticl)
(in-package :opticl)

; (defparameter *truck* (read-jpeg-file "linha.jpeg"))
(defparameter *truck* (read-jpeg-file "pentagon.jpeg"))

;(defparameter *truck* (read-jpeg-file "img1.jpeg"))

(defparameter *edge-kernel* #2A((0 1 0)
                                (1 -4 1)
                                (0 1 0)))

(defparameter *gaussian-kernel*
  (normalize-array #2A((1 2 1)
                       (2 4 2)
                       (1 2 1))))

; pixel colorido?
(defun is-color-pixel (r g b)
  (or (> r 50) (> g 50) (> b 50)) )

; transformada de hough
(defun hough-transform (image)
  (with-image-bounds (height width) image
   (let* ((maxradius (1+ (truncate (/ (sqrt (+ (* width width) (* height height))) 2))))
          (hough (make-32-bit-gray-image maxradius 360)) )
    (do-pixels (i j) image
     (multiple-value-bind (r g b) (pixel image i j)
      (when (is-color-pixel r g b)
       (let* ((x (- j (/ width 2.0)))
              (y (- i (/ height 2.0))) )
        (loop for degree from 0 to 359 do
         (let* ((theta (* (/ degree 180) PI))
                (radius (+ (* x (cos theta)) (* y (sin theta)))) )
           (if (>= radius 0.0)
            (let* ((offradius (truncate radius))
                   (prevpixel (pixel hough offradius degree)) )
              (setf (pixel hough offradius degree) (1+ prevpixel))
            )
            (let* ((degree2 (mod (+ degree 180) 360))
                   (offradius (truncate (abs radius)))
                   (prevpixel (pixel hough offradius degree2)) )
              (setf (pixel hough offradius degree2) (1+ prevpixel))
            )
           ) ; if
         ) ; let*
        ) ; loop
       ) ; let*
      ) ; when
     ) ; multiple-value-bind
    ) ; do-pixels
    hough
   ) ; let*
  ) ; with-image-bounds
) ; defun

; intensidade máxima
(defun max-pixel-value (image)
  (let ((maxvalue 0))
   (do-pixels (i j) image
    (setf maxvalue (max maxvalue (pixel image i j)))
   )
   maxvalue
  )
)

; normaliza imagem de forma linear [0..max] -> [0..255]
(defun normalize-linear-image (image)
 (with-image-bounds (height width) image
  (let ((result (make-8-bit-gray-image height width))
        (maxpixel (max-pixel-value image)) )
   (when (> maxpixel 0)
    (do-pixels (i j) image
     (let ((origpixel (pixel image i j)))
      (setf (pixel result i j) (truncate (* 255.0 (/ origpixel maxpixel))))
     ) ; let
    ) ; do-pixels
   ) ; when
   result
  ) ; let
 ) ; width-image-bounds
) ; defun normalize-image


; normaliza imagem de forma exponencial
(defun normalize-gamma-image (image &optional (gamma 1.5))
 (with-image-bounds (height width) image
  (let ((result (make-8-bit-gray-image height width))
        (maxpixel (expt (max-pixel-value image) gamma)) )
   (when (> maxpixel 0)
    (do-pixels (i j) image
     (let ((origpixel (expt (pixel image i j) gamma)))
      (setf (pixel result i j) (truncate (* 255.0 (/ origpixel maxpixel))))
     ) ; let
    ) ; do-pixels
   ) ; when
   result
  ) ; let
 ) ; width-image-bounds
) ; defun normalize-image

; Retorna os maiores picos da transformada
(defun find-peaks (image numpeaks)
 (with-image-bounds (height width) image
  (let ( (peaks '()) )
    (do-pixels (i j) image
     (let ((pixelvalue (pixel image i j))
           (radius i)
           (theta  (* (/ j 180) PI)) )
      (push (list pixelvalue radius theta) peaks)
     ) ; let
    ) ; do-pixels
   (subseq
    (sort peaks #'(lambda (l1 l2) (> (car l1) (car l2))))
    0 numpeaks
   ) ; subseq
  ) ; let
 ) ; width-image-bounds
) ; defun find-peaks

; retorna lista de pixels em volta
(defun neighbors (image i j window)
  (with-image-bounds (height width) image
   (remove-if #'null
    (loop for dy from (- window) to window append
     (loop for dx from (- window) to window collect
      (let  ((y (+ i dy))
             (x (+ j dx)) )
       (when (pixel-in-bounds image y x) (pixel image y x))
      ); let*
     ); loop
    ); loop
   ); remove-if
  ); with-image-bounds
); defun neighbors


; Retorna media de pixels em volta de i j 
(defun mean-cluster (image i j window)
 (let ((neighborpixels (neighbors image i j window)))
  ;(print neighborpixels)
  (truncate (/ (apply #'+ neighborpixels) (length neighborpixels)))
 )
)

; Retorna lista de (intensidade raio theta)
(defun find-peaks2 (image threshold window &optional (erasewindow window))
 (let ((image (copy-image image)))
  (with-image-bounds (height width) image
  (let ( (peaks '()) )
    (do-pixels (i j) image
     (let ((pixelvalue (pixel image i j))
           (radius i)
           (theta  (* (/ j 180) PI))
           (meanpixelvalue (mean-cluster image i j window)) )
       (when (>= meanpixelvalue threshold)
        ;(print meanpixelvalue)
        (push (list meanpixelvalue radius theta) peaks)
        (fill-rectangle image (- i erasewindow) (- j erasewindow) (+ i erasewindow) (+ j erasewindow) 0)
       ) ; when
     ) ; let
    ) ; do-pixels
   peaks
  ) ; let
  ) ; width-image-bounds
 ) ; let
) ; defun find-peaks


; truncate especial, para não ter problemas com retas verticais
(defun fit (v)
 (cond
  ((< v -100000) -100000)
  ((> v 100000) 100000)
  (t (truncate v))
 )
)

; retona uma imagem com as retas desenhadas
(defun hough-inverse (peaks origimage)
 (with-image-bounds (height width) origimage
  (let* ((image (make-8-bit-gray-image height width))
         (cx (/ width 2.0))
         (cy (/ height 2.0)) )
   (loop for peak in peaks do
    (let* ((radius (second peak))
           (theta (if (= 0 (sin (third peak))) 0.00000001 (third peak)))
           (y0 (+ (/ (- radius (* (- cx) (cos theta))) (sin theta)) cy))
           (y1 (+ (/ (- radius (* cx (cos theta))) (sin theta)) cy))  )
     (draw-line image (fit y0) 0 (fit y1) width 255)
    ) ; let*
   ) ; loop
   image
  ) ; let*
 ) ; with-image-bounds
) ; defun hough-inverse
   



;(defparameter bluredimage (discrete-convolve *truck* *gaussian-kernel*))
;(defparameter houghblur (hough-transform bluredimage))
;(write-jpeg-file "houghblur.jpeg" (normalize-image houghblur))


(defparameter hough (hough-transform *truck*))
(write-jpeg-file "hough.jpeg" (normalize-linear-image hough))

(defparameter toppeaks1 (find-peaks hough 20))
(write-jpeg-file "peaks1.jpeg" (hough-inverse toppeaks1 *truck*))

(defparameter toppeaks2 (find-peaks2 (normalize-gamma-image hough 1.5) 35 3 10))
(write-jpeg-file "peaks2.jpeg" (hough-inverse toppeaks2 *truck*))


(in-package :cl-user)
