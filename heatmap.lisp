;;;; heatmap.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:heatmap)

(defun increment-pixel (img x y cnt)
  "Increment each color component of the pixel in img at location x,y by 1"
  (let ((max-val (if (= (png:image-bit-depth img) 8) 255 65535)))
  (when (and (<= (+ (aref img y x 0) cnt) max-val)
             (<= (+ (aref img y x 1) cnt) max-val)
             (<= (+ (aref img y x 2) cnt) max-val))
    (incf (aref img y x 0) cnt)
    (incf (aref img y x 1) cnt)
    (incf (aref img y x 2) cnt))))

(defun map-pt (lat lon width height min-lat max-lat min-lon max-lon)
  (let ((lat (* (1- width) (/ (- lat min-lat) (- max-lat min-lat))))
        (lon (* (1- height) (/ (- lon min-lon) (- max-lon min-lon)))))
    (cons lat lon)))

(defun create-heatmap (file-name dir &optional (width 1600) (height 1200))
  (let* ((files (directory (format nil "~a/*.gpx" dir)))
         (all-points (apply (curry #'concatenate 'list) (mapcar (compose #'collect-points #'read-gpx) files)))
         (min-lat (gpx-pt-lat (car all-points)))
         (max-lat min-lat)
         (min-lon (gpx-pt-lon (car all-points)))
         (max-lon min-lon)
         (min-ele (gpx-pt-ele (car all-points)))
         (max-ele min-ele))
    (loop for pt in all-points
       do
         (with-slots (lat lon ele) pt
           (setf min-lat (min lat min-lat))
           (setf max-lat (max lat max-lat))

           (setf min-lon (min lon min-lon))
           (setf max-lon (max lon max-lon))

           (setf min-ele (min ele min-ele))
           (setf max-ele (max ele max-ele))))

    (let ((img (png:make-image height width 3 8)))
      (dolist (pt all-points)
        (with-slots (lat lon) pt
          (let* ((lat-lon (map-pt lat lon height width min-lat max-lat min-lon max-lon))
                 (lat (car lat-lon))
                 (lon (cdr lat-lon)))
            (increment-pixel img (floor lon) (floor lat) 4))))
      (with-open-file (output file-name :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)
        (png:encode img output)))))
