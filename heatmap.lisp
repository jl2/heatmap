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
    (values lat lon)))

(defun bounding-box (points)
  (loop for pt in points
     minimizing (gpx-pt-lat pt) into min-lat
     maximizing (gpx-pt-lat pt) into max-lat
     minimizing (gpx-pt-lon pt) into min-lon
     maximizing (gpx-pt-lon pt) into max-lon
     minimizing (gpx-pt-ele pt) into min-ele
     maximizing (gpx-pt-ele pt) into max-ele
     finally (return (values min-lat max-lat min-lon max-lon min-ele max-ele))))

(defun create-heatmap (file-name dir &optional (width 16000) (height 12000))
  (let* ((files (directory (format nil "~a/*.gpx" dir)))
         (all-points (apply (curry #'concatenate 'list) (mapcar (compose #'collect-points #'read-gpx) files))))
    (multiple-value-bind (min-lat max-lat min-lon max-lon) (bounding-box all-points)
      (let ((img (png:make-image height width 3 8)))
        (dolist (pt all-points)
          (with-slots (lat lon) pt
            (multiple-value-bind (x-pixel y-pixel) (map-pt lat lon width height min-lat max-lat min-lon max-lon)
              (increment-pixel img (floor x-pixel) (- (1- height) (floor y-pixel)) 1))))
        (with-open-file (output file-name :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)
          (png:encode img output))))))
