;;;; heatmap.asd
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(asdf:defsystem #:heatmap
  :description "Describe heatmap here"
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license "ISC (BSD-like)"
  :depends-on (#:utm
               #:gpxtools
               #:png
               #:alexandria)
  :serial t
  :components ((:file "package")
               (:file "heatmap")))

