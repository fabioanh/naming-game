;; Class related to the external data loader. Used to load contents of files.

(defpackage :data-loader 
  (:use :common-lisp) 
  (:export :read-file-lines))

(in-package :data-loader)

;; Function to load a file line by line and return its content in a list
(defun read-file-lines (filename)
  "Returns the content of a file line by line as elements of a list"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
         while line
         collect line)))

