;; Class related to the external data loader. Used to load contents of files.

(defpackage :data-loader 
  (:use :common-lisp) 
  (:export :read-input-objects-file :read-file-lines))

(in-package :data-loader)

;; Definition of structure to manage a file input object
(defclass input-object()
  ((x :initarg :x)
   (y :initarg :y)
   (z :initarg :z)
   (width :initarg :width)
   (height :initarg :height)
   (avg-y :initarg :avg-y)
   (stdv-y :initarg :stdv-y)
   (min-y :initarg :min-y)
   (Maxk-y :initarg :max-y)
   (avg-u :initarg :avg-u)
   (stdv-u :initarg :stdv-u)
   (min-u :initarg :min-u)
   (max-u :initarg :max-u)
   (avg-v :initarg :avg-v)
   (stdv-v :initarg :stdv-v)
   (min-v :initarg :min-v)
   (max-v :initarg :max-v)))

;; Function used to replace values on string
(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
                         :start2 old-pos
                         :test test)
       do (write-string string out
                        :start old-pos
                        :end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))

;; Function used to convert a string line into a list of numbers
(defun line-to-numbers (input-line)
  "Given a line read from a file as a string, gives back a list with
the real numbers corresponding to the string values"
  (with-input-from-string (in (replace-all input-line "," ""))
    (loop for x = (read in nil nil) while x collect x)))

;; Gets a list with the contents of the object and returns an input-object based on it.
(defun input-object-from-list(list-args)
  (let ((input-obj (make-instance 'input-object  :x (nth 0 list-args) :y (nth 1 list-args) :z (nth 2 list-args) :width (nth 3 list-args) :height (nth 4 list-args) :avg-y (nth 5 list-args) :avg-u (nth 6 list-args) :avg-v (nth 7 list-args) :min-y (nth 8 list-args) :min-u (nth 9 list-args) :min-v (nth 10 list-args) :max-y (nth 11 list-args) :max-u (nth 12 list-args) :max-v (nth 13 list-args) :stdv-y (nth 14 list-args) :stdv-u (nth 15 list-args) :stdv-v (nth 16 list-args))))
    input-obj))

;; Definition of function to load situation data
(defun read-input-objects-file (filename)
  "Function that reads a file line by line returning the 
list of lines as strings."
  (let ((result '()))
    (with-open-file (stream filename)
      (loop for line = (read-line stream nil)
         while line
         do(setq result (append result (list (input-object-from-list (line-to-numbers line)))))))
    result))

;; Function to load a file line by line and return its content in a list
(defun read-file-lines (filename)
  "Returns the content of a file line by line as elements of a list"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
         while line
         collect line)))

