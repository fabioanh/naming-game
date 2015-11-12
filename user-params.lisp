(defpackage :user-params 
  (:documentation "Parameters deffined by the user in order to run the naming game")
  (:use :common-lisp)
  (:export :*input-file-location* 
           :*n-grams-file-location*))

(in-package :user-params)

(defparameter *input-file-location* "~/Documents/vub/programming_paradigms/dev/lisp_code/assignment_2/data/object-features.txt")
(defparameter *n-grams-file-location* "~/Documents/vub/programming_paradigms/dev/lisp_code/assignment_2/data/english_n_grams.txt")
(defparameter *population-size* 20)
(defparameter *number-of-meanings* 20)
(defparameter *lateral-inhibition-delta* 0.05)
