(defpackage :user-params 
  (:documentation "Parameters deffined by the user in order to run the naming game")
  (:use :common-lisp)
  (:export :*birth-rate*
           :*categories*
           :*death-rate*
           :*initial-population-size*
           :*insert-frequency*
           :*lateral-inhibition-delta*
           :*n-grams-file-location*
           :*num-new-agents*         
           :*total-games*
           :*word-creation-rate*
           :*word-absorption-rate*
           ))

(in-package :user-params)

;; File used to create the new words
(defparameter *n-grams-file-location* "~/Documents/vub/programming_paradigms/dev/lisp_code/assignment_2/data/english_n_grams.txt")
(defparameter *total-games* 3000)
(defparameter *initial-population-size* 5)
(defparameter *birth-rate* 0.0)
(defparameter *death-rate* 0.0)
(defparameter *num-new-agents* 2)
(defparameter *insert-frequency* 800)
(defparameter *lateral-inhibition-delta* 0.1)
(defparameter *word-creation-rate* 0.9)
(defparameter *word-absorption-rate* 0.9)
(defparameter *categories* '("DARK" "LIGHT" "RED" "SMALL" "LARGE" "WHITE" "SUNNY" "LOW" "GREEN" "HIGH" "LOUD" "ORANGE" "SMART" "BLACK"))
