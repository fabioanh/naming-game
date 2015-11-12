(defpackage :word-inventor
  (:use :common-lisp :data-loader :user-params) 
  (:export :invent-word)
  (:documentation "Package in charge of the creation of new words"))

(in-package :word-inventor)

;; word length meassured in number of n-grams
(defparameter *max-word-length* 3)

;; List of n-grams used to create new words
(defparameter *n-grams* (read-file-lines *n-grams-file-location*))

;; Creates a random word by concatenating random n-grams
(defun invent-word ()
  "Returns a new word concatenating random n-grams"
  (let ((new-word ""))
    (loop for i from 0 to (+ (random (- *max-word-length* 1) (make-random-state t)) 1) do
         (setf new-word (concatenate 'string new-word (nth (random (length *n-grams*) (make-random-state t)) *n-grams*))))
    new-word))

