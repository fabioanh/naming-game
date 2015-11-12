(defpackage :naming-game
  (:use :common-lisp :data-loader :user-params))

(in-package :naming-game)

(defparameter *input* (read-input-objects-file *input-file-location*))
