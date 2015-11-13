;; Definition of an agent for the gaming name
(defpackage :agent
  (:use :common-lisp :user-params)
  (:export :agent))

(in-package :agent)

(defparameter *score-delta* *lateral-inhibition-delta*)
(defparameter *max-score* 1.0)
(defparameter *min-score* 0.0)

(defclass agent ()
  ((language-inventory
    :initarg :language-inventory
    :accessor language-inventory))
  (:documentation "Agent class used to perform a naming game"))

(defclass word-score()
  ((word :initarg :word
         :accessor word)
   (score :initarg :score
          :accessor score)))

(defclass lexicon()
  ((meaning :initarg :meaning
            :accessor meaning)
   (word-scores :initarg :word-scores
               :accessor word-scores)))

;; Method in charge of increasing and decreasing score according to conditions after a successful game
(defmethod update-score-success ((agent agent) (meaning String) (word String))
  "Increases and decreases the score of a word for a meaning in the language inventory of the agent after a successful game"
  (loop for entry in (language-inventory agent) do
       (when (String= (meaning entry) meaning)
         (loop for ws in (word-scores entry) do
              (if (String= word (word ws))
                  (if (> (+ (score ws) *score-delta*) *max-score*)
                      (setf (score ws) *max-score*)
                      (setf (score ws) (+ (score ws) *score-delta*)))
                  (if (< (- (score ws) *score-delta*) *min-score*)
                      (setf (score ws) *min-score*)
                      (setf (score ws) (- (score ws) *score-delta*))))))))

;; Method in charge of decreasing the score according to conditions after a failed game
(defmethod update-score-failure ((agent agent) (meaning String) (word String))
  "Decreases the score of a word for a meaning in the language inventory of the agent after a failed game"
  (loop for entry in (language-inventory agent) do
       (when (String= (meaning entry) meaning)
         (loop for ws in (word-scores entry)
            when(String= word (word ws)) do
              (if (< (- (score ws) *score-delta*) *min-score*)
                  (setf (score ws) *min-score*)
                  (setf (score ws) (- (score ws) *score-delta*)))))))

