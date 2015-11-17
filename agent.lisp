;; Definition of an agent for the gaming name
(defpackage :agent
  (:use :common-lisp :user-params :word-inventor)
  (:export :agent
           :agent-words
           :get-word-for-meaning 
           :create-word 
           :get-meaning-for-word 
           :learn-word
           :update-score-failure
           :update-score-success
           :lexicon
           :word-score
           :language-inventory
           :print-word-scores
           :name))

(in-package :agent)

(defparameter *max-score* 1.0)
(defparameter *min-score* 0.0)

(defclass agent ()
  ((language-inventory
    :initarg :language-inventory
    :accessor language-inventory
    :initform '())
   (name
    :initarg :name
    :accessor name))
  (:documentation "Agent class used to perform a naming game"))

(defclass word-score()
  ((word :initarg :word
         :accessor word)
   (score :initarg :score
          :accessor score
          :initform 0.0))
  (:documentation "Class containing a word representation and its score"))

(defclass lexicon()
  ((meaning :initarg :meaning
            :accessor meaning)
   (word-scores :initarg :word-scores
               :accessor word-scores
               :initform '()))
  (:documentation "Class containing a meaning and a list for its scores"))

;; Auxiliary structure to store meanings and scores in a same place for computations
(defstruct meaning-score
  (meaning)
  (score))

;; Method in charge of increasing and decreasing score according to conditions after a successful game
(defmethod update-score-success ((agent agent) (meaning String) (word String))
  "Increases and decreases the score of a word for a meaning in the language inventory of the agent after a successful game"
  (loop for entry in (language-inventory agent) do
       (when (String= (meaning entry) meaning)
         (loop for ws in (word-scores entry) do
              (if (String= word (word ws))
                  (if (> (+ (score ws) *lateral-inhibition-delta*) *max-score*)
                      (setf (score ws) *max-score*)
                      (setf (score ws) (+ (score ws) *lateral-inhibition-delta*)))
                  (if (< (- (score ws) *lateral-inhibition-delta*) *min-score*)
                      (setf (score ws) *min-score*)
                      (setf (score ws) (- (score ws) *lateral-inhibition-delta*))))))))

;; Method in charge of decreasing the score according to conditions after a failed game
(defmethod update-score-failure ((agent agent) (meaning String) (word String))
  "Decreases the score of a word for a meaning in the language inventory of the agent after a failed game"
  (loop for entry in (language-inventory agent) do
       (when (String= (meaning entry) meaning)
         (loop for ws in (word-scores entry)
            when(String= word (word ws)) do
              (if (< (- (score ws) *lateral-inhibition-delta*) *min-score*)
                  (setf (score ws) *min-score*)
                  (setf (score ws) (- (score ws) *lateral-inhibition-delta*)))))))

;; Creates a word for the given agent and meaning
(defmethod create-word ((agent agent) (meaning String))
  "Creates a word for the given agent and meaning. Considers the creation rate threshold"
  (when (< (random 100 (make-random-state t)) (* *word-creation-rate* 100))
    (progn
      (loop for entry in (language-inventory agent) do
           (when (String= (meaning entry) meaning)
             (progn
               (setf (word-scores entry) (append (word-scores entry) (list (make-instance 'word-score :word (invent-word)))))
               (return-from create-word))))
      (setf (language-inventory agent) (append (language-inventory agent) (list (make-instance 'lexicon :meaning meaning :word-scores (list (make-instance 'word-score :word (invent-word))))))))))

;; The given agent tries to learn the given word
(defmethod learn-word ((agent agent) (meaning String) (word String))
  "The given agent learns a word for the given meaning/category
considering the probability/threshold to learn a word"
  (when (< (random 100 (make-random-state t)) (* *word-absorption-rate* 100))
    (progn
      (loop for entry in (language-inventory agent) do
           (when (String= (meaning entry) meaning)
             (progn
               (setf (word-scores entry) (append (word-scores entry) (list (make-instance 'word-score :word word))))
               (return-from learn-word))))
      (setf (language-inventory agent) (append (language-inventory agent) (list (make-instance 'lexicon :meaning meaning :word-scores (list (make-instance 'word-score :word word)))))))))

;; Returns all the words associated to the agent. Doesn't consider duplicated meanings for words
(defmethod agent-words ((agent agent))
  "Returns the list of words for the given agent with score bigger than 0.0"
  (let ((words '()))
    (loop for entry in (language-inventory agent) do
         (loop for ws in (word-scores entry) do
              (when (> (score ws) 0.0)
                (setf words (append words (list (word ws)))))))
    words))

(defun get-word (word-scores)
  "Gets the word with the highest score for the given word-scores"
  (let ((word-score (nth (random (length word-scores) (make-random-state t)) word-scores)))
    (loop for ws in word-scores do
         (when (> (score ws) (score word-score))
           (setf word-score ws)))
    (word word-score)))

(defmethod get-word-for-meaning ((agent agent) (meaning String))
  "Gets the word with the highest score for the given meaning and agent"
  (loop for entry in (language-inventory agent) do
       (when (String= (meaning entry) meaning)
         (return-from get-word-for-meaning (get-word (word-scores entry))))))

(defmethod get-meaning-scores ((lexicon lexicon) (word String))
  "Returns a list of meaning-score structs for the given word"
  (loop for ws in (word-scores lexicon)
     when(String= (word ws) word)
     collect (make-meaning-score :meaning (meaning lexicon) :score (score ws))))

(defmethod get-meaning-for-word ((agent agent) (word String))
  "Returns the associated meaning with the highest score for the given word"
  (let ((meaning-scores '()))
    (loop for entry in (language-inventory agent) do
         (setf meaning-scores (append meaning-scores (get-meaning-scores entry word))))
    (when (not (eq meaning-scores nil))
      (let* ((meaning-score (nth (random (length meaning-scores) (make-random-state t)) meaning-scores)))
        (loop for ms in meaning-scores do
             (when (> (meaning-score-score ms) (meaning-score-score meaning-score))
               (setf meaning-score ms)))
        (meaning-score-meaning meaning-score)))))

(defmethod print-word-scores ((agent agent))
  "Prints the scores of a word for a given agent"
  (loop for entry in (language-inventory agent) do
       (when(not (eq entry nil))
         (loop for ws in (word-scores entry) do
              (when (> (score ws) 0.0)
                (format t "Meaning: ~s, Word: ~s, Score: ~d ~C" (meaning entry) (word ws) (score ws) #\linefeed))))))
