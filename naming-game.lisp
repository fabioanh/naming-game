(defpackage :naming-game
  (:use :common-lisp :user-params :agent)
  (:export :run-games))

(in-package :naming-game)

(defparameter *agents* '())
(defparameter *agent-name-prefix* "Agent")
(defparameter *agent-counter* *initial-population-size*)
(defparameter *moving-avg-span* 10)
(defparameter *moving-avg-increase-rate* 0.04)
(defparameter *success-log* (make-array (+ *total-games* 1)))
(defparameter *current-game* 1)
(defparameter *output-stream* nil)

;; Function to randomly select a meaning/category as topic
(defun select-topic()
  (nth (random (length *categories*) (make-random-state t)) *categories*))

(defun create-agents ()
  "Creates the required agents for the game"
  (let ((agents '()))
    (loop for i from 0 below *initial-population-size* do
         (setf agents (append agents (list (make-instance 'agent :name (concatenate 'string *agent-name-prefix* (write-to-string i)))))))
    agents))

(defun add-virgin-agents (agents num-agents)
  (loop for i from 0 below num-agents do
       (setf agents (append agents (list (make-instance 'agent :name (concatenate 'string *agent-name-prefix* (write-to-string *agent-counter*))))))
       (setf *agent-counter* (+ *agent-counter* 1)))
  agents)

(defun add-new-agent (agents)
  "Adds a new agent to the current agents list depending on the birth rate"
  (when (< (random 100 (make-random-state t)) (* *birth-rate* 100))
    (progn
      (setf agents (append agents (list (make-instance 'agent :name (concatenate 'string *agent-name-prefix* (write-to-string *agent-counter*))))))
      (setf *agent-counter* (+ *agent-counter* 1))))
  agents)

(defun kill-agent (agentss)
  "Removes a random agent from the current list of agents depending on the death rate"
  (let ((agents '()))
    (when (< (random 100 (make-random-state t)) (* *death-rate* 100))
      (let ((pos (random (length agentss))) (num-agents (length agentss)))
        (loop for i from 0 below num-agents do
             (when (not (eq i pos))
               (setf agents (append agents (list (nth i agentss)))))))
      (setf agentss agents))
    agentss))

(defun get-random-agent (agents &optional agent)
  "Gets a random agent from the list of agents. If the parameter agent is provided
makes sure that the random agent is different to the input one."
  (let ((ag (nth (random (length agents) (make-random-state t)) agents)))
    (loop while (eq ag agent) do
      (setf ag (nth (random (length agents) (make-random-state t)) agents)))
    ag))

(defun total-number-of-words (agents)
  "count the number of words with score bigger than 0.0."
  (let ((words '()))
    (loop for a in agents do
         (setf words (append words (agent-words a))))
    (length (remove-duplicates words :test #'equal))))

(defun get-avg-success ()
  "Returns a running average value"
  (let ((span (floor (+ *moving-avg-span* (* *moving-avg-increase-rate* *current-game*)))) (avg 0))
    (when (< (- *current-game* 1) span)
      (setq span *current-game*))
    (loop for i from 0 below span do
         (setq avg (+ avg (aref *success-log* (- (- *current-game* 1) i)))))
    (* (/ avg span) 100.0)))

;;Prints a running average
(defun print-avg-success()
  (let ((avg (get-avg-success)))
    (format t "Success rate: ~d % ~C" avg #\linefeed)))

(defun print-all-word-scores (agents)
  "Prints all the word-score pairs for the given agents"
  (loop for a in agents do
       (progn
         (format t "Agent: ~s~C" (name a) #\linefeed)
         (print-word-scores a))))

(defun run-single-game(agents)
  (let* ((topic (select-topic)) 
         (speaker (get-random-agent agents)) 
         (hearer (get-random-agent agents speaker))
         (speaker-word (get-word-for-meaning speaker topic))
         (success nil))
    (setf (aref *success-log* (- *current-game* 1)) 0)
    (when (eq speaker-word nil)
      ;; Try to create word in speaker before talking to the hearer
      (progn 
        (create-word speaker topic)
        (setf speaker-word (get-word-for-meaning speaker topic))))
    (when (not (eq speaker-word nil))
      (let ((hearer-meaning (get-meaning-for-word hearer speaker-word)))
        (if (eq hearer-meaning nil)
            (progn
              (learn-word hearer topic speaker-word))
            (if (eq hearer-meaning topic)
                (progn
                  (update-score-success speaker topic speaker-word)
                  (update-score-success hearer topic speaker-word)
                  (setf success t)
                  (setf (aref *success-log* (- *current-game* 1)) 1))
                (progn
                  (update-score-failure speaker topic speaker-word)
                  (update-score-failure hearer topic speaker-word))))))))

(defun run-games()
  (setf *current-game* 1)
  (let ((agents (create-agents)))
    ;;    (loop for i from 0 below *total-games* do
    (with-open-file (*output-stream* "/tmp/naming-game-output.txt"
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
      (loop for i from 0 below *total-games* do
           (progn
             (run-single-game agents)
             (when (and (not (eq *insert-frequency* 0)) (eq (mod *current-game* *insert-frequency*) 0))
               (setf agents (add-virgin-agents agents *num-new-agents*)))
             (setf agents (kill-agent agents))
             (setf agents (add-new-agent agents))
             ;(format t "No of agents: ~d ~C" (length agents) #\linefeed)
             ;(format t "No of words: ~d ~C" (total-number-of-words agents) #\linefeed)
             (format *output-stream* "~d;~d;~d;~d~C" *current-game* (length agents) (total-number-of-words agents) (get-avg-success) #\linefeed)
             (setf *current-game* (+ *current-game* 1))
             ;(print-avg-success)
             ))
      (format t "~s ~C" (print-all-word-scores agents) #\linefeed))))
