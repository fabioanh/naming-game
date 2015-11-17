(defpackage :exec
  (:use :common-lisp :naming-game :user-params :data-loader))

(in-package :exec)

;; Compile and evaluate files. Order may be important
;; 1. user-params.lisp
;; 2. data-loader.lisp
;; 3. word-inventor.lisp
;; 4. agent.lisp
;; 5. naming-game.lisp
;; 6. exec.lisp

;; User parameters can be overridden here after compilation and evaluation of user-params.lisp file

;; File provided together with the code. Contains some of the most common digrams and trigrams used in English
(setf *n-grams-file-location* "~/Documents/vub/programming_paradigms/dev/lisp_code/assignment_2/data/english_n_grams.txt")
(setf *total-games* 600)
(setf *initial-population-size* 5)
(setf *num-new-agents* 0)
(setf *insert-frequency* 125)
(setf *lateral-inhibition-delta* 0.1)
(setf *word-creation-rate* 1.0)
(setf *word-absorption-rate* 1.0)
(setf *categories* '("DARK" "LIGHT" "RED" "SMALL" "LARGE" "WHITE" "SUNNY" "LOW" "GREEN" "HIGH" "LOUD" "ORANGE" "SMART" "BLACK"))
(setf *categories* '("DARK" "LIGHT" "RED" "SMALL" "LARGE"))

;; Execute this function and see the console output
(run-games)

;; Current parameters set up to see the insertion of 2 new agents every 800 iterations for 14 provided categories.
