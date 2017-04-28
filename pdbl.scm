;;;
;;; Interpreter for P", the language for simulation of a Turing machine with
;;; left-infinite tape developed by Corrado Böhm in 1964.
;;;
;; Copyright 2017 Matthew Lavin
;;
;; This program utilizes the lalr-scm parser generation library, which is free
;; software licensed under the GNU LGPL.

(load "lalr-scm/lalr.scm")

(define *tape* #())	; Tape operated on by the machine
(define *position* 0)	; Current position of tape head
(define *state* 's1)	; Current state of machine

(define (setup tape pos state)
  (set! *tape* tape)
  (set! *position* pos)
  (set! *state* state))

(define (set-state! state)
  (set! *state* state))
