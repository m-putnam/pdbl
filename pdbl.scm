;;; Interpreter for P", the language for simulation of a Turing machine with
;;; left-infinite tape developed by Corrado Böhm in 1964.
;;;
;; Copyright 2017 Matthew Lavin
;;
;; This program utilizes the lalr-scm parser generation library, which is free
;; software licensed under the GNU LGPL. Development and testing is done using
;; GNU Guile, so interoperability cannot be guaranteed.
;;

(load "lalr-scm/lalr.scm")

(define *tape* #())	; Tape operated on by the machine
(define *position* 0)	; Current position of tape head
(define *state* 's1)	; Current state of machine
(define *base* 0)	; Largest digit in notation

(define (setup tape pos state base)
  (set! *tape* tape)
  (set! *position* pos)
  (set! *state* state)
  (set! *base* base))

(define (set-state! state)
  (set! *state* state))

(define (set-pos! pos)
  (set! *position* pos))

(define (movl)
  (set! *position* (- *position* 1)))

(define (movr)
  (set! *position* (+ *position* 1)))

(define (write out)
  (vector-set! *tape* *position* out))

(define (read)
  (vector-ref *tape* *position*))

(define (neq a b)
  (not (= a b)))

; Increment square & then move head left if possible
(define (Lambda)
  (if (Alpha)
    (write 1)
    (if (eq? (read) *base*)
      (write 0)
      (write (+ (read) 1))))
  (if (neq *position* 0)
    (movl)))

(define (Alpha)
  (eq? (read) '_))

(define (while-tape func)
  (while (neq (read) 0)
	 (func)))

; Some example input to work with
(define tape1
  (list->vector '(0 1 1 1 1 0 _ _ _)))

(setup tape1 0 's1 1)
