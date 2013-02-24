;; Copyright (C) 2013 Rosario Raulin

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defpackage :de-raulin-rosario-turing-machine
  (:use :cl)
  (:export :simulate-machine
	   :make-turing-machine
	   :*even-length-machine*
	   :*example-machine*
	   :+accept+
	   :+reject+))

(in-package :de-raulin-rosario-turing-machine)

(defconstant +accept+ 'accept)
(defconstant +reject+ 'reject)

(defparameter *default-tape-len* 8)

(defun make-turing-machine (alphabet initial-state transition-fn)
  "creates a turing machine suitable for simulate-machine"
  (list alphabet initial-state transition-fn))

(defparameter *even-length-machine*
  ;; decides if input word is of even length
  (make-turing-machine
   '(a b) 0 (lambda (state input)
	      (case state
		(0 (values (case input
			     (#\0 +accept+)
			     (otherwise 1))
			   input 'r))
		(1 (values (case input
			     (#\0 +reject+)
			     (otherwise 0))
			   input 'r))))))

(defparameter *example-machine*
  (make-turing-machine
   '(a b) 0 (lambda (state input)
	      (if (= state 100)
		  (values +accept+ input 's)
		  (values (1+ state) input
			  (if (< state 4)
			      'r
			      's))))))

(defun make-tape (&key old-tape fill-with (head 0))
  (let* ((size (max
		*default-tape-len*
		(if old-tape (* 4 (length old-tape)) 0)
		(if fill-with (* 4 (length fill-with)) 0)))
	 (tape (make-array size :element-type 'character :initial-element #\0)))
    (flet ((fill-tape (word)
	     (loop for x across word and i from (/ size 2)
		do (setf (aref tape i) x))))
      (when old-tape (fill-tape old-tape))
      (when fill-with (fill-tape fill-with))
      (values tape size (+ head (/ size 2))))))
       
(defun simulate-machine (machine word)
  "simulates the turing machine 'maschine' for the input word 'word'"
  (multiple-value-bind (tape tape-len head) (make-tape :fill-with word)
    (destructuring-bind (alpha state transition-fn) machine
      (declare (ignore alpha))
      (loop until (or (eq state +accept+) (eq state +reject+)) do
	   (multiple-value-bind (new-state write-to-tape move-head)
	       (funcall transition-fn state (aref tape head))
	     (setf (aref tape head) write-to-tape
		   state new-state)
	     (case move-head
	       (l (decf head))
	       (r (incf head)))
	     (when (or (>= head tape-len) (zerop head))
	       (multiple-value-bind (new-tape new-tape-len new-head)
		   (make-tape :old-tape tape :head head)
		 (setf tape new-tape
		       tape-len new-tape-len
		       head new-head))))
	 finally (return (values state tape head))))))
