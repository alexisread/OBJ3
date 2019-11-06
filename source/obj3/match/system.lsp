;; OBJ3 version 2
;; Copyright (c) 2000, Joseph Kiniry, Joseph Goguen, Sula Ma
;; Copyright (c) 1988,1991,1993 SRI International
;; All Rights Reserved
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;   o Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.
;; 
;;   o Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in the
;;   documentation and/or other materials provided with the distribution.
;; 
;;   o Neither name of the Joseph Kiniry, Joseph Goguen, Sula Ma, or SRI
;;   International, nor the names of its contributors may be used to
;;   endorse or promote products derived from this software without
;;   specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL SRI
;; INTERNATIONAL OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

;; $Id: system.lsp,v 205.2.1.1 2003/09/23 14:09:52 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                 The systems for matching
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; Claude Kirchner ;;;; Created: April 86.
;; [ck: Mar 4 87]: extract modified to handle overloaded attribute with 
;; different attributes.

;;;;;;;;;;;;;;;; Summary

; op system$create: Term Term -> System
; op system$new: -> System .
; op system$make_system: List[Equation] List[Equation] -> System 
; op system$add: System System -> System
; op system$add_eq: System Equation -> System~
; op system$list_eq: System -> List[Equation]
; op system$size: System -> Integer .
; op system$add_eq: System Equation -> System~
; op system$is_empty: System -> Bool
; op system$extract_one_system: System -> System TheoryName
 
;;;;;;;;;;;;;;;; Representation

;;; A system is a set of equations represented by a list
;;; treated as a set.

; rep = list[equation] or (nil.list[equation]), in order to be able to 
; modify it

;;;;;;;;;;;;;;;;

; op system$create: Term Term -> System
(defun system$create (t1 t2)
  (list (match_equation$create t1 t2))
  )

; op system$new: -> System .
(defun system$new ()
  (cons nil nil) ; In order to be able to modify it
  )

; returns a system from two list of equations. One of 
; the two lists should be non empty

; op system$make_system: List[Equation] List[Equation] -> System 
(defun system$make_system (l1 l2)
  (if (null (car l1))
      (union (cdr l1) l2)
      (if (null (car l2))
	  (union l1 (cdr l2))
	  (union l1 l2)))
  )

; adjoint to sys1 each equation of sys2 which is not already
; in sys1. 
;-ck- Mar  2 87: no--> Z: "sys1" is modified, "sys2" must not be modified

; op system$add: System System -> System
(defun system$add (sys1 sys2)
  ;(union sys1 sys2)
  (unless snark1
    (dolist (x sys1) (when (member x sys2) (princ "SNARK 1") (terpri)
			   (setq snark1 t) (return))))
  (append sys1 sys2)
  )
(defvar snark1 nil)

; returns the  list of equations in sys

; op system$list_eq: System -> List[Equation]

(defun system$list_eq (sys)
  (if (null (car sys))
      (cdr sys)
      sys)
  )

; returns the number of equation in "sys"

; op system$size: System -> Integer .
(defun system$size (sys)
  (if (null (car sys))
      (length (cdr sys))
      (length sys))
  )

; Add an equation to the system which is modified

; op system$add_eq: System Equation -> System~
(defun system$add_eq (sys eq)
  (unless (member eq sys)
    (if (null (car sys))
	(rplaca sys eq)	; remove the nil on top
	(rplacd sys (cons eq (cdr sys))))
    )
  )

; op system$is_empty: System -> Bool
(defun system$is_empty (sys)
  (null (car sys))
)

;;; returns the biggest system extracted 
;;; from "sys", which is homogenous with respect to
;;; the current theory (or list of properties). For example
;;; if * and + are AC then it may return a system which equations
;;; have all + for top symbol of they left hand side.

;;; The greatest priority is given to the empty theory

;;; --- Note that if + and * are only commutative then the notion of
;;; --- homogenous system can be extended to include system like
;;; --- {x+z == a+b, x*a == b*a}. So the notion of homogenous system
;;; --- depends on the theory. This is not implemented @@.

;;; Z: "sys" is supposed to be non empty and to contain equation of the 
;;; form t1==t2 which are all such that t1 is NOT a variable

; op system$extract_one_system: System -> System TheoryName
;&&&& handling of built-in constants in the following is unclear
(defun system$extract_one_system (sys)
  (let ((extract_sys nil)
	(theory_is_empty nil)
	(disc_symbol nil))
    (dolist (eq (system$list_eq sys))
	;; [ck: Mar 4 87] modified to keep into account overloaded operators
        ;; with different attributes.
	;; was:(let ((t1_top (term$head (match_equation$t1 eq)))) ...)
	;; 31 Jul 87 TCW changed back since not doing this yet
	(let ((t1_top (term$head (match_equation$t1 eq))))
	(cond ((theory_name$is_empty_for_matching
		(operator$theory_name_for_matching t1_top)) ;@@@
	       (when disc_symbol
		     (unless (theory_name$is_empty_for_matching
			      (operator$theory_name_for_matching disc_symbol))
		                    ;@@@
			     ;; the extracted system is reset
			     (setq disc_symbol t1_top)
			     (setq extract_sys nil))
		     )
	       (setq theory_is_empty t)
	       (push eq extract_sys))
	      (t (if disc_symbol 
		     (when (operator$is_same_overloaded_operator
			    ; &&&& necessary to restrict
			    disc_symbol t1_top)
			   (push eq extract_sys))
		     (unless theory_is_empty
			     (progn
			       (setq disc_symbol t1_top)
			       (push eq extract_sys))
			     )
		     ) ;; if
		 )
	      ) ;; cond
	)
      )
    (values extract_sys
	    (if disc_symbol (operator$theory_name_for_matching disc_symbol)
	      the_empty_property)) ;&&&& avoid nil as default
    )
  )
