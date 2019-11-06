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

;; $Id: empty.lsp,v 205.2.1.1 2003/09/23 14:09:51 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;      All the procedures specific to the empty theory
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; Claude Kirchner ;;;; Created: April 86.

;;;;;;;;;;;;;;;; Summary

;;;;;;;;;;;;;;;; Note

; It would be certainly more efficient to built in the empty theory in
; the mutation process of a (non empty) theory.

;;;;;;;;;;;;;;;; Representation

; An empty state consists into a system and a flag 0 or 1. 
; 0 means that the state is a new one and that one as to decompose the system.
; 1 means that the decomposition has been already done and that there is 
; no more next state

; empty$state = record{flag: bool
;                      sys: system}

(defstruct empty$state
  (flag 0 :type bit)
  sys )

(defvar empty_empty_state
  (make-empty$state :flag 0 :sys nil))

;;;;;;;;;;;;;;;;

; Initialize an empty state. It check if the top symbols of
; each equation of the system 
; have the same head function.

; op empty$state_initialize : System Environemnt -> EmptyState NoMatch
(defun empty$state_initialize (sys env)
  (declare (ignore env)) ;@@
  (block no_match
    (dolist (equation (system$list_eq sys))
      (unless
       (term$have_same_top
	(match_equation$t1 equation) (match_equation$t2 equation))
       (return-from no_match (values nil t)))
      )
    (values 
      (make-empty$state :flag 0 :sys sys)
      nil)
    )
  )

; op empty$next_state : EmptyState -> System EmptyState NoMore
(defun empty$next_state (empty$st)
  (let* ((flag (empty$state-flag empty$st))
	 (sys (empty$state-sys empty$st))
	 )
    (if (= flag 1)
	; no more state
	(values nil nil t)
	(multiple-value-bind (new_m_sys no_match)
	    (match_system$dec_merg
	      (match_system$create 
		(environment$new)
		sys))
	  (if no_match
	      (values nil nil t)
	      (progn
		(setf (empty$state-flag empty$st) 1)
		(values (match_system$match_system2system new_m_sys) 
			empty$st
			nil)
		)
	      ) ;; if
	) ;; mult
	) ;; if
    ) ;; let
  )

; t1 and t2 are no-variable terms
; t1 and t2 are not variables and not built-in constants
(defun empty$equal (t1 t2)
  (let ((hd1 (term$head t1)) (hd2 (term$head t2)))
  (and
    (operator$is_same_operator_fast hd1 hd2) ; for macro want vars hd1,hd2
    (do* ((sub1 (term$subterms t1) (cdr sub1))
	  (sub2 (term$subterms t2) (cdr sub2))
	  (st1 nil) (st2 nil))
	((null sub1) t)
	(setq st1 (car sub1))
	(setq st2 (car sub2))
	(cond
	  ((term$is_var st1) (unless (eq st1 st2) (return nil)))
	  ((term$is_var st2) (return nil))
	  ((term$is_built_in_constant_fast st1)
	    (unless (term$equal_built_in_fast st1 st2) (return nil)))
	  (t (unless
		(if (theory_name$is_empty_for_matching
;		     (theory$name (operator$theory (term$head st1))) @@@
		     (operator$theory_name_for_matching (term$head st1))
		     )
		  (empty$equal st1 st2)
		  (term$equational_equal st1 st2))
	      (return nil))))))))
