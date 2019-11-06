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

;; $Id: c.lsp,v 205.2.1.1 2003/09/23 14:09:51 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;    All the procedures specific to the commutative theory
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; Claude Kirchner ;;;; Created: April 86.

;;;;;;;;;;;;;;;; Summary

; op C$state_initialize: System -> CommutativeState, signals(no_match)
; op C$next_state: CommutativeState -> System CommutativeState, 
; op C$equal: Term Term -> Bool

;;;;;;;;;;;;;;;; Representation

; the commutative state consists into an array of booleen and the original 
; system. The state of booleen represent the state of the exploration of 
; all the permutation. It has to be see as the representation in basis 
; 2 of a number < 2^r.

;C_state = commutative_state

;commutative_state = record{X: int, 
;                 sys: system}

(defstruct (C_state
	    (:constructor make_C_state (count sys)))
	(count 0 :type integer)
	sys )

;;;;;;;;;;;;;;;;

; Initialize a commutative state. It check if the top symbols of
; each equation of the system 
; have the same (commutative) head function.

; op C$state_initialize: System Environment
;			 -> CommutativeState, signals(no_match)

(defun C$state_initialize (sys env)
  (declare (ignore env))
  ;; "env" not used for now in commutative [ck: Nov 29 87] @@
  (block no_match
    (dolist (equation (system$list_eq sys))
      (unless
	  (and
	   (not (term$is_var (match_equation$t2 equation)))
	   (operator$is_commutative_restriction_of
	    (term$head (match_equation$t2 equation))
	    (term$head (match_equation$t1 equation))))
	(return-from no_match (values nil t)))
      )
    (values 
      (make_C_state 0 sys)
      nil)
    )
  )

; op C$next_state: CommutativeState -> System CommutativeState, 
;                                      signals(no_more)

(defun C$next_state (C_st)
  (let* ((N   (C_state-count C_st))
	 (sys (C_state-sys C_st))
	 (q   N)
	 (r   0)
	 (point (system$list_eq sys))
	 (equation nil)
	 (t1 nil)
	 (t2 nil)
	 (new_sys (system$new))
	 (lg (length point))
	 )
    (declare (fixnum q r lg N))
    (if (= N (expt 2 lg))
	; there is no more C_state
	(values nil nil t)
	(progn 
	  (dotimes-fixnum (k lg)				; k = lg,...,1
		   #+GCL (setq r (rem q 2) q (truncate q 2))
		   #-GCL (multiple-value-setq (q r) (truncate q 2))
		   (setq equation (car point)
			 point (cdr point)
			 t1 (match_equation$t1 equation)
			 t2 (match_equation$t2 equation))
		   (cond ((= r 0) 
			  (system$add_eq 
			   new_sys 
			   (match_equation$create (term$arg_1 t1) 
						  (term$arg_1 t2)))
			  (system$add_eq 
			   new_sys 
			   (match_equation$create (term$arg_2 t1) 
						  (term$arg_2 t2))))
			 (t (system$add_eq 
			     new_sys 
			     (match_equation$create (term$arg_1 t1) 
						    (term$arg_2 t2)))
			    (system$add_eq 
			     new_sys 
			     (match_equation$create (term$arg_2 t1) 
						    (term$arg_1 t2)))
			    )
			 ) ;; cond
	  ) ;; do
	  (setf (C_state-count C_st) (1+ N))
	  ;	(print C_st)
	  (values new_sys C_st nil)
	  )
	) ;; if
    )
  )

; "t1" and "t2" are supposed to be terms with same head commutative symbol

; op C$equal: Term Term -> Bool
(defun C$equal (t1 t2)
;  (or (and (term$equational_equal (term$arg_1 t1) 
;			 (term$arg_1 t2))
;	   (term$equational_equal (term$arg_2 t1) 
;			 (term$arg_2 t2)))
;      (and (term$equational_equal (term$arg_1 t1) 
;			 (term$arg_2 t2))
;	   (term$equational_equal (term$arg_2 t1) 
;			 (term$arg_1 t2))))
; optimization of the above
  (if (term$equational_equal (term$arg_1 t1) (term$arg_1 t2))
    (term$equational_equal (term$arg_2 t1) (term$arg_2 t2))
    (and (term$equational_equal (term$arg_1 t1) (term$arg_2 t2))
	 (term$equational_equal (term$arg_2 t1) (term$arg_1 t2))))
  )
