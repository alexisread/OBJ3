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

;; $Id: cz.lsp,v 205.2.1.1 2003/09/23 14:09:51 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  CZ: commutativity with identity
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Patrick Lincoln     May 1990,Aug 1990
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The idea is that there are only two possible matches for any 
;; pair of commutative terms - 11 and 22, or 12 and 21.  Since we
;; may actually get a system of multiple equations, we have to 
;; generate all possible pairings.
;;
;; Also, we check for identity binding possibilities for each of the 
;; four immediate subterms.  Thus there are 4 possible match sub-equations
;; where one term is dropped, and 4 possible match sub-equations where
;; two terms are dropped.
;;
;; PDL's current understanding is that the cases with two terms dropped
;; due to identity are not needed, since e.g. 4 = 4*1   must be 
;; handled by the matching algorithm anyway.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct cz-state
  n
  sys)

;;; t1 of each eq of sys is pattern, has variables.  t2 is (ground) term.

(defun CZ$state_initialize (sys env)
  env ;; why isn't env used here or in C$?
  (values (make-cz-state :n 0 :sys sys) nil))

; if we have only one equation to solve, then this will simply
; count from 0 to 5, where each number determines a particular
; solution to the equation modulo CZ.  If we have more equations,
; then we have to generate all possibilities - (0-5)*(0-5)*...*(0-5)
; we keep track of which solution to try next with one integer which
; enumerates all possible solutions.

(defun CZ$next_state (CZ_st)
  (let* ((sys (cz-state-sys CZ_st))
	 (point (system$list_eq sys))
	 (equation nil)
	 (r 0)
	 (t1 nil)
	 (t2 nil)
	 (new_sys (system$new))
	 (lg (length point))
	 (op1 nil)
	 (op2 nil)
	 )
        (do* ((N (cz-state-n CZ_st))
	      (q N N)
	      ;(point2 point point)
	      )
	    ((or (not (system$is_empty new_sys))
		 (>= N (expt 6 lg)))
	     (progn 
	       (setf (cz-state-n CZ_st) N)
	       (if (not (system$is_empty new_sys))
		   (values new_sys CZ_st nil) ;succes case
	           (values nil nil t)))) ; fail case
	    (incf N) ; try the next N
	    (dotimes (k lg)				; k = lg,...,1
		   ; this treats q as a bitvector in base 6
	           #+GCL (setq r (rem q 6) q (truncate q 6))
		   #-GCL (multiple-value-setq (q r) (truncate q 6))
		   (setq equation (car point)
			 point (cdr point)
			 t1 (match_equation$t1 equation)
			 t2 (match_equation$t2 equation)
			 op1 (if (or (term$is_constant t1)
				     (term$is_var t1))
				 nil 
			         (term$head t1))
			 op2 (if (or (term$is_constant t2)
				     (term$is_var t2))
				 nil 
			         (term$head t2)))
		   (cond ((and (= r 0)  ; as if no thoery applied - 11 22
			       op1 op2)
			  (system$add_eq new_sys 
			   (match_equation$create (term$arg_1 t1) 
						  (term$arg_1 t2)))
			  (system$add_eq new_sys 
			   (match_equation$create (term$arg_2 t1) 
						  (term$arg_2 t2))))
			 ((and (= r 1)  ; comm - 12 21
			       op1 op2) ;
			  (system$add_eq new_sys 
			   (match_equation$create (term$arg_1 t1) 
						  (term$arg_2 t2)))
			  (system$add_eq new_sys 
			   (match_equation$create (term$arg_2 t1) 
						  (term$arg_1 t2))))
			 ((and (= r 2)
			       op1 ; term is non atomic
			       (not (term$is_zero_for_op (term$arg_1 t1) op1)))
			  (system$add_eq new_sys
			   (match_equation$create (term$arg_1 t1)
						  (term$make_zero op1)))
			  (system$add_eq new_sys
			   (match_equation$create (term$arg_2 t1) t2)))
			 ((and (= r 3)
			       op1 ; term is non atomic
			       (not (term$is_zero_for_op (term$arg_2 t1) op1)))
			  (system$add_eq new_sys
			   (match_equation$create (term$arg_2 t1)
						  (term$make_zero op1)))
			  (system$add_eq new_sys
			   (match_equation$create (term$arg_1 t1) t2)))
			 ;; note these are redundant if we have terms 
			 ;; in normal form (no identities).
			 ((and (= r 4)
			       op2 ; term is non atomic
			       (not (term$is_zero_for_op (term$arg_1 t2) op2)))
			  (system$add_eq new_sys
			   (match_equation$create (term$make_zero op2)
						  (term$arg_1 t2)))
			  (system$add_eq new_sys
			   (match_equation$create t1 (term$arg_2 t2))))
			 ((and (= r 5)
			       op2 ; term is non atomic
			       (not (term$is_zero_for_op (term$arg_2 t2) op2)))
			  (system$add_eq new_sys
			   (match_equation$create (term$make_zero op2)
						  (term$arg_2 t2)))
			  (system$add_eq new_sys
			   (match_equation$create t1 (term$arg_1 t2))))
			 (t nil))))))

; same comment as above applies here - 
; we don't return the other possible solutions because they
; are subsumed by the ones present here.
; CZ$equal assumes non-atomic args.

(defun CZ$equal (t1 t2)
  (let ((op1 (term$head t1))
	(op2 (term$head t2)))
  (or (term$similar t1 t2) ; was equal
      (and (term$is_zero_for_op (term$arg_1 t1) op1)
	   (term$equational_equal (term$arg_2 t1) t2))
      (and (term$is_zero_for_op (term$arg_2 t1) op1)
	   (term$equational_equal (term$arg_1 t1) t2))
      (and (term$is_zero_for_op (term$arg_1 t2) op2)
	   (term$equational_equal t1 (term$arg_2 t2)))
      (and (term$is_zero_for_op (term$arg_2 t2) op2)
	   (term$equational_equal t1 (term$arg_1 t2)))
      (and (term$equational_equal (term$arg_1 t1) (term$arg_1 t2))
	   (term$equational_equal (term$arg_2 t1) (term$arg_2 t2)))
      (and (term$equational_equal (term$arg_1 t1) (term$arg_2 t2))
	   (term$equational_equal (term$arg_2 t1) (term$arg_1 t2))))))
