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

;; $Id: theory.lsp,v 205.2.1.1 2003/09/23 14:09:52 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Regroup all the procedures specific to the equational theory
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; Claude Kirchner ;;;; Created: April 86.

;;;;;;;;;;;;;;;; Summary
; op theory$zero: theory -> List[Term]

;;;;;;;;;;;;;;;; Representation

; with the following representation, theory is for example
; [ACZ,(0)] which means that if this theory is
; associated to the operator "+" then the axioms related to "+"
; are x+(y+z) = (x+y)+z, x+y = y+x, x+0 = x, 0+x = x.

; The current theories supported are a combinaison of
; --- associativity x+(y+z) = (x+y)+z
; --- commutativity x+y = y+x
; --- idempotence   x+x = x
; --- zero          x+0 = x, 0+x = x

; denoted by

; empty
; A, C, Z, I,
; AC, AI, AZ, CZ, CI, IZ
; ACI, ACZ, CIZ, AIZ
; ACIZ

; --- In the lisp implementation, a "theory" like AC is
; --- called a "property".

; rep = structure[name: theory_name, zero: term_list]

(defstruct (theory
  	     (:conc-name theory$)
	     (:constructor theory$make (name zero)))
  name ; in fact this is now a theory_info structure
  zero)

; op theory$name : Theory -> Theory_Info
; op theory$zero: Theory -> Term Rule_only_flag

(defstruct (theory_info
	     (:conc-name theory_info$)
	     (:constructor theory_info$make
			   (name code empty_for_matching equal init next)))
  name
  code
  empty_for_matching
  equal
  init
  next)

(defun theory$zero_rule_only (th)
  (let ((val (theory$zero th)))
    (and val (cdr val))))

(defun theory$symbol (th)
  (theory_info$name (theory$name th)))

(defun theory$create (name zero)
    (theory$make
     (if (symbolp name) (theory$name_to_info name) name)
                     ; may be extracted from existing theory
     zero)
    )

(defun theory$name_to_info (name)
  (aref theory_info_array (theory_name$code_direct name))
  )

(defconstant Z_flag 1)
(defconstant I_flag 2)
(defconstant C_flag 4)
(defconstant A_flag 8)

(defconstant AC_flags 12)

; multiple bits can be tested
(defmacro test-flag (x y)
  `(not (= 0 (logand ,x ,y)))
  )

;@@@@@ not used yet
(defun theory$attributes_to_info (a c i z)
  (aref theory_info_array
      (+ (if a A_flag 0) (if c C_flag 0) (if i I_flag 0) (if z Z_flag 0)))
  )

(defun theory$code_to_info (x)
  (aref theory_info_array x))

(defvar the_empty_property
  (theory_info$make
   'empty_property 0 t
   'empty$equal
   'empty$state_initialize 'empty$next_state))

(defvar the_Z_property
  (theory_info$make
   'Z_property 1 nil
   'Z$equal
   'Z$state_initialize 'Z$next_state))

(defvar the_I_property
  (theory_info$make
   'I_property 2 t
   'empty$equal
   'empty$state_initialize 'empty$next_state))

(defvar the_IZ_property
  (theory_info$make
   'IZ_property 3 nil
   'Z$equal
   'Z$state_initialize 'Z$next_state))

(defvar the_C_property
  (theory_info$make
   'C_property 4 nil
   'C$equal
   'C$state_initialize 'C$next_state))

(defvar the_CZ_property
  (theory_info$make
   'CZ_property 5 nil
   'CZ$equal
   'CZ$state_initialize 'CZ$next_state))

(defvar the_CI_property
  (theory_info$make
   'CI_property 6 nil
   'C$equal
   'C$state_initialize 'C$next_state))

(defvar the_CIZ_property
  (theory_info$make
   'CIZ_property 7 nil
   'CZ$equal
   'CZ$state_initialize 'CZ$next_state))

(defvar the_A_property
  (theory_info$make
   'A_property 8 nil
   'A$equal
   'A$state_initialize 'A$next_state))

(defvar the_AZ_property
  (theory_info$make
   'AZ_property 9 nil
   'AZ$equal
   'AZ$state_initialize 'AZ$next_state))

(defvar the_AI_property
  (theory_info$make
   'AI_property 10 nil
   'A$equal
   'A$state_initialize 'A$next_state))

(defvar the_AIZ_property
  (theory_info$make
   'AIZ_property 11 nil
   'AZ$equal
   'AZ$state_initialize 'AZ$next_state))

(defvar the_AC_property
  (theory_info$make
   'AC_property 12 nil
   'AC$equal
   'AC$state_initialize 'AC$next_state))

(defvar the_ACZ_property
  (theory_info$make
   'ACZ_property 13 nil
   'ACZ$equal
   'ACZ$state_initialize 'ACZ$next_state))

(defvar the_ACI_property
  (theory_info$make
   'ACI_property 14 nil
   'AC$equal
   'AC$state_initialize 'AC$next_state))

(defvar the_ACIZ_property
  (theory_info$make
   'ACIZ_property 15 nil
   'ACZ$equal
   'ACZ$state_initialize 'ACZ$next_state))

(defvar theory_info_array
  (make-array '(16) :initial-contents
    (list
      the_empty_property
      the_Z_property
      the_I_property
      the_IZ_property
      the_C_property
      the_CZ_property
      the_CI_property
      the_CIZ_property
      the_A_property
      the_AZ_property
      the_AI_property
      the_AIZ_property
      the_AC_property
      the_ACZ_property
      the_ACI_property
      the_ACIZ_property
     )))

; Compute the (empty)-normal form of the term "t" with respect to
; the axioms of the current theory. For example if the current theory
; is AC(+)Z(+,0) then it computes the normal form for the axioms x+0 -> x
; 0+x -> x. May be modified if one adds a new theory. Be carefull with the
; potential extensions.
; --- t is supposed of the form f(t1,..., tn).

; op theory$standard_form : Term -> Term
(defun theory$standard_form (term)
  (if (or (term$is_var term) (term$is_constant term))
      term
    (let* ((f (term$head term))
	   (subterms (mapcar 'theory$standard_form (term$subterms term)))
	   (th (operator$theory f))
	   (th_name  (theory$name th))
	   (t1 nil)
	   (t2 nil))
    (let ((val
      (cond ((theory_name$is_empty th_name) (term$make_term f subterms))
	    ;; case x+0 -> x, 0+x -> x
;	    ((and (theory$zero th)
;		(setq t1 (car subterms) t2 (cadr subterms))) ...)
	    ((and (progn
		    (setq t1 (car subterms) t2 (cadr subterms))
		    (theory$zero th))
		  (let ((zero (car (theory$zero th))))
		    (cond ((term$similar t1 zero) t2)
			  ((term$similar t2 zero) t1)))))
	    ;; case x+x -> x
	    ((or (theory_name$is_I th_name) (theory_name$is_CI th_name))
	     (if (term$similar t1 t2) t1))
	    ;; It is more complex in the next cases because of 
	    ;; the presence of non trivial extensions
	    ;; and of commutativity, so we refer to appropriate procedure
	    ((theory_name$is_AI th_name)
	     (A$idempotent_normal_form f t1 t2))
	    ((or (theory_name$is_ACI th_name) 
		 (theory_name$is_ACIZ th_name))
	     (AC$idempotent_normal_form f t1 t2))
	    )))
      (if val val (term$make_term f subterms))
      )
      )
    )
  )

(defun A$idempotent_normal_form (f t1 t2)
  (if (term$similar t1 t2) t1 (term$make_term f (list t1 t2))))

(defun AC$idempotent_normal_form (f t1 t2)
  (if (term$similar t1 t2) t1 (term$make_term f (list t1 t2))))

; Returns true iff the two terms "t1" and "t2" are
; E_equal in the theory "th" which the theory of the top symbol of "t1"
; It supposes that "t1" and "t2" are already in standard form

; op theory$E_equal_in_theory : Theory Term Term -> Bool
(defun theory$E_equal_in_theory (th t1 t2)
  (funcall (theory_info$equal (theory$name th)) t1 t2)
  )

; op theory$E_equal_in_theory : Theory Term Term -> Bool
(defun theory$E_equal_in_theory_direct (th t1 t2)
  (let ((th_name (theory$name th)))
    (cond ((or (theory_name$is_empty th_name)
	       (theory_name$is_Z th_name)
	       (theory_name$is_I th_name)
	       (theory_name$is_IZ th_name)) (empty$equal t1 t2))
	  ((or (theory_name$is_AC th_name)
	       (theory_name$is_ACI th_name)
	       (theory_name$is_ACZ th_name)
	       (theory_name$is_ACIZ th_name)) (AC$equal t1 t2))
	  ((or (theory_name$is_A th_name)	
	       (theory_name$is_AI th_name)
	       (theory_name$is_AZ th_name)
	       (theory_name$is_AIZ th_name)) (A$equal t1 t2))
	  ((or (theory_name$is_C th_name)
	       (theory_name$is_CI th_name)
	       (theory_name$is_CZ th_name)
	       (theory_name$is_CIZ th_name)) (C$equal t1 t2))
	  )
    )
  )


; returns true iff the theory "th" contains the associativity axiom
(defun theory$contains_associativity (th)
  (test-flag A_flag (theory_info$code (theory$name th)))
  )

(defun theory$contains_associativity_direct (th)
  (let ((th_name (theory$name th)))
    (and (not (theory_name$is_empty th_name)) ;25 Feb 88 speed up empty case
    (or (theory_name$is_A th_name) 
	(theory_name$is_AC th_name) 
	(theory_name$is_AI th_name) 
	(theory_name$is_AZ th_name) 
	(theory_name$is_AIZ th_name) 
	(theory_name$is_ACI th_name)
	(theory_name$is_ACZ th_name) 
	(theory_name$is_ACIZ th_name))))
  )

; returns true iff the theory "th" contains the commutativity axiom
(defun theory$contains_commutativity (th)
  (test-flag C_flag (theory_info$code (theory$name th)))
  )

(defun theory$contains_commutativity_direct (th)
  (let ((th_name (theory$name th)))
    (and (not (theory_name$is_empty th_name)) ;25 Feb 88 speed up empty case
    (or (theory_name$is_C th_name) 
	(theory_name$is_AC th_name) 
	(theory_name$is_CI th_name) 
	(theory_name$is_CZ th_name) 
	(theory_name$is_CIZ th_name) 
	(theory_name$is_ACI th_name)
	(theory_name$is_ACZ th_name) 
	(theory_name$is_ACIZ th_name))))
  )

(defun theory$contains_AC (th)
  (test-flag AC_flags (theory_info$code (theory$name th)))
  )
(defun theory$contains_AC_direct (th)
  (let ((th_name (theory$name th)))
    (or (theory_name$is_AC th_name) 
	(theory_name$is_ACZ th_name)
	(theory_name$is_ACI th_name)
	(theory_name$is_ACIZ th_name)))
  )


; returns true iff the theory "th" contains the idempotency axiom
(defun theory$contains_idempotency (th)
  (test-flag I_flag (theory_info$code (theory$name th)))
  )
(defun theory$contains_idempotency_direct (th)
  (let ((th_name (theory$name th)))
    (or 
	(theory_name$is_I th_name) 
	(theory_name$is_CI th_name) 
	(theory_name$is_IZ th_name) 
	(theory_name$is_AI th_name) 
	(theory_name$is_AIZ th_name) 
	(theory_name$is_ACI th_name)
	(theory_name$is_CIZ th_name)
	(theory_name$is_ACIZ th_name)))
  )

(defun theory$contains_identity (th)
  (test-flag Z_flag (theory_info$code (theory$name th)))
  )

(defun theory$contains_identity_direct (th)
  (let ((th_name (theory$name th)))
    (or 
	(theory_name$is_Z th_name) 
	(theory_name$is_CZ th_name) 
	(theory_name$is_AZ th_name) 
	(theory_name$is_IZ th_name) 
	(theory_name$is_AIZ th_name) 
	(theory_name$is_ACZ th_name)
	(theory_name$is_CIZ th_name)
	(theory_name$is_ACIZ th_name)))
  )

(defvar theory$the_empty_theory
  ;(theory$create the_empty_property nil)
  (theory$make the_empty_property nil)
  )

(defconstant ACZ_flags 13)

(defun theory$contains_ACZ (th)
  (test-flag ACZ_flags (theory_info$code (theory$name th))))

(defun theory$contains_ACZ_direct (th)
  (let ((th_name (theory$name th)))
    (or (theory_name$is_ACZ th_name)
	(theory_name$is_ACIZ th_name))))

(defconstant AZ_flags 9)

(defun theory$contains_AZ (th)
  (test-flag AZ_flags (theory_info$code (theory$name th))))

(defun theory$contains_AZ_direct (th)
  (let ((th_name (theory$name th)))
    (or (theory_name$is_AZ th_name)
	(theory_name$is_AIZ th_name)
	(theory_name$is_ACZ th_name)
	(theory_name$is_ACIZ th_name))))
