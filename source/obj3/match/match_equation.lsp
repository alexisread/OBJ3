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

;; $Id: match_equation.lsp,v 205.2.1.1 2003/09/23 14:09:51 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                     The equation cluster
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; Claude Kirchner ;;;; Created: April 86.

;;;;;;;;;;;;;;;; Summary

; op match_equation$create: Term Term -> Equation .
; op match_equation$decomposition: Equation -> List[Equation], 
;                                        signals(no_match)
; --- unexportable
; op match_equation$$!decomposition: Term Term List[Equation] -> 
;                                    List[equation]~
;                                    signals(no_match)

;;;;;;;;;;;;;;;; Representation 

; an equation t1 == t2 is represented by a doublet (cons t1 t2)

;;;;;;;;;;;;;;;;

; create a new equation containing the terms "t1" "t2"

; op match_equation$create: Term Term -> Equation .

(defun match_equation$create (t1 t2)
  (cons t1 t2)
  )

; The equation "eq" to be decomposed is supposed to be
; of the form t1 == t2 with t1 and t2 in standard form
; (i.e. in normal form for idempotence and zero if any).

; op match_equation$decomposition: Equation -> List[Equation], 
;                                     signals(clash_of_symbols)
(defun match_equation$decomposition (eq)
  ; the following curious initialisation allows to modify "list_dec_eq"
  ; inside match_equation$$!decomposition
  (let ((list_dec_eq (cons nil nil))
	 (no_match nil))
    (setq no_match (match_equation$$!decomposition (match_equation$t1 eq)
						   (match_equation$t2 eq)
						   list_dec_eq))
    (values (cdr list_dec_eq) no_match)
    )
  )

; help the previous one.
; Z: This contains the only difference between mono-sorted and order-sorted
; matching, watch the "obj3-devel" tags.
; Z: "l_result" is modified .

(defvar *match$check_variables* t)

; op match_equation$$!decomposition: Term Term List[Equation] -> 
;                                   List[equation]~
;                                   signals(no_match)
(defun match_equation$$!decomposition (t1 t2 l_result)
  (block no_match
    (cond ((term$is_var t1)
	   ;; This is a point of difference with OBJ2
	   ;; test of the sort of the variable,
	   ;; NO test is done if the variable is general
	   (unless 
	    (or (and ;&&&&!!!! 17 Mar 87
		 (not *match$check_variables*)
		 (variable$is_general_variable t1))
;		(member
;		 (term$sort t2) 
;		 (variable$sorts t1)
;		 :test #'(lambda (s1 s2)
;			   (sort_order$is_included_in
;			    (module$sort_order
;			     (if obj$current_module obj$current_module
;			       (sort$module s2)))
;			    s1 s2
;			    )
;			   )
;		)
		(let ((ts (term$sort t2)))
		(if obj$current_module
		    (let ((so (module$sort_order obj$current_module)))
		    (dolist (vs (variable$sorts t1) nil)
		      (when (sort_order$is_included_in so ts vs)
			(return t))))
		  (dolist (vs (variable$sorts t1) nil)
		    (when (sort_order$is_included_in
			   (module$sort_order (sort$module vs))
			   ts vs)
		    (return t)))
		))
		)
	    (match_equation$$!decomposition_on_demand t1 t2 l_result))
	   (push (match_equation$create t1 t2) (cdr l_result))
	   nil)
	  ((term$is_built_in_constant t1)
	   (if (term$equal_built_in t1 t2) nil
	     (return-from no_match t))) ;&&&& simplify?
	  ((term$is_var t2) ;&&&& 17 Aug 87 -- trying to do a general fix
	   (return-from no_match 'no_match_to_var))
	  (t (let ((t1_top (term$head t1)))
	       (cond ((not (theory_name$is_empty_for_matching
			    (operator$theory_name_for_matching t1_top))) ;@@@
		      (push (match_equation$create t1 t2) 
			    (cdr l_result))
		      (return-from no_match nil))
		     (t (if (term$is_var t2)
			    ;(return-from no_match  t) ;&&&& redund.?
			    (match_equation$$!decomposition_on_demand
			     t1 t2 l_result)
			    (let ((t2_top (term$head t2)))
			      (if ;; since it is OS-matching, we only
				  ;; test the name of the operator.
				  ;; vs OBJ2
				  (operator$is_same_operator
				   t1_top t2_top)
				  (let ((t1_subterms (term$subterms t1))
					(t2_subterms (term$subterms t2)))
				    (do ((tt1)
					 (tt2))
					((not t1_subterms))
				      (setq tt1 (pop t1_subterms) 
					    tt2 (pop t2_subterms))
				      (let ((not_ok 
					     (match_equation$$!decomposition 
					      tt1 tt2 l_result)))
					(when not_ok
					(when
				      ;whoops ^^
				      (match_equation$$!decomposition_on_demand
				                t1 t2 l_result)
					    (return-from no_match t)))
					) ;; let 
				      ) ;;do
				    (return-from no_match nil))
				(match_equation$$!decomposition_on_demand
				 t1 t2 l_result)
				))
			    ))
		     )))
	  )
    );; block no_match
  )

(defvar *rew$perform_on_demand_reduction* nil)

; on failure return true value
(defun match_equation$$!decomposition_on_demand (t1 t2 l_result)
  (if (and *rew$perform_on_demand_reduction*
	   (term$is_on_demand t2))
      (progn
        (term$!mark_as_lowest_parsed t2)
	(if (rew$!normalize_with_memo t2)
	    t
	  (match_equation$$!decomposition t1 t2 l_result)
	  )
	)
    t))

(defmacro match_equation$t1 (eq)
  `(car ,eq))

(defmacro match_equation$t2 (eq)
  `(cdr ,eq))

;(defun match_equation$unparse (eq)
;  (princ (term$!print (car eq))) (princ "==") (princ (term$!print (cdr eq)))
;  (terpri))
