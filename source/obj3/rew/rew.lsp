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

;; $Id: rew.lsp,v 205.2.1.1 2003/09/23 13:48:13 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;               The Rewrite Engine of OBJ3
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Claude Kirchner ;;;; created: June 2, 86 ;;;;

;;;;;;;;;;;;;;;; Summary

; op rew$!normalize: Term -> Term~
; op rew$!normalize_with_memo: Term MemoTable -> Term~ Bool
; op rew$!reduce: Term List[Int] List[Int] -> Term~
; op rew$!apply_rules_with_memo:  Term List[Int] List[Int] -> Term~
; op rew$!apply_rules: Term List[Int] List[Int] -> Term~
; op rew$!apply_rules_with_same_top_on_top: Term Rule_Ring -> Set[Occurrence] 
; op rew$!apply_rules_with_different_top_on_top: Term Rule_Set -> ~Term Bool

(defvar rew$clean_memo_in_normalize nil)

; The present code is inspired from the rewrite engine of OBJ2, written
; by Jean-Pierre Jouannaud. The main differences are:
; 1) the fact that it implements Order-Sorted rewritting and thus that
;    the sort information has to be updated in the term to be reduced.
;    It suppose of course that the equations given by the user have been
;    correctly compiled.
;    The sort updating is done at two levels.
;    a) After replacement, the ancestors of the node which have been replaced
;       must possibly update they lowest sort. 
;       That is done only by necessity in
;       "rew$!reduce" by "term$!update_lowest_parse_on_top"
;       when the first element of "strategy" is "0".
;    b) By "substitution$image" which is supposed to compute the lowest parse
;       of "sigma(term)".
; 2) the rule are factorized in the sense that a variable in a lhs of a rule
;    have a non-empty set of sorts. It comes form the way that the 
;    operational  semantic in implemented. Instead of having "separate" rules
;    as specialization, they are factorized and discrimined on the variables.
; 3) a more modular code

; op rew$!normalize: Term -> Term~
(defun rew$!normalize (term)
  (rew$!normalize_with_memo 
   term
   (if rew$clean_memo_in_normalize
       (memo_rew$clean_memo_table *memo_rew$table*)
     *memo_rew$table*)
   )
  term
  )

;like normalize but doesn't clear hash-table
(defun rew$!normalize_term (term)
  (rew$!normalize_with_memo term *memo_rew$table*)
  term
  )

;; normalize returns a booleen which is true iff the term
;; to normalize was already in normal form (equivalent to
;; "signals(already normalized)" in CLU.
;; It produces by side effect 
;; a term in normal form for the current state of OBJ.
;; Note that it destroys the term to be reduced by using the "term$replace"

;; op rew$!normalize_with_memo: Term MemoTable -> ~Term Bool
(defun rew$!normalize_with_memo (term &optional (memo_table *memo_rew$table*))
  (let ((top nil)
	(strategy nil))
    (alt :rew_statistics (rew_stat$normalization_count))
    (cond ((term$is_reduced term)
	   ;;a variable is considered as a term all ready reduced.
	   (alt :rew_statistics (rew_stat$all_ready_reduced_count))
	   ;; in this case the value returned is "true"
	   ;; i.e. no reduction have been performed.
	   (when (and (term$is_built_in_constant_fast term)
		      (sort_order$lower_sorts
		       (module$sort_order  obj$current_module)
		       (term$sort term))
		      )
	     (term$!update_lowest_parse_on_top term))
	   t)
	  ((null (setq strategy (module$operator_rew_strategy ;14 Jul 87
				 obj$current_module
				 (setq top (term$head term)))))
	   ;; only irreducible constants may have an empty strategy.
	   (term$!mark_as_reduced term)
	   ;; in this case the value returned id "true"
	   ;; i.e. no reduction have been performed.
	   t)
	  ((operator$is_memo top)
	   (term$!replace term 
			 (memo_rew$normal_form term memo_table strategy))
	   ;; the term has probably been modified
	   nil)
	  (t
	   (rew$!reduce term 
			strategy 
			nil)
	   ;; the term has probably been modified
	   nil)
	  ) ;; cond
    )
  )

; reduce is called by normalize in order to made the work of normalization
; with respect with the strategies.

; Z: "term" is never a variable or an irreducible constant
; Z: "term" is supposed to be in right associative normal form (replacement
; must take care of that).

; op rew$!reduce: Term List[Int] List[Int] -> Term~
(defun rew$!reduce (term strategy end_reductions)
  (alt :rew_statistics (rew_stat$reduce_count))
  (let ((occ nil)(top nil))
    (cond ((null strategy)
	   ;; the strategy is exhausted
	   (if (null end_reductions)
	       (progn 
		 (when (term$is_not_well_lowest_parsed term)
		   (setq top (term$head term))
		   (term$!update_lowest_parse_on_top term)
		   (when (not (eq top (term$head term)))
		     (return-from rew$!reduce
			(rew$!normalize_with_memo term))))
		 (term$!mark_as_reduced term) ;;
		 )
       	       (rew$!reduce term
			    ;; avoid append if can &&&& ever help?
			    (if (member 0 end_reductions) end_reductions
			      (append end_reductions (list 0)))
			    nil)))
	  ((zerop (setq occ (car strategy)))
	   (unless (term$is_reduced term)
	       (rew$!apply_rules_with_memo term strategy end_reductions))
	  )
	  ((< occ 0)
	   (let ((that_arg (term$arg_n term (abs occ))))
	     (unless (term$is_reduced that_arg)
	       (term$!mark_as_on_demand that_arg)))
	   (rew$!reduce term (cdr strategy) end_reductions))
	  (t 
	   ;; Non zero strategy transfers immediate control to 
	   ;; adequate subterm: 
	   ;; -- if the top operator contains associativity then the subterms
	   ;; to reduce are the first term with a different top symbol
	   ;; -- otherwise, keep the number specified in the strategy
	   ;;  *ck: 8 may 87*
	   (if (operator$is_associative_fast (setq top (term$head term)))
	       (let ((list_subterms (term$list_assoc_subterms term top))
		     (lowest_parsed t))
		 (dolist (x list_subterms)
		  (unless (rew$!normalize_with_memo x)
		    (setq lowest_parsed nil)))
		 (unless lowest_parsed
		   (term$!mark_as_not_lowest_parsed_on_top term))
		 ;; forget about the end_reductions and reduce on top
		 ;&&&&  wonder about this; ignores strategy
		 (rew$!reduce term (list 0) nil)
		 )
	     ;; else top is non associative *ck: 8 may 87*
	     (progn
	       (unless (rew$!normalize_with_memo (term$arg_n term occ))
		       ;; then the subterm at occurrence "occ" as probably
		       ;; been modified so that the lowest parse of "term" has
		       ;; to be recomputed (at the beginning of rew$!reduce)
		       (term$!mark_as_not_lowest_parsed_on_top term))
	       (rew$!reduce term (cdr strategy) end_reductions))
	     ) ;; if
       )
    ))
  )

; op rew$!apply_rules_with_memo:  Term List[Int] List[Int] -> Term~
(defun rew$!apply_rules_with_memo (term strategy end_reductions
				   &optional (memo_table *memo_rew$table*))
  (cond
   ((operator$is_memo (term$head term))
    (term$!replace
      term
      (memo_rew$!apply_rules term memo_table strategy end_reductions))
   )
   (t
    (rew$!apply_rules term strategy end_reductions)
   )
  ))

; op rew$!apply_rules: Term List[Int] List[Int] -> Term~
(defun rew$!apply_rules (term strategy end_reductions)
  (let ((top (term$head term)))
  ;; first check if the top level is well lowest parsed
  (when (or (term$is_not_well_lowest_parsed term)
	    (operator$is_a_retract top)) ;&&&&  eliminate ??
	;&&&& warning the following is grotesquely adhoc
	;&&&& this is necessary since term$!update_lowest_parse_on_top
	;&&&&  may restructure the term
	(let ((tl (cdr term)))
	  (term$!update_lowest_parse_on_top term)
	  (when (not (eq (cdr term) tl))
		(return-from rew$!apply_rules
		  (rew$!normalize_with_memo term)))
	  )
	(setq top (term$head term)))
  ;; then apply the rules with same top
  (let ((rwst ; (operator$rules_with_same_top top)
	 (module$rules_with_same_top obj$current_module top)
	 ))
    (when rwst
	  (setq end_reductions 
		(union end_reductions
		       (rew$!apply_rules_with_same_top_on_top 
			term 
			rwst))))
    ) ;; let
  ;; then try to apply rule with different top,
  ;; Note that the function 
  ;; "rew$!apply_rules_with_different_top_on_top"
  ;; returns true iff one rule has successfully be apply
  (if (or (term$is_var term)
	  (not (eq top (term$head term))))
      ; This case added only because of change to a simplifying instantiator
      ; Top of term may have changed
      (rew$!normalize_with_memo term)
  (if (rew$!apply_rules_with_different_top_on_top
       term
       ;; (operator$rules_with_different_top top)
       (module$rules_with_different_top obj$current_module top)
       )
      ;; then the term "term" has changed of top symbol
      ;; and normalize must be recursively apply,
      (rew$!normalize_with_memo term)
    ;; else the term must be reduced using the rest of the strategy
    (rew$!reduce term (cdr strategy) end_reductions)
    )
  )))

;; apply the rules with same top on "term".
;; Returns the set of occurrences to be checked after.

;; op rew$!apply_rules_with_same_top_on_top: Term Rule_Ring -> 
;                                            Set[Occurrence] 
(defun rew$!apply_rules_with_same_top_on_top (term rule_ring)
  (let ((rr_ring (rule_ring-ring rule_ring)))
  (if (null rr_ring) nil
  (let ((rr_current rr_ring)
	(rr_mark rr_ring)
	(original_top (term$head term))
	(end_red nil)
	rule)
  (loop
   (setq rule (car rr_current))
   ;; apply_rule returns true iff the rule has been successfully apply.
   (when (rule$!apply_rule rule term)
	 ;; the mark is set to this rule
	 (setq rr_mark rr_current)
	 ;; Memorize  the set of occurrences to be checked after
	 ;; that the reduction with same top has ended
	 (setq end_red (union end_red (rule$end_reduction rule)))
	 ;; and the same rule is apply until it fails
	 (do () ((not (rule$!apply_rule rule term))))
	 ) ;; when
   (setq rr_current (cdr rr_current))
   (when (eq rr_current rr_mark)
     (return
       (if (eq original_top (term$head term)) ; term went down
	   end_red
	 (if (member 0 end_red) end_red ;&&&& simplify??
	   (append end_red (list 0))))))
   ) ;; loop
 ))))

; apply a rule with different top
; returns true iff one rule has successfully be apply

; op rew$!apply_rules_with_different_top_on_top: Term Rule_Set -> ~Term Bool
(defun rew$!apply_rules_with_different_top_on_top (term rule_s)
  (block the_end
	 (dolist (rule rule_s)
		 (when (rule$!apply_rule rule term)
		       (return-from the_end t))
		 )
	 (return-from the_end nil)
	 )
  )

;@@@ use in simplifying conditions and rhs in id_completions
; this should only be used when the rules are unconditional
; op rew$!simplify_on_top : Term -> ~Term
(defun rew$!simplify_on_top (term)
  (if (term$is_var term) term
    (rew$!apply_rules_with_different_top_on_top
     term
     (module$rules_with_different_top
      obj$current_module
      (term$head term))))
  )
