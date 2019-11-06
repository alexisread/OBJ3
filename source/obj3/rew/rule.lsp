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

;; $Id: rule.lsp,v 205.2.1.1 2003/09/23 13:48:18 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                            Rules
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Claude Kirchner ;;;; Created: June 2 1986

;;;;;;;;;;;;;;;; Summary
; op rule$make_rule: Term Term Term Bool -> Rule
; op rule$make_standard_rule: Term Term Term -> Rule
; op rule$make_bi_rule: term lisp_expression Term -> Rule
; op rule$unparse: Rule -> void (output) [omited]
; op rule$!apply_rule: Rule Term -> Bool -- modify "term"
; op rule$end_reduction: Rule -> List[Int] .
; op rule$is_built_in: Rule -> Bool .
; op rule$rhs: Rule -> Term or lisp_expression
; op rule$lhs: Rule -> Term
; op rule$condition: Rule -> Term .
; op rule$copy: Rule -> Rule .
; op rule$A_extensions: Rule -> List[Rule]
; op rule$AC_extension: Rule -> List[Rule]
; op rule$give_A_extensions: Rule -> List[Rule]
; op rule$give_AC_extension: Rule -> List[Rule]
; op rule$!compute_A_extensions : Rule Operator -> List[Rule]
; op rule$!compute_AC_extension : Rule Operator -> List[Rule]
; op rule$similar : Rule Rule -> Bool
; op rule$!update_kind : Rule Lisp -> Rule [side-effect]
; op rule$member : Rule RuleSet -> Bool
; op rule$!adjoin_rule : Rule RuleSet -> RuleSet

; op $$get_end_reduction: Term -> Set[Int] .

;;;;;;;;;;;;;;;; Representation ;;;;;;;;;;;;;;;;

;; There are two kind of rewrite rules:
;;  1) the standard one like f(x,x) -> x
;;  2) the Built-in equation like: f(x,g(a,y)) -> (lisp expression)
;;     in which the occurrence of the variable "x" and "y" will be binded 
;;     to the subtrees assigned to "x" and "y" as a result of the matching

;; All rules are represented as structure containing the following selectors:

;; lhs : Term
;; rhs : Term or Lisp Expresssion
;; condition : Term
;; bi:  may be nil and it this case "rhs" is a standard obj-term or
;;      bi can be "t" and in this case rhs is a lisp expression which
;;      would be evaluated is "lhs" match and "cond" is true.
;; end_reduction : is a set of occurrences used at reduction time in order
;;           to be sure that the term is in normal form after terminating 
;;           the strategy.
;; AC_extension : the possible AC extension (list of less than 1 element)
;; A_extensions : the possible Associative extensions (list empty or of 3 
;;                elements)

(defstruct (rule
	    (:conc-name rule$))
  lhs
  rhs
  condition
  (id_condition nil)
  (is_built_in nil)
  (end_reduction nil)
  (AC_extension nil)
  (A_extensions nil)
  (kind nil)
  (method nil)
  (labels nil)
  )

;; these are directly provided by the defstruct:  (as of Oct 91)

; returns the condition of a rule
; op rule$condition: rule -> term .

; returns the left hand side of a rule
; op rule$lhs: rule -> term

; returns the right hand side of a rule
; op rule$rhs: rule -> term or lisp_expression

; returns the list of occurrence to be checked after reducing only on top
; op rule$end_reduction: Rule -> List[Int] .

; op rule$AC_extension: Rule -> List[Rule]

; op rule$A_extensions: Rule -> List[Rule]

; returns true iff "rule" is a built-ins rule
; op rule$is_built_in: rule -> Bool .

; returns the kind of a rule (an arbitrary lisp description)
; op rule$kind : rule -> Lisp .

; returns the pattern matching method of a rule
; if nil, use default
; op rule$method : rule -> Lisp .

; returns the names or symbolic labels of a rule
; if nil, none
; op rule$labels : rule -> Lisp .


; returns a standard rule from "lhs", "rhs" cond in this order.
; op rule$make_standard_rule: term term term -> rule

(defun rule$make_standard_rule (lhs rhs cond)
  (make-rule
   :lhs lhs
   :rhs rhs
   :condition cond
   :end_reduction  ($$get_end_reduction rhs)
   :is_built_in nil
   :method (match$choose_rule_method lhs cond nil)
   :labels nil
   )
  )

; returns a built-ins rule
; op rule$make_bi_rule: term lisp_expression term -> rule

(defun  rule$make_bi_rule (lhs rhs cond)
  (make-rule
   :lhs lhs
   :rhs rhs
   :condition cond
   :end_reduction nil
   :is_built_in t
   :method (match$choose_rule_method lhs cond nil)
   :labels nil
   )
  )

; returns a standard rule from "lhs", "rhs" cond in this order with labels.
; op rule$make_standard_rule_labels: term term term -> rule

(defun rule$make_standard_rule_labels (lhs rhs cond)
  (let ((lbls *obj$current_labels*))
  (setq *obj$current_labels* nil)
  (make-rule
   :lhs lhs
   :rhs rhs
   :condition cond
   :end_reduction  ($$get_end_reduction rhs)
   :is_built_in nil
   :method (match$choose_rule_method lhs cond nil)
   :labels lbls
   )
  ))

; returns a standard rule from "lhs", "rhs" cond in this order with
; specified labels.
; op rule$make_standard_rule_labelled: term term term Labels -> rule

(defun rule$make_standard_rule_labelled (lhs rhs cond labels)
  (make-rule
   :lhs lhs
   :rhs rhs
   :condition cond
   :end_reduction  ($$get_end_reduction rhs)
   :is_built_in nil
   :method (match$choose_rule_method lhs cond nil)
   :labels labels
   )
  )

; returns a built-ins rule
; op rule$make_bi_rule_labels: term lisp_expression term -> rule

(defun  rule$make_bi_rule_labels (lhs rhs cond)
  (let ((lbls *obj$current_labels*))
  (setq *obj$current_labels* nil)
  (make-rule
   :lhs lhs
   :rhs rhs
   :condition cond
   :end_reduction nil
   :is_built_in t
   :method (match$choose_rule_method lhs cond nil)
   :labels lbls
   )
  ))

; returns a standard rule from "lhs", "rhs" cond in this order.
; the 4-th argument indicates whether the rule should be built-in
;   if it is true, the rule will be built-in
; op rule$make_rule: Term Term Term Bool -> Rule

(defun rule$make_rule (lhs rhs cond end_red bi)
  ;(list* lhs cond rhs end-red bi)
 (make-rule
   :lhs lhs
   :rhs rhs
   :condition cond
   :end_reduction end_red
   :is_built_in bi
   :method (match$choose_rule_method lhs cond nil)
   :labels nil
   )
  )

; labelled version of the above
; op rule$make_rule_labelled: Term Term Term Bool Labels -> Rule

(defun rule$make_rule_labelled (lhs rhs cond end_red bi labels)
 (make-rule
   :lhs lhs
   :rhs rhs
   :condition cond
   :end_reduction end_red
   :is_built_in bi
   :method (match$choose_rule_method lhs cond nil)
   :labels labels
   )
  )

;; Local function used by rule$make_bi_rule and rule$make_standard_rule.
;; returns the set of occurrences which have to be checked after completing
;; the strategy. 
;; op $$get_end_reduction: Term -> Set[Int] .

(defun $$get_end_reduction (rhs)
  (if (term$is_var rhs) nil
    (let ((rhs_strat (module$operator_rew_strategy
			obj$current_module
			(term$head rhs))))
    (do* (
	  (end_red nil)
	  (ll (term$subterms rhs) (cdr ll))
	  (i 1 (+ i 1))
	  (subterm nil)
	  )
	 ((null ll) (nreverse end_red)) ; want increasing, left-to-right
	 (setq subterm (car ll))
	 (unless (or 
		  (term$is_var subterm)
		  ;(term$is_constant subterm) ;25 Aug 89 should refine
		  (and rhs_strat
		       (not (member i rhs_strat :test #'equal))))
	    (push i end_red)
	    )
	 ) ;; do
    )))

;; returns true iff the rule has been sucessfully apply. Note that
;; in this case "term" is also modified.
;; The associative extensions are automatiquely generated and applied is 
;; needed.
;; op rule$!apply_rule: rule term -> Bool -- modify "term"

(defvar $$rew_blips nil)

(defun rule$!apply_rule (rule term)
  ;; first the rule itself
  (let ((is_applied (rule$$!apply_one_rule rule term)))
    ;; then there may be some extensions
    ; 24 Aug 87 believe there is a problem here when rule is
    ; applied since the top may not be the same as the top of the LHS
    ; of the rule anymore -- so put in unless
    (unless is_applied
    (let ((top (term$head term)))
      (unless
       (let ((val (rule$kind rule))) ;??
	 (and val
	     (not (eq 'id_theory val))
	     (not (eq 'idem_theory val))))
      (when (operator$is_associative_fast top)
	    (if (operator$is_commutative top)
	      (setq is_applied
		    (or (rule$$!apply_AC_extension rule term top)
			is_applied))
	      ;; else the operator is only associative,
	      (setq is_applied
		    (or (rule$$!apply_A_extensions rule term top)
			is_applied))
		   )
	   ; (when is_applied
		  ;; since the change is no more local  all the 
		  ;; sort info has to be checked *ck19may87*
		;  (term$!normalize_for_morphism_rules_assoc_term term))
	    ))))
    (when $$rew_blips (print$check) (prin1 (if is_applied '! `-)))
    is_applied
    )
  )

(defvar *rule$use_meth* t)

;; returns true iff the rule has been sucessfully apply. Note that
;; in this case "term" is also modified.
;; op rule$!apply_one_rule: rule term -> Bool -- modify "term"

(defvar self nil)
(defvar $$cond nil)
(defvar $$matches nil)
(defvar $$trials 1)
;was "***********************************"
(defvar $$trace_mark "***********")

(defun rule$$!apply_one_rule (rule term)
  (block the_end
    (let ((condition nil)
	  (meth1 'match$first_match)
	  (methn 'match$next_match)
	  (meth_flag nil)
	  cur_trial
	  (self term))
      (let ((meth_info (rule$method rule)))
      (when (and *rule$use_meth* meth_info)
	(setq meth_flag t)
	(setq meth1 (car meth_info))
	(setq methn (cadr meth_info))))
       (multiple-value-bind
	(global_state subst no_match E_equal)
	(progn
	  (when $$matches (setq $$matches (+ $$matches 1)))
	(if meth_flag
	    (funcall meth1 (rule$lhs rule) term)
	  (match$first_match (rule$lhs rule) term)))
	    (when no_match 
		  (return-from the_end nil))
	    ;; technical assignation related to substitution$image
	    (when E_equal
		  (setq subst nil))
      	    ;; then, the condition must be checked
	    (block try_rule
	    (catch 'rule_failure
	    (when (and
		   (obj_BOOL$is_true (setq condition (rule$condition rule)))
		   (null (rule$id_condition rule)))
		  ;; there is no condition
(when (or $$trace_rewrite (and $$debug (< 10 $$debug_level))) ;&
(princ $$trace_mark) (princ " rule") (terpri) ;&
;(print$print_as 'rule rule) (terpri) ;&
(if *fancy-print* (print$rule_brief rule) (print$rule rule)) (terpri)
(print$print_as 'substitution subst)) ;&
		  (term$!replace-dd
 		    term
		    (if (rule$is_built_in rule)
			(multiple-value-bind
			 (new_term success) (funcall (rule$rhs rule) subst)
			 (if success new_term
(progn
(when (or $$trace_rewrite (and $$debug (< 10 $$debug_level)))
(princ "*** built-in equation failed") (terpri))
			   (return-from try_rule nil)))
			 )
		      ;; else:
		      ;; note that the computation of the substitution
		      ;; made a copy of the rhs.
		      (substitution$image_simplifying
                        subst (rule$rhs rule))))
		  (return-from the_end t)
		  );; when
	    );; catch rule_failure
	    );; block try_rule
	    ;; if the condition is not trivial, we enter in a loop
	    ;; where one try to find a match such that the condition 
	    ;; is satisfied
            (loop
	      (when no_match
(when (or $$trace_rewrite (and $$debug (< 10 $$debug_level))) ;&
(princ $$trace_mark) (princ " exhausted (#") (prin1 cur_trial) (princ ")")
  (terpri)) ;&
                (return)) ;; exit from loop
(when (or $$trace_rewrite (and $$debug (< 10 $$debug_level))) ;&
(setq cur_trial $$trials)
(incf $$trials)
(princ $$trace_mark) (princ " trial #")
  (prin1 cur_trial)
  (terpri) ;&
;(print$print_as 'rule rule) (terpri) ;&
(if *fancy-print* (print$rule_brief rule) (print$rule rule)) (terpri)
(print$print_as 'substitution subst)) ;&
	      (block try_rule
              (catch 'rule_failure
	      (if (and
		   (or (null (rule$id_condition rule))
		       (rule$eval_id_condition subst
		           (rule$id_condition rule)))
		   (obj_BOOL$is_true
		    (let (($$cond (substitution$image subst condition)))
			; no simplyfing since probably wouldn't pay
		      (rew$!normalize_term $$cond))))
		   ;; the condition is satisfied
		(progn
(when (or $$trace_rewrite (and $$debug (< 10 $$debug_level)))
  (princ $$trace_mark) (princ " success #")
  (prin1 cur_trial)
  (princ " ***")
  (terpri));&
		    (term$!replace-dd
		     term
		     (if (rule$is_built_in rule)
			  (multiple-value-bind
			   (new_term success) (funcall (rule$rhs rule) subst)
			   (if success new_term
			     (return-from try_rule nil))
			  )
		       (substitution$image_simplifying
			subst (rule$rhs rule))
		       ) ;; if
		     ) ;; replace
		     (return-from the_end t)
                )
; might omit following
(when (or $$trace_rewrite (and $$debug (< 10 $$debug_level)))
  (princ $$trace_mark) (princ " failure #")
  (prin1 cur_trial)
  (terpri));&
                );; if
              );; catch rule_failure
	      );; block try_rule
	      ;; else, try another ...
	      (multiple-value-setq
                (global_state subst no_match)
                (progn
                  (when $$matches (setq $$matches (+ $$matches 1)))
                (if meth_flag
	            (funcall methn global_state)
                  (match$next_match global_state)))
                )
	      );; loop
          ;; In this case there is no match at all and the rule does not apply
	  (return-from the_end nil)
        );; bind
       );; let
      );; block
  )

(defvar copy_conditions nil)

; The simplifying done above is probably undesirable in some cases
; also have removed tracing below
; note: also doesn't insert retracts
(defun rule$$!apply_one_rule_no_simplify (rule term)
  (block the_end
    (let ((condition nil)
	  (meth1 'match$first_match)
	  (methn 'match$next_match)
	  (meth_flag nil)
	  (self term))
      (let ((meth_info (rule$method rule)))
      (when (and *rule$use_meth* meth_info)
	(setq meth_flag t)
	(setq meth1 (car meth_info))
	(setq methn (cadr meth_info))))
       (multiple-value-bind (global_state subst no_match E_equal)
	(if meth_flag (funcall meth1 (rule$lhs rule) term)
	  (match$first_match (rule$lhs rule) term))
	 (when no_match (return-from the_end nil))
	 ;; technical assignation related to substitution$image
	 (when E_equal (setq subst nil))
	 ;; the condition must be checked
	 (block try_rule
	 (catch 'rule_failure
	 (when (obj_BOOL$is_true (setq condition (rule$condition rule)))
	   ;; there is no condition
	   (term$!replace-dd-no-retract term
	    (if (rule$is_built_in rule)
	      (multiple-value-bind
		    (new_term success) (funcall (rule$rhs rule) subst)
		(if success new_term (return-from try_rule nil)))
	      ;; note that the computation of the substitution
	      ;; made a copy of the rhs.
	      (substitution$image subst (rule$rhs rule))))
	   (return-from the_end t))))
	 ;; if the condition is not trivial, we enter in a loop
	 ;; where one try to find a match such that the condition 
	 ;; is satisfied
	 (loop
	  (when no_match (return)) ;; exit from loop
	  (block try_rule
	  (catch 'rule_failure
	    (when (obj_BOOL$is_true
		   (let (($$cond (substitution$image
				  ; would like to avoid recopying the whole
				  ; thing
				  (if copy_conditions
				    (mapcar #'(lambda (x)
						(cons (car x)
						      (copy_term (cdr x))))
				      subst)
				    subst
				    )
				  condition)))
		     (rew$!normalize_term $$cond)))
	      ;; the condition is satisfied
	      (term$!replace-dd-no-retract
	       term
	       (if (rule$is_built_in rule)
		 (multiple-value-bind
		       (new_term success) (funcall (rule$rhs rule) subst)
		   (if success new_term (return-from try_rule nil)))
		 (substitution$image subst (rule$rhs rule))
		 ) ;; if
	       ) ;; replace
	      (return-from the_end t)
	      );; when
	    );; catch rule_failure
	  );; block try_rule
	  ;; else, try another ...
	  (multiple-value-setq (global_state subst no_match)
	    (if meth_flag (funcall methn global_state)
	      (match$next_match global_state)))
	  );; loop
	 ;; In this case there is no match at all and the rule does not apply
	 (return-from the_end nil)
	 );; bind
       );; let
    );; block
  )

; Returns a copy of "rule". The variable occuring in the rule are also
; copied and are shared by the right and left hand side. 
; U: used by "rule_gen$$specialize_on_sorts".
; *ck12may87*: Modified by adding A and AC extensions fields. In the copy 
; there are supposed to be empty.

; op rule$copy: Rule -> Rule .

(defun rule$copy (rule)
  (multiple-value-bind (new_lhs list_new_var)
      (term$copy_and_returns_list_variable 
       (rule$lhs rule))
      (rule$!update_kind
      (make-rule
       :lhs new_lhs
       :condition
         (if (eq *obj_BOOL$true* (rule$condition rule))
	   *obj_BOOL$true*
	   (term$copy_using_variable (rule$condition rule)
				     list_new_var))
       :id_condition
         (if (null (rule$id_condition rule)) nil
         (if (eq *obj_BOOL$true* (rule$id_condition rule))
	   *obj_BOOL$true*
	   (term$copy_id_cond
	    (rule$id_condition rule) list_new_var)))
       :rhs (if (rule$is_built_in rule)
		(rule$rhs rule) ; the case of a built in rule
	      (term$copy_using_variable (rule$rhs rule)
					list_new_var)
	      )
       :end_reduction (copy-list (rule$end_reduction rule))
       :is_built_in (rule$is_built_in rule)
       :method (rule$method rule)
       :labels (rule$labels rule)
       );; make-rule
      (rule$kind rule))
      )
  )

(defun term$copy_id_cond (tm vars)
  (cond
    ((null tm) nil)
    ((and (consp tm) (not (symbolp (car tm))) (term$is_var tm))
     (let ((val (assoc tm vars)))
       (if val (cdr val)
	 (variable$copy tm) ; This should never occur
	 )))
    (t (cons (car tm)
	     (mapcar #'(lambda (stm) (term$copy_id_cond stm vars)) (cdr tm))))
  ))

(defun term$!replace-dd (old new)
  (when *obj$show_stats* (incf *rule_count*))
  (when $$trace_rewrite
      (print$term old)
      (princ " ---> ")
      (print$term new) (terpri))
  (if $$trace_rewrite_whole
    (if $$cond
      (progn
	(princ "cond: ") (print$term $$cond) (terpri)
	(let ((res (term$!replace_safely old new)))
	  (princ " ===> ") (print$term $$cond) (terpri)
	  res))
      (progn
	(princ "term: ") (print$term $$term) (terpri)
	(let ((res (term$!replace_safely old new)))
	  (princ " ===> ") (print$term $$term) (terpri)
	  res)))
    (term$!replace_safely old new))
  )

(defun term$!replace-dd-no-retract (old new)
  (when *obj$show_stats* (incf *rule_count*))
  (when $$trace_rewrite
      (print$term old)
      (princ " ---> ")
      (print$term new) (terpri))
  (if $$trace_rewrite_whole
    (if $$cond
      (progn
	(princ "cond: ") (print$term $$cond) (terpri)
	(let ((res (term$!replace old new)))
	  (princ " ===> ") (print$term $$cond) (terpri)
	  res))
      (progn
	(princ "term: ") (print$term $$term) (terpri)
	(let ((res (term$!replace old new)))
	  (princ " ===> ") (print$term $$term) (terpri)
	  res)))
    (term$!replace old new))
  )

(defun term$!replace_safely (old new)
  (let ((olds (term$sort old)) (news (term$sort new)))
  (if (or (eq olds news)
	  (sort_order$is_included_in
	   (module$sort_order obj$current_module) news olds))
    (term$!replace old new)
    (term$!replace
     old (term$make_term (operator$make_retract news olds) (list new))))
  ))

;; Apply the associative_extensions

(defun rule$$!apply_A_extensions (rule term top)
  (let ((listext (rule$A_extensions rule))
	(a_ext nil)
	(is_applied nil))
    (when (null listext)
      ;; then need to pre-compute the extensions and store then
      (setq listext (rule$!compute_A_extensions rule top)))
    (when (setq a_ext (car listext))
      ;; the first extension exists
      (setq is_applied (rule$$!apply_one_rule a_ext term)))
    (setq listext (cdr listext))
    (when (setq a_ext (car listext))
      ;; the second extension exists
      (when (rule$$!apply_one_rule a_ext term)
	(setq is_applied t)))
    (setq listext (cdr listext))
    (when (setq a_ext (car listext))
      ;; the third extension exists
      (when (rule$$!apply_one_rule a_ext term)
	(setq is_applied t)))
    is_applied
    )
  )

;; Compute the list of associative extensions
;; op rule$!compute_A_extensions : Rule Operator -> List[Rule]

(defun rule$!compute_A_extensions (rule top)
  (let ((knd (rule$kind rule)))
  (if (and knd (not (eq 'id_theory knd)) (not (eq 'idem_theory knd)))
      (setf  (rule$A_extensions rule) '(nil nil nil))
  (let* ((list_subterms (term$list_assoc_subterms (rule$lhs rule) top))
	 (varl nil)
	 (varr nil)
	 (linear_l nil)
	 (linear_r nil)
	 (listext nil)
	 (new_var (variable$make_new_var "A" *obj$sort_Universal*))
	 (new_var2 (variable$make_new_var "A2" *obj$sort_Universal*))
	 )
    ;; first the left associative extension
    (if (setq linear_l
	      (and  (term$is_var (setq varl (car list_subterms)))
		   (eq (car (operator$arity top))
		       (variable$initial_sort varl))
		   nil
		   ;; the test of membership of varl in the rest of the term:
		   (block test
			  (dolist (subterm (cdr list_subterms))
				  (when (member varl (term$vars subterm))
					(return-from test nil))
				  )
			  (return-from test t)
			  ))
	     ) ;; and
	;; then no need to add a left associative extension
	(push nil listext)
        ;; else need an extension
        (push
	 (rule$!update_id_cond (rule$!update_kind
	   (rule$make_standard_rule
	    (term$make_right_assoc_normal_form top
	       (cons new_var
		     (term$list_assoc_subterms
		      (rule$lhs rule)
		      (term$head (rule$lhs rule)))))
	    (term$make_term top (list new_var
	       (substitution$check_built_in (rule$rhs rule))))
	    (rule$condition rule))
	   (if (eq 'id_theory knd) 'id_ext_theory 'Aleft_theory))
	   (rule$id_condition rule))
	 listext)
	)
    ;; now the right associative extension:
    (if (setq linear_r 
	      (and (term$is_var (setq varr (car (last list_subterms))))
		   ;; the test of membership of var in the rest of the term:
		   (eq (cadr (operator$arity top))
		       (variable$initial_sort varr))
		   nil
		   (block test
			  (dolist (subterm (butlast list_subterms))
				  (when (member varr (term$vars subterm))
					(return-from test nil))
				  )
			  (return-from test t)
			  ))
	      ) ;; and
	;; then no need to add a right associative extension
	(push nil listext)
        ;; else need an extension
        (push
	 (rule$!update_id_cond (rule$!update_kind
	  (rule$make_standard_rule 
	       (term$make_right_assoc_normal_form top
		   (append
		    (term$list_assoc_subterms
			  (rule$lhs rule)
			  (term$head (rule$lhs rule)))
		    (list new_var)))
	       (term$make_term top (list
		 (substitution$check_built_in (rule$rhs rule)) new_var))
	       (rule$condition rule)
	       ) (if (eq 'id_theory knd) 'id_ext_theory 'Aright_theory))
	         (rule$id_condition rule))
	      listext)
	)
    ;; now the middle associative extension:
    (if (and linear_l linear_r)
	;; no need to add middle associative ext
	(push nil listext)
        ;; else need an extension
        (push
	 (rule$!update_id_cond
	 (rule$!update_kind (rule$make_standard_rule 
	       (term$make_right_assoc_normal_form 
		top (list new_var2 (rule$lhs rule) new_var))
	       (term$make_right_assoc_normal_form
		top (list new_var2
		      (substitution$check_built_in (rule$rhs rule)) new_var))
	       (rule$condition rule)
	       ) (if (eq 'id_theory knd) 'id_ext_theory 'Amiddle_theory))
	         (rule$id_condition rule))
	      listext)
	)
    ;; storage and value returned
    (setf (rule$A_extensions rule) listext)
    )
  )))

;; Apply the associative_commutative_extension

(defun rule$$!apply_AC_extension (rule term top)
  (let ((listext (rule$AC_extension rule))
	(is_applied nil))
    (when (null listext)
	  ;; then need to pre-compute the extensions and store then
	  (setq listext
		(rule$!compute_AC_extension rule top))
	  )
    (when (car listext)
	  ;; the extension exists
	  (setq is_applied (rule$$!apply_one_rule (car listext) term))
	  )
    is_applied
    )
  )

;; Compute the list of associative commutative extensions
;; op rule$!compute_AC_extension : Rule Operator -> List[Rule]

(defun rule$!compute_AC_extension (rule top)
  (let ((knd (rule$kind rule)))
  (if (and knd (not (eq 'id_theory knd)) (not (eq 'idem_theory knd)))
    (setf (rule$AC_extension rule) '(nil))
  (let* ((list_subterms (term$list_assoc_subterms (rule$lhs rule) top))
	 (varl nil)
	 (varr nil)
	 (listext nil)
	 (new_var (variable$make_new_var "AC" *obj$sort_Universal*))
	 )
    (if (or
	 (and (term$is_var (setq varl (car list_subterms)))
	      (eq (car (operator$arity top))
		  (variable$initial_sort varl))
	      nil
	      ;&&&& 4 Aug 87 need to always consider adding extensions
	      ; since sort of vars introduced are "Universal" ???
	      ;; the test of membership of varl in the rest of the term:
	      (block test
		     (dolist (subterm (cdr list_subterms))
			     (when (member varl (term$vars subterm))
					(return-from test nil))
				  )
		     (return-from test t)))
	 (and (term$is_var (setq varr (car (last list_subterms))))
	      (eq (cadr (operator$arity top))
		  (variable$initial_sort varr))
	      nil
	      ;; the test of membership of var in the rest of the term:
	      (block test
		     (dolist (subterm (butlast list_subterms))
			     (when (member varr (term$vars subterm))
				   (return-from test nil))
			     )
		     (return-from test t)
		     ))
	 ) ;; or
	;; then no need to add an AC extension
	(push nil listext)
        ;; else need an extension
        (push
	 (rule$!update_id_cond
	 (rule$!update_kind (rule$make_standard_rule
	       (term$make_right_assoc_normal_form top
		   (cons new_var
			 (term$list_assoc_subterms
			  (rule$lhs rule)
			  (term$head (rule$lhs rule)))))
	       (term$make_term top (list new_var
	         (substitution$check_built_in (rule$rhs rule))))
	       (rule$condition rule)
	       ) (if (eq 'id_theory knd) 'id_ext_theory 'AC_theory))
	         (rule$id_condition rule))
	      listext)
	)
    ;; storage and returned value
    (setf (rule$AC_extension rule) listext)
    )
  )))

; op rule$give_C_extension: Rule -> List[Rule]

(defun rule$give_AC_extension (rule)
  (let ((listext (rule$AC_extension rule)))
    (when (null listext)
	;; then need to pre-compute the extensions and store then
	(setq listext (rule$!compute_AC_extension 
		        rule (term$head (rule$lhs rule)))))
    listext)
  )

; op rule$give_A_extensions: Rule -> List[Rule]

(defun rule$give_A_extensions (rule)
  (let ((listext (rule$A_extensions rule)))
    (when (null listext)
	;; then need to pre-compute the extensions and store then
	(setq listext (rule$!compute_A_extensions
		        rule (term$head (rule$lhs rule)))))
    listext)
  )

; op rule$similar : Rule Rule -> Bool

(defun rule$similar (r1 r2)
  (and
   (term$congruent (rule$lhs r1) (rule$lhs r2))
   (term$congruent (rule$condition r1) (rule$condition r2))
   (let ((r1bi (rule$is_built_in r1))
	 (r2bi (rule$is_built_in r2)))
     (if (or r1bi r2bi)
	 (if (and r1bi r2bi)
	     (equal (rule$rhs r1) (rule$rhs r2))
	   nil)
       (term$congruent (rule$rhs r1) (rule$rhs r2))))))

;@@@@ eliminate this
; the intent is that the kind be a description how the rule was
; automatically generated
; op rule$!update_kind : Rule Lisp -> Rule [side-effect]
(defun rule$!update_kind (rule knd)
  (setf (rule$kind rule) knd)
  (setf (rule$method rule)
      (match$choose_rule_method
       (rule$lhs rule) (rule$condition rule) knd))
  rule ; to make the result perfectly clear
  )

(defun rule$!update_id_cond (rule cond)
  (setf (rule$id_condition rule) cond)
  rule ; to make the result perfectly clear
  )

; op rule$member : Rule RuleSet -> Bool
(defun rule$member (r l)
  (dolist (e l nil)
    (when (rule$similar r e) (return t))))

; op rule$!adjoin_rule : Rule RuleSet -> RuleSet
; when the rule is added it is added to a returned value

(defun rule$!adjoin_rule (rule rs)
  (do* ((lst rs (cdr lst))
	(r (car lst) (car lst)))
    ((null lst) (cons rule rs))
    (when (rule$similar rule r)
      (let ((module obj$current_module))
      (unless module (break "rule$!adjoin_rule: need current module"))
      (let ((so (module$sort_order module))
	    (newlhs (rule$lhs rule))
	    (oldlhs (rule$lhs r)))
	(when (and
	       (not (term$is_var newlhs))
	       (not (term$is_var oldlhs))
	       (not (eq (term$head newlhs) (term$head oldlhs)))
	       (sort_order$is_included_in so
		 (term$sort oldlhs)
		 (term$sort newlhs)))
	  (rplaca lst rule))
	(return-from rule$!adjoin_rule rs)
	))))
  )

; op rule$rule_occurs : Rule RuleSet -> Bool
; cf. rule$!adjoin_rule

(defun rule$rule_occurs (rule rs)
  (let ((module obj$current_module))
  (unless module (break "rule$rule_occurs: need current module"))
  (let ((so (module$sort_order module))
	(newlhs (rule$lhs rule)))
  (do* ((lst rs (cdr lst))
	(r (car lst) (car lst)))
    ((null lst) nil)
    (when (and
	   (rule$similar rule r)
	   (let ((oldlhs (rule$lhs r)))
	     (and
	      (or (term$is_var newlhs)
		  (and (not (term$is_var oldlhs)) ;very defensive
		       (eq (term$head newlhs) (term$head oldlhs))))
	      (sort_order$is_included_in so
	          (term$sort oldlhs)
		  (term$sort newlhs)))))
      (return t)))
  )))

; op rule$!update_method : Rule Lisp -> Rule [side-effect]
(defun rule$!update_method (rule meth)
  (setf (rule$method rule) meth)
  rule ; to make the result perfectly clear
  )

; op rule$!update_labels : Rule Lisp -> Rule [side-effect]
(defun rule$!update_labels (rule labels)
  (setf (rule$labels rule) labels)
  rule ; to make the result perfectly clear
  )

(defun rule$make_or_list (l)
  (if (null (cdr l)) (car l) (cons 'or l))
  )

(defun rule$make_and_list (l)
  (if (null (cdr l)) (car l) (cons 'and l))
  )

(defun rule$make_eqeqeq (x)
  (list 'equal (car x) (cdr x)))

(defun rule$make_or_cond (x y)
  (if (eq nil y) x
  (if (and (consp y) (eq 'or (car y)))
    (list* 'or x (cdr y))
    (list 'or x y)))
  )

(defun rule$make_and_cond (x y)
  (if (eq 't y) x
  (if (and (consp y) (eq 'and (car y)))
    (list* 'and x (cdr y))
    (list 'and x y)))
  )

; really not not want to use normalize -- perhaps could use normal
; expressions
(defun rule$eval_id_condition (subst cond)
  (cond
    ((eq 'and (car cond))
     (dolist (sc (cdr cond) t)
       (unless (rule$eval_id_condition subst sc) (return nil)))
     )
    ((eq 'not-equal (car cond))
     (not (term$similar
	   (rule$eval_term subst (cadr cond))
	   (rule$eval_term subst (caddr cond))))
     )
    ((eq 'equal (car cond))
     (term$similar
      (rule$eval_term subst (cadr cond))
      (rule$eval_term subst (caddr cond)))
     )
    ((eq 'or (car cond))
     (dolist (sc (cdr cond) nil)
       (when (rule$eval_id_condition subst sc) (return t)))
     )
    ((eq 'not (car cond))
     (not (rule$eval_id_condition subst (cadr cond)))
     )
    ((eq 'xor (car cond)) ;@@ remove?
     (let ((res nil))
     (dolist (sc (cdr cond))
       (setq res (if (rule$eval_id_condition subst sc) (not res) res)))
     res)
     )
    (t (break "rule$eval_id_condition: illegal condition"))
  ))

; take from substitution$image
(defun rule$eval_term (teta term)
  (macrolet ((assoc% (x y)
	       `(let ((lst ,y))
		  (loop
		   (when (null lst) (return nil))
		   (when (eq ,x (caar lst)) (return (car lst)))
		   (setq lst (cdr lst))))))
  (cond
    ((term$is_var term)
     (let ((im (cdr (assoc% term teta))))
       (if im  ;; i.e. im = teta(term)
	 im
	 ; if variable doesn't have a binding, it evaluates to itself
	 term)))
    (t term))))

; keep here?
(defun print$id_condition (x)
  (print$id_cond x 10))

; second parameter is a binding precedence
; lower means enclosing operator has stronger binding precedence
; 10 none
; 8 or   (from IDENTICAL: 59)
; 6 xor  (from IDENTICAL: 56)
; 4 and  (from IDENTICAL: 55)
; 2 not  (from IDENTICAL: 53)
; 0 equal/not-equal (from IDENTICAL: 51)

(defun print$id_cond (x p)
  (print$chk)
  (cond
    ((eq 'and (car x))
     (let ((paren (< p 4)))
       (when paren (princ "("))
       (print$id_cond_list " and " (cdr x) 4)
       (when paren (princ ")"))
     ))
    ((eq 'not-equal (car x))
     (term$print (cadr x)) (princ " =/== ") (term$print (caddr x))
     )
    ((eq 'equal (car x))
     (term$print (cadr x)) (princ " === ") (term$print (caddr x))
     )
    ((eq 'or (car x))
     (let ((paren (< p 8)))
       (when paren (princ "("))
       (print$id_cond_list " or " (cdr x) 8)
       (when paren (princ ")"))
     ))
    ((eq 'xor (car x))
     (let ((paren (< p 6)))
       (when paren (princ "("))
       (print$id_cond_list " xor " (cdr x) 6)
       (when paren (princ ")"))
     ))
    ((eq 'not (car x))
     (let ((paren (< p 2)))
       (when paren (princ "("))
       (princ "not ")
       (print$id_cond (cadr x) 2)
       (when paren (princ ")"))
     ))
    (t (break "print$id_cond illegal condition"))
  ))

(defun print$id_cond_list (tok lst r)
  (let ((flag nil))
  (dolist (c lst)
    (if flag (princ tok) (setq flag t))
    (print$id_cond c r)))
  )

(defun rule$subsumes (r1 r2)
  (let ((lhs1 (rule$lhs r1))
	(lhs2 (rule$lhs r2)))
    (or
     (term$is_var lhs1)
     (and
      (not (term$is_var lhs1))
      (not (term$is_var lhs2))
      (operator$is_same_operator (term$head lhs1) (term$head lhs2))
      (multiple-value-bind  (gs subst no eeq) (match$first_match lhs1 lhs2)
	(declare (ignore gs))
	(and (or eeq (not no))
	     (or (eq *obj_BOOL$true* (rule$condition r1))
		 (let (($$trace_rewrite nil)
		       ($$trace_rewrite_whole nil))
		 (let ((newcond
			(term$!simplify
			 (term$normalize_for_identity_total
			  (substitution$partial_image subst
			      (rule$condition r1))))))
		   (match$matches newcond (rule$condition r2))
  )))))))))

(defun rule$strictly_subsumes (r1 r2)
  (and (rule$subsumes r1 r2)
       (not (rule$subsumes r2 r1))))
