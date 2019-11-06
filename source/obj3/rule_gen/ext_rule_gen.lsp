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

;; $Id: ext_rule_gen.lsp,v 205.2.1.1 2003/09/23 13:46:26 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  The rule generation of OBJ3 allowing extending without copy
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Tim Winkler ;;;; Heavily rewritten Jan 1992
;;;; Claude Kirchner ;;;; Created: *ck20may87*

;; Z ::: no notion of general variable implemented here.

;; Since the top operator of a rule is not the same as the operator to which
;; the rule is associated, we need to store (operator, rule) insteed of the
;; rule only. Moreover we also need the module in which it appears.
;; We call the structure (module (operator. rule)) a disambiguated 
;; rule or drule. The structure (operator.rule) is called a specific_rule.

;; op rule_gen$$strip_operator_drule : List[3tuple(Module Operator Rule)] -> 
;;                                     List[2tuple(Module Rule)]
(defun rule_gen$$strip_operator_drule (DRS)
  (let ((result nil))
    (dolist (el DRS)
	    (push (cons (car el)(cddr el)) result))
    result)
  )

;; op rule_gen$$strip_module_drule : List[3tuple(Module Operator Rule)] -> 
;;                                   List[2tuple(Operator Rule)
(defun rule_gen$$strip_module_drule (DRS)
  (let ((result nil))
    (dolist (el DRS)
	    (push (cdr el) result))
    result)
  )


;   op rule-generation : Module ObjSpace -> ObjSpace .
;                            ** Installs the set of equations of a 
;                            ** module by associating each of them to 
;                            ** an operator and a module
;                            ** it uses a signature that is determined by the
;                            ** module in question and modules protedted, 
;                            ** used or extended by it.
(defun rule_gen$rule_generation (MOD)
; (when (eq 'object (module$kind MOD)) ;always do rule ge
   (unless (module$is_compiled MOD)
     (rule_gen$$!install_module_equations 
      MOD (rule_gen$$gather_sub_modules_drules MOD))
     (module$!mark_as_compiled MOD)
     )
;   )
 )

;; op rule_gen$$gather_sub_modules_drules : Module -> DruleSet
(defun rule_gen$$gather_sub_modules_drules (MOD)
  (let ((GATHER nil))
    (dolist (SUBMODULE (module$imported_modules MOD))
	    (unless (module$is_compiled SUBMODULE)
		    (rule_gen$rule_generation SUBMODULE))
	    (setq GATHER (rule_gen$$union_drules
			  (rule_gen$gather_drules SUBMODULE) GATHER))
	    )
    GATHER
    )
  )

;; op rule_gen$gather_drules : Module -> DrulesSet .
(defun rule_gen$gather_drules (MOD)
  (let ((mlsr nil))
    (dolist (sr  
	     (module$specific_equations MOD)
	     )
	    (push (cons MOD sr) mlsr))
    (rule_gen$$union_drules mlsr (rule_gen$$gather_sub_modules_drules MOD))
    )
  )

; op rule_gen$$union_drules : DruleSet DruleSet -> DruleSet
(defun rule_gen$$union_drules (x y)
  (let ((res y))
  (dolist (ex x)
    (unless (rule_gen$$member_drule ex res) (push ex res)))
  res
  ))

; op rule_gen$$member_drule : Drule DruleSet -> Bool
(defun rule_gen$$member_drule (x l)
  (let (;(x1 (car x))
	(x2 (cadr x))
	(x3 (cddr x)))
  (dolist (e l nil)
    (when (and ;(eq x1 (car e)) ;can drop this condition
	       (eq x2 (cadr e))
	       (rule$similar x3 (cddr e)))
      (return t))
  )))

;; op rule_gen$$!install_module_equations : Module DruleSet -> Module~
(defun rule_gen$$!install_module_equations (MOD DRS)
  (module$compute_protected_modules MOD)
  (let* ((STRIP_OP_DRS (rule_gen$$strip_operator_drule DRS))
	 (CURRENT_MOD_RULES nil)
	)
    ;; 1**  installation of the given current rules
    (setq CURRENT_MOD_RULES (rule_gen$$!install_module_rules MOD))
    ;; 2**  installation in the module of the gathered rules
    ;; --a- installation in the rule table of the module:
    ;; TCW removed the following
    ;;(module$$!install_specific_rules MOD (rule_gen$$strip_module_drule DRS))
    ;; --b- specialization of the gathered rules in the current module
    ;;      in case the submodule is extending
;    (rule_gen$$!specialize_gathered_rules_in_current_module MOD STRIP_OP_DRS)
    (rule_gen$$!specialize_gathered_rules_in_current_module MOD STRIP_OP_DRS)
    ;; 3**  specialization of the current rules and gathered rules to the 
    ;;      submodules in case the submodule is extending
; out 1/23/92
;    (rule_gen$$!specialize_to_submodules
;     MOD 
;     (append CURRENT_MOD_RULES STRIP_OP_DRS))
    )
  )

;; op rule_gen$$!install_module_rules : Module -> DruleSet

(defun rule_gen$$!install_module_rules (MOD)
  (let ((MODRULESET nil))
    (dolist (rule (module$equations MOD))
	    (push (cons MOD rule) MODRULESET)
	    (rule_gen$$!install_one_equation rule MOD)
	    )
    MODRULESET
    )
  )

(defvar *rule_gen$current_rule* nil)

;; op rule_gen$$!install_one_equation : Rule Module -> Module~

(defun rule_gen$$!install_one_equation (R MOD)
  (setq *rule_gen$current_rule* R)
  (let ((LHSV (term$vars (rule$lhs R))))
  (if (or (term$is_var (rule$lhs R)) ;cf. mod_eval$check_eq
	  (and
	   (not (rule$is_built_in R))
	   (or
	    (not (subsetp (term$vars (rule$rhs R)) LHSV))
	    (not (subsetp (term$vars (rule$condition R)) LHSV)))))
      (progn
	;(princ "LHS of equation is a variable (ignored)") (terpri)
	;(princ "  ") (print$rule_brief r) (terpri)
	(setf (rule$kind R) 'bad_rule) ;@@
	(push R (module$rules MOD)))
  (progn
  ;; 1** install the rule as such
  (module$!add_rule (mod_eval$insert_retract R MOD) MOD)
  ;; and look for associative (-commutative) extensions:
  (rule_gen$$!add_associative_extensions MOD (term$head (rule$lhs R)) R)
  ;; 2** add the potential specialization on subsorts in the same module
  (rule_gen$$specialize R MOD)
  )) ;if
  ))

;  op specialize : RewriteRule Module -> Module .
;  *************** specialize the rule to sorts local to the module.
;  *************** These sorts are always strictly smaller than the sort
;  *************** of the rule
;
;    eq :  specialize(R, MOD)
;          =
;          specialize-on-sorts(R, 
;                    get-strict-lower-coarities(get-top(get-lhs(R)),MOD),
;                    MOD) .
(defun rule_gen$$specialize (R MOD)
  (unless (term$is_var (rule$lhs r))
  (rule_gen$$specialize_for_ops
   R 
   (let ((OP (term$head (rule$lhs R))))
     (let ((val (optable$operator_info
		 (module$operator_table MOD) OP)))
       (if (null val) nil
	 (delete-if #'(lambda (op)
			(member (operator$module op) module$protected_modules))
	   (remove OP (operator_info$lowest val)))
	 )))
   MOD)
  ))

;  op specialize-for-ops : RewriteRule OpSet Module -> Module~ .
;
;     eq : specialize-for-ops(R, OP union OPS, MOD)
;          =
;          specialize-for-ops(R, OPS,
;                              compute-specialization(R, OP, MOD)) .
;
;     eq : specialize-for-ops(R, empty-op-set, MOD)
;          =
;          MOD .
(defun rule_gen$$specialize_for_ops (R OPS MOD)
  (dolist (OP OPS)
    (rule_gen$$compute_specialization R OP MOD))
  (let ((LHS (rule$lhs R)))
    (rule_gen$$!add_associative_extensions MOD (term$head LHS) R))
  MOD
  )

;  op compute-specialization : RewriteRule Operator Module
;                                  -> Module~ .
 
;; Notice that the rule given as argument must not be modified, so the first
;; thing to do is to made a copy of it. Because of the sharing of variable
;; between lhs and rhs, it is difficult to perform this copy later on.

(defun rule_gen$$compute_specialization (R OP MOD)
  (let* ((LHS (rule$lhs R)))
    (when (rule_gen$$check_down MOD OP (term$subterms LHS))
      (module$!add_rule_to_op MOD OP R)
      ;; and look for associative (-commutative) extensions:
      (rule_gen$$!add_associative_extensions MOD OP R)
      )
    )
  )

;; op rule_gen$$!specialize_gathered_rules_in_current_module MOD STRIP_DRS
;;                          Module 2tuple(Module Rule)
(defun rule_gen$$!specialize_gathered_rules_in_current_module (MOD STRIP_DRS)
  (dolist (SDRS STRIP_DRS)
	  (unless (eq (car SDRS) MOD)
		  (rule_gen$$specialize (cdr SDRS) MOD)
		  )
	  )
  )

;; op rule_gen$$!specialize_to_submodules : Module 2tuple(Module Rule)
;(defun rule_gen$$!specialize_to_submodules (MOD STRIP_DRS)
;  (dolist (submod (module$imported_modules MOD))
;	  (when (module$extends MOD submod)
;		(dolist (SDRS STRIP_DRS)
;			(unless (eq (car SDRS) submod)
;				(rule_gen$$specialize (cdr SDRS) submod)
;				)
;			)
;		(rule_gen$$!specialize_to_submodules submod STRIP_DRS)
;		)
;	  )
;  )

;; op rule_gen$$!add_associative_extensions : Module Operator Rule -> MOD~
;; look for associative (-commutative) extensions
(defun rule_gen$$!add_associative_extensions (MOD OP R)
  (when (operator$is_associative OP)
    (dolist (op_above (operator$list_associative_operator_above OP MOD))
      (unless (member R (module$all_rules MOD op_above))
	(let ((knd (rule$kind R)))
	(if (operator$is_commutative op_above)
	    (let ((new_var (variable$make_new_var "ac" *obj$sort_Universal*)))
		  ;; built it and add it.
	      (module$!add_rule_to_op MOD op_above
		(rule$!update_labels
		(rule$!update_id_cond
		(rule$!update_kind ;&&&& 23 Sep 87 see also below
		(rule$make_standard_rule
		 (term$make_right_assoc_normal_form op_above
		   (cons new_var
			 (term$list_assoc_subterms
			  (rule$lhs R)
			  (term$head (rule$lhs R))
			  )))
		 (term$make_term op_above (list new_var
		   (substitution$check_built_in (rule$rhs R))))
		 (rule$condition R)
		 )
		(if (eq 'id_theory knd)
		    'id_ext_theory 'AC_rule_theory)
		)
		(rule$id_condition R))
		(rule$labels R)
		))
	      )
	    (let ((new_var (variable$make_new_var "a" *obj$sort_Universal*))
		  (new_var2 (variable$make_new_var "a2" *obj$sort_Universal*)))
	      (module$!add_rule_to_op MOD op_above
		(rule$!update_labels
		(rule$!update_id_cond
		(rule$!update_kind
		(rule$make_standard_rule 
		 (term$make_right_assoc_normal_form op_above  ;25 Nov 87
		   (cons new_var
			 (term$list_assoc_subterms
			  (rule$lhs R)
			  (term$head (rule$lhs R))
			  )))
		 (term$make_term op_above (list new_var
		   (substitution$check_built_in (rule$rhs R))))
		 (rule$condition R)
		 )
		(if (eq 'id_theory knd)
		    'id_ext_theory 'A_left_rule_theory)
		)
		(rule$id_condition R))
		(rule$labels R)
		))
	      (module$!add_rule_to_op MOD op_above
		(rule$!update_labels
		(rule$!update_id_cond
		(rule$!update_kind
		(rule$make_standard_rule 
		 (term$make_right_assoc_normal_form op_above  ;25 Nov 87
		   (append
			 (term$list_assoc_subterms
			  (rule$lhs R)
			  (term$head (rule$lhs R))
			  )
			 (list new_var)
			 ))
		 (term$make_term op_above (list
		   (substitution$check_built_in (rule$rhs R)) new_var))
		 (rule$condition R)
		 )
		(if (eq 'id_theory knd)
		    'id_ext_theory 'A_right_rule_theory)
		)
		(rule$id_condition R))
		(rule$labels R)
		))
	      (module$!add_rule_to_op MOD op_above
		(rule$!update_labels
		(rule$!update_id_cond
		(rule$!update_kind
		(rule$make_standard_rule 
		 (term$make_right_assoc_normal_form 
		  op_above
		  ;(list new_var2 (rule$lhs R) new_var)
		  (append (list new_var2) ;25 Nov 87
			  (term$list_assoc_subterms
			   (rule$lhs R)
			   (term$head (rule$lhs R)))
			  (list new_var))
		  )
		 (term$make_right_assoc_normal_form
		  op_above (list new_var2
		    (substitution$check_built_in (rule$rhs R)) new_var))
		 (rule$condition R)
		 )
		(if (eq 'id_theory knd)
		    'id_ext_theory 'A_middle_rule_theory)
		)
		(rule$id_condition R))
		(rule$labels R)
		))
		))))))
  MOD
  )

;; May be placed in Module$
;; For the moment realize only the simplification of the rhs. It may be 
;; extended to lhs.

(defun rule_gen$$normalize_rules_in (MOD)
  (declare (ignore mod))
;  (dolist (RULE (module$rules MOD))
;	  (rew$!normalize (rule$rhs RULE))
;	  )
  )

; op rul_gen$$check_down : Module Operator LIST[Terms] -> Bool
; true if (term$make_term OP TERMS) has specialization
(defun rule_gen$$check_down (MOD OP TERMS)
  (let ((obj$current_module MOD))
    (not (eq 'fail
        (rule_gen$$compute_var_info
	 (mapcar #'cons TERMS (operator$arity OP))
	 nil)))))

; uses: obj$current_module
; term-s: is list of (term . sort); the terms and required sorts
; cvi: is the Current Var_Info (only non-destructive creation of this)
(defun rule_gen$$compute_var_info (term_s cvi)
  (if (null term_s)
    cvi
  (let ((term (caar term_s))
	(sort (cdar term_s)))
  (if (term$is_var term)
    (let ((vi (cdr (assoc term cvi))))
    (if vi
      (if (member sort vi)
	(rule_gen$$compute_var_info (cdr term_s) cvi)
	(let ((res (sort_order$max_minorants
		    (module$sort_order obj$current_module) (cons sort vi))))
	  (if res
	    (rule_gen$$compute_var_info
	     (cdr term_s) (cons (cons term res) cvi))
	             ; don't really need to add new if res = [sort set] vi
	    'fail)))
      (let ((vs (variable$initial_sort term)))
	(if (eq vs sort)
	  (rule_gen$$compute_var_info
	   (cdr term_s) (cons (cons term (list sort)) cvi))
	  (let ((res (sort_order$max_minorants
		      (module$sort_order obj$current_module) (list sort vs))))
	    (if res
	      (rule_gen$$compute_var_info
	       (cdr term_s) (cons (cons term res) cvi))
	      'fail))))))
  (if (term$is_built_in_constant_fast term)
    (if (let ((ts (term$sort term)))
	  (or (eq ts sort)
	      (sort_order$is_included_in
	       (module$sort_order obj$current_module)
	       ts sort)))
      (rule_gen$$compute_var_info (cdr term_s) cvi)
      'fail)
  (let ((ops (operator$highest_operators_below
	       obj$current_module (term$head term) sort)))
  (if (null ops)
    'fail
    (dolist (op ops 'fail)
      (let ((res (rule_gen$$compute_var_info
		  (append
		   (mapcar #'cons  (term$subterms term) (operator$arity op))
		   (cdr term_s))
		  cvi)))
	(unless (eq 'fail res)
	  (return res))
      ))))
  )))))
