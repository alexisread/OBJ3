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

;; $Id: module.lsp,v 205.2.1.1 2003/09/23 14:05:26 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                    Module
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Tim Winkler ;;;; Created: 7/7/1986

;;;;;;;;;;;;;;;; Summary ;;;;;;;;;;;;;;;;

; op module$create : Name Parameters -> Module
; op module$name : Module -> Name
; op module$kind : Module -> {object theory}
; op module$parameters : Module -> ParameterList
; op module$sub_modules : Module -> sub_module_information
; op module$sorts : Module -> SET[Sort]
; op module$sort_relation : Module -> RELATION[Sort]
; op module$operators : Module -> SET[Operator]
; op module$variables : Module -> SET[Variable]
; op module$sort_constraints : Module -> SET[Sort_Constraints]
; op module$equations : Module -> SET[Equation]
; op module$sort_order : Module -> Sort_Order
; op module$parse_dictionary : Module -> Dictionary
; op module$juxtaposition : Module -> SET[Operator]
; op module$operator_table : Module -> Operator_Table
; op module$rules : Module -> SET[Rules]
; op module$assertions : Module -> LIST[TUPLE[Module,Mode,SET[Rule]]

; op module$is_compiled : Module -> Bool
; op module$!mark_as_compiled : Module -> Module [side-effect]
; op module$!add_rule : Rule Module -> Module [side-effect]
; op module$extends : Module{1} Module{2} -> Bool
; op module$protects : Module{1} Module{2} -> Bool
; op module$imported_modules : Module -> LIST[Modules]
; op module$!add_sort : Module Sort -> {Module}
; op module$!adjoin_sort : Module Sort -> {Module}
; op module$!add_sort_relationship : Module Sort{1} Sort{2} -> {Module}
; op module$!adjoin_sort_relationship : Module Sort{1} Sort{2} -> {Module}
; op module$!add_operator : Module Operator -> {Module}
; op module$!adjoin_operator : Module Operator -> {Module}
; op module$!transfer_operator : Module Module Operator -> {Module}
; op module$!transfer_operator_info : Module Module Operator -> {Module}
; op module$!add_operator_with_rules : Module Operator Rules -> {Module}
; op module$!add_variable : Module Variable -> {Module}
; op module$!add_sort_constraint : Module Sort_Constraint -> {Module}
; op module$!add_equation : Module Equation -> {Module}
; op module$!replace_parse_dictionary : Module Dictionary -> {Module}
; op module$!replace_juxtaposition : Module SET[Operators] -> {Module}
; op module$!add_juxtaposition : Module Operator -> {Module}
; op module$!init_parse_dictionary : Module -> {Module}
; op module$!add_imported_module : Module{1} Mode Module -> {Module{1}}
; op module$!replace_sort_order : Module Sort-Order -> {Module}
; op module$!add_assertions : Module{1} SET[Rules] Module Mode -> {Module{1}}
; op module$rules_with_same_top : Module Operator -> RingRule
; op module$rules_with_different_top :  Module Operator -> List[Rule]
; op module$specific_rules : Module -> List[2tuple(Operator Rule)]
; op module$$!install_specific_rules : Module List[2tuple(Operator Rule)] 
;                                     -> {Module}
; op module$!add_rule_to_op : Module Operator Rule -> {Module}
; op module$all_rules : Module Operator -> LIST[Rule]
; op module$operator_equational_theory : Module Operator -> Theory
; op module$!operator_update_equational_theory : Module Operator Theory ->
;     {Module}
; op module$operator_rew_strategy : Module Operator -> Strategy
; op module$!operator_update_rewrite_strategy : Module Operator Strategy ->
;     {Module}
; op module$operator_memo : Module Operator -> Bool
; op module$operator_error_strategy : Module Operator  -> Error-Strategy
; op module$operator_is_strictly_overloaded : Module Operator -> Bool
; op module$!operator_mark_strictly_overloaded : Module Operator -> {Module}
; op module$operator_polymorphic : Module Operator -> Function+False
; op module$!operator_make_polymorphic : Module Operator Function -> {Module}

(defstruct (module
	    (:conc-name module$)
	    (:constructor module$make)
	    (:print-function print$mod_name_3))
  name ; module name
  kind ; object -or- theory
  (parameters nil)
  (sub_modules nil) ; module and hierarchymode (assoc-list)
  (sorts nil)
  (sort_relation nil)
  (principal_sort nil)
  (user_principal_sort nil)
  (operators nil)
  (variables nil)
  (sort_constraints nil)
  (equations nil)
  (sort_order nil)
  (parse_dictionary nil)
  (juxtaposition nil)
  (operator_table (optable$make 137))
  (rules nil)
  (assertions nil) ; see rule_gen
  (status nil); property list of status descriptions, e.g. for is_compiled
)

(defun print$mod_name_3 (mod str depth)
  (declare (ignore depth))
  (let ((*standard-output* str))
    (print$mod_name mod)
  ))

;;; first attempts
; imported modules : property list of sub-module to mode

; op module$create : Name Parameters -> Module
; most components are initially empty
(defun module$create (name kind parameters)
  (module$make
    :name name
    :kind kind
    :parameters parameters
  ))

; op module$name : Module -> Name
; op module$parameters : Module -> ParameterList
; op module$sub_modules : Module -> sub_module_information
; op module$sorts : Module -> SET[Sort]
; op module$sort_relation : Module -> RELATION[Sort]
; op module$operators : Module -> SET[Operator]
; op module$variables : Module -> SET[Variable]
; op module$sort_constraints : Module -> SET[Sort_Constraints]
; op module$equations : Module -> SET[Equation]
; op module$sort_order : Module -> Sort_Order
; op moduled$parse_dictionary : Module -> Dictionary
; op module$juxtaposition : Module -> SET[Operator]
; op module$operator_table : Module -> Operator_Table
; op module$rules : Module -> SET[Rule]
; op module$assertions : Module -> LIST[TUPLE[Module,Mode,SET[Rule]]
;; these are directly provided by the defstruct


; op module$is_compiled : Module -> Bool

(defun module$is_compiled (module)
  (getf (module$status module) 'is_compiled))

; op module$needs_compiling : Module -> Bool

(defun module$needs_compiling (module)
  (not (eq t (getf (module$status module) 'is_compiled))))

; op module$!mark_as_compiled : Module -> Module [side-effect]

(defun module$!mark_as_compiled (module)
  (setf (getf (module$status module) 'is_compiled) t))

; op module$!mark_as_needs_compiling : Module -> Module [side-effect]

(defun module$!mark_as_needs_compiling (module)
  (setf (getf (module$status module) 'is_compiled) 'redo))

; op module$is_parse_setup : Module -> Bool

(defun module$is_parse_setup (module)
  (getf (module$status module) 'is_parse_setup))

; op module$!mark_as_parse_setup : Module -> Module [side-effect]

(defun module$!mark_as_parse_setup (module)
  (setf (getf (module$status module) 'is_parse_setup) t))

; op module$!mark_as_needs_parse_setup : Module -> Module [side-effect]

(defun module$!mark_as_needs_parse_setup (module)
  (setf (getf (module$status module) 'is_parse_setup) nil))

; op module$!add_rule : Rule Module -> Module [side-effect]
(defun module$!add_rule (rule module)
  (optable$!add_rule (module$operator_table module)
		     (term$head (rule$lhs rule)) rule)
  (push rule (module$rules module)))

; op module$extends : Module{1} Module{2} -> Bool
; does module1 extend module2 ?
(defun module$extends (module1 module2)
  (equal 'extending (cdr (assoc module2 (module$sub_modules module1)))))

; op module$protects : Module{1} Module{2} -> Bool
; does module1 extend module2 ?
(defun module$protects (module1 module2)
  (equal 'protecting (cdr (assoc module2 (module$sub_modules module1)))))

; op module$imported_modules : Module -> LIST[Modules]
(defun module$imported_modules (module)
  (mapcar #'car (module$sub_modules module)))

; op module$!add_sort : Module Sort -> {Module}
(defun module$!add_sort (module sort)
  (push sort (module$sorts module)))

; op module$!adjoin_sort : Module Sort -> {Module}
(defun module$!adjoin_sort (module sort)
  (unless (member sort (module$sorts module))
    (push sort (module$sorts module))))

; op module$!add_sort_relationship : Module Sort{1} Sort{2} -> {Module}
(defun module$!add_sort_relationship (module sort1 sort2)
  (push (cons sort1 sort2) (module$sort_relation module))
  )

; op module$!update_principal_sort : Module Sort -> {Module}
(defun module$!update_principal_sort (module sort)
  (setf (module$principal_sort module) sort)
  )

; op module$!update_user_principal_sort : Module Sort -> {Module}
(defun module$!update_user_principal_sort (module sort)
  (setf (module$user_principal_sort module) sort)
  (setf (module$principal_sort module) sort)
  )

; op module$!modify_principal_sort : Module Sort -> {Module}
; consider a new sort as a potential principal sort
(defun module$!modify_principal_sort (module sort)
  (unless (module$user_principal_sort module)
    (let ((cur (module$principal_sort module)))
    (if (null cur)
	(module$!update_principal_sort module sort)
      (let  ((curmod (sort$module cur))
	     (sortmod (sort$module sort)))
	(when
	 (and (not (eq 'object (module$kind curmod)))
	      (modexp_eval$is_parameter_theory (module$name curmod))
	      (or (eq 'object (module$kind sortmod))
		  (not (modexp_eval$is_parameter_theory
			(module$name sortmod)))))
	 (module$!update_principal_sort module sort)))))))

; op module$!adjoin_sort_relationship : Module Sort{1} Sort{2} -> {Module}
(defun module$!adjoin_sort_relationship (module sort1 sort2)
  (unless (find-if #'(lambda (x) (and (eq sort1 (car x)) (eq sort2 (cdr x))))
		   (module$sort_relation module))
    (push (cons sort1 sort2) (module$sort_relation module)))
  )

; op module$!add_operator : Module Operator -> {Module}
(defun module$!add_operator (module operator)
  (push operator (module$operators module))
  )

; op module$!adjoin_operator : Module Operator -> {Module}
(defun module$!adjoin_operator (module operator)
  (unless (member operator (module$operators module))
    (push operator (module$operators module)))
  )

; op module$!transfer_operator : Module Module Operator -> {Module}
(defun module$!transfer_operator (module from_module operator)
  (unless (member operator (module$operators module))
    (push operator (module$operators module)))
  (module$!transfer_operator_info module from_module operator)
  )

; op module$!transfer_operator_info : Module Module Operator -> {Module}
(defun module$!transfer_operator_info (module from_module operator)
  (let* ((optab (module$operator_table module))
	 (from_optab (module$operator_table from_module))
	 (allrules (if optab
		      (if (null (module$rules_with_same_top
				 module operator))
			(module$rules_with_different_top module operator)
			(module$all_rules module operator)))))
    (when (null optab)
      (setq optab (operator_table$!create module))
      (setf (module$operator_table module) optab))
    (when (null from_optab)
      (break "module$!transfer_operator_info: source is null"))
    (optable$install_operator optab operator)
    (let ((from_op_info (optable$operator_info from_optab operator))
	  (op_info (optable$operator_info optab operator)))
      (when from_op_info
      (setf (operator_info$equational_theory op_info)
	    (operator_info$equational_theory from_op_info))
      (setf (operator_info$rew_strategy op_info)
	    (operator_info$rew_strategy from_op_info))
      (setf (operator_info$user_rew_strategy op_info)
	    (operator_info$user_rew_strategy from_op_info))
      (setf (operator_info$memo op_info)
	    (operator_info$memo from_op_info))
      (setf (operator_info$error_strategy op_info)
	    (operator_info$error_strategy from_op_info))
      (setf (operator_info$polymorphic op_info)
	    (operator_info$polymorphic from_op_info))
      (let ((rule_ring (operator_info$rules_with_same_top from_op_info)))
      (do ((rule (rule_ring$initialize rule_ring) (rule_ring$next rule_ring)))
	  ((rule_ring$end_test rule_ring))
	(unless (member rule allrules)
	  (optable$!add_rule optab operator rule))))
      (dolist (r (operator_info$rules_with_different_top from_op_info))
	(unless (member r allrules)
	  (optable$!add_rule optab operator r))))
    )
  ))

; op module$!add_operator_with_rules : Module Operator Rules -> {Module}
(defun module$!add_operator_with_rules (module operator rules)
  (push operator (module$operators module))
  (let ((optab (module$operator_table module)))
    (dolist (r rules) (optable$!add_rule optab operator r)))
  )

; op module$!add_variable : Module Variable -> {Module}
(defun module$!add_variable (module variable)
  (push variable (module$variables module))
  )

; op module$!add_equation : Module Equation -> {Module}
(defun module$!add_equation (module equation)
  (push equation (module$equations module))
  )

; op module$!adjoin_equation : Module Equation -> {Module}
(defun module$!adjoin_equation (module equation)
  (setf (module$equations module)
	(rule$!adjoin_rule equation (module$equations module)))
  )

; op module$!replace_parsing_dictionary : Module Dictionary -> {Module}
(defun module$!replace_parse_dictionary (module dictionary)
  (setf (module$parse_dictionary module) dictionary))

; op module$!replace_juxtaposition : Module SET[Operators] -> {Module}
(defun module$!replace_juxtaposition (module ops)
  (setf (module$juxtaposition module) ops))

; op module$!add_juxtaposition : Module Operator -> {Module}
(defun module$!add_juxtaposition (module operator)
  (pushnew operator (module$juxtaposition module) :test 'eq))

; op module$!init_parse_dictionary : Module -> {Module}
(defun module$!init_parse_dictionary (module)
  (setf (module$parse_dictionary module) (dictionary$make_empty)))

; op module$!add_imported_module : Module{1} Mode Module -> {Module{1}}
(defun module$!add_imported_module (module mode submodule)
  (push (cons submodule mode) (module$sub_modules module)))

; op module$!replace_sort_order : Module Sort-Order -> {Module}
(defun module$!replace_sort_order (module sort_order)
  (setf (module$sort_order module) sort_order))

; op module$!add_sort_constraint : Module Sort_Constraint -> {Module}
(defun module$!add_sort_constraint (module sort_constraint)
  (push sort_constraint (module$sort_constraints module))
  )

; op module$!add_assertions : Module{1} SET[Rules] Module Mode -> {Module{1}}
(defun module$!add_assertions (module1 rules module2 mode)
  (push (list module1 mode rules) (module$assertions module2)))

; op module$all_sub_modules : Module -> SET[Module]
(defun module$all_sub_modules (module)
  (let ((res nil)
	(todo (module$sub_modules module)))
    (loop
     (when (null todo) (return))
     (let ((cur (car todo)))
       (setq todo (cdr todo))
       (unless (assoc (car cur) res)
	 (push cur res)
	 (dolist (x (module$sub_modules (car cur)))
	   (unless (or (assoc (car x) res)
		       (assoc (car x) todo))
	     (push x todo)))))
    )
    res
  ))

; op module$operator_equational_theory : Module Operator -> Theory
(defun module$operator_equational_theory (module operator)
  (operator_info$equational_theory
   (optable$operator_info (module$operator_table module) operator)))

; op module$!operator_update_equational_theory : Module Operator Theory ->
;     {Module}
(defun module$!operator_update_equational_theory (module operator theory)
  (setf
   (operator_info$equational_theory
    (optable$operator_info (module$operator_table module) operator))
   theory)
  )

; op module$operator_rew_strategy : Module Operator -> Strategy
(defun module$operator_rew_strategy (module operator)
  (operator_info$rew_strategy
   (optable$operator_info (module$operator_table module) operator)))

; op module$operator_user_rew_strategy : Module Operator -> Strategy
(defun module$operator_user_rew_strategy (module operator)
  (operator_info$user_rew_strategy
   (optable$operator_info (module$operator_table module) operator)))

; op module$!operator_update_rewrite_strategy : Module Operator Strategy ->
;     {Module}
(defun module$!operator_update_rewrite_strategy (module operator strat)
  (setf
    (operator_info$rew_strategy
     (optable$operator_info (module$operator_table module) operator))
    strat)
  )

; module$!operator_make_bottom_up : Module Operator -> ~Operator
(defun module$!operator_make_bottom_up (module op)
  (module$!operator_update_rewrite_strategy module op
   (util$make_list_1_n_0 (length (operator$arity op)))))

; op module$operator_memo : Module Operator -> Bool
(defun module$operator_memo (module operator)
  (operator_info$memo
   (optable$operator_info (module$operator_table module) operator)))

; op module$operator_error_strategy : Module Operator  -> Error-Strategy
(defun module$operator_error_strategy (module operator)
  (operator_info$error_strategy
   (optable$operator_info (module$operator_table module) operator)))

; op module$operator_is_strictly_overloaded : Module Operator -> Bool
(defun module$operator_is_strictly_overloaded (module operator)
  (operator_info$strictly_overloaded
   (optable$operator_info (module$operator_table module) operator)))

; op module$!operator_mark_strictly_overloaded : Module Operator -> {Module}
(defun module$!operator_mark_strictly_overloaded (module operator)
  (setf (operator_info$strictly_overloaded
	 (optable$operator_info (module$operator_table module) operator))
	t))

; op module$operator_polymorphic : Module Operator -> Function+False
(defun module$operator_polymorphic (module operator)
  (operator_info$polymorphic
   (optable$operator_info (module$operator_table module) operator)))

; op module$!operator_make_polymorphic : Module Operator Function -> {Module}
(defun module$!operator_make_polymorphic (module operator fn)
  (setf (operator_info$polymorphic
	 (optable$operator_info (module$operator_table module) operator))
	fn))
	
; op module$rules_with_same_top : Module Operator -> RingRule
(defun module$rules_with_same_top (module operator)
  (let ((val (optable$operator_info (module$operator_table module) operator)))
  (if val
      (operator_info$rules_with_same_top val)
    (rule_ring$create nil))) ;&&&& 6 Jul 87 could improve?
  )

; op module$rules_with_different_top :  Module Operator -> List[Rule]
(defun module$rules_with_different_top (module operator)
  (let ((val (optable$operator_info (module$operator_table module) operator)))
  (if val
      (operator_info$rules_with_different_top val)
    nil))
  )

; op module$specific_rules : Module -> List[2tuple(Operator Rule)]
(defun module$specific_rules (module)
  (let ((optab (module$operator_table module)))
  (if (null optab) nil
  (let ((res nil))
    (maphash #'(lambda (op op_info)
		 (dolist (r (operator_info$rules_with_different_top op_info))
		   (push (cons op r) res))
		 (let ((ring (operator_info$rules_with_same_top op_info)))
		 (do ((rule (rule_ring$initialize ring)
			    (rule_ring$next ring)))
		     ((rule_ring$end_test ring))
		     (push (cons op rule) res))))
       (module$operator_table module))
    res
  ))))

; op module$$!install_specific_rules : Module List[2tuple(Operator Rule)] 
;                                     -> {Module}
(defun module$$!install_specific_rules (module srules)
  (let ((optab (module$operator_table module)))
  (dolist (i srules)
    (optable$!add_rule optab (car i) (cdr i))
  )))

; op module$!add_rule_to_op : Module Operator Rule -> {Module}
(defun module$!add_rule_to_op (module operator rule)
  (let ((optab (module$operator_table module)))
    (when (null optab) (setq optab (operator_table$!create module)))
    (optable$!add_rule optab operator rule))
  )

;&&&& should use rule_ring$ring2list as in
;(defun operator$rules (op)
;  (append (rule_ring$ring2list (operator$rules_with_same_top op))
;	  (operator$rule_with_different_top op)))

; op module$all_rules : Module Operator -> LIST[Rule]
(defun module$all_rules (module operator)
  (let ((res (module$rules_with_different_top module operator))
	(ring (module$rules_with_same_top module operator)))
    (do ((rule (rule_ring$initialize ring) (rule_ring$next ring)))
	((rule_ring$end_test ring))
	(push rule res))
    res
    ))

; op module$module$has_rules_for : Module Operator -> Bool
(defun module$has_rules_for (module operator)
  (dolist (r (module$equations module) nil)
    (when (let ((val (rule$lhs r)))
	    (and (not (term$is_var val))
		 (operator$is_same_operator
		  operator (term$head val))))
      (return t))))

; op module$specific_equations : Module -> List[2tuple(Operator Rule)]
(defun module$specific_equations (module)
  (let ((res nil))
  (dolist (e (module$equations module))
    (unless (term$is_var (rule$lhs e))
      (push (cons (term$head (rule$lhs e)) e) res)))
  res))

(defun module$clear (mod)
  (setf (module$sorts mod) nil)
  (setf (module$sort_relation mod) nil)
  (setf (module$principal_sort mod) nil)
  (setf (module$user_principal_sort mod) nil)
  (setf (module$operators mod) nil)
  (setf (module$variables mod) nil)
  (setf (module$sort_constraints mod) nil)
  (setf (module$equations mod) nil)
  (setf (module$sort_order mod) nil)
  (setf (module$parse_dictionary mod) nil)
  (setf (module$juxtaposition mod) nil)
  (setf (module$operator_table mod) (optable$make 137))
  (setf (module$rules mod) nil)
  (setf (module$assertions mod) nil)
  (setf (module$status mod) nil)
  )

; The following is used to reduce the cost of testing
; whether a module is directly or indirectly protected
; Note: Saved information (in module$protected_modules)
; is only valid for the last module for which module$compute_protected_modules
; was called.

(defvar module$protected_modules nil)

(defun module$compute_protected_modules (module)
  (setq module$protected_modules nil)
  (module$compute_protected module)
  )

(defun module$compute_protected (module)
  (dolist (mp (module$sub_modules module))
    (if (eq 'protecting (cdr mp))
      (progn
	(pushnew (car mp) module$protected_modules)
	(module$compute_protected (car mp)))
      (if (member (car mp) module$protected_modules)
	(setq module$protected_modules
	    (remove (car mp) module$protected_modules))))
    )
  )
