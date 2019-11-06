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

;; $Id: module_basic.lsp,v 206.1 2003/09/26 13:04:02 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                    module basic routines
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Tim Winkler ;;;; Created: 10/2/91
;;;; Derived from the original module_eval.lsp

;;;;;;;;;;;;;;;; Summary ;;;;;;;;;;;;;;;;
; omitted

(defvar *mod_eval$$current_module* nil)
(defvar obj$current_module)
(defvar *obj$include_BOOL* nil)
(defvar *mod_eval$local_vars*)

(defun obj$obj2_term (x)
  (if *obj$obj2_mode*
      (if (equal ":" (car x)) (cdr x) x)
    x)
  )

; op mod_eval$$find_constant : {cur-mod} Name -> Operator
;; this doesn't require setting up for parsing
(defun mod_eval$$find_constant (op_name)
  (let ((ops (module$operators *mod_eval$$current_module*))
	(opname (if (atom op_name) (list op_name) op_name)))
    (car (find-if #'(lambda (op)
		 (equal opname (operator$name op)))
	     ops))
  )
  )

(defun mod_eval$check_eq (rul)
  (when (eq 'object (module$kind obj$current_module))
  (when (term$is_var (rule$lhs rul))
    (princ "Warning: LHS of equation is a variable") (terpri)
    (princ "  ") (print$rule_brief rul) (terpri))
  (when (term$is_built_in_constant (rule$lhs rul))
    (princ "Warning: LHS of equation is a built-in constant") (terpri)
    (princ "  ") (print$rule_brief rul) (terpri))
  (let ((lhsv (term$vars (rule$lhs rul))))
  (unless (subsetp (term$vars (rule$rhs rul)) lhsv)
    (princ "Warning: variables in RHS not subset of those in LHS") (terpri)
    (princ "    ") (print$rule_brief rul) (terpri))
  (unless (subsetp (term$vars (rule$condition rul)) lhsv)
    (princ "Warning: variables in condition not subset of those in LHS")
    (terpri)
    (princ "    ") (print$rule_brief rul) (terpri))
  ))
  rul
  )

; op mod_eval$$!import_module : Module{1} Mode Module -> {Module{1}}
(defun mod_eval$$!import_module (module mode submodule)
  (let ((val (assoc submodule (module$sub_modules module))))
  (if val
      (if (not (eq mode (cdr val)))
	  (progn
	    (princ "Warning: module ")
	    (print$mod_name submodule) (terpri)
	    (princ " imported into ")
	    (print$mod_name module) (terpri)
	    (princ " with the different modes ")
	    (princ (cdr val)) (princ " and ") (princ mode)
	    (princ " the second has been ignored") (terpri)
	    )
	;(nothing)
      )
  (if (modexp_eval$is_error val)
      (progn
	(princ "Undefined module: ")
	(print$simple_princ_open (cdr val))
	(obj3-to-top))
    (progn
      (module$!add_imported_module module mode submodule)
      (mod_eval$$!incorporate_module module mode submodule)
    ))))
  )

; op mod_eval$is_directly_using : Module Module -> Bool
(defun mod_eval$is_directly_using (mod1 mod2)
  (or
   (some #'(lambda (x) (eq mod2 (sort$module x)))
	 (module$sorts mod1))
   (some #'(lambda (x) (eq mod2 (operator$module x)))
	 (module$operators mod1))))

;; op mod_eval$!eval_import_modexp : {cur_mod} ModExp Mode -> {cur_mod} 
(defun mod_eval$!eval_import_modexp (modexp mode)
  (let ((modval (modexp_eval$top_level_eval_whole
		 modexp *mod_eval$$current_module*)))
    (when (modexp_eval$is_error modval)
      (princ "Cannot evaluate module expression ")
      (print$modexp modexp) (terpri)
      (obj3-to-top))
    (when (eq *mod_eval$$current_module* modval)
      (princ "Module cannot incorporate itself (ignored)") (terpri)
      (obj3-to-top))
    (if (or (eq 'using mode)
	    (not (mod_eval$is_directly_using
		  modval *mod_eval$$current_module*)))
      (mod_eval$$!import_module *mod_eval$$current_module* mode modval)
      (let ((newmod (module$create
		     (module$name *mod_eval$$current_module*)
		     (module$kind *mod_eval$$current_module*)
		     (module$parameters *mod_eval$$current_module*)))
	    (modname (module$name *mod_eval$$current_module*))
	    (submod *mod_eval$$current_module*))
	(setq *mod_eval$$current_module* newmod)
	(setq obj$current_module newmod)
	(let ((subname (mod_eval$create_variant_name newmod submod)))
	  (setf (module$name submod) subname)
	  (modexp_eval$!add_canon subname subname)
	  (modexp_eval$!update_name subname submod))
 	(let ((obj$current_module submod))
        (mod_eval$$!process_final submod))
	(modexp_eval$!add_defn modname newmod)
	(module$!add_imported_module newmod 'protecting submod) ;@including?
	(mod_eval$$!incorporate_module newmod 'protecting submod)
	(module$!add_imported_module newmod mode modval)
	(mod_eval$$!incorporate_module newmod mode modval)
	(dolist (par (module$parameters newmod))
	  (module$!add_imported_module newmod 'protecting (cdr par))
	  (mod_eval$$!incorporate_module newmod 'protecting (cdr par)))
      )) ;let / if
  ))

; op mod_eval$create_variant_name : Module -> Module-Name
(defun mod_eval$create_variant_name (parent mod)
  (let ((modname (module$name parent)) (num -1))
    (dolist (sm (module$sub_modules mod))
      (when (eq 'protecting (cdr sm))
      (let ((smnm (module$name (car sm))))
	(when (and (consp smnm)
		   (eq 'name (car smnm))
		   (eq (cadr smnm) modname) ;by construction works
		   (< num (caddr smnm)))
	  (setq num (caddr smnm))))))
    `(name ,modname ,(1+ num))
  ))

;;; conditionally automatically include BOOL
(defun mod_eval$$include_BOOL ()
  (when *obj$include_BOOL*
    (unless (member *obj_BOOL$sort_Bool*
		    (module$sorts *mod_eval$$current_module*))
    (mod_eval$!eval_import_modexp '("BOOL") 'protecting)))
  )

; op mod_eval$$!incorporate_module : Module Module -> {cur_mod}
(defun mod_eval$$!incorporate_module (module mode submodule)
    (when (not (typep submodule 'module))
      (let ((val (modexp_eval$eval_whole submodule)))
	(unless (typep val 'module)
	  (princ "Cannot evaluate module: ") (print$name submodule) (terpri)
	  (obj3-to-top))
	(setq submodule val)))
    (let ((*mod_eval$local_vars* nil))
    (cond
     ((or (eq 'protecting mode) (eq 'extending mode) (eq 'including mode))
      (let ((submodprs (module$principal_sort submodule)))
      (when submodprs
      (module$!modify_principal_sort module submodprs)))
      ;; sorts
      (dolist (s (reverse (module$sorts submodule)))
	(module$!adjoin_sort module s)
	(module$!replace_sort_order module
	  (sort_order$adjoin_sort (module$sort_order module) s))
	)
      (dolist (r (module$sort_relation submodule))
	(module$!adjoin_sort_relationship module (car r) (cdr r)))
      (dolist (o (module$operators submodule))
	(module$!transfer_operator module submodule o))
      (module$!replace_sort_order module ;&sort &&&& is this the right way?
	(sort_order$transitive_closure1 ;&&&& could delay this
	 (sort_order$merge
	  (sort_order$copy (module$sort_order submodule))
	  (module$sort_order module))))
      )
     ((eq 'using mode) ; used to include extending
      (dolist (s (reverse (module$sorts submodule)))
	(let ((new_sort
	       (if (eq submodule (sort$module s))
		   (mod_eval$$recreate_sort module s)
		 s)))
	(module$!adjoin_sort module new_sort)
	(module$!replace_sort_order module
	  (sort_order$adjoin_sort (module$sort_order module) new_sort))))
      (let ((submodprs (module$principal_sort submodule)))
      (when submodprs
      (module$!modify_principal_sort module
	   (mod_eval$$recreate_sort module submodprs))))
      (dolist (r (module$sort_relation submodule))
	(module$!add_sort_relationship module
	  (mod_eval$$find_sort_in module (sort$name (car r))) ;&&&&><
	  (mod_eval$$find_sort_in module (sort$name (cdr r))))
	)
      (dolist (o (reverse (module$operators submodule)))
	(if (eq submodule (operator$module o))
	    (module$!add_operator module
	      (mod_eval$$recreate_operator submodule module o))
	  (module$!transfer_operator module submodule o)))
      (dolist (o (module$operators module))
	(mod_eval$$update_poly_rules module o)
	(mod_eval$$update_theory module o)
	)
      (dolist (e (module$equations submodule))
	(module$!adjoin_equation module (mod_eval$$recreate_rule module e)))
      (module$!replace_sort_order module
	(sort_order$transitive_closure
	 (mod_eval$$recreate_sort_order
	  module
	  (module$sort_order submodule))
	 (module$sort_order module))))
     (t (break "SNARK: mod_eval$$!incorporate_module")))
  ))

(defun mod_eval$!update_sort_order (module)
  (module$!replace_sort_order module ;&sort
    (sort_order$transitive_closure1
     (module$sort_order module))))

(defun mod_eval$$update_theory (mod op)
  (module$!operator_update_equational_theory mod op
    (mod_eval$$recreate_theory mod
      (module$operator_equational_theory mod op)))
  )

(defun mod_eval$$update_poly_rules (module op)
  (let ((rs (module$rules_with_different_top module op)))
  (when (eq 'poly (car rs))
    (let ((val (optable$operator_info
		(module$operator_table module) op)))
      (when val
	  (setf (operator_info$rules_with_different_top val) nil)))
    (dolist (r (cdr rs)) ; drop the 'poly flag
      (module$!add_rule_to_op module op
	  (mod_eval$$recreate_rule module r)))
  )))

; op mod_eval$$find_sort : {current-module} Sort-Name -> Sort
(defun mod_eval$$find_sort (sort_name)
  (let ((val (mod_eval$$find_sort_in *mod_eval$$current_module* sort_name)))
    (if val val
      (progn
	(princ "Warning: sort not defined: ") (princ sort_name) (terpri)
	(sort$create sort_name *mod_eval$$current_module*)
	;add to module?
      ))
  ))

; op mod_eval$$find_sort_in : Module Sort-Name -> Sort
(defun mod_eval$$find_sort_in (module sort_name)
  (when (not (typep module 'module))
    (princ "mod_eval$$find_sort_in non-module") (terpri)
    (print$name sort_name) (princ " in ") (print$name module) (terpri)
    (break "SNARK: mod_eval$$find_sort_in"))
  (let ((res 
	 (find sort_name (module$sorts module)
	       :test #'(lambda (n s) (equal n (sort$name s))))))
  (if res res
    (let ((tst (mod_eval$$check_dotted_sort_name sort_name)))
    (if tst
	(let ((subm 
	      (car (assoc (cdr tst) (module$sub_modules module)
			  :test #'(lambda (x y)
				    (let ((yname (module$name y)))
				    (or (equal x yname)
					(and (consp yname)
					     (equal "::" (cadr yname))
					     (equal x (car yname)))
					)))))))
	  (if subm
	      (mod_eval$$find_sort_in subm (car tst))
	  nil))
      nil)
    ))
  ))

(defun mod_eval$$check_dotted_sort_name (sn)
  (if (stringp sn)
      (let ((pos (position #\. sn :from-end t)))
      (if pos
	  (cons (subseq sn 0 pos) (subseq sn (1+ pos)))
	nil))
  (if (and (consp sn) (null (cdr sn)))
      (mod_eval$$check_dotted_sort_name (car sn))
  (if (typep sn 'sort)
      (mod_eval$$check_dotted_sort_name (sort$name sn))
    nil
    )
  )))

; op mod_eval$$find_sorts : {current-module} LIST[Sort-Name] -> LIST[Sort]
(defun mod_eval$$find_sorts (l)
  (mapcar #'mod_eval$$find_qual_sort l))

; op mod_eval$$recreate_sort : Module Sort -> Sort
;&&&& builtin sort?
(defun mod_eval$$recreate_sort (module sort)
  (let ((sort_name (sort$name sort)))
  (let ((val (mod_eval$$find_sort_in module sort_name))) ;&&&&><
  (if val val
    (let ((newsort (sort$create sort_name module)))
      (when (sort$info sort)
	(sort$!replace_info newsort (sort$info sort))
	(let ((*mod_eval$$current_module* module))
	(sort$!replace_constructor newsort
	    (mod_eval$create_built_in_constructor newsort))))
      newsort
    )))))

; op mod_eval$$recreate_sorts : Module LIST[Sort] -> LIST[Sort]
(defun mod_eval$$recreate_sorts (module sort_list)
  (mapcar #'(lambda (s) (mod_eval$$recreate_sort module s))
	  sort_list)
  )

;;; within a given module the "name" arity and coarity of an operator
;;;   uniquely identify that operator
; op mod_eval$$find_operator_in : Module Name Arity Coarity -> Operator
(defun mod_eval$$find_operator_in (module op_name arity coarity)
  (let ((len (length arity)))
  (let ((res1
	 (find-if
	  #'(lambda (op)
	      (and (equal op_name (operator$name op))
		   (eq coarity (operator$coarity op))
		   (= len (length (operator$arity op)))
		   (every #'eq arity (operator$arity op))))
	  (module$operators module))))
    (if res1 res1
      (if (and (null arity) (sort$is_built_in coarity))
	  (mod_eval$$find_built_in_operator_in module coarity op_name)
	nil)))))

(defun mod_eval$$find_built_in_operator_in (module sort op_name)
  (if (null (cdr op_name))
    (let ((sort_info (sort$info sort)))
      ;try constructor for sort itself
      (let ((opnm (car op_name)))
      (if (funcall (car sort_info) opnm)
	  (term$make_built_in_constant_op sort opnm)
	(let ((so (module$sort_order module))
	      (srt nil))
	  (dolist (x 
		   (sort_order$lower_sorts
		    (module$sort_order module)
		    sort))
	    (let ((si (sort$info x)))
	      (when (and si
			 (or (null srt)
			     (sort_order$is_strictly_included_in so
			         x srt))
			 (funcall (car si) opnm))
		(setq srt x))))
	  (if srt (term$make_built_in_constant_op srt opnm)
	    nil)
	  ))))
    nil))
 
(defun mod_eval$$find_operator_named_in (module op_name)
  (let ((res1
	 (find-if
	  #'(lambda (op)
	      (or (equal op_name (operator$name op))
		  (and (operator$is_standard op)
		       (if (atom op_name)
			   (equal op_name (car (operator$name op)))
			 (and (null (cdr op_name))
			   (equal (car op_name) (car (operator$name op))))))))
	  (module$operators module))))
    (if res1 res1
      ;&&&& >< suppose multiple
      (dolist (srt (module$sorts module) nil)
	(if (sort$is_built_in srt)
	  (let ((res (mod_eval$$find_built_in_operator_in module srt op_name)))
	    (if res (return res)))))
      )
    ))

; cf. function above
(defun mod_eval$$find_all_operators_named_in (module op_name)
  (nconc
    (mapcan
     #'(lambda (op)
	 (if
	   (or (equal op_name (operator$name op))
	       (and (operator$is_standard op)
		    (if (atom op_name)
			(equal op_name (car (operator$name op)))
		      (and (null (cdr op_name))
		        (equal (car op_name) (car (operator$name op)))))))
	   (list op)))
     (module$operators module))
    (mapcan
     #'(lambda (srt)
	 (if (sort$is_built_in srt)
	     (let ((res (mod_eval$$find_built_in_operator_in module
			  srt op_name)))
	       (if res (list res)))))
     (module$sorts module))))

; op mod_eval$$recreate_operator : Module Operator -> Operator
(defun mod_eval$$recreate_operator (oldmodule module operator)
  (let ((op_name (operator$name operator))
	(op_arity (mod_eval$$recreate_sorts module (operator$arity operator)))
	(op_coarity (mod_eval$$recreate_sort
		     module (operator$coarity operator))))
  (let ((val (mod_eval$$find_qual_operator_in
	      module op_name op_arity op_coarity)))
    (if val val
      (let ((obj$current_module oldmodule))
      (operator$!update_intrinsic
      (operator$create op_name op_arity op_coarity module
			 (operator$theory operator)
		       (module$operator_rew_strategy oldmodule operator)
		       (module$operator_user_rew_strategy oldmodule operator)
		       (operator$is_memo operator)
		       (operator$error_strategy operator)
		       (rule_ring$create nil) ; rules wst
		       ; rules wdt:
		       (if (operator$polymorphic operator)
			   (cons 'poly
				 (module$all_rules oldmodule operator))
			    nil)
		       (operator$precedence operator)
		       (mod_eval$$recreate_op_form
			module (operator$form operator))
		       (operator$syntactic_type operator)
		       (operator$is_standard operator)
		       (operator$polymorphic operator)
		       (operator$is_marked_strictly_overloaded operator)
		       )
        (operator$intrinsic operator))))))
        ; intrinsic info is independent of context
  )

(defun mod_eval$$recreate_theory (module thy)
  (theory$create (theory$name thy)
    (let ((val (theory$zero thy)))
    (when val
    (cons (mod_eval$$recreate_term module (car val))
	  (cdr val)))))
  )

; op mod_eval$$recreate_op_form : Module Operator-Form -> Operator-Form
(defun mod_eval$$recreate_op_form (module frm)
  (mapcar #'(lambda (i)
	      (if (eq 'argument (car i))
		  (list* 'argument (cadr i)
			 (mod_eval$$find_sort_in module (sort$name (cddr i))))
		             ;><
		i))
    frm)
  )

; op mod_eval$$find_variable_in : Module Variable-Name -> Variable
(defun mod_eval$$find_variable_in (module variable-name)
  (find variable-name (module$variables module)
	:test #'(lambda (n v) (equal n (variable$name v))))
  )

; op mod_eval$$recreate_variable : Module Variable -> Variable
(defun mod_eval$$recreate_variable (module variable)
  (let ((var_name (variable$name variable)))
  (let ((val (mod_eval$$find_variable_in module var_name)))
  (if val val
    (variable$make_var var_name
      (mod_eval$$find_sort_in ;><
       module
       (sort$name (variable$initial_sort variable)))))))
  )

; op mod_eval$$recreate_rule : Module Equation -> Equation
(defun mod_eval$$recreate_rule (module rule)
  (if (rule$is_built_in rule)
      (rule$!update_labels
      (rule$make_bi_rule
       (mod_eval$$recreate_term Module (rule$lhs rule))
       (mod_eval$$recreate_bi_term module (rule$rhs rule))
       (if (eq *obj_BOOL$true* (rule$condition rule))
	  *obj_BOOL$true*
	 (mod_eval$$recreate_term Module (rule$condition rule))))
       (rule$labels rule))
      (rule$!update_labels
      (rule$!update_kind
      (rule$make_standard_rule
       (mod_eval$$recreate_term Module (rule$lhs rule))
       (mod_eval$$recreate_term Module (rule$rhs rule))
       (if (eq *obj_BOOL$true* (rule$condition rule))
	 *obj_BOOL$true*
	 (mod_eval$$recreate_term Module (rule$condition rule))))
      (rule$kind rule))
      (rule$labels rule))
  ))

(defun mod_eval$$recreate_bi_term (module tm)
  (if (consp tm)
      (cons (mod_eval$$recreate_bi_term module (car tm))
	    (mod_eval$$recreate_bi_term module (cdr tm)))
      (if (typep tm 'sort)
          (mod_eval$$find_sort_in module (sort$name tm))
          ;; kiniry 25 Sept 2003 - Should CLISP use the second variant?
          #-CMU tm
          #+CMU (if (and (typep tm 'function)
                         (function-lambda-expression tm))
                    (make_function
                     (mod_eval$$recreate_bi_term module
                                                 (function-lambda-expression tm)))
                    tm)
          ))
  )

; op mod_eval$$recreate_term : Module Term -> Term
(defun mod_eval$$recreate_term (module term)
(cond
  ((term$is_an_error term) term)
  ((term$is_built_in_constant term) ;13 Nov 87 was omitted
   (let ((ts (operator$coarity (term$head term))))
   (term$make_built_in_constant
    (mod_eval$$find_sort_in module (sort$name ts)) ;><
    (if (eq obj_BUILT-IN$sort_Built-in ts)
	(mod_eval$$recreate_bi_term module (term$built_in_value term))
      (term$built_in_value term)))))
  ((term$is_var term)
   (let ((var_name (variable$name term)))
     (let ((new-sort (mod_eval$$find_sort_in ;><
				 module
				 (sort$name (variable$initial_sort term)))))
       (let ((val2 (find-if #'(lambda (x)
				(and
				 (equal var_name (variable$name x))
				 (eq new-sort (variable$initial_sort x))))
		     *mod_eval$local_vars*)))
	 (if val2 val2
	   (let ((new_var (variable$make_var var_name new-sort)))
	   (push new_var *mod_eval$local_vars*)
	   new_var)))
       ))
   )
  ;; &&&& what about retracts?
  (t (let ((head (term$head term)))
     (let ((new_head (mod_eval$$find_qual_operator_in
		      module (operator$name head)
		      (mod_eval$$recreate_sorts module
			(operator$arity head))
		      (mod_eval$$recreate_sort module
			(operator$coarity head)))))
       (when (null new_head)
	 (when $$debug
	 (princ "### mod_eval$$recreate_term null new_head ###") (terpri)
	 (princ " using ") (print$name head) (terpri))
	 (setq new_head head))
       (term$make_term_check_op new_head
	 (mapcar #'(lambda (tm) (mod_eval$$recreate_term module tm))
		 (term$subterms term)))
     ))
   )
  ))

(defun mod_eval$is_same_theory_but (th1 th2)
  (and
   (eq (theory$contains_associativity th1)
       (theory$contains_associativity th2))
   (eq (theory$contains_commutativity th1)
       (theory$contains_commutativity th2))
;   (eq (theory$contains_idempotency th1)
;       (theory$contains_idempotency th2))
  ))

(defun mod_eval$has_theory_submodule (module)
  (some #'(lambda (x) (eq 'theory (module$kind (car x))))
    (module$all_sub_modules module)))

; op mod_eval$$process_parameter : {cur_mod} Name . Modexp -> Name . Param
(defun mod_eval$$process_parameter (lst)
  (let ((param_name (caar lst))
	(param_mode (cdr lst))
	(param_modexp (cdar lst)))
  (let ((theory (modexp_eval$top_level_eval param_modexp)))
    (when (modexp_eval$is_error theory)
      (princ "Error: unknown theory for parameter") (terpri)
      (print$simple_princ_open param_modexp)
      (obj3-to-top))
  (let ((true_name (list param_name "::" theory)))
  ; Have realized that really need to copy all parameter theories
  ; This will require more work, since will have to reconstruct things
  ;(let ((res (modexp_eval$find_equiv_in_env true_name)))
  (let ((param_mod
	  ;(if res (cdr res)
	    (mod_eval$create_renamed_module theory
	      ;; the name of a parameter theory
	      ;; is based on the parameter name
	      ;; and the theory and that is all
	      true_name)
	   ; )
	  ))
    (modexp_eval$!add_local_defn param_name param_mod)
    (cons (cons param_name param_mod) param_mode)
  )
  ;) find_equiv
  ))))

; op mod_eval$$recreate_sort_order : Module Sort-Order -> Sort-Order
(defun mod_eval$$recreate_sort_order (module sort_order)
  (mapcar #'(lambda (x)
	      (cons (mod_eval$$find_sort_in_keep module ;><
		        (car x)) ;2/13/87 added sort$name below too
		(cons
		 (mod_eval$find_sorts_in_keep module (cadr x)) ;><
		 (mod_eval$find_sorts_in_keep module (cddr x)) ;><
		 )
	      ))
    sort_order)
  )

(defun mod_eval$$find_sort_in_keep (module sort)
  (if (member sort (module$sorts module)) sort
    (mod_eval$$find_sort_in module (sort$name sort))) ;><
  )

(defun mod_eval$find_sorts_in_keep (module sort_list)
  (mapcar #'(lambda (s) (mod_eval$$find_sort_in_keep module s)) sort_list) ;><
  )

;&&&& not used
; op mod_eval$find_sorts_in : Module LIST[Sort] -> LIST[Sort]
(defun mod_eval$find_sorts_in (module sort_list)
  (mapcar #'(lambda (s) (mod_eval$$find_sort_in module s)) sort_list)
  )

; op mod_eval$create_built_in_constructor : Sort -> Operator
(defun mod_eval$create_built_in_constructor (sort)
  (operator$create 
   (list "Constructor:" (sort$name sort))
   nil
   sort
   *mod_eval$$current_module*
   theory$the_empty_theory
   nil nil nil t (rule_ring$create nil) nil
   0 nil
   ))

; op mod_eval$$create_built_in_rhs : Sort Term Lisp Module -> Lisp
(defun mod_eval$$create_built_in_rhs (sort lhs rhs module)
  (declare (ignore module))
  ;; compute vars in left-hand-side and use to create binding context
  ;;   for the rhs (extracting values from terms)
  ;;   -- checking that all values in terms are reduced
  ;; take result and create a term from it
  ;; -- also may use directly given functions
  (if (and (consp rhs) (eq 'function (car rhs)))
      (eval rhs)
      ;; (eval '(function car)) --> #<compiled-function car>
      (let ((vars (term$vars lhs)))
        (let ((params (mapcar #'(lambda (v)
                                  (intern (string-upcase (variable$name v))))
                              vars)))
          (make_function `(lambda (subst)
                            ;; what about the name of the following function
                            (obj$invoke
                             ',(make_function `(lambda ,params ,rhs))
                             ',(reverse vars)
                             subst
                             ',sort)))))))

; op obj$invoke : Function LIST[Variables] Substitution Sort -> Term
; this is not a purely internal function so choice of name is perhaps
;   a little bit unusual
(defun obj$invoke (fn vars subst sort)
  (block invokation
  (let ((vals nil))
    (dolist (v vars)
      (let ((val (substitution$image_by_name subst v)))
	(if (term$is_built_in_constant val)
	  (push (term$built_in_value val) vals)
	    (return-from invokation (values nil nil))
	)))
    (if (sort$is_built_in sort)
	(values (term$make_built_in_constant_with_sort_check
		 sort (apply fn vals)) t)
      (values (obj$coerce_lisp_to sort (apply fn vals)) t))
  )))

(defun obj$coerce_lisp_to (sort val)
  (cond
   ((eq *obj_BOOL$sort_Bool* sort) (obj_BOOL$coerce_to_Bool val))
   (t val)))

; op substitution$image_by_name : Substitution Variable -> Term
(defun substitution$image_by_name (subst var)
  (let ((var_name (variable$name var)))
  (dolist (m subst '((error image)))
    (when (equal var_name (variable$name (car m)))
      (return (cdr m)))
  )))

; op mod_eval$$create_general_built_in_rhs : Sort Term Lisp Module -> Lisp
(defun mod_eval$$create_general_built_in_rhs (sort lhs rhs mod)
  (declare (ignore sort))
  ;; compute vars in left-hand-side and use to create binding context
  ;;   for the rhs using values of variables as-is
  ;;   -- checking that all values in terms are reduced
  ;; take result and create a term from it
  ;; -- also may use directly given functions
  (if (and (consp rhs) (eq 'function (car rhs)))
      (eval rhs)
      ;; (eval '(function car)) --> #<compiled-function car>
      (let ((vars (term$vars lhs)))
        (let ((params (mapcar #'(lambda (v)
                                  (intern (string-upcase (variable$name v))))
                              vars)))
          (make_function `(lambda (subst)
                            (obj$general_invoke
                             ',(make_function
                                `(lambda (module ,@params)
                                   #+CMU module ; dummy use of module
                                   ,rhs))
                             ',(reverse vars)
                             subst
                             ',mod)
                            ))
          )))
  )

; op obj$general_invoke : Function LIST[Variable] Substitution Sort -> Term
; function should fit: function : Module Term ... -> Term
(defun obj$general_invoke (fn vars subst mod)
  (let ((vals nil))
    (dolist (v vars) (push (substitution$image_by_name subst v) vals))
    (catch 'rewrite_failure
      (values (apply fn mod vals) t))
  ))

(defun obj$rewrite_fail ()
  (throw 'rewrite_failure (values nil nil)))

(defun mod_eval$insert_retract (rul mod)
  (if (rule$is_built_in rul) rul
  (let ((lhs (rule$lhs rul))
	(rhs (rule$rhs rul))
	(so (module$sort_order mod)))
    (if (sort_order$is_included_in so (term$sort rhs) (term$sort lhs))
	rul
      (rule$!update_labels
      (rule$!update_kind
      (rule$make_standard_rule
       lhs
       (term$make_term (operator$make_retract (term$sort rhs) (term$sort lhs))
		       (list rhs))
       (rule$condition rul)
      )
      (rule$kind rul))
      (rule$labels rul))
      )
  )))

(defun mod_eval$fix_qualification (module qual)
  (if (and (consp qual) (eq 'qual (car qual)))
    ;must be a sort
    (let ((qualqual (mod_eval$fix_qualification module (caddr qual))))
      (mod_eval$$find_sort_in qualqual (car (cadr qual))))
  (let ((modval (modexp_eval$eval qual)))
    (if (modexp_eval$is_error modval)
      (let ((srt (if (stringp qual)
		   (if module
		     (mod_eval$$find_sort_in module qual)
		     (progn
		       (princ "Unknown qualification: ")
		       (princ qual) (terpri)
		       (obj3-to-top))))))
	(if srt srt
	  nil ; error indicator
	  )
      )
      modval))
  ))

(defun mod_eval$$find_qual_sorts_in (module sort_list)
  (mapcar #'(lambda (s) (mod_eval$$find_qual_sort_in module s))
    sort_list)
  )

;@@ what about (sort (qual x y)) and specn too
(defun mod_eval$$find_qual_sort_in (module sort_name)
  (if (typep sort_name 'sort) sort_name
  (if (and (consp sort_name) 
	   (eq 'qual (car sort_name)))
      (let ((qual (mod_eval$fix_qualification module (caddr sort_name))))
	(if qual
	    (if (typep qual 'module)
		(mod_eval$$find_sort_in qual
		  (let ((nm (cadr sort_name)))
		  (if (and (consp nm) (null (cdr nm)))
		      (car nm)
		    nm)))
	      nil)
	  nil
	))
    (let ((val
	   (mod_eval$$find_sort_in module
	     (if (and (consp sort_name) (null (cdr sort_name)))
		 (car sort_name)
	       sort_name))))
      (if val val
	(mod_eval$$find_qual_sort_in_parsed
	 module
	 (modexp_parse$parse_sort_specn
	  (if (consp sort_name) sort_name (list sort_name))))))
    )
  ))

(defun mod_eval$$find_qual_sort_in_parsed (module sort_name)
  (if (and (consp sort_name) 
	   (eq 'qual (car sort_name)))
      (let ((qual (mod_eval$fix_qualification module (caddr sort_name))))
	(if qual
	    (if (typep qual 'module)
		(mod_eval$$find_sort_in qual
		  (let ((nm (cadr sort_name)))
		  (if (and (consp nm) (null (cdr nm)))
		      (car nm)
		    nm)))
	      nil)
	  nil
	))
    (mod_eval$$find_sort_in module
      (if (and (consp sort_name) (null (cdr sort_name)))
	  (car sort_name)
	sort_name)))
  )

;&&&& not used?
(defun mod_eval$qual_name_of_op (mod op)
  (declare (ignore mod))
  `(qual ,(operator$name op)
	 ,(operator$module op))
  )

;@@ what about (sort (qual x y)) and specn too
(defun mod_eval$$find_qual_operator_in (module op_name arity coarity)
  (if (and (consp op_name)
	   (eq 'qual (car op_name)))
      (let ((qual (mod_eval$fix_qualification module (caddr op_name))))
	(if qual
	    (if (typep qual 'module)
		(mod_eval$$find_operator_in qual (cadr op_name) arity coarity)
	    (if (typep qual 'sort)
		(if (sort_order$is_included_in (module$sort_order module)
		      coarity qual)
		    (mod_eval$$find_operator_in module
		      (cadr op_name) arity coarity)
		  nil)
	      nil
	      ))
	  nil
	  ))
    (mod_eval$$find_operator_in module op_name arity coarity)
    )
  )

(defun mod_eval$$find_qual_operator_named_in (module op_name)
  (if (and (consp op_name)
	   (eq 'qual (car op_name)))
      (let ((qual (mod_eval$fix_qualification module (caddr op_name))))
	(if qual
	    (if (typep qual 'module)
		(mod_eval$find_operator_named_in qual (cadr op_name))
	    (if (typep qual 'sort)
		(mod_eval$$find_operator_named_in_sort module
		  (cadr op_name) qual)
	      nil))
	nil
	))
    (if module
      (mod_eval$find_operator_named_in module op_name)
      nil)
  ))

(defun mod_eval$$find_operator_named_in_sort (module op_name sort)
  (let ((so (module$sort_order module)))
  (let ((res1
	 (find-if
	  #'(lambda (op)
	      (and
	       (sort_order$is_included_in so (operator$coarity op) sort)
	       (or (equal op_name (operator$name op))
		   (and (operator$is_standard op)
			(if (atom op_name)
			    (equal op_name (car (operator$name op)))
			  (and (null (cdr op_name))
			    (equal (car op_name)
				   (car (operator$name op)))))))))
	  (module$operators module))))
    (if res1 res1
      (dolist (srt (module$sorts module) nil)
	(if (and (sort$is_built_in srt)
		 (sort_order$is_included_in so srt sort))
	  (let ((res (mod_eval$$find_built_in_operator_in module
	               srt op_name)))
	    (if res (return res)))))
      )
    )
  ))

(defun mod_eval$find_operator_named_in (mod opn)
  (let ((opnm (if (util$check_enclosing_parens opn) (butlast (cdr opn)) opn)))
    (if (member "->" (member ":" opnm :test #'equal) :test #'equal)
	(mod_eval$find_operator_from_rank mod opnm)
      (mod_eval$$find_operator_named_in mod opn)))
  )

(defun mod_eval$find_all_qual_operators_named_in (module opn) ;@@
  (if (and (consp opn)
	   (eq 'qual (car opn)))
      (let ((qual (mod_eval$fix_qualification module (caddr opn))))
	(if qual
	    (if (typep qual 'module)
		(mod_eval$find_all_qual_operators_named_in qual (cadr opn))
	    (if (typep qual 'sort)
		(mod_eval$$find_all_operators_named_in_sort module
		  (cadr opn) qual)
	      nil))
	nil
	))
  (mod_eval$find_all_operators_named_in module opn)
  ))

(defun mod_eval$$find_all_operators_named_in_sort (module op_name sort)
  (let ((so (module$sort_order module)))
  (append
   (mapcan
    #'(lambda (op)
	(if
	    (and
	     (sort_order$is_included_in so (operator$coarity op) sort)
	     (or (equal op_name (operator$name op))
		 (and (operator$is_standard op)
		      (if (atom op_name)
			  (equal op_name (car (operator$name op)))
			(and (null (cdr op_name))
			  (equal (car op_name) (car (operator$name op))))))))
	    (list op)))
    (module$operators module))
   (mapcan
    #'(lambda (srt)
	(if (and (sort$is_built_in srt)
		 (sort_order$is_included_in so srt sort))
	  (let ((res (mod_eval$$find_built_in_operator_in module
                       srt op_name)))
	    (if res (list res)))))
    (module$sorts module))
   )
  ))

(defun mod_eval$find_all_operators_named_in (mod opn) ;@@
  (let ((opnm (if (util$check_enclosing_parens opn) (butlast (cdr opn)) opn)))
    (if (member "->" (member ":" opnm :test #'equal) :test #'equal)
	(let ((val (mod_eval$find_operator_from_rank mod opnm)))
	  (when val (list val)))
      (mod_eval$$find_all_operators_named_in mod opn)))
  )

(defun mod_eval$find_operator_from_rank (mod opnm) ;@@
  (let* ((pos1 (position ":" opnm :from-end t :test #'equal))
	 (pos2 (position "->" opnm :from-end t :test #'equal))
	 (opname (subseq opnm 0 pos1))
	 (ar (subseq opnm (1+ pos1) pos2))
	 (coar (nth (1+ pos2) opnm)))
    ;&&&& want some consistency checking
  (let ((val
	 (mod_eval$$find_operator_in mod
	   opname
	   (mod_eval$$find_qual_sorts_in mod ar)
	   (mod_eval$$find_qual_sort_in mod coar))))

    (if val val
      nil
      )
  )))

; op mod_eval$$find_qual_sort : {current-module} Sort-Name -> Sort
(defun mod_eval$$find_qual_sort (sort_name)
  (let ((val (mod_eval$$find_qual_sort_in
	      *mod_eval$$current_module* sort_name)))
    (if val val
      (progn
	(princ "Warning: sort not defined: ") (princ sort_name) (terpri)
	(let ((s (sort$create sort_name *mod_eval$$current_module*)))
	(module$!modify_principal_sort *mod_eval$$current_module* s)
	(module$!adjoin_sort *mod_eval$$current_module* s)
	(module$!replace_sort_order *mod_eval$$current_module*
          (sort_order$adjoin_sort
	   (module$sort_order *mod_eval$$current_module*)
	   s))
	s)
      ))
  ))

; op mod_eval$$find_qual_sorts : {current-module} LIST[Sort-Name] -> LIST[Sort]
(defun mod_eval$$find_qual_sorts (l)
  (mapcar #'mod_eval$$find_qual_sort l))

; mod_eval$process_labels : LIST[Token] -> LIST[Token]
; might instead want to group sequences of tokens between ","s together
(defun mod_eval$process_labels (x)
  (let ((val (delete "," x :test #'equal)) (res nil))
    (dolist (l val)
      (if (find #\. l)
	(progn
	  (princ "Warning: Label ")
	  (princ l)
	  (princ " contains a '.' (ignored)") (terpri))
      (if (digit-char-p (char l 0))
	(progn
	  (princ "Warning: Label ")
	  (princ l)
	  (princ " contains an initial digit (ignored)") (terpri))
	(push l res)))
      )
    (nreverse res)
  ))
