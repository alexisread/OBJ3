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

;; $Id: module_eval.lsp,v 206.1 2003/09/26 13:04:05 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                    module evaluation
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; input is assumed to be of the form produced by module_parse
;;;; note: all uses of "find sort" are suspect because of "Elt" vs "Elt.X"

;;;; Tim Winkler ;;;; Created: 7/8/86

;;;;;;;;;;;;;;;; Summary ;;;;;;;;;;;;;;;;
; op mod_eval$eval_definition : definition {Obj_State} -> Module

; var *mod_eval$$current_module* : Module
;   used as implicit parameter in many cases below: as cur_mod
; op mod_eval$$!add_sort : {cur_mod} Module-Item  -> {cur_mod}
; op mod_eval$$!set_principal_sort : {cur_mod} Module-Item  -> {cur_mod}
; op mod_eval$$!add_subsorts : {cur_mod} Module-Item  -> {cur_mod}
; op mod_eval$$!add_op : {cur_mod} Module-Item  Parse-flag ->
;        {cur_mod}
; op mod_eval$$compute_op_parsing_attr : Name Arity Coarity Attribute-Item  ->
;     <Precedence,Form,Standard>
; op mod_eval$$compute_gathering : Decl Bool Name Arity Bool -> LIST[Atoms]
; op mod_eval$build_standard_pattern : Operator -> Name
; op mod_eval$$!add_ops : {cur_mod} Module-Item -> {cur_mod}
; op mod_eval$$!add_var : {cur_mod} Module-Item -> {cur_mod} 
; op mod_eval$$!add_eq : {cur_mod} Module-Item -> {cur_mod} 
; op mod_eval$$!add_as : {cur_mod} Module-Item -> {cur_mod} 
; op mod_eval$$!add_op-as : {cur_mod} Module-Item -> {cur_mod} 
; op mod_eval$$!add_ceq : {cur_mod} Module-Item -> {cur_mod} 
; op mod_eval$$!add_beq : {cur_mod} Module-Item -> {cur_mod} 
; op mod_eval$$!add_bq : {cur_mod} Module-Item -> {cur_mod}
; op mod_eval$$!import_module : Module{1} Mode Module -> {Module{1}}
; op mod_eval$$!add_pr : {cur_mod} Module-Item -> {cur_mod} 
; op mod_eval$$!add_us : {cur_mod} Module-Item -> {cur_mod} 
; op mod_eval$$!add_ex : {cur_mod} Module-Item -> {cur_mod} 
; op mod_eval$$find_sort : {current-module} Sort-Name -> Sort
; op mod_eval$$find_sort_in : Module Sort-Name -> Sort
; op mod_eval$$find_sorts : {cur_mod} LIST[Sort-Name] -> LIST[Sort]
; op mod_eval$$!parse_setup : {cur_mod} -> {cur_mod}
; op mod_eval$$!incorporate_module : Module Mode Module -> {cur_mod}
; op mod_eval$$recreate_sort : Module Sort -> Sort
; op mod_eval$$recreate_sorts : Module LIST[Sort] -> LIST[Sort]
; op mod_eval$$find_operator_in : Module Name Arity Coarity -> Operator
; op mod_eval$$recreate_operator : Module Operator -> Operator
; op mod_eval$$recreate_op_form : Module Operator-Form -> Operator-Form
; op mod_eval$$find_variable_in : Module Variable-Name -> Variable
; op mod_eval$$recreate_variable : Module Variable -> Variable
; op mod_eval$$recreate_term : Module Term -> Term
; op mod_eval$$!process_final : Module Bool -> Module
; op mod_eval$$add_identity_completions : Module -> {Module}
; op mod_eval$$add_extensions : Module -> {Module}
; op mod_eval$$process_parameters : {cur_mod} Module-Item -> {cur_mod}
; op  mod_eval$$compute_equational_theory :
;         Name Arity Coarity Attributes Parse-flag ->
;         Theory Parse-flag
; op mod_eval$$find_constant : {cur-mod} Name -> Operator
; op mod_eval$$compute_rew_strategy : Name Arity Coarity Attributes ->
;         Strategy
; op mod_eval$$compute_memo : Attributes -> Bool
; op mod_eval$$compute_error_strategy : Attributes -> Bool
; op mod_eval$$!add_theory_rules : Module Opeator -> ~Module
; op mod_eval$$recreate_sort_order : Module Sort-Order -> Sort-Order
; op mod_eval$find_sorts_in : Module LIST[Sort] -> LIST[Sort]
; op mod_eval$$!add_sort_constraint : {cur_mod} PreTerm Name PreTerm ->
;        {cur_mod}
; op mod_eval$$!add_built_in_sort : {cur_mod} Module-Item -> {cur_mod}
; op mod_eval$create_built_in_constructor : Sort -> Operator
; op mod_eval$$create_built_in_rhs : Sort Term Lisp Module -> Lisp
; op obj$invoke : Function LIST[Variables] Substitution Sort -> Term
; op substitution$image_by_name : Substitution Variable -> Term
; op mod_eval$$create_general_built_in_rhs : Sort Term Lisp Module -> Lisp
; op obj$general_invoke : Function LIST[Variable] Substitution Sort -> Term

; var *mod_eval$$current_module* : Module
(defvar *mod_eval$$current_module* nil)

(defvar obj$current_module)

(defvar *modexp_eval$local_env* nil)
(defvar *mod_eval$$last_module* nil)
(defvar *obj$obj2_mode* nil)
(defvar *obj$include_BOOL* nil)
(defvar *mod_eval$local_vars*)

; op mod_eval$eval_definition : definition {Obj_State} -> Module
(defun mod_eval$eval_definition (d)
  (let* ((name (nth 1 d))
;@	 (*mod_eval$parse_flag* nil) ; has structure for parsing been prepared?
	 (*modexp_eval$local_env* nil) ;24 May 88
	 mod s args)
    (if (equal "is" (nth 2 d))
	(progn (setq args nil) (setq s (nth 3 d)))
        (progn (setq args (nth 2 d))
	       (setq s (nth 4 d))))
    (when (atom s) (setq s nil))
    (let ((modval (modexp_eval$eval name)))
      (unless (or (modexp_eval$is_error modval)
		  (equal "%" name))
	(if (null (module$parameters modval))
	    (progn
	      (princ "Warning: redefining module ") (princ name) (terpri)
	    )
	  (progn
	    (princ "Warning: redefining parameterized module ")
            (princ name) (terpri)))
	(modexp_eval$delete_module modval)
      ))
    (let ((param_info (mod_eval$$process_parameters args)))
    (setq mod (module$create name
			     (if (member (car d) '("th" "theory")
					 :test #'equal)
				 'theory
			         'object)

			     (mapcar #'car param_info)))
    (module$!replace_sort_order mod (sort_order$new)) ;&sort
    (setq *mod_eval$$current_module* mod)
    (modexp_eval$!add_defn name mod) ;&&&&
    (let ((obj$current_module mod))
    ;; operate on mod by side-effect
    ;; first put in the parameters
    (dolist (par param_info)
      (mod_eval$$!import_module *mod_eval$$current_module* (cdr par)
	(cdr (car par)))) ; par car's are (name . param)
    (dolist (e s)
     (catch 'obj3-error ;14 Jun 88
       (mod_eval$!module_element e)
     ))
    (mod_eval$$!process_final obj$current_module)
    (setq *mod_eval$$last_module* obj$current_module)
    obj$current_module
  ))))

(defun obj3-module-error ()
  (throw 'obj3-error nil))

; op mod_eval$!module_element : {cur_mod} Module-Item  -> {cur_mod}
; key globals: obj$current_module
(defun mod_eval$!module_element (e)
  (macrolet
      ((set_needs_parse ()
           '(module$!mark_as_needs_parse_setup *mod_eval$$current_module*))
       (needs_parse () '(mod_eval$$!parse_setup))
       (set_needs_rule ()
	   '(when (module$is_compiled *mod_eval$$current_module*)
	      (module$!mark_as_needs_compiling *mod_eval$$current_module*)))
       (needs_rule ()
	   '(when (module$needs_compiling *mod_eval$$current_module*)
	      (mod_eval$!compile mod))))
  (let ((tag (car e)))
  (when (and $$debug (<= 20 $$debug_level)) (format t "~a~%" tag))
  (cond
   ((equal "op" tag) (set_needs_parse)
    (mod_eval$$include_BOOL)
    (mod_eval$$!add_op e)
    (set_needs_parse) (set_needs_rule))
   ((equal "let" tag) (set_needs_parse)
    (mod_eval$$include_BOOL)
    (mod_eval$$!do_let e)
    (set_needs_parse) (set_needs_rule))
   ((equal "ops" tag) (set_needs_parse)
    (mod_eval$$include_BOOL)
    (mod_eval$$!add_ops e)
    (set_needs_parse) (set_needs_rule))
   ((equal "eq" tag) (needs_parse)
    (mod_eval$$!add_eq e)
    (set_needs_rule))
   ((or (equal "ceq" tag) (equal "cq" tag)) (needs_parse)
    (mod_eval$$!add_ceq e)
    (set_needs_rule))
   ((or (equal "var" tag) (equal "vars" tag))
    (set_needs_parse)
    (mod_eval$$include_BOOL)
    (mod_eval$$!add_var e))
   ((equal "vars-of" tag)
    (set_needs_parse)
    (mod_eval$$include_BOOL)
    (mod_eval$$!add_vars_of e))
   ((or (equal "sort" tag) (equal "sorts" tag))
    (set_needs_parse)
    (mod_eval$$!add_sort e))
   ((or (equal "psort" tag) (equal "principal-sort" tag))
    (mod_eval$$!set_principal_sort e))
   ((equal "bsort" tag)
    (set_needs_parse)
    (mod_eval$$!add_built_in_sort e))
   ((or (equal "subsorts" tag) (equal "subsort" tag)) ;18 Nov 87
    (set_needs_parse)
    (mod_eval$$include_BOOL)
    (mod_eval$$!add_subsorts e)
    (set_needs_rule))
   ((equal "beq" tag) ;general form
    (needs_parse)
    (mod_eval$$!add_beq e)
    (set_needs_rule))
   ((equal "cbeq" tag) ;general form
    (needs_parse)
    (mod_eval$$!add_cbeq e)
    (set_needs_rule))
   ((equal "bq" tag)
    (needs_parse)
    (mod_eval$$!add_bq e)
    (set_needs_rule))
   ((equal "cbq" tag)
    (needs_parse)
    (mod_eval$$!add_cbq e)
    (set_needs_rule))
   ((or (equal "pr" tag) (equal "protecting" tag))
    (set_needs_parse)
    (mod_eval$$!add_pr e)
    (set_needs_rule))
   ((or (equal "ex" tag) (equal "extending" tag))
    (set_needs_parse)
    (mod_eval$$!add_ex e)
    (set_needs_rule))
   ((or (equal "us" tag) (equal "using" tag))
    (set_needs_parse)
    (mod_eval$$!add_us e)
    (set_needs_rule))
   ((or (equal "inc" tag) (equal "including" tag))
    (set_needs_parse)
    (mod_eval$$!add_inc e)
    (set_needs_rule))
   ((or (equal "---" tag) (equal "***" tag))) ; do nothing
   ((or (equal "--->" tag) (equal "***>" tag))
    (print$simple_princ_open e) (terpri))
   ((equal "as" tag)
    (mod_eval$$include_BOOL)
    (mod_eval$$!add_as e)) ;rule?
   ((equal "op-as" tag)
    (set_needs_parse)
    (mod_eval$$include_BOOL)
    (mod_eval$$!add_op-as e)
    (set_needs_parse) (set_needs_rule))
   ((or (equal "define" tag) (equal "dfn" tag)) ;an abbreviation
    (set_needs_parse)
    (let* ((sortnm (nth 1 e))
	  (modexp (nth 3 e))
	  (modval (modexp_eval$top_level_eval modexp)))
    (when (modexp_eval$is_error modval)
      (princ "Error: in define/is module is not defined: ")
      (princ "name: ") (princ sortnm)
      (princ "; module: ")
      (print$simple_princ_open modexp) (terpri)
      (obj3-to-top))
    (let ((psort (modexp_eval$principal_sort modval)))
	  ;&&&& what if requires qualification?
	  ;(sortref `(qual ,(sort$name psort) ,(sort$module psort)))
    (if (null psort)
      (progn
	(princ "In defined/dfn, module has no principal sort") (terpri)
	(princ "  ") (print$name modval) (terpri)
	(obj3-to-top))
    (mod_eval$$!add_pr
     `("pr"
       ,(append modexp
	       `("*" "(" "sort" ,(sort$name psort) "to" ,sortnm ")"))
       ".")))))
    (set_needs_rule))
   ((equal "ev" tag) ;&&&&
    (eval (cadr e)))
   ((equal "parse" tag)
    (needs_parse)
    (mod_eval$$include_BOOL)
    ;@@@ check for current module?
    (let ((res (parse$parse obj$current_module
		 (obj$obj2_term (cadr e)) *obj$sort_Universal*)))
    (setq $$term res)
    (unless *obj$input_quiet*
      (princ "parse ")
      (print$short_sort_name (term$sort res))
      (princ " : ")
      (let ((*fancy-print* nil))
	(term$print res) (terpri)
	))))
   ((equal "start" tag)
    (misc$start e))
   ((equal "apply" tag)
    (misc$apply e))
   ((equal "[" tag)
    (setq *obj$current_labels* (mod_eval$process_labels (cadr e))))
   (t (format t "module_eval: Unknown form: ~a~%" e))
   )
  )))

; op mod_eval$$!add_sort : {cur_mod} Module-Item  -> {cur_mod}
(defun mod_eval$$!add_sort (e)
  (let ((first t))
  (dolist (i (cadr e))
    (let ((s (sort$create i *mod_eval$$current_module*)))
      (when first
	    (module$!modify_principal_sort *mod_eval$$current_module* s)
	    (setq first nil))
      (module$!adjoin_sort *mod_eval$$current_module* s)
      (module$!replace_sort_order *mod_eval$$current_module*
        (sort_order$adjoin_sort (module$sort_order *mod_eval$$current_module*)
				s))
      ))))

; op mod_eval$$!set_principal_sort : {cur_mod} Module-Item  -> {cur_mod}
(defun mod_eval$$!set_principal_sort (e)
  (if (module$user_principal_sort obj$current_module) (progn
      (princ "Warning: principal sort specified twice") (terpri)
      (princ "declaration for ") (print$simple_princ_open (cadr e))
      (princ " ignored") (terpri)
      (obj3-module-error))
  (let ((srt (mod_eval$$find_qual_sort (cadr e))))
  (if srt
      (module$!update_user_principal_sort obj$current_module srt)
    (progn
      (princ "Warning: sort specified to be principal sort not found") (terpri)
      (princ  "name is ") (print$simple_princ_open (cadr e))
      (princ  ", declaration ignored") (terpri)
      (obj3-module-error))))))

; op mod_eval$$!add_subsorts : {cur_mod} Module-Item  -> {cur_mod}
(defun mod_eval$$!add_subsorts (e)
  (let ((lst nil))
    (dolist (i (cdr (reverse (cdr e))))
      (when (not (equal "<" i))
	(push (mod_eval$$find_qual_sorts i) lst)))
    (loop
     (when (null lst) (return))
     (dolist (x (car lst))
       (dolist (y (cadr lst))
	 (module$!adjoin_sort_relationship *mod_eval$$current_module*
	   x y)
	 (sort_order$!add_user_relation
	  (module$sort_order *mod_eval$$current_module*)
	  x (list y))
	 ))
     (setq lst (cdr lst))
    )
  ))

; op mod_eval$$!add_op : {cur_mod} Module-Item Parse-flag ->
;        {cur_mod} Parse-flag
(defun mod_eval$$!add_op (e)
  (let ((pat
	 (let ((val (nth 1 e)))
	   (if (atom val) (list val)
	   (if (util$check_enclosing_parens val) (butlast (cdr val)) val))))
	(flg (equal "->" (nth 3 e))))
  (when (or (null pat) (equal '(nil) pat))
    (princ "Warning: operator name is empty, declaration ignored") (terpri)
    (obj3-module-error))
  (when (equal '("_") pat)
    (princ "Warning: operator pattern is just _, declaration ignored")
    (terpri)
    (obj3-module-error))
  (let ((arity (if flg nil (nth 3 e)))
	(coarity (nth (if flg 4 5) e))
	(attr (let ((val (nth (if flg 5 6) e))) (if (equal "." val) nil val))))
  (let ((opar (mod_eval$$find_qual_sorts arity))
	(opcoar (mod_eval$$find_qual_sort coarity)))
  (let ((ethy (mod_eval$$compute_equational_theory
			   pat opar opcoar (cadr attr))))
  (when
     (and (member '"_" pat :test #'equal)
	  (theory$contains_associativity ethy)
	  (not
	   (sort_order$is_included_in (module$sort_order obj$current_module)
	     opcoar (cadr opar)))
	  (sort_order$is_included_in (module$sort_order obj$current_module)
	     opcoar (car opar))
	  )
     ;force gathering "gather (E e)"
     (princ "Warning: the operator ")
         (print$simple_princ_open pat) (princ " : ")
	 (print$simple_princ_open arity) (princ " -> ")
	 (print$simple_princ_open coarity) (terpri)
     (princ "has been forced to use a left gathering rule") (terpri)
     (setq attr (list "["
		      (append (cadr attr)
			      '(("gather" "(" ("E" "e") ")")))
		      "]"))
   )
  (let* ((val (mod_eval$$compute_op_parsing_attr pat arity coarity attr))
	 (prec (car val)) (op_form (cadr val)) (is_standard (caddr val)))
  (let ((strat (mod_eval$$compute_rew_strategy pat arity coarity
		 (cadr attr))))
  (let ((op
      (operator$create pat
		       opar
		       opcoar
		       *mod_eval$$current_module*
		       ethy
		       (if (eq 'none strat) nil strat)
		       strat
		       (mod_eval$$compute_memo (cadr attr))
		       (mod_eval$$compute_error_strategy (cadr attr))
		       (rule_ring$create nil)
		       nil
		       prec op_form
		       (operator$syntactic_type_from_name pat)
		       is_standard)))
    (mod_eval$$!update_poly op (cadr attr))
    (push op (module$operators *mod_eval$$current_module*))
    (when (mod_eval$$compute_intrinsic (cadr attr))
      (when (and $$debug (<= 15 $$debug_level)
		 (null (optable$operator_info
			(module$operator_table *mod_eval$$current_module*)
			op)))
	(princ "### null intrinsic information") (terpri))
      (let ((optab (module$operator_table *mod_eval$$current_module*)))
      (setf (operator-intrinsic op)
	    (optable$operator_info optab op))
      (setf (gethash op optab) nil)
      ))
    )))))))
  )

; op mod_eval$$!do_let : {cur_mod} Module-Item Parse-flag -> {cur_mod}
(defun mod_eval$$!do_let (e)
  (unless (module$is_parse_setup *mod_eval$$current_module*)
      (mod_eval$$!parse_setup))
  (let ((flg (equal "=" (nth 2 e))))
  (let ((opnm (list (nth 1 e)))
	(sortnm (if flg nil (cadr (nth 2 e))))
	(toks (nth (if flg 3 4) e))
	(mod (if *mod_eval$$current_module* *mod_eval$$current_module*
                 *mod_eval$$last_module*)))
  (let ((tm 
	 (if (null toks)
	   (if (and $$term (not (eq 'void $$term)))
	     (copy_term $$term)
	     (progn (princ "No current term") (terpri)
	       (term$make_built_in_constant obj_ERR$sort_Err '("term"))))
	   (parse$parse_ground "a let the"
	     mod toks *obj$sort_Universal*))
	  ))
  (let ((srt (if sortnm (mod_eval$$find_sort_in mod sortnm) (term$sort tm)))
	(val (list 0 (list (cons 'token opnm)) t))
	(strat  '(0)))
  (when (null srt)
    (princ "Error: sort in let is unknown: ")
    (princ sortnm) (terpri)
    (obj3-module-error))
  (let ((op (operator$create
	     opnm nil srt mod theory$the_empty_theory
	                      ;(theory$create 'empty_property nil)
	     strat strat
	     nil nil (rule_ring$create nil) nil (car val) (cadr val)
	     (operator$syntactic_type_from_name opnm) (caddr val))))
  (push op (module$operators mod))
  (let ((rul (rule$make_standard_rule_labels
	      (term$make_term op nil) tm *obj_BOOL$true*)))
  (module$!add_equation mod rul)
  )))))))

; op mod_eval$$!add_ops : {cur_mod} Module-Item -> {cur_mod}
(defun mod_eval$$!add_ops (e)
  (mapc #'(lambda (pat)
	    (mod_eval$$!add_op	
	     (list* "op"
		    (if (consp pat) pat (list pat))
		    (cddr e))))
	(util$group_paren_units (cadr e))))

; op mod_eval$$!add_var : {cur_mod} Module-Item -> {cur_mod} 
(defun mod_eval$$!add_var (e)
  (let ((sort (mod_eval$$find_qual_sort (nth 3 e))))
    (dolist (i (cadr e))
      (let ((val (mod_eval$$find_variable_in *mod_eval$$current_module* i)))
	(cond
	 (val
	  (unless (eq sort (variable$initial_sort val))
	    (princ "Warning: variable ") (princ i)
	      (princ " defined with two different sorts") (terpri)
	    (princ "sorts ") (print$name (variable$initial_sort val))
	      (princ " and ") (print$name sort) (princ " (ignored)")
	      (terpri)
	    )
	  )
	 (t
	  (module$!add_variable *mod_eval$$current_module*
				(variable$make_var i sort)))))
    )))

; op mod_eval$$!add_eq : {current_module} Module-Item -> {cur_mod} 
(defun mod_eval$$!add_eq (e)
  (let ((sort *obj$sort_Universal*)
	(lhs (obj$obj2_term (cadr e)))
	(rhs (cadddr e)))
  (let ((parses_lhs (parser$parses *mod_eval$$current_module* lhs sort)))
  (let ((parses_rhs (parser$parses *mod_eval$$current_module* rhs sort)))
		    ;(term$sort parsed_lhs) used to specify
  (let ((res (parser$find_rule_pair *mod_eval$$current_module*
	       parses_lhs parses_rhs)))
    (if (null res)
	(progn
	  (princ "Error: no parse for equation (ignored):") (terpri)
	  (princ "eq ") (print$simple_princ_open lhs) (princ " = ") ;: out
	  (print$simple_princ_open rhs) (princ " .") (terpri)
	  (when (null parses_lhs)
	    (princ "-- No parse for LHS:") (terpri)
	    (parser$diagnose *mod_eval$$current_module* lhs sort))
	  (when (null parses_rhs)
	    (princ "-- No parse for RHS:") (terpri)
	    (parser$diagnose *mod_eval$$current_module* rhs sort))
	  )
      (progn
	(when (eq 'bad-lhs (car res))
	  (princ "Warning: parse of LHS is not completely well-defined")
	  (terpri)
	  (princ "LHS: ") (print$simple_princ_open lhs) (terpri)
	  (setq res (cdr res)))
	(unless (null (cdr res))
	  (princ "Warning: equation is ambiguous: ") (terpri)
	  (princ "eq ") (print$simple_princ_open lhs) (princ " = ") ;: out
	  (print$simple_princ_open rhs) (princ " .") (terpri)
	  (when (not (null (cdr parses_lhs)))
	    (princ "-- More than one parse for the LHS") (terpri))
	  (when (not (null (cdr parses_rhs)))
	    (princ "-- More than one parse for the RHS") (terpri))
	  )
	(let ((parsed_lhs (car (car res)))
	      (parsed_rhs (parse$convert (cadr (car res)))))
	(module$!add_equation *mod_eval$$current_module*
	  (mod_eval$check_eq
	   (rule$make_standard_rule_labels ;&&&& 8/4/86
	     parsed_lhs parsed_rhs *obj_BOOL$true*))))) ;&&&&
      )
  )))))

; op mod_eval$!add_ceq : {cur_mod} Module-Item -> {cur_mod} 
(defun mod_eval$$!add_ceq (e)
  (let ((sort *obj$sort_Universal*)
	(lhs (obj$obj2_term (nth 1 e)))
	(rhs (nth 3 e))
	(cnd (nth 5 e)))
  (let ((parses_lhs (parser$parses *mod_eval$$current_module* lhs sort)))
  (let ((parses_rhs (parser$parses *mod_eval$$current_module* rhs sort))
	(parsed_cnd (parse$parse *mod_eval$$current_module* cnd)))
  (let ((res (parser$find_rule_pair *mod_eval$$current_module*
               parses_lhs parses_rhs)))
    ;&&&& should reconsider handling of condition errors?
    (if (null res)
	(progn
	  (princ "Error: no parse for equation (ignored): ") (terpri)
	  (princ "cq ") (print$simple_princ_open lhs) (princ " = ")
	  (print$simple_princ_open rhs) (princ " if ")
	  (print$simple_princ_open cnd) (princ " .") (terpri)
	  (when (null parses_lhs)
	    (princ "-- No parse for LHS:") (terpri)
	    (parser$diagnose *mod_eval$$current_module* lhs sort))
	  (when (null parses_rhs)
	    (princ "-- No parse for RHS:") (terpri)
	    (parser$diagnose *mod_eval$$current_module* rhs sort))
	  )
      (progn
	(when (eq 'bad-lhs (car res))
	  (princ "Warning: parse of LHS is not completely well-defined")
	  (terpri)
	  (princ "LHS: ") (print$simple_princ_open lhs) (terpri)
	  (setq res (cdr res)))
	(unless (null (cdr res))
	  (princ "Warning: equation is ambiguous: ") (terpri)
	  (princ "cq ") (print$simple_princ_open lhs) (princ " = ")
	  (print$simple_princ_open rhs) (princ " if ")
	  (print$simple_princ_open cnd) (princ " .") (terpri)
	  (when (not (null (cdr parses_lhs)))
	    (princ "-- More than one parse for the LHS") (terpri))
	  (when (not (null (cdr parses_rhs)))
	    (princ "-- More than one parse for the RHS") (terpri)))
	(let ((parsed_lhs (car (car res)))
	      (parsed_rhs (parse$convert (cadr (car res)))))
	(module$!add_equation *mod_eval$$current_module*
	  (mod_eval$check_eq
	   (rule$make_standard_rule_labels ;&&&& 8/4/86
	    parsed_lhs parsed_rhs parsed_cnd)))))
      )
  )))))

; op mod_eval$$!add_beq : {cur_mod} Module-Item -> {cur_mod} 
(defun mod_eval$$!add_beq (e)
  (let ((sort *obj$sort_Universal*)
	(lhs (obj$obj2_term (nth 1 e)))
	(rhs (nth 3 e)))
  (let ((parsed_lhs (parse$parse *mod_eval$$current_module* lhs
		      *obj$sort_Universal*
		      )))
      (setq sort (operator$coarity (term$head parsed_lhs)))
    (module$!add_equation *mod_eval$$current_module*
      (rule$make_bi_rule_labels
        parsed_lhs
	(mod_eval$$create_general_built_in_rhs sort parsed_lhs rhs
					       *mod_eval$$current_module*)
	*obj_BOOL$true*))
  )))

; op mod_eval$$!add_cbeq : {cur_mod} Module-Item -> {cur_mod} 
(defun mod_eval$$!add_cbeq (e)
  (let ((sort *obj$sort_Universal*)
	(lhs (obj$obj2_term (nth 1 e)))
	(rhs (nth 3 e))
	(cnd (nth 5 e)))
  (let ((parsed_lhs (parse$parse *mod_eval$$current_module* lhs
		      (if sort sort *obj$sort_Universal*)))
	(parsed_cnd (parse$parse *mod_eval$$current_module* cnd)))
      (setq sort (operator$coarity (term$head parsed_lhs)))
    (module$!add_equation *mod_eval$$current_module*
      (rule$make_bi_rule_labels
        parsed_lhs
	(mod_eval$$create_general_built_in_rhs sort parsed_lhs rhs
					       *mod_eval$$current_module*)
	  parsed_cnd))
  )))

; op mod_eval$$!add_bq : {cur_mod} Module-Item -> {cur_mod} 
(defun mod_eval$$!add_bq (e)
  (let ((sort *obj$sort_Universal*)
	(lhs (obj$obj2_term (nth 1 e)))
	(rhs (nth 3 e)))
  (let ((parsed_lhs (parse$parse *mod_eval$$current_module* lhs
		      (if sort sort *obj$sort_Universal*))))
    (setq sort (operator$coarity (term$head parsed_lhs)))
    (module$!add_equation *mod_eval$$current_module*
      (rule$make_bi_rule_labels
       parsed_lhs
       (mod_eval$$create_built_in_rhs sort parsed_lhs rhs
				      *mod_eval$$current_module*)
       *obj_BOOL$true*))
    (module$!operator_make_bottom_up *mod_eval$$current_module*
      (term$head parsed_lhs))
  )))

; op mod_eval$$!add_cbq : {cur_mod} Module-Item -> {cur_mod} 
(defun mod_eval$$!add_cbq (e)
  (let ((sort *obj$sort_Universal*)
	(lhs (obj$obj2_term (nth 1 e)))
	(rhs (nth 3 e))
	(cnd (nth 5 e)))
  (let ((parsed_lhs (parse$parse *mod_eval$$current_module* lhs
		      (if sort sort *obj$sort_Universal*)))
	(parsed_cnd (parse$parse *mod_eval$$current_module* cnd)))
    (setq sort (operator$coarity (term$head parsed_lhs)))
    (module$!add_equation *mod_eval$$current_module*
      (rule$make_bi_rule_labels
       parsed_lhs
       (mod_eval$$create_built_in_rhs sort parsed_lhs rhs
				      *mod_eval$$current_module*)
         parsed_cnd))
    (module$!operator_make_bottom_up *mod_eval$$current_module*
      (term$head parsed_lhs))
  )))


; op mod_eval$$!add_pr : {cur_mod} Module-Item -> {cur_mod} 
(defun mod_eval$$!add_pr (e)
  (mod_eval$!eval_import_modexp (cadr e) 'protecting)
  )

; op mod_eval$$!add_inc : {cur_mod} Module-Item -> {cur_mod} 
(defun mod_eval$$!add_inc (e)
  (mod_eval$!eval_import_modexp (cadr e) 'including)
  )

; op mod_eval$$!add_us : {cur_mod} Module-Item -> {cur_mod} 
(defun mod_eval$$!add_us (e)
  ; expansion of modexp_eval$top_level_eval
  (let ((mepars (modexp_parse$parse (cadr e))))
  (if (and (consp mepars) (eq 'with (car mepars)))
      (mod_eval$$!add_using_with *mod_eval$$current_module* mepars)
  (let ((val (modexp_eval$eval mepars *mod_eval$$current_module*)))
    (if (eq *obj_TRUTH$module* val) ;7 Jun 88was (... *obj_BOOL$module* val)
      (progn
	(princ "using TRUTH not allowed, replaced by extending") (terpri)
	(princ "In module ") (print$mod_name *mod_eval$$current_module*)
	(terpri)
	(mod_eval$$!import_module *mod_eval$$current_module* 'extending val))
    (if (eq *mod_eval$$current_module* val)
      (progn
	(princ "Module cannot use itself (ignored)") (terpri))
      (mod_eval$$!import_module *mod_eval$$current_module* 'using val))))
  )))

; op mod_eval$$!add_ex : {cur_mod} Module-Item -> {cur_mod} 
(defun mod_eval$$!add_ex (e)
  (mod_eval$!eval_import_modexp (cadr e) 'extending)
  )

; op mod_eval$$process_parameters : {cur_mod} Module-Item -> {cur_mod}
; parameter X::TH will be named ("X" "::" "TH"), will insert into
; local environment
(defun mod_eval$$process_parameters (a)
  (when a
    (let ((args nil) (lst (cdr a)))
    (loop
       (when (null (cdr lst)) (return))
       (let ((tag (car lst)) (mode 'protecting))
       (when (member tag '("ex" "extending" "us" "using" "pr" "protecting"
			   "inc" "including")
		:test #'equal)
	     (if (or (equal "ex" tag) (equal "extending" tag))
		 (setq mode 'extending)
	     (if (or (equal "us" tag) (equal "using" tag))
		 (setq mode 'using)
	     (if (or (equal "inc" tag) (equal "including" tag))
		 (setq mode 'including))))
	     (setq lst (cdr lst)))
       (dolist (nm (car lst))
	 (push (cons (cons nm (caddr lst)) mode) args))
       (setq lst (cddddr lst))
       ))
    (mapcar #'mod_eval$$process_parameter (nreverse args))
  )))

; op mod_eval$$!add_as : {cur_mod} Module-Item -> {cur_mod} 
(defun mod_eval$$!add_as (e)
  ;(mod_eval$$!add_sort_constraint
  ; (nth 3 e) (nth 1 e) (nth 5 e))
  (unless (module$is_parse_setup *mod_eval$$current_module*)
    (mod_eval$$!parse_setup))
  (let* ((so (module$sort_order *mod_eval$$current_module*))
	 (sort (mod_eval$$find_qual_sort (nth 1 e)))
	 (tm (parser$parses *mod_eval$$current_module* (nth 3 e)
			    (if sort sort *obj$sort_Universal*)))
	 (cnd (parser$parses *mod_eval$$current_module* (nth 5 e))))
    (when (null sort)
      (princ "Unknown sort in sort constraint"))
    (when (null tm)
      (princ "No parse for term in sort constraint") (terpri))
    (when (or (null cnd) (not (null (cdr cnd))))
      (princ "No single parse for condition in sort constraint") (terpri))
    (if (and tm (not (null (cdr tm))))
      (when tm (princ "Term in sort constraint is ambiguous") (terpri))
      (when (and tm (null (cdr tm)))
	(when (and sort tm)
	(unless (sort_order$is_included_in so sort (term$sort (car tm)))
	  (princ "Specified sort and sort of term incompatible")))
	(when (and tm cnd (null (cdr tm)) (null (cdr cnd)))
	(unless (subsetp (term$vars (car cnd)) (term$vars (car tm)))
	  (princ "Condition variables not subset of those in term") (terpri)))
	)
      )
    )
  (princ "Error: general sort constraint not currently handled (ignored)")
  (terpri)
  (princ "    ")
  (princ "as ")
  (print$simple_princ_open (nth 1 e))
  (princ " : ")
  (print$simple_princ_open (nth 3 e))
  (princ " if ")
  (print$simple_princ_open (nth 5 e))
  (princ " .")
  (terpri)
  )

; op mod_eval$$!add_op-as : {cur_mod} Module-Item -> {cur_mod} 
(defun mod_eval$$!add_op-as (e)
  ;(mod_eval$$!add_sort_constraint
  ; (nth 7 e) (nth 5 e) (nth 9 e))
  (princ "Warning: operator assertion being treated simply as a")
  (princ " declaration") (terpri)
  (princ "for operator: ") (print$simple_princ_open (nth 1 e)) (terpri)
  (mod_eval$$!add_op
   `("op" ,(nth 1 e) ":" ,(nth 3 e) "->" ,(nth 5 e)
	  ,@(if (equal "." (nth 10 e)) nil  (list (nth 10 e)))
	  "."))
  )

; op mod_eval$$!add_sort_constraint : {cur_mod} PreTerm Name PreTerm ->
;        {cur_mod}
(defun mod_eval$$!add_sort_constraint (trm srt_name cnd)
  (let ((trm_parsed (parse$parse *mod_eval$$current_module* trm))
	(srt (mod_eval$$find_qual_sort srt_name))
	(cnd_parsed (parse$parse *mod_eval$$current_module* cnd)))
  (if (or (term$is_an_error trm_parsed) (term$is_an_error cnd_parsed))
      (push `(bad sort contraint ,trm ,srt_name ,cnd)
	    (getf (module$status *mod_eval$$current_module*) 'history))
  (let ((op (term$head trm_parsed)))
  (let ((mod (operator$module op)))
  (let ((op2 (mod_eval$$find_qual_operator_in mod (operator$name op)
	         (operator$arity op) srt)))
    (module$!add_sort_constraint
     *mod_eval$$current_module*
     (sort_constraint$create
      op op2 (term$subterms trm_parsed)
      cnd_parsed))
  )))))
  )

; op mod_eval$$!add_built_in_sort : {cur_mod} Module-Item -> {cur_mod}
(defun mod_eval$$!add_built_in_sort (e)
  (let ((sort_name (cadr e))
	(lisp_info (caddr e)))
  (let ((sort (sort$create sort_name *mod_eval$$current_module*)))
    (sort$!replace_info sort lisp_info)
    ;; in the following: what about putting constructor in ops? (using)
    (module$!modify_principal_sort *mod_eval$$current_module* sort)
    (sort$!replace_constructor sort
      (mod_eval$create_built_in_constructor sort))
    (module$!adjoin_sort *mod_eval$$current_module* sort)
    (module$!replace_sort_order *mod_eval$$current_module*
      (sort_order$adjoin_sort (module$sort_order *mod_eval$$current_module*)
			      sort))
  )))

; op mod_eval$$!add_vars_of : {cur_mod} Module-Item -> {cur_mod} 
(defun mod_eval$$!add_vars_of (e)
  (let ((modval (if (equal "." (cadr e))
		    (if *mod_eval$$last_module* *mod_eval$$last_module*
		      (progn
			(princ "Error: no last module") (terpri)
			(obj3-module-error)))
		  (modexp_eval$top_level_eval (car (cadr e))
					      *mod_eval$$current_module*))))
    (if (modexp_eval$is_error modval)
      (progn
	(princ "Error: in vars-of module is not defined: ")
	(princ "name: ") (print$simple_princ_open (cadr e)) (terpri)
	(obj3-module-error))
    (let ((sorts (module$sorts obj$current_module)))
    (dolist (v (module$variables modval))
       (when (member (variable$initial_sort v) sorts) ;@ error?
	 (let ((nm (variable$name v))
	       (sort (variable$initial_sort v)))
	 (let ((val (mod_eval$$find_variable_in obj$current_module nm)))
	   (if (and val (not (eq sort
				 (variable$initial_sort val))))
	     (progn
	       (princ "Warning: imported variable discarded due to conflict")
	       (terpri)
	       (princ "with existing variable: ") (print$name v)
	       (terpri)
	       )
	     (unless val
	       (module$!add_variable obj$current_module
				     (variable$make_var nm sort)))
	     ))))
    )))
  ))

; handle using X with A and B
(defun mod_eval$$!add_using_with (module mepars)
  (let ((mod (modexp_eval$eval (cadr mepars))))
  (when (modexp_eval$is_error mod)
    (princ "Cannot evaluate module: ")
    (print$modexp (cadr mepars)) (terpri)
    (obj3-to-top))
  (let ((submods
	 (let ((obj$current_module mod))
	   (mapcar #'(lambda (me)
		       (let ((val (modexp_eval$eval me)))
			 (when (modexp_eval$is_error val)
			       (princ "Cannot evaluate module: ")
			       (print$modexp me) (terpri)
			       (obj3-to-top))
			 val))
		   (if (equal '(nil) (caddr mepars)) nil
		     (caddr mepars))))))
    (mod_eval$$!incorporate_using_with module mod submods)
  )))

; op mod_eval$$!incorporate_using_with : Module Module LIST[Module]
;        -> {cur_mod}
; derived from part of mod_eval$$!incorporate_module
(defun mod_eval$$!incorporate_using_with (module submod other)
  (when (not (typep submod 'module))
    (let ((val (modexp_eval$eval_whole submod)))
      (unless (typep val 'module)
	(princ "Cannot evaluate module: ") (print$name submod) (terpri)
	(obj3-to-top))
      (setq submod val)))
  (let ((snm (module$name submod)) ;><
	(lst (module$sub_modules submod)))
    (loop
     (when (null lst) (return))
     (let ((md (car lst)))
     (when (eq 'protecting (cdr md))
       (let ((mdnm (module$name (car md))))
	 (when (and (consp mdnm) (eq 'name (car mdnm))
		    (eq (cadr mdnm) snm))
	   (push (car md) other)
	   (setq lst (module$sub_modules (car md)))
	   )))
     (setq lst (cdr lst))
     )))
  (let ((allmods (nreverse (cons submod (reverse other)))))
  (dolist (amod allmods)
    (module$!add_imported_module module 'using amod))
  (let ((*mod_eval$local_vars* nil))
  (dolist (s (reverse (module$sorts submod)))
    (let ((new_sort
	   (if (let ((srtmod (sort$module s)))
		 (or (eq submod srtmod)
		     (member srtmod other)))
	       (mod_eval$$recreate_sort module s)
	     s)))
    (module$!adjoin_sort module new_sort)
    (module$!replace_sort_order module
      (sort_order$adjoin_sort (module$sort_order module) new_sort))))
  (let ((submodprs (module$principal_sort submod)))
  (when submodprs
  (module$!modify_principal_sort module
       (mod_eval$$recreate_sort module submodprs))))
  (dolist (amod allmods)
    (dolist (r (module$sort_relation amod))
      (module$!add_sort_relationship module
	(mod_eval$$find_sort_in module (sort$name (car r))) ;&&&&><
	(mod_eval$$find_sort_in module (sort$name (cdr r))))
    )
    (module$!replace_sort_order module
      (sort_order$transitive_closure
       (mod_eval$$recreate_sort_order
	module
	(module$sort_order amod))
       (module$sort_order module))))
  (dolist (o (reverse (module$operators submod)))
    (if (let ((opmod (operator$module o)))
	  (or (eq submod opmod)
	      (member opmod other)))
	(module$!add_operator module
	  (mod_eval$$recreate_operator submod module o))
      (module$!transfer_operator module submod o)))
  (dolist (o (module$operators module))
    (mod_eval$$update_poly_rules module o)
    (mod_eval$$update_theory module o)
    )
  (dolist (amod allmods)
    (dolist (e (module$equations amod))
      (module$!adjoin_equation module (mod_eval$$recreate_rule module e)))
    )
  )))

; op mod_eval$view_eval : definition {Obj_State} -> View
(defun mod_eval$view_eval (defn)
  (let ((view_name (cadr defn))
	(view_frag (caddr defn)))
  (when (modexp_eval$find_in_view_env view_name)
    (princ "Warning: redefining view ")
    (princ view_name) (terpri))
  (let ((view_form
	  (if (equal "of" (car view_frag))
	      `("view" "from" ,(nth 3 view_frag)
		       "to" ,(nth 1 view_frag)
		       ,@(cddddr view_frag)
		       "endv")
	    `("view" ,@view_frag "endv"))))
    (modexp_eval$top_level_view_eval view_name view_form))))

(defvar *mod_eval$open_module* nil)

(defun mod_eval$!open (e)
  (when *mod_eval$open_module*
    (princ "Warning: module already open: ")
    (print$name *mod_eval$open_module*) (terpri)
    (princ "Closing this module") (terpri)
    (mod_eval$!close nil))
  (setq *mod_eval$open_module*
      (if (null (cadr e))
	*mod_eval$$last_module*
	(modexp_eval$top_level_eval (cadr e) *mod_eval$$current_module*)))
  (setq obj$current_module *mod_eval$open_module*)
  (setq *mod_eval$$current_module* *mod_eval$open_module*)
  (setq *mod_eval$$last_module* obj$current_module)
      ;(module$!mark_as_needs_compiling obj$current_module) ;#
      ;(module$!mark_as_needs_parse_setup obj$current_module) ;#
  )

(defun mod_eval$!close (e)
  (declare (ignore e))
  (if *mod_eval$open_module*
      (progn
	(setq *mod_eval$$last_module* obj$current_module)
	(setq obj$current_module nil)
	;(mod_eval$$!process_final *mod_eval$open_module*) ;@ replaced by:
	(unless (module$is_parse_setup *mod_eval$open_module*)
	  (mod_eval$$!do_parse_setup *mod_eval$open_module*))
	(mod_eval$!compile *mod_eval$open_module* t) ;force compilation
	(when (eq *mod_eval$$current_module* *mod_eval$open_module*)
	    (setq *mod_eval$$current_module* nil))
	(setq *mod_eval$open_module* nil)
	)
    (progn
      (princ "Warning: no module open") (terpri)
    ))
  )
