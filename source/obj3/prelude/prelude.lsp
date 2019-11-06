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

;; $Id: prelude.lsp,v 205.2.1.1 2003/09/23 13:49:51 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                    OBJ-standard-prelude
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Tim Winkler ;;;; Created: 7/22/86

;;;;;;;;;;;;;;;; Summary ;;;;;;;;;;;;;;;;
;somewhat out-of-date
; op obj_BOOL$is_true : Term -> Bool
; op obj_BOOL$make_conjunction : Term Term -> Term
; op obj_BOOL$coerce_to_Bool : Lisp -> Bool
; op ci$osp_install : ->
; op obj_BOOL$setup : ->
; op obj_BOOL$if_resolver : Operator Arity Module -> Operator
; op obj_BOOL$create_if : Operator Sort Sort Module -> Operator
; op obj_BOOL$create_if_rules : Operator Sort Sort Module Operator ->
;        SET[Rules]

; var *obj_TRUTH$module* -- the module TRUTH
; var *obj_BOOL$module* -- the module BOOL
; var *obj_BOOL$sort_Bool* -- the sort Bool
; var *obj_BOOL$true* -- true as a term
; var *obj_BOOL$false* -- false as a term
; var *obj_BOOL$and* -- the operator and
; var *obj$sort_Universal* -- the universal sort (from BOOL)

;&&&&del
;;;; this is the minimal possible prelude
;(defvar *ci$$osp* '(
;("obj" "TRUTH-VALUES" "is"
; (("sort" ("Bool") ".")
;  ("op" ("false") ":" "->" "Bool" ".")
;  ("op" ("true") ":" "->" "Bool" "."))
; "endo")
;("obj" "BOOL" "is"
; (("pr" ("TRUTH-VALUES") ".")
;  ("sort" ("Universal") ".")
;  ("op" ("_" "and" "_") ":" ("Bool" "Bool") "->" "Bool" "."))
; "endo")
;))

; var *obj_TRUTH$module* -- the module BOOL
(defvar *obj_TRUTH$module* 'void)
; var *obj_BOOL$sort_Bool* -- the sort Bool
(defvar *obj_BOOL$sort_Bool* 'void)
; var *obj_BOOL$true* -- true as a term
(defvar *obj_BOOL$true* 'void)
; var *obj_BOOL$false* -- false as a term
(defvar *obj_BOOL$false* 'void)
; var *obj$sort_Universal* -- the universal sort (from BOOL)
(defvar *obj$sort_Universal* 'void)

; var *obj_BOOL$module* -- the module BOOL
(defvar *obj_BOOL$module* 'void)
; var *obj_BOOL$and* -- the operator and
(defvar *obj_BOOL$and* 'void)
; var *obj_BOOL$or* -- the operator or
(defvar *obj_BOOL$or* 'void)
; var *obj_BOOL$not* -- the operator not
(defvar *obj_BOOL$not* 'void)
; var *obj_BOOL$equal* -- the operator _==_
(defvar *obj_BOOL$equal* 'void)
; var *obj_BOOL$nonequal* -- the operator _=/=_
(defvar *obj_BOOL$nonequal* 'void)
; var *obj_BOOL$if* -- the operator if_then_else_fi
(defvar *obj_BOOL$if* 'void)

;&&&&del
; op ci$osp_install : ->
;(defun ci$osp_install ()
;  ;(mapcar #'ci$!process_definition *ci$$osp*) ;8 Jun 88 step-by-step
;  )

; op obj_TRUTH$setup : ->
(defun obj_TRUTH$setup ()
  (setq *obj_TRUTH$module* (modexp_eval$eval "TRUTH"))
  (setq *obj_BOOL$sort_Bool*
    (mod_eval$$find_sort_in *obj_TRUTH$module* "Bool"))
  (setq *obj_BOOL$true*
    (term$make_term (mod_eval$$find_operator_in
		      *obj_TRUTH$module* '("true") nil *obj_BOOL$sort_Bool*)
		     nil))
  (setq *obj_BOOL$false*
     (term$make_term (mod_eval$$find_operator_in
		      *obj_TRUTH$module* '("false") nil *obj_BOOL$sort_Bool*)
		     nil))
  ;&&&& 20 May 87 -- due to new handling of operator information
  (setq *obj_BOOL$if*
      (mod_eval$$find_operator_named_in  *obj_TRUTH$module*
        '("if" "_" "then" "_" "else" "_" "fi")))
  (when (null *obj_BOOL$if*) (break "SNARK: prelude -- couldn't find if"))
  (let ((optab (module$operator_table *obj_TRUTH$module*)))
    (when (null optab)
      (setq optab (operator_table$!create *obj_TRUTH$module*))
      (setf (module$operator_table *obj_TRUTH$module*) optab))
    (operator_info$!install_in_optable optab *obj_TRUTH$module* *obj_BOOL$if*)
    )
  ; the following two are less basic
  (setq *obj_BOOL$equal*
    (mod_eval$$find_operator_in
     *obj_TRUTH$module* '("_" "==" "_")
     (list *obj$sort_Universal* *obj$sort_Universal*)
     *obj_BOOL$sort_Bool*))
  (setq *obj_BOOL$nonequal*
    (mod_eval$$find_operator_in
     *obj_TRUTH$module* '("_" "=/=" "_")
     (list *obj$sort_Universal* *obj$sort_Universal*)
     *obj_BOOL$sort_Bool*))
  )

; op obj_BOOL$setup : ->
(defun obj_BOOL$setup ()
  (setq *obj_BOOL$module* (modexp_eval$eval "BOOL"))
  (setq *obj_BOOL$and*
    (mod_eval$$find_operator_in
     *obj_BOOL$module* '("_" "and" "_")
     (list *obj_BOOL$sort_Bool* *obj_BOOL$sort_Bool*)
     *obj_BOOL$sort_Bool*))
  ; the following are not as fundamental (used in simplifying conditions)
  (setq *obj_BOOL$or*
    (mod_eval$$find_operator_in
     *obj_BOOL$module* '("_" "or" "_")
     (list *obj_BOOL$sort_Bool* *obj_BOOL$sort_Bool*)
     *obj_BOOL$sort_Bool*))
  (setq *obj_BOOL$not*
    (mod_eval$$find_operator_in
     *obj_BOOL$module* '("not" "_")
     (list *obj_BOOL$sort_Bool*)
     *obj_BOOL$sort_Bool*))
  ;&&&& 20 May 87 -- due to new handling of operator information
  )

; op obj_BOOL$is_true : Term -> Bool
(defun obj_BOOL$is_true (term)
  (term$similar term *obj_BOOL$true*))

; op obj_BOOL$is_false : Term -> Bool
(defun obj_BOOL$is_false (term)
  (term$similar term *obj_BOOL$false*))

; op obj_BOOL$make_conjunction : Term Term -> Term
(defun obj_BOOL$make_conjunction (t1 t2)
  (term$make_term *obj_BOOL$and* (list t1 t2)))

; op obj_BOOL$coerce_to_Bool : Lisp -> Bool
(defun obj_BOOL$coerce_to_Bool (x)
  (if x *obj_BOOL$true* *obj_BOOL$false*))

; op obj_BOOL$if_resolver : Operator Arity Module -> Operator
; use: if_the_else_fi [ polymorphic obj_BOOL$if_resolver ]
; morphism rules given by a function; 'none as value means none applies
(defun obj_BOOL$if_resolver (op arity module)
  (let ((sort_order (module$sort_order module))
	(sc (car arity))
	(s1 (cadr arity))
	(s2 (caddr arity))
	(sort nil))
    (if (or (eq s1 s2) ;&&&& 6 Nov 87 shortcut
	    (sort_order$is_included_in sort_order s1 s2))
	(setq sort s2)
      (if (sort_order$is_included_in sort_order s2 s1)
	  (setq sort s1)))
    (if (and sort (eq *obj_BOOL$sort_Bool* sc))
	(let ((oparity (operator$arity op))) ;&&&& 6 Nov 87 shortcut
	  (if (and (eq *obj_BOOL$sort_Bool* (car arity))
		   (eq (cadr oparity) sort)
		   (eq (caddr oparity) sort))
	      op
	    (obj_BOOL$create_if op *obj_BOOL$sort_Bool* sort module)))
      ;;
      ;; Really we should really construct something like a top
      ;; sort in the connected component containing the
      ;; argument sorts.  For efficiency we merely grab a.n.other
      ;; common super-sort (this code is called at *run-time* because
      ;; the work isn't done when modules are imported)
      (let ((tsc (sort_order$component_top sort_order sc))
	    (ts
	     (intersection (cons s1 (sort_order$greater_sorts sort_order s1))
			   (cons s2 (sort_order$greater_sorts sort_order s2)))
	     ))
	(if (null ts)
	    'none
	  (obj_BOOL$create_if op tsc (car ts) module)
	  )
	
	)))
  )

; op obj_BOOL$create_if : Operator Sort Sort Module -> Operator
(defun obj_BOOL$create_if (op cond_sort sort module)
  (let ((arity (list cond_sort sort sort)))
  (let ((find_op (mod_eval$$find_operator_in module
                   (operator$name op) arity sort)))
    (if find_op find_op
      (let ((new_op
             (make-operator
	      :name (operator$name op) ;&&&& no need to canonicalize
	      :arity arity
	      :coarity sort
	      :module *obj_TRUTH$module* ;10 Jun 88 was module
	      :illdefined nil
	      :precedence ;(operator-precedence op)
	        (operator$precedence op)
	      :form
	        (list '(token . "if") ;&&&&
		      (list* 'argument 127 cond_sort)
		      '(token . "then")
		      (list* 'argument 127 sort)
		      '(token . "else")
		      (list* 'argument 127 sort)
		      '(token . "fi"))
	      :syntactic_type ;(operator-syntactic_type op)
	        (operator$syntactic_type op)
	      )))
	(let ((optab (module$operator_table module)))
	  (when (null optab)
	    (setq optab (operator_table$!create module)))
	  (optable$update_operator_info optab new_op
	    (operator_info$make
	     (module$operator_equational_theory *obj_TRUTH$module* op)
	     (module$operator_user_rew_strategy *obj_TRUTH$module* op)
	     (module$operator_user_rew_strategy *obj_TRUTH$module* op)
	     (module$operator_memo  *obj_TRUTH$module* op)
	     (module$operator_error_strategy *obj_TRUTH$module* op)
	     nil
	     (module$operator_polymorphic *obj_TRUTH$module* op)
	     (rule_ring$create nil)
	     nil
	   ))
	  (dolist (r (obj_BOOL$create_if_rules
		      op cond_sort sort module new_op))
	    (optable$!add_rule optab new_op r))
	  (setf (operator-intrinsic new_op)
		(optable$operator_info optab new_op))
	  (setf (gethash new_op optab) nil) ;&&&&
	      ;15 Mar 88 added last; improve somehow?
	  )
	(module$!add_operator module new_op)
	new_op)))))

; op obj_BOOL$create_if_rules : Operator Sort Sort Module Operator ->
;        SET[Rules]
;  use ruls from the original operator (use rule_gen functions?)
(defun obj_BOOL$create_if_rules (op cond_sort sort module new_op)
  (declare (ignore cond_sort sort))
  (mapcar #'(lambda (r)
	      (rule$make_rule
	       (let ((lhs (rule$lhs r)))
		 (term$make_term new_op (term$subterms lhs))
		 )
	       (rule$rhs r)
	       (rule$condition r)
	       (rule$end_reduction r)
	       (rule$is_built_in r)
	       )
	      )
    (module$rules_with_different_top module op))
  )

(defun obj_TRUTH$install ()
  (mod_eval$!update_sort_order *obj_TRUTH$module*)
  (operator_table$!update_operator_info *obj_TRUTH$module*)
  (parse$!update_parse_information *obj_TRUTH$module*)
  (rule_gen$rule_generation *obj_TRUTH$module*)
  (setf (module$equations *obj_TRUTH$module*) nil)
  (setf (module$status *obj_TRUTH$module*) nil) ; clear compiled
  )

; op obj_BOOL$eqeq_resolver : Operator Arity Module -> Operator
; morphism rules given by a function; 'none as value means none applies
(defun obj_BOOL$eqeq_resolver (op arity module)
  (let ((sort_order (module$sort_order module))
	(s1 (car arity))
	(s2 (cadr arity))
	(sort nil))
    (if (or (eq s1 s2) ; shortcut
	    (sort_order$is_included_in sort_order s1 s2))
	(setq sort s2)
      (if (sort_order$is_included_in sort_order s2 s1)
	  (setq sort s1)))
    (if sort
	(let ((oparity (operator$arity op))) ; shortcut
	  (if (and (eq (car oparity) sort)
		   (eq (cadr oparity) sort))
	      op
	    (obj_BOOL$create_eqeq op sort module)))
      ;;
      ;; Really we should really construct something like a top
      ;; sort in the connected component containing the
      ;; argument sorts.  For efficiency we merely grab a.n.other
      ;; common super-sort (this code is called at *run-time* because
      ;; the work isn't done when modules are imported)
      (let ((ts
	     (intersection (cons s1 (sort_order$greater_sorts sort_order s1))
			   (cons s2 (sort_order$greater_sorts sort_order s2)))
	     ))
	(if (consp ts)
	    (obj_BOOL$create_eqeq op ts module)
	    'none
	    ))
      )))

; op obj_BOOL$create_eqeq : Operator Sort Sort Module -> Operator
(defun obj_BOOL$create_eqeq (op sort module)
  (let ((arity (list sort sort)))
  (let ((find_op (mod_eval$$find_operator_in module
                   (operator$name op) arity *obj_BOOL$sort_Bool*)))
    (if find_op find_op
      (let ((new_op
             (make-operator
	      :name (operator$name op) ;&&&& no need to canonicalize
	      :arity arity
	      :coarity *obj_BOOL$sort_Bool*
	      :module *obj_TRUTH$module* ;10 Jun 88 was module
	      :illdefined nil
	      :precedence ;(operator-precedence op)
	        (operator$precedence op)
	      :form
	        (list 
		 (list* 'argument 51 sort)
		 '(token . "==")
		 (list* 'argument 51 sort))
	      :syntactic_type
	        (operator$syntactic_type op)
	      )))
	(let ((optab (module$operator_table module)))
	  (when (null optab)
	    (setq optab (operator_table$!create module)))
	  (optable$update_operator_info optab new_op
	    (operator_info$make
	     (module$operator_equational_theory *obj_TRUTH$module* op)
	     (module$operator_user_rew_strategy *obj_TRUTH$module* op)
	     (module$operator_user_rew_strategy *obj_TRUTH$module* op)
	     (module$operator_memo  *obj_TRUTH$module* op)
	     (module$operator_error_strategy *obj_TRUTH$module* op)
	     nil
	     (module$operator_polymorphic *obj_TRUTH$module* op)
	     (rule_ring$create nil)
	     nil
	   ))
	  (dolist (r (obj$recreate_rules op sort module new_op))
	    (optable$!add_rule optab new_op r))
	  (setf (operator-intrinsic new_op)
		(optable$operator_info optab new_op))
	  (setf (gethash new_op optab) nil)
	  )
	(module$!add_operator module new_op)
	new_op)))))

; op obj$recreate_rules : Operator Sort Sort Module Operator ->
;        SET[Rules]
;  use ruls from the original operator
(defun obj$recreate_rules (op sort module new_op)
  (declare (ignore sort))
  ; &&& labels/kind
  (mapcar #'(lambda (r)
	      (rule$make_rule
	       (let ((lhs (rule$lhs r)))
		 (term$make_term new_op (term$subterms lhs))
		 )
	       (rule$rhs r)
	       (rule$condition r)
	       (rule$end_reduction r)
	       (rule$is_built_in r)
	       )
	      )
    (module$rules_with_different_top module op))
  )

; op obj_BOOL$non-eqeq_resolver : Operator Arity Module -> Operator
; morphism rules given by a function; 'none as value means none applies
(defun obj_BOOL$non-eqeq_resolver (op arity module)
  (let ((sort_order (module$sort_order module))
	(s1 (car arity))
	(s2 (cadr arity))
	(sort nil))
    (if (or (eq s1 s2) ; shortcut
	    (sort_order$is_included_in sort_order s1 s2))
	(setq sort s2)
      (if (sort_order$is_included_in sort_order s2 s1)
	  (setq sort s1)))
    (if sort
	(let ((oparity (operator$arity op))) ; shortcut
	  (if (and (eq (car oparity) sort)
		   (eq (cadr oparity) sort))
	      op
	    (obj_BOOL$create_non-eqeq op sort module)))
      ;;
      ;; Really we should really construct something like a
      ;; top sort in the connected component containing the
      ;; argument sorts.  For efficiency we merely grab a.n.other
      ;; common super-sort (this code is called at *run-time* because
      ;; the work isn't done when modules are imported)
      (let ((ts
	     (intersection (cons s1 (sort_order$greater_sorts sort_order s1))
			   (cons s2 (sort_order$greater_sorts sort_order s2)))
	     ))
	(if (consp ts)
	    (obj_BOOL$create_non-eqeq op (car ts) module)
	    'none
	    ))
      )))

; op obj_BOOL$create_non-eqeq : Operator Sort Sort Module -> Operator
; note: almost identical to eqeq version
(defun obj_BOOL$create_non-eqeq (op sort module)
  (let ((arity (list sort sort)))
  (let ((find_op (mod_eval$$find_operator_in module
                   (operator$name op) arity *obj_BOOL$sort_Bool*)))
    (if find_op find_op
      (let ((new_op
             (make-operator
	      :name (operator$name op) ;&&&& no need to canonicalize
	      :arity arity
	      :coarity *obj_BOOL$sort_Bool*
	      :module *obj_TRUTH$module* ;10 Jun 88 was module
	      :illdefined nil
	      :precedence ;(operator-precedence op)
	        (operator$precedence op)
	      :form
	        (list 
		 (list* 'argument 51 sort)
		 '(token . "=/=")
		 (list* 'argument 51 sort))
	      :syntactic_type
	        (operator$syntactic_type op)
	      )))
	(let ((optab (module$operator_table module)))
	  (when (null optab)
	    (setq optab (operator_table$!create module)))
	  (optable$update_operator_info optab new_op
	    (operator_info$make
	     (module$operator_equational_theory *obj_TRUTH$module* op)
	     (module$operator_user_rew_strategy *obj_TRUTH$module* op)
	     (module$operator_user_rew_strategy *obj_TRUTH$module* op)
	     (module$operator_memo  *obj_TRUTH$module* op)
	     (module$operator_error_strategy *obj_TRUTH$module* op)
	     nil
	     (module$operator_polymorphic *obj_TRUTH$module* op)
	     (rule_ring$create nil)
	     nil
	   ))
	  (dolist (r (obj$recreate_rules op sort module new_op))
	    (optable$!add_rule optab new_op r))
	  (setf (operator-intrinsic new_op)
		(optable$operator_info optab new_op))
	  (setf (gethash new_op optab) nil)
	  )
	(module$!add_operator module new_op)
	new_op)))))
