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

;; $Id: module_other.lsp,v 205.2.1.1 2003/09/23 14:05:13 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                    module "other" routines
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Tim Winkler ;;;; Created: 10/2/91
;;;; Derived from the original module_eval.lsp

;;;;;;;;;;;;;;;; Summary ;;;;;;;;;;;;;;;;
; omitted

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; a group related to operator theories/attributes and rules

(defvar *mod_eval$$current_module* nil)
(defvar obj$current_module)
(defvar *mod_eval$$last_module* nil)

; op mod_eval$$!add_theory_rules : Module Opeator -> ~Module
;@@ do check to see if the rules are already there
(defun mod_eval$$!add_theory_rules (module op)
  (let ((thy (module$operator_equational_theory module op))
	(var nil) (sort nil) (var2 nil) (sort2 nil) (ids nil))
    (when (theory$contains_idempotency thy)
      ;add rule x op x = x
      (setq sort (car (operator$arity op)))
      (setq var (variable$make_var "U_idem" sort))
      (module$!adjoin_equation module
	(rule$!update_kind
        (rule$make_standard_rule_labelled
	 (term$make_term op (list var var)) var *obj_BOOL$true*
	 (mod_eval$create_rule_name module "idem"))
	'idem_theory))
    ) ;when
    (when (and (or (theory$contains_identity thy) (theory$zero thy))
	       (and (cdr (operator$arity op))
		    (null (cddr (operator$arity op)))))
      (let ((so (module$sort_order module)))
      (setq sort (car (operator$arity op)))
      (setq sort2 (cadr (operator$arity op)))
      (let ((comm (theory$contains_commutativity thy)))
      (let ((id (car (theory$zero thy))))
	(let ((flag nil))
	(when id
	;add rule id op x = x
	(when (sort_order$is_included_in so
		(setq ids (operator$coarity (term$head id))) sort)
	(setq flag t)
	(setq var (variable$make_var "X_id" sort2))
	(module$!adjoin_equation module
	  (rule$!update_kind
	  (rule$make_standard_rule_labelled
	   (term$make_term op (list id var)) var *obj_BOOL$true*
	   (mod_eval$create_rule_name module "ident"))
	  'id_theory
	  )))
	;add rule x op id = x (possibly)
	(unless (and flag comm)
	(when (sort_order$is_included_in so ids sort2)
	  (setq var2 (variable$make_var "Y_id" sort))
	  (module$!adjoin_equation module
	    (rule$!update_kind
	    (rule$make_standard_rule_labelled
	     (term$make_term op (list var2 id)) var2 *obj_BOOL$true*
	     (mod_eval$create_rule_name module "ident"))
	    'id_theory
	    ))))
	)))) ;let / dolist / let
      )) ;let / when
    ))

(defvar mod_eval$creation_counter 0)

(defun mod_eval$create_rule_name (module label)
  (declare (ignore module))
;  (let ((num (module$creation_counter module))) ;@@ delete eventually
;    (incf (module$creation_counter module))
;    (list (format nil "~a~a" label num))
;  )
  (let ((num mod_eval$creation_counter))
    (incf mod_eval$creation_counter)
    (list (format nil "~a~a" label num))
  ))

(defun mod_eval$$propagate_attributes (module)
  (let ((ops (module$operators module))
	(obj$current_module module))
    (dolist (o ops)
      (when (operator$is_marked_strictly_overloaded o)
	(let ((iops (operator_info$lowest
		     (optable$operator_info
		      (module$operator_table module) o))))
	(dolist (io iops)
	  (let ((assoc nil) (comm nil) (idem nil) (iden nil) (no_compl nil) id
		(othy (module$operator_equational_theory
		       module io))
		nmn newthy)
	  (let ((nmo (theory$name othy)))
	  (setq id (car (theory$zero othy)))
	  (dolist (anop iops)
	    (when (operator$is_restriction_of io anop)
	      (let ((thy (module$operator_equational_theory module anop)))
		(when (theory$contains_associativity thy) (setq assoc t))
		(when (theory$contains_commutativity thy) (setq comm t))
		(when (theory$contains_idempotency thy) (setq idem t))
		(when (theory$contains_identity thy)
		  (setq iden t)
		  (when (cdr (theory$zero thy)) (setq no_compl t))
		  (if (null id)
		      (setq id (car (theory$zero thy)))
		    (let ((nid (car (theory$zero thy))))
		      (when (and nid (not (term$congruent id nid)))
		        (princ
			 "Warning: different possible identities for operator")
			(terpri)
			(print$name o) (terpri)
			(when $$debug
			(princ "### Gory details") (terpri)
			(print$name anop) (terpri)
			(print$struct (theory$zero othy))
			(princ " -vs-")
			(terpri)
			(print$struct (theory$zero thy))
			(terpri))
			)
		      ))))))
	  (when id
	    (let ((so (module$sort_order module))
		  (idsrt (term$sort id))
		  (ar (operator$arity io)))
	    (when (not (or (sort_order$is_included_in so idsrt (car ar))
			   (sort_order$is_included_in so idsrt (cadr ar))))
	      (setq id nil))))
	  (setq nmn
	      (theory$name_to_info
	       (if (or assoc comm idem iden)
		 (intern (concatenate 'string
		     (if assoc "A" "")
		     (if comm "C" "")
		     (if idem "I" "")
		     (if iden "Z" "")
		     "_PROPERTY"))
		 'empty_property)))
	  (setq newthy (theory$create nmn
			 (when id
			   (cons id no_compl
				 ))))
	  (when (and no_compl
		     (theory$zero othy) (not (cdr (theory$zero othy))))
	    (princ "Warning: variation in id completion") (terpri)
	    (princ "for operator ") (print$name o) (terpri))
	  (if (and (not (eq the_empty_property nmo))
		   (not (eq nmo nmn))
		   (not (mod_eval$is_same_theory_but othy newthy))) (progn
	      (princ "Warning: variation in attributes for operator: ")
	      (print$name o) (terpri)
	      (princ "theory given is ")
	      (print$theory_brief othy)
	      (princ " and from context is ")
	      (print$theory_brief newthy) (terpri)
	      )
	    (module$!operator_update_equational_theory module
	      io newthy))
	  )))))))
  )

(defvar perform_id_completion nil) ; eventually eliminate

; op mod_eval$$add_identity_completions : Module -> {Module}
; latest version of this is much more of a worklist algorithm
(defun mod_eval$$add_identity_completions (module)
  (let ((flag (get-env-var "OBJ3-BATCH"))
	time1 time2) ;@@@ just for testing
  (when flag (setq time1 (get-internal-run-time)))
  (let ((obj$current_module module))
  (when (some #'(lambda (op)
		  (let ((thy (operator$theory op)))
		  (and
		   (theory$contains_identity thy)
		   (not (cdr (theory$zero thy)))
		   )))
	  (module$operators module))
  (when obj$verbose
    (princ "Performing id processing for rules in ")
    (print$mod_name module)
    (terpri))
  (dolist (r (module$equations module))
    (unless (rule$kind r)
    ; (setq r (rule$copy r)) ;@@@ don't need to delete this one?
      (let (varval (showflag nil)
	    (res nil)
	    (newres (list (list r nil nil)))
	    arule subst val)
      (loop
 (when (and $$debug obj$verbose)
   (princ "###### new res") (terpri)
   (princ "---------------------") (terpri)
   (dolist (x newres)
     (print$rule_id_inf x)
     (princ "---------------------") (terpri)
     )
   )
       (setq val (car newres))
       (setq newres (cdr newres))
       (setq arule (car val))
       (setq subst (cadr val))
       ; res is list of (rule subst [status])
       ; varval value to substitute in,
       ; and arule is rule generating new rules from
       ; status is bad/good --- in res have status, but not in newres
       (if (mod_eval$$test_bad_rule arule)
	 (unless (mod_eval$rule_inf_subst_member subst res)
	   (setq res (cons (list arule subst 'bad) res)))
	 (progn
	   (setq res (cons (list arule subst 'good) res))
	   (let ((donesubst nil) sub1 newarule newsubst)
	   (loop
	    (setq varval (mod_eval$$compute_var_for_identity_completions
			  (rule$lhs arule) donesubst))
	    (unless varval (return))
            (setq sub1 (cons varval nil))
            (setq newsubst (substitution$can (cons varval subst)))
	    (setq donesubst (cons (car sub1) donesubst))
	    (setq newarule (mod_eval$$insert_val sub1 arule))
	    (unless (or
		     (null newarule)
		     (mod_eval$rule_inf_subst_member newsubst res))
	      (setq newres (cons
			    (list newarule newsubst)
			    newres)))
	   ) ; loop
	   )))
 (when (and $$debug obj$verbose)
 (princ "########") (terpri)
 (print$rule_id_inf (car res)))
       (when (null newres) (return))
       ) ; loop

 (when (and $$debug obj$verbose)
   (princ "###### final res") (terpri)
   (princ "---------------------") (terpri)
   (dolist (x res)
     (print$rule_id_inf x)
     (princ "---------------------") (terpri)
     )
      )

      (let ((errs nil) (rules1 nil) (rules2 nil) badver)
      (dolist (ruli res)
	(let ((rul (car ruli)))
	  (if (eq 'bad (nth 2 ruli))
	    ;(unless (mod_eval$rule_inf_member rul errs)
	      (push ruli errs)
	    ;)
	    (unless (mod_eval$rule_inf_member rul rules1)
	      (push ruli rules1)))))
      ;@@@@
      (setq rules2 (list (list r nil)))
      (dolist (ruli rules1)
	(let ((rul (car ruli)))
	(unless
	    (or
	     (dolist (ruli2 rules2 nil)
	       (when (rule$subsumes (car ruli2) rul)
		 (return t)))
	     (dolist (ruli2 rules1 nil) ;This is horrific
	       (let ((rul2 (car ruli2)))
	       (when (and (not (eq rul rul2))
			  (rule$strictly_subsumes rul2 rul))
		 (return t)))))
	  (push ruli rules2))))
 (when (and $$debug obj$verbose)
   (when (null rules2) (princ "####### rules2 null") (terpri)))
 (when obj$verbose
	(when (not showflag)
	  (setq showflag t)
	  (princ "For rule: ") (print$rule_brief r) (terpri))
	(let ((rs (reverse rules1)))
	  (princ "  Generated valid rule instances:") (terpri)
	  (if rs
	    (let ((obj$verbose nil))
	    (dolist (r (reverse rs))
	      (princ "  ")
	      (print$rule_brief (car r)) (terpri)))
	    (progn (princ "  none") (terpri)))
	  (princ "  Generated invalid rule instances:") (terpri)
	  (if errs
	    (let ((lst (reverse errs)))
	    (loop
	     (when (null lst) (return))
	     (unless (mod_eval$rule_inf_member (caar lst) (cdr lst))
	       (princ "  ")
	       (print$rule_brief (caar lst)) (terpri))
	     (setq lst (cdr lst))
	    ))
	    (progn (princ "  none") (terpri)))
 ))
      (dolist (ruli rules2)
	(let ((rul (car ruli))
	      (rulsubst (cadr ruli)))
	(setq badver nil)
	(dolist (bruli errs)
	  (when (substitution$subset rulsubst (cadr bruli))
	    (push (cadr bruli) badver)))
	(let ((newrule rul) ; (rule$copy rul) ;@@@
	      (occursflag nil)
	      (newidcond (mod_eval$make_id_condition
			  (term$vars (rule$lhs rul))
			  badver)))
 (when obj$verbose 
  (unless (and (null newidcond) (eq r newrule))
    (when (not showflag)
      (setq showflag t)
      (princ "For rule: ") (print$rule_brief r) (terpri))
    (if (eq r newrule)
      (princ "  Modified rule: ")
      (if (rule$rule_occurs newrule (module$equations module))
	(progn (princ "  Required rule: ") (setq occursflag t))
	(princ "  Added rule: ")))))

       (setf (rule$id_condition newrule) newidcond)
       (when (and (null (rule$labels newrule))
		  (not (eq r newrule)))
	 (rule$!update_labels newrule
	     (mod_eval$create_rule_name module "compl")))
       (dolist (e (rule$A_extensions newrule)) ;@@
	 (setf (rule$id_condition e) newidcond))
       (dolist (e (rule$AC_extension newrule))
	 (setf (rule$id_condition e) newidcond))

 (when obj$verbose 
  (unless (and (null newidcond) (eq r rul))
    (if occursflag 
      (print$rule_brief_no_label rul)
      (print$rule_brief rul))
    (terpri)))

       (unless (eq r rul)
	 (module$!adjoin_equation module newrule))
      )))
  ))))
  (when obj$verbose
    (princ "Done with id processing for rules") (terpri))
  ))
  (when flag
    (setq time2 (get-internal-run-time))
    (let ((val (/ (float (- time2 time1)) internal-time-units-per-second)))
      (when (< 1 val)
	(format t "~&id processing time: ~8,3f cpu~%" val)))
    )
  ))

(defun mod_eval$$test_bad_rule (rul)
  (let ((bi (rule$is_built_in rul)))
  (or
   (term$is_var (rule$lhs rul))
   (and (not bi)
	(eq *obj_BOOL$true* (rule$condition rul))
	(term$occurs_as_subterm
	 (rule$lhs rul) (rule$rhs rul)))
   (term$occurs_as_subterm
    (rule$lhs rul) (rule$condition rul))
   (and (not bi)
	(term$similar (rule$lhs rul) (rule$rhs rul)))
;   (and (not perform_id_completion)
;	(operator$is_same_operator
;	 (term$head (rule$lhs rul))
;	 (term$head (rule$rhs rul))))
   )))

(defun mod_eval$rule_inf_member (rul riset)
  (dolist (rul2 riset nil)
    (when (rule$similar rul (car rul2))
      (return t))))

(defun mod_eval$rule_inf_subst_member (subst riset)
  (dolist (rul2 riset nil)
    (when (substitution$equal subst (cadr rul2))
      (return t))))

(defun print$rule_id_inf (x)
  (print$rule_brief (nth 0 x)) (terpri)
  (print$substitution (nth 1 x))
  (when (cddr x)
    (progn (print$obj (nth 2 x) nil) (terpri)))
  )

; t1 is not a var
; Should really deal with some sort of evaluation strategy issues
; but we are in a bind, since we don't know them at "this point".
(defun term$occurs_as_subterm (t1 t2)
  (if (term$is_var t2) nil
  (multiple-value-bind (gst subs nomatch eequal)
    (if (operator$is_same_operator (term$head t1) (term$head t2))
	(match$first_match t1 t2)
      (values nil nil t nil))
   (declare (ignore gst subs eequal))
   (if (not nomatch) t
     (dolist (t2st (term$subterms t2) nil) ; nil is default ans
       (when (term$occurs_as_subterm t1 t2st) (return t)))
     ))))

(defun mod_eval$$compute_var_for_identity_completions (term donesubst)
  (mod_eval$$select_var_for_identity_completions
	      term donesubst))

(defun mod_eval$$select_var_for_identity_completions (term donesubst)
  (cond
    ((or (term$is_var term) (term$is_constant term)) nil)
    (t (let* ((op (term$head term))
	      (thy (operator$theory op))
	      (id_flag (and (theory$contains_identity thy)
			    (null (cdr (theory$zero thy)))))
	      (id (if id_flag (car (theory$zero thy)))))
      (if id
	(mod_eval$$select_var_for_identity_completions_alt2
	 op id term donesubst)
	(dolist (sb (term$subterms term))
	  (let ((val (mod_eval$$select_var_for_identity_completions
		      sb donesubst)))
	    (when val (return val)))))))))

(defun mod_eval$$select_var_for_identity_completions_alt2
           (op id term donesubst)
  (let ((val1 (mod_eval$$select_var_for_identity_completions_alt
	       op id (term$arg_1 term) donesubst)))
  (if val1 val1
  (let ((val2 (mod_eval$$select_var_for_identity_completions_alt
	       op id (term$arg_2 term) donesubst)))
    val2))))

(defun mod_eval$$select_var_for_identity_completions_alt
           (op id term donesubst)
  (cond
    ((term$is_var term)
     (let ((srt (variable$initial_sort term))
	   (so (module$sort_order *mod_eval$$current_module*)))
       (when (and
	      (not (term$is_an_error id))
	      (sort_order$is_included_in so
		(operator$coarity (term$head id)) srt)
	      (not (mod_eval$$occurs_var_val term id donesubst)))
	 (cons term id))))
    ((term$is_constant term) nil)
    ((eq op (term$head term))
     (mod_eval$$select_var_for_identity_completions_alt2
      op id term donesubst))
    ((theory_name$is_empty_for_matching (theory$name (operator$theory
	    (term$head term))))
     (mod_eval$$select_var_for_identity_completions
      term donesubst))
    (t (mod_eval$$select_var_for_identity_completions
	term donesubst))))

(defun mod_eval$$occurs_var_val (var val y)
  (dolist (ye y nil) ; default answer
    (when (and (eq var (car ye))
	       (eq val (cdr ye)))
      (return t))))

(defun mod_eval$$insert_val (subs rul)
  (let (($$trace_rewrite nil)
	($$trace_rewrite_whole nil))
  (let ((newcond
	 (if (eq *obj_BOOL$true* (rule$condition rul))
	   *obj_BOOL$true*
	   (term$!simplify
	    (term$normalize_for_identity_total
	     (substitution$partial_image subs (rule$condition rul)))))))
  (if (obj_BOOL$is_false newcond)
    nil
   (rule$!update_labels
    (rule$!update_kind
     (mod_eval$make_a_rule
      (term$normalize_for_identity_total
       (substitution$partial_image subs (rule$lhs rul)))
      (term$!simplify
       (term$normalize_for_identity_total
	(substitution$partial_image subs (rule$rhs rul))))
      (if (obj_BOOL$is_true newcond) *obj_BOOL$true* newcond)
      )
     'id_completion
     )
    (rule$labels rul))))))
  
(defun mod_eval$make_id_condition (vars subs)
 (let ((subs2 nil))
   (dolist (sub subs)
     (when sub
       (let ((subcan (substitution$can (substitution$restrict vars sub))))
       (unless (or (null subcan)
		   (member subcan subs2 :test #'substitution$equal))
	 (push subcan subs2))
     )))
   (when subs2
     (list 'not (mod_eval$$make_improved_id_cond subs2)))
  ))

; cond as list of substitutions
(defun mod_eval$$make_improved_id_cond (cond)
  (if cond
    (let ((atomic (mod_eval$$compute_atomic cond)))
      (mod_eval$$improve_id_cnd
       (mod_eval$$elim_supersets
	(mod_eval$canonicalize_atomic cond atomic))))
    nil)
  )

; c assumed canonicalized and in DNF
; result is and/or/equal expression
(defun mod_eval$$improve_id_cnd (c)
  (if (null (cdr c))
    (rule$make_and_list
     (mapcar #'rule$make_eqeqeq (car c)))
  (let ((freqs (mod_eval$$compute_atomic_freq c)) max nxt p1 p2 flag)
    (setq nxt (caar freqs))
    (setq max (cdar freqs))
    (dolist (e (cdr freqs))
      (when (< max (cdr e))
	(setq nxt (car e)  max (cdr e)))
      )
    (setq p1 nil  p2 nil  flag t)
    (dolist (s c)
      (unless (null s)
      (if (mod_eval$$member_atomic nxt s)
	(when flag
	  (let ((res (mod_eval$$remove_atomic nxt s)))
	    (if (null res)
	      (setq flag nil  p1 nil)
	      (push res p1))))
	(push s p2))))
    (when (and p1 (null flag)) (break "ERR"))
    (if p1 (setq p1 (mod_eval$$improve_id_cnd p1)) (setq p1 t))
    (when p2 (setq p2 (mod_eval$$improve_id_cnd p2)))
    (rule$make_or_cond
     (rule$make_and_cond (rule$make_eqeqeq nxt) p1)
     p2)
 )))

; arg is list of substs
(defun mod_eval$$compute_atomic (expr)
  (let ((res nil))
    (dolist (x expr)
      (dolist (y x)
	(unless (mod_eval$$member_atomic y res)
	  (push y res))))
    res
  ))

; arg is list of substs
(defun mod_eval$canonicalize_atomic (e atoms)
  (mapcar #'(lambda (x)
	      (mapcar #'(lambda (y)
			  (or (mod_eval$$member_atomic y atoms)
			      y)
			  )
		x
		))
    e)
  )

(defun mod_eval$$compute_atomic_freq (expr)
  (let ((res nil))
    (dolist (x expr)
      (dolist (y x)
	(let ((val (mod_eval$$assoc_atomic y res)))
	  (if val (incf (cdr val))
	    (push (cons y 1) res))
	  )
	))
    res
  ))

(defun mod_eval$$member_atomic (x lst)
  (dolist (e lst nil)
    (when (mod_eval$$same_atomic x e) (return e))))

(defun mod_eval$$remove_atomic (x lst)
  (let ((res nil))
  (dolist (e lst)
    (unless (mod_eval$$same_atomic x e)
      (push e res)))
  (nreverse res)))

(defun mod_eval$$assoc_atomic (x alist)
  (dolist (e alist nil)
    (when (mod_eval$$same_atomic x (car e)) (return e))))

(defun mod_eval$$same_atomic (x y)
  (and (consp x) (consp y)
       (term$similar (car x) (car y))
       (term$similar (cdr x) (cdr y)))
  )

(defun mod_eval$$elim_supersets (e)
  (let ((res nil))
    (dolist (x e)
      (unless (dolist (y e nil)
		(when (and (not (eq x y))
			   (substitution$subset y x))
		  (return t)))
	(push x res))
      )
    res
 ))

(defun mod_eval$make_id_condition_direct (vars subs)
 (let ((subs2 nil))
   (dolist (sub subs)
     (when sub
       (let ((subcan (substitution$can (substitution$restrict vars sub))))
       (unless (member subcan subs2 :test #'substitution$equal)
	 (push subcan subs2))
     )))
  (if subs2
    (list
     'not
     (rule$make_or_list
      (mapcar #'(lambda (sub)
		  (rule$make_and_list
		   (mapcar #'(lambda (si)
			       (list 'equal (car si) (cdr si)))
		     sub
		     ))
		  )
	subs2)
      )
     )
    nil
  )))

(defun mod_eval$make_a_rule (lhs rhs cond)
  (if (and (term$is_constant rhs)
	   (eq obj_BUILT-IN$sort_Built-in (term$sort rhs)))
    (rule$make_bi_rule lhs (term$built_in_value rhs) cond)
    (rule$make_standard_rule lhs rhs cond)))

(defun term$normalize_for_identity_total (tm)
  (theory$standard_form (term$normalize_for_identity tm))
  )

; rules for and or not == =/= identical nonidentical must not have conditions
(defun term$!simplify (tm)
  (if (term$is_var tm) nil
  (if (term$is_constant tm) nil
  (let ((op (term$head tm)))
    (dolist (subtm (term$subterms tm)) (term$!simplify subtm))
    (if (or (eq *obj_BOOL$and* op)
	    (eq *obj_BOOL$or* op)
	    (eq *obj_BOOL$not* op)
	    (eq *obj_BOOL$if* op))
      (rew$!simplify_on_top tm)
    (if (and (or (eq *obj_BOOL$equal* op)
		 (eq *obj_BOOL$nonequal* op)
		 (eq *obj_IDENTICAL$identical* op)
		 (eq *obj_IDENTICAL$nonidentical* op))
	     (not (term$is_containing_var (term$arg_1 tm)))
	     (not (term$is_containing_var (term$arg_2 tm))))
      (if (or (eq *obj_BOOL$equal* op)
	      (eq *obj_IDENTICAL$identical* op))
	(if (term$similar (term$arg_1 tm) (term$arg_2 tm))
	  (term$!replace tm *obj_BOOL$true*)
	  nil)
	(if (term$similar (term$arg_1 tm) (term$arg_2 tm))
	  (term$!replace tm *obj_BOOL$false*)
	  nil)
	)
      nil))
  )))
  tm
  )

;&&&& where to put the following
(defun term$normalize_for_identity (term)
  (cond
    ((or (term$is_var term) (term$is_constant term)) term)
    (t (let* ((op (term$head term))
	      (thy (operator$theory op))
	      (id_flag (and (theory$contains_identity thy)
			    (null (cdr (theory$zero thy)))))
	      (id (if id_flag (car (theory$zero thy))))
	      (subs (mapcar #'term$normalize_for_identity
		      (term$subterms term))))
      (if id
	(if (term$similar (car subs) id)
	    (cadr subs)
	(if (term$similar (cadr subs) id)
	    (car subs)
	(term$make_term_check_op_with_sort_check op subs)))
	(term$make_term_check_op_with_sort_check op subs))
    ))
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; a group related to "compilation"

(defun mod_eval$!compile (&optional (module *mod_eval$$last_module*)
				   (force nil))
  (let ((obj$current_module module))
  (dolist (o (module$operators module)) ;@@ new here
    (when (eq module (operator$module o))
      (mod_eval$$!add_theory_rules module o)))
  (mod_eval$$add_identity_completions module)
  ;(rule_gen$rule_generation module) ;... expanded to the following
  (unless (and (not force) (module$is_compiled module)
	       (not (module$needs_compiling module)))
    ; when forcing a recompile, need to reset things
    (when (and (or force (module$needs_compiling module))
	       (module$is_compiled module)) ;???
	;reset info on vars?
	;delete equations
	;(setf (module$equations module) nil) ;@@
	;(setq save-rules (module$rules module))
	(setf (module$rules module) nil)
	(dolist (o (module$operators module))
	  (when (eq module (operator$module o))
	    (let ((opinf (optable$operator_info (module$operator_table module)
			     o)))
	    (setf (operator_info$rules_with_same_top opinf)
	        (rule_ring$create nil))
	    (setf (operator_info$rules_with_different_top opinf) nil))))
	; undo effects of add module$!add_rule_to_op module$!add_rule in rule_g
    )
    (rule_gen$$!install_module_equations 
	module (rule_gen$$gather_sub_modules_drules module))
    )
  (mapc #'(lambda (op)
	(operator$!compute_rew_strategy module op)
	(operator$!fix_strategy_and_rules module op)
	)  
    (module$operators module))
  (mapc #'(lambda (srt) ; for bi constants with equations
	(when (sort$constructor srt)
	  (operator$!compute_rew_strategy
	   module
	   (sort$constructor srt))))
    (module$sorts module))
  (rule_gen$$normalize_rules_in module)
  (mod_eval$error_check module)
  (module$!mark_as_compiled module)
  ))

;;; create structures needed for parsing
;;;     sort_order -- depends on explicit and imported sorts
;;;     parse dictionary -- depends on full signature; explicit
;;;       and imported
; op mod_eval$$!parse_setup : {cur_mod} -> {cur_mod}
(defun mod_eval$$!parse_setup ()
  (unless (module$is_parse_setup *mod_eval$$current_module*)
  (mod_eval$$include_BOOL)
  (mod_eval$$!do_parse_setup *mod_eval$$current_module*)
  ))

(defun mod_eval$$!setup_parse (mod)
  (unless (module$is_parse_setup mod)
  (let ((*mod_eval$$current_module* mod))
    (mod_eval$$include_BOOL))
  (mod_eval$$!do_parse_setup mod)
  ))

(defun mod_eval$$!do_parse_setup (mod)
  (let ((obj$current_module mod))
  (mod_eval$!update_sort_order mod)
  (operator_table$!update_operator_info mod)
  ;; create parsing information
  (parse$!update_parse_information mod)
  (mod_eval$$propagate_attributes mod)
  (module$!mark_as_parse_setup mod)
  ))

; op mod_eval$$!process_final : Module Bool -> Module
(defun mod_eval$$!process_final (module)
  ; delete variables from parse-dictionary
  ;   redo parse-setup
  (let ((obj$current_module module))
  (unless (module$is_parse_setup module)
    (mod_eval$$!do_parse_setup module))
;@@
;  (let ((dict (module$parse_dictionary module)))
;  (dolist (v (module$variables module))
;    (dictionary$!delete_info_on_token dict (variable$name v) v)))
  (dolist (o (module$operators module))
    (when (eq module (operator$module o))
      (mod_eval$$!add_theory_rules module o)))
;  (unless (or (module$parameters module) ;try always compiling
;	      (eq 'theory (module$kind module))
;	      (mod_eval$has_theory_submodule module))
      ; if there are parameters don't do rule generation; also no if theory
    (mod_eval$$add_identity_completions module)
    (rule_gen$rule_generation module)
    (mapc #'(lambda (op)
	      (operator$!compute_rew_strategy module op)
	      (operator$!fix_strategy_and_rules module op)
	      )
	  (module$operators module))
    (mapc #'(lambda (srt) ; for bi constants with equations
	      (when (sort$constructor srt)
		(operator$!compute_rew_strategy
		 module
		 (sort$constructor srt))))
      (module$sorts module))
    (rule_gen$$normalize_rules_in module)
    (mod_eval$error_check module)
;    )
    (module$!mark_as_compiled module)
  ))

(defun mod_eval$error_check (module)
  (let ((obj$current_module module))
  ; sort cycles
  (let ((so (module$sort_order module)))
  (dolist (s (module$sorts module))
    (when (member s (sort_order$lower_sorts so s))
      (princ "Warning: cycle in sort structure") (terpri)
      (princ "the sort ") (print$name s) (princ " is in a cycle") (terpri)
      (return)))
  (let ((done nil) (optab (module$operator_table module)))
  (dolist (o (module$operators module))
    (let ((op (or (car (operator_info$highest
			(optable$operator_info optab o)))
		  o)))
    (unless (member op done)
    (let ((ethy (module$operator_equational_theory module op))
	  (opcoar (operator$coarity op))
	  (opar (operator$arity op)))
    (when (theory$contains_associativity ethy)
      (unless (and
	       (cdr opar) (null (cddr opar))
	       (sort_order$is_included_in so opcoar (car opar))
	       (sort_order$is_included_in so opcoar (cadr opar)))
	(princ "Warning: associative operator ") (print$name op)
	(princ " has bad rank") (terpri)
      ))
    (when (theory$contains_commutativity ethy)
      (unless (and
	       (cdr opar) (null (cddr opar))
	       (sort_order$is_in_same_component_safe so
	           (car opar) (cadr opar)))
	(princ "Warning: commutative operator ") (print$name op)
	(princ " has bad rank") (terpri)
      ))
    (when (theory$contains_identity ethy)
      (unless (and (cdr opar) (null (cddr opar)))
	(princ "Warning: operator with identity ") (print$name op)
	(princ " has bad rank") (terpri)
      ))
    (push op done)
    )))))
  )))

(defun mod_eval$!setup_reduction (mod)
  (unless (module$is_parse_setup mod)
    (let ((*mod_eval$$current_module* mod))
      (mod_eval$$!parse_setup)))
  (when (module$needs_compiling mod)
     (mod_eval$!compile mod))
  (setq *mod_eval$$last_module* mod)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; a group related to creation of operators

; op mod_eval$$!update_poly : Operator Attributes -> {Operator}
(defun mod_eval$$!update_poly (op attr)
  (let ((poly_decl (find-if #'(lambda (i)
				(unless (atom i)
				  (or (equal "poly" (car i))
				      (equal "polymorphic" (car i)))))
			    attr)))
    (when poly_decl
      (operator$!make_polymorphic op (cadr poly_decl))
      (operator$!mark_strictly_overloaded op)
  )))

; op  mod_eval$$compute_equational_theory :
;         Name Arity Coarity Attributes Parse-flag ->
;         Theory Parse-flag
(defun mod_eval$$compute_equational_theory (pat arity coarity attr)
  (declare (ignore coarity))
  ;; assoc comm idem -- flags (and variants)
  ;; id: term &&&& assume is name of a constant function
  (let ((is_assoc nil)
	(is_comm nil)
	(is_idem nil)
	(is_iden nil)
	(is_iden_r nil)
	(id 'void))
  (dolist (i attr)
    (unless (atom i)
    (let ((tag (car i)))
    (cond
     ((or (equal "assoc" tag) (equal "associative" tag))
      (setq is_assoc t))
     ((or (equal "comm" tag) (equal "commutative" tag))
      (setq is_comm t))
     ((or (equal "idem" tag) (equal "idempotent" tag))
      (setq is_idem t))
     ((or (equal "id:" tag) (equal "identity:" tag))
      (setq is_iden t)
      (setq id (if (consp (cadr i)) (cadr i) (cdr i)))
      )
     ((or (equal "idr:" tag) (equal "identity-rules:" tag))
      (setq is_iden t)
      (setq is_iden_r t)
      (setq id (if (consp (cadr i)) (cadr i) (cdr i)))
      )
     ;; ignore otherwise
     )))
  )
  ;; build the theory; sample name: ACIZ_property
  (let ((theory_name
	 (if (or is_assoc is_comm is_idem is_iden)
	   (intern (concatenate 'string
		     (if is_assoc "A" "")
		     (if is_comm "C" "")
		     (if is_idem "I" "")
		     (if is_iden "Z" "")
		     "_PROPERTY"))
	   'empty_property)))
  (let ((idn 
        (if (eq 'void id)
            nil
	    (progn
	      (unless (module$is_parse_setup *mod_eval$$current_module*)
	          (mod_eval$$!parse_setup))
	      (let ((pars (parser$parses *mod_eval$$current_module* id)))
	      (let ((ids
		      (parser$find_parse_strict_sorts
		       *mod_eval$$current_module*
			pars arity)))
		(if (null ids)
		    (unless (null arity)
		      (princ "Error: no parse for identity for operator ")
		      (print$simple_princ_open pat)
		      ;(princ " identity expression: ")
		      ;(print$simple_princ_open id)
		      (terpri)
		      (parser$diagnose *mod_eval$$current_module* id
		          *obj$sort_Universal*))
		  (unless (null (cdr ids))
		    (princ
		     "Warning: multiple possible identities for operator")
		    (princ pat) (princ " identity expression: ")
		    (print$simple_princ_open id) (terpri)))
		(if (and ids (term$is_containing_var (car ids))) ;@@
		    (progn
		   (princ "Warning: identity expression contains a variable: ")
		      (term$print (parse$convert (car ids)))
		      (princ ", ignored") (terpri)
		      nil)
		  (parse$convert (car ids)))
		))
	      ))))
    (theory$create theory_name (if idn (cons idn is_iden_r)))
  ))))

; op mod_eval$$compute_rew_strategy : Name Arity Coarity Attributes ->
;         Strategy
(defun mod_eval$$compute_rew_strategy (pat arity coarity attr)
  (declare (ignore pat arity coarity))
  ;; for now default to bottom-up
  (let ((strat_decl (find-if #'(lambda (i)
				 (unless (atom i)
				 (let ((x (car i)))
				 (or (equal "strat" x)
				     (equal "strategy" x)
				     (equal "(" x)
				     )))) ;")"
			     attr)))
  (let ((strats (if (or (equal "strat" (car strat_decl))
			(equal "strategy" (car strat_decl)))
		  (if (equal ")" (caddr strat_decl))
		    nil
		    (caddr strat_decl))
		  (if (equal ")" (cadr strat_decl))
		    nil
		    (cadr strat_decl)))))
    (if strat_decl
	(mapcar #'read-from-string strats)
      ;(append (iota (length arity)) (list 0)) &&&&
      'none
    ))))

(defun iota (n)
  (do ((i n (1- i))
       (res nil (cons i res)))
      ((= 0 i) res)))

; op mod_eval$$compute_memo : Attributes -> Bool
(defun mod_eval$$compute_memo (attr)
  (let ((memo_decl (find-if #'(lambda (i)
				(unless (atom i)
				(equal "memo" (car i))))
			    attr)))
    (if memo_decl t nil))
  )

; op mod_eval$$compute_intrinsic : Attributes -> Bool
(defun mod_eval$$compute_intrinsic (attr)
  (let ((memo_decl (find-if #'(lambda (i)
				(unless (atom i)
				(equal "intrinsic" (car i))))
			    attr)))
    (if memo_decl t nil))
  )

; op mod_eval$$compute_error_strategy : Attributes -> Bool
(defun mod_eval$$compute_error_strategy (attr)
  (let ((err_decl (find-if #'(lambda (i)
			        (unless (atom i)
				(equal "nonstrict" (car i))))
			    attr)))
    (if err_decl nil t)) ; t means strict
  )

(defvar *mod_eval$default_prec* 41)
(defvar *mod_eval$default_unary_prec* 15)

; op mod_eval$$compute_op_parsing_attr : Name Arity Coarity Attribute-Item  ->
;     <Precedence,Form,Standard>
(defun mod_eval$$compute_op_parsing_attr (op_name op_arity op_coarity attr)
  (declare (ignore op_coarity))
  ;using general_read$$numberp from module_parse
  (let ((attr_list (cadr attr))
	(is_standard (not (member "_" op_name :test #'equal))))
  (let ((prec_decl
	 (find-if #'(lambda (i)
		      (or (equal "prec" (car i))
			  (general_read$$numberp (car i))))
		  attr_list))
        (gather_decl
	 (find-if #'(lambda (i) (let ((val (car i)))
		      (or (equal "gather" val) (equal "gathering" val))))
		  attr_list))
	;; semantic associative implies a default choice of gathering rule
	(assoc_decl
	 (find-if #'(lambda (i)
		      (member (car i) '("assoc" "associative")
			      :test #'equal))
		  attr_list)))
  (let ((gather (mod_eval$$compute_gathering
		 gather_decl assoc_decl op_name op_arity is_standard))
	lower_prec ; defined in the following cond
	op_form
	)
  (let ((prec (if prec_decl (read-from-string
	       (if (equal "prec" (car prec_decl)) (cadr prec_decl)
		 (car prec_decl)))
	       (if is_standard 0
	       (if (and op_name
			(not (equal "_" (car op_name)))
			(not (equal "_" (car (last op_name)))))
		   0
	       (if (and op_name
			(equal "_" (car (last op_name)))
			(not (member "_" (butlast op_name) :test #'equal)))
		   *mod_eval$default_unary_prec*
		 *mod_eval$default_prec*))))))
    (cond
     ((and (not prec) is_standard)
      ;; 1+ since gathering should all be e's
      (setq prec parser$max_precedence lower_prec parser$max_precedence))
     (t (setq lower_prec (if (zerop prec) prec (1- prec)))))
    ;; create the appropriate "form"
    (let ((op_pat_lst
	   (if is_standard
	       (mod_eval$build_standard_pattern op_name op_arity)
	       op_name))
	  (op_arity_lst op_arity)
	  (op_gather_lst gather)
	  (cur_item nil) (last_item nil) (next_item nil)
	  (res nil))
      (loop
       (when (null op_pat_lst)
	     (unless (null op_arity_lst)
	       (princ "Warning: operator pattern doesn't contain enough")
	       (terpri)
	       (princ "argument positions: ")
	       (print$simple_princ_open op_name)
	       (terpri))
             (setq op_form (nreverse res))
             (return)
         )
       (setq last_item cur_item)
       (setq cur_item (car op_pat_lst))
       (setq next_item (cadr op_pat_lst))
       (cond
	((equal "_" cur_item)
	 (if (null op_arity_lst)
	   (progn
	     (princ "Warning: too many _'s in operator pattern") (terpri)
	     (princ "operator: ") (princ op_name) (terpri)
	     (princ "remainder of pattern ignored: ")
	     (princ op_pat_lst) (terpri)
	     (setq op_pat_lst nil))
	   (progn
	     (push (list* 'argument
		  (if (equal "&" (car op_gather_lst)) parser$max_precedence
		  (if (and last_item (not (equal "_" last_item))
			   next_item (not (equal "_" next_item)))
		      parser$max_precedence
		  (if (equal "e" (car op_gather_lst)) lower_prec
		  (if (equal "E" (car op_gather_lst)) prec
		    0))))
		  (mod_eval$$find_qual_sort (car op_arity_lst)))
		res)
	     (setq op_arity_lst (cdr op_arity_lst)
		   op_gather_lst (cdr op_gather_lst)))))
	(t
	 (push (cons 'token cur_item) res)))
       (setq op_pat_lst (cdr op_pat_lst))
      )
    )
    (list prec op_form is_standard)
  )))))

; op mod_eval$$compute_gathering : Decl Bool Name Arity Bool -> LIST[Atoms]
(defun mod_eval$$compute_gathering (gather_decl assoc_decl op_name op_arity
				    is_standard)
  (if gather_decl (caddr gather_decl)
  ;; if unary prefix use E not e
  (if is_standard
      (mapcar #'(lambda (x) (declare (ignore x)) "&") op_arity)
  (if (and (equal '("_") (last op_name))
	   (not (member "_" (butlast op_name) :test #'equal)))
      '("E")
  (if assoc_decl '("e" "E")
    (mapcar #'(lambda (x) (declare (ignore x)) "E") op_arity))))))

; op mod_eval$build_standard_pattern : Operator -> Name
(defun mod_eval$build_standard_pattern (op_name op_arity)
  (if (null op_arity) op_name
  (let ((args (cdr (mapcan #'(lambda (x) (declare (ignore x)) (list "," "_"))
			   op_arity))))
    (append op_name '("(") args '(")"))
  )))

(defun util$group_paren_units (tokens)
  (let ((res nil) (lst tokens) unit)
  (loop
   (multiple-value-setq (unit lst) (util$scan_parenthesized_unit lst))
   (when (eq 'unbalanced unit) (return tokens))
   (setq res (cons unit res))
   (when (null lst) (return (nreverse res)))
  ))
  )

; derived from parser$scan_parenthesized_unit
(defun util$scan_parenthesized_unit (tokens)
  (if (equal "(" (car tokens))
      (let ((count 1) (lst (cdr tokens)) (res nil) tok)
	(loop
	 (when (null lst) (return (values 'unbalanced tokens)))
	 (setq tok (car lst)  lst (cdr lst))
	 (when (and (= 1 count) (equal ")" tok))
	   (return (values (nreverse res) lst)))
	 (setq res (cons tok res))
	 (if (equal "(" tok) (incf count)
	 (if (equal ")" tok) (decf count)))
	));loop/let
    (values (list (car tokens)) (cdr tokens)))
  )

(defun util$check_enclosing_parens (tokens)
  (and (equal "(" (car tokens))
       (multiple-value-bind (par rst) (util$scan_parenthesized_unit tokens)
         (declare (ignore par))
        (null rst))))
