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

;; $Id: optables.lsp,v 206.1 2003/09/23 13:40:26 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                    Operator Tables
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Tim Winkler ;;;; Created: 7/22/86

;;;;;;;;;;;;;;;; Summary ;;;;;;;;;;;;;;;;
; many have X_direct versions: which don't use module$operator_table
; op operator_table$lowest_operator : Operator LIST[Sort] -> Operator + none
; op operator_table$highest_operator : Operator Sort ->
;        Operator + nil
; op operator$lowest_operator : Operator LIST[Sort] Module -> Operator
; op operator$lowest_constrained_operator : Operator LIST[Sort] ->
;        <Operator,Sort-Constraint> or <Operator,Nil>
; op operator$find_sort_constraint : Operator -> Sort_Constraint
; op operator$highest_operator : Operator Sort Module -> Operator + nil

; op operator$lowest_operator_direct : Operator LIST[Sort] Module -> Operator
(defun operator$lowest_operator_direct (op arity module)
  (let ((poly (operator$polymorphic op)))
  (if poly
      (let ((ov (funcall poly op arity module)))
	(if (eq ov 'null)
	    op
	  ov))
  (let ((op_name (operator$name op)))
  (let ((ops (module$operators module))
	(sort_order (module$sort_order module)))
    (let ((op_res op)
	  (cur_coarity (operator$coarity op))
	  (cur_arity (operator$arity op)))
      (dolist (op2 ops)
	(let ((new_coarity (operator$coarity op2))
	      (new_arity (operator$arity op2)))
	(when (and (eq op_name (operator$name op2)) ;&&&& is_same_operator
		   (sort_order$is_included_in sort_order
		    new_coarity cur_coarity)
		   (sort$$list_included_in sort_order
		    arity new_arity)
		   (sort$$list_included_in sort_order
		    new_arity cur_arity))
	  (setq op_res op2
		cur_coarity new_coarity
		cur_arity new_arity)
	))
      )
      op_res
    ))))))

(defun sort$lowest_operator (op arity) ;&&&&
  (operator$lowest_operator op arity
    (if obj$current_module obj$current_module (operator$module op))))
(defun operator_table$lowest_operator (op arity) ;&&&&
  (operator$lowest_operator op arity
    (if obj$current_module obj$current_module (operator$module op))))

(defun sort$$list_included_in (so sl1 sl2)
  (every2len #'(lambda (x y) (sort_order$is_included_in so x y)) sl1 sl2)
  )

; op operator$highest_operator_direct : Operator Sort Module -> Operator + nil
(defun operator$highest_operator_direct (op srt module)
  (let ((op_name (operator$name op)))
  (let ((ops (module$operators module))
	(sort_order (module$sort_order module)))
  (let ((eligible_flag (sort_order$is_included_in
			sort_order (operator$coarity op) srt)))
  (let ((op_res (if eligible_flag op nil))
	(cur_arity (if eligible_flag (operator$arity op) nil))
	(cur_coarity (if eligible_flag (operator$coarity op) nil)))
    (dolist (op2 ops)
	(let ((new_arity (operator$arity op2))
	      (new_coarity (operator$coarity op2)))
	(when (and (or (eq op_name (operator$name op2)) ;&&&& is_same_operator
		       (equal op_name (operator$name op2)))
		   (sort_order$is_included_in sort_order
		    new_coarity srt)
		   (or (null op_res)
		       (and (sort_order$is_included_in sort_order
			     cur_coarity new_coarity)
			    (sort$$list_included_in sort_order
			     cur_arity new_arity))))
	  (setq op_res op2  cur_coarity new_coarity  cur_arity new_arity)
	))
      )
      op_res
    )))))

; op operator_table$highest_operator : Operator Sort ->
;        Operator
(defun operator_table$highest_operator (op srt) ;&&&& for now
  (operator$highest_operator op srt
    (if obj$current_module obj$current_module (sort$module srt))))
(defun sort$maximal_operator (op srt coreg) ;&&&& for now
  (declare (ignore coreg))
  (operator$highest_operator op srt
    (if obj$current_module obj$current_module (sort$module srt))))

; op operator$lowest_constrained_operator : Operator LIST[Sort] ->
;        Operator Sort-Constraint  -or-  Operator nil
; assume that operator_table$lowest_operator is "ready for use"
(defun operator$lowest_constrained_operator (operator arity module)
  (let ((sc (operator$find_sort_constraint operator module)))
    (if sc
      (values (sort_constraint$operator sc) sc)
      (values (operator_table$lowest_operator operator arity) nil)))
  )

(defun operator_table$lowest_constrained_operator (operator arity)
  (operator$lowest_constrained_operator operator arity
    (if obj$current_module obj$current_module (operator$module operator))))
  ;&&&&

; op operator$find_sort_constraint_direct : Operator -> Sort_Constraint
(defun operator$find_sort_constraint_direct (operator module)
  (let ((scs (module$sort_constraints module)))
    (dolist (sc scs)
      (when (eq operator (sort_constraint$operator sc))
	(return sc))))
  )

; op operator$get_strict_lower_coarities_direct : Operator Module -> LIST[Sort]
(defun operator$get_strict_lower_coarities_direct (op module)
  (if (operator$polymorphic op) nil
  (let ((op_name (operator$name op))
	(op_ar (operator$arity op))
	(op_coar (operator$coarity op))
	(ops (module$operators module))
	(sort_order (module$sort_order module)))
    (let ((res nil))
      (dolist (op2 ops)
	(let ((new_coarity (operator$coarity op2)))
	(when (and (not (member new_coarity res))
	           (equal op_name (operator$name op2))
		   (eq module (operator$module op2))
		   (sort_order$is_strictly_included_in sort_order
		    (operator$coarity op2) op_coar)
		   (= (length (operator$arity op2)) (length op_ar))
;	           (sort$list_included_in sort_order
;		     (operator$arity op2) op_ar)
		   ; not needed by co-regularity
		   )
	  (push new_coarity res)
	)
      ))
      res
    ))))

; in module will be a $operator_table
; module$operator_table will be hashtable from operators to operator-info

;  operator_info structure moved to operator.lsp

(defun optable$make (size)
  (make-hash-table :test #'eq :size size))

(defun optable$update_operator_info (optab op info)
  (setf (gethash op optab) info))

(defvar optable-op1 nil) (defvar optable-tab1 nil) (defvar optable-val1 nil)
(defvar optable-op2 nil) (defvar optable-tab2 nil) (defvar optable-val2 nil)

; use two since often work with two operators (comparing two terms)
; don't cache if don't really have any info
(defun optable$operator_info (optab op)
  (if (and (eq op optable-op1) (eq optab optable-tab1)) optable-val1
  (if (and (eq op optable-op2) (eq optab optable-tab2)) optable-val2
  (let ((res
	  ;this is the basic definition:
	  (let ((val (gethash op optab)))
	    (if val val
	      (let ((val2 (operator$intrinsic op)))
		(if val2 val2
		  nil
		  )))
	    )))
    (if res
	(progn
	  (setq optable-op2 optable-op1
		optable-tab2 optable-tab1
		optable-val2 optable-val1)
	  (setq optable-op1 op
		optable-tab1 optab
		optable-val1 res)
	  res)
      (operator_info$make))
    ))))

;&&&& not used?
; would need to use if update value when wasn't nil (and possibly cached)
(defun optable$flush_cache ()
  (setq optable-op1 nil optable-tab1 nil optable-val1 nil)
  (setq optable-op2 nil optable-tab2 nil optable-val2 nil))

(defun optable$install_operator (optab op)
  (unless (or (gethash op optab) (operator$intrinsic op))
    (setf (gethash op optab) (operator_info$make)))
  )

(defvar $$min_op_table_size 60)

(defun operator_table$!create (mod)
  (let ((numops (length (module$operators mod))))
  (let ((optab (optable$make
		(max
		 $$min_op_table_size
		 (* 2 numops))))) ;&&&&
    (setf (module$operator_table mod) optab))))

(defun operator_table$!update_operator_info (mod)
  (let ((optab (module$operator_table mod)))
  (when (null optab) (setq optab (operator_table$!create mod)))
  (dolist (op (module$operators mod))
    (operator_info$!install_in_optable optab mod op))
  ))

(defun operator_info$!install_in_optable (optab mod op)
  (let ((val (optable$operator_info optab op)))
  (when (null val) (setq val (operator_info$make)))
  (optable$update_operator_info optab op
      (progn
	(when (null (operator_info$rules_with_same_top val))
	  (setf (operator_info$rules_with_same_top val)
		(rule_ring$create nil)))
	(setf (operator_info$lowest val)
	      (operator_info$compute_lowest_info mod op))
	(setf (operator_info$highest val)
	      (operator_info$compute_highest_info mod op))
	(setf (operator_info$sort_constraint val)
	      (operator$find_sort_constraint_direct op mod))
	val))))

(defvar obj$current_module)

(defun operator_info$compute_lowest_info (mod op)
  (let ((obj$current_module mod))
  (let ((ops (module$operators mod)) (oplist nil))
    (dolist (o ops)
      (when (operator$is_restriction_of o op) (push o oplist)))
    (topo-sort oplist #'operator$is_restriction_of)
  )))

(defun operator_info$compute_highest_info (mod op)
  (let ((obj$current_module mod))
  (let ((ops (module$operators mod)) (oplist nil))
    (dolist (o ops)
      (when (operator$is_same_general_operator op o) ;><><
	(push o oplist))
      )
    (topo-sort oplist #'operator_info$$is_above)
  )))

(defun operator_info$$is_above (op1 op2)
  (let ((coar1 (operator$coarity op1))
	(coar2 (operator$coarity op2))
	(so (module$sort_order obj$current_module)))
  (or (sort_order$is_strictly_included_in so coar2 coar1)
      (and (eq coar1 coar2)
	   (sort$$list_included_in so
             (operator$arity op2) (operator$arity op1))))))

; op operator_info$lowest_operator : Operator LIST[Sort] Module -> Operator
; for polymorphism need to add case for a polymorphic operator &&&&
(defun operator$lowest_operator (op arity module)
  (let ((poly (operator$polymorphic op)))
  (if poly
      (let ((ov (funcall poly op arity module)))
	(if (eq ov 'none)
	    op
	  ov))
  (let ((ops
	  (let ((val
		 (optable$operator_info (module$operator_table module) op)))
	    (if (null val) nil (operator_info$lowest val)))))
  (if (null (cdr ops)) (if ops (car ops) op) ;@@
  (let ((sort_order (module$sort_order module)))
    (let ((cur_coarity (operator$coarity op))
	  (cur_arity (operator$arity op)))
      (dolist (op2 ops op)
	(let ((new_coarity (operator$coarity op2))
	      (new_arity (operator$arity op2)))
	  (when (and (sort_order$is_included_in
		      sort_order new_coarity cur_coarity)
		     (sort$$list_included_in
		      sort_order arity new_arity)
		     (sort$$list_included_in
		      sort_order new_arity cur_arity))
	    (return op2))))
      )))))))

; op operator$highest_operator : Operator Sort Module -> Operator + nil
(defun operator$highest_operator (op srt module)
  (let ((ops
	  (let ((val
	         (optable$operator_info (module$operator_table module) op)))
	    (if (null val) nil (operator_info$highest val))))
	(sort_order (module$sort_order module)))
  (if (null (cdr ops)) (if ops (car ops) op) ;@@
  (let ((eligible_flag (sort_order$is_included_in
			sort_order (operator$coarity op) srt)))
  (let ((op_res (if eligible_flag op nil))
	(cur_arity (if eligible_flag (operator$arity op) nil))
	(cur_coarity (if eligible_flag (operator$coarity op) nil)))
    (dolist (op2 ops op_res)
	(let ((new_arity (operator$arity op2))
	      (new_coarity (operator$coarity op2)))
	(when (and (sort_order$is_included_in sort_order
		    new_coarity srt)
		   (or (null op_res)
		       (and (sort_order$is_included_in sort_order
			     cur_coarity new_coarity)
			    (sort$$list_included_in sort_order
			     cur_arity new_arity))))
	  (return op2)
	))
    ))))))

; op operator$find_sort_constraint : Operator -> Sort_Constraint
(defun operator$find_sort_constraint (operator module)
  (operator_info$sort_constraint
   (optable$operator_info (module$operator_table module) operator)))

; op operator$get_strict_lower_coarities : Operator Module -> LIST[Sort]
(defun operator$get_strict_lower_coarities (op module)
  (if (operator$polymorphic op) nil
  (let ((op_ar (operator$arity op))
	(op_coar (operator$coarity op))
	(ops
	 (let ((val 
	   (optable$operator_info (module$operator_table module) op)))
	   (if val (operator_info$lowest val)
	     nil)))
	(sort_order (module$sort_order module)))
    (let ((res nil))
      (dolist (op2 ops)
        (let ((new_coarity (operator$coarity op2)))
	(when (and (not (member new_coarity res)) ;&&&&
		   (sort_order$is_strictly_included_in sort_order
		    (operator$coarity op2) op_coar)
		   (= (length (operator$arity op2)) (length op_ar)))
	  (push (operator$coarity op2) res)
	))
      )
      res
    ))))

(defun tmp$$same_set_sorts (x y)
  (and (subsetp x y) (subsetp y x)))

(defun optable$!add_rule (optab op rule)
  (when (null optab) (break "SNARK: optable$!add_rule null operator table")) 
  (let ((rule_rhs (rule$rhs rule))
	(rule_lhs (rule$lhs rule))
	(op_info (optable$operator_info optab op)))
  (when (null op_info)
    (setq op_info (operator_info$make))
    (optable$update_operator_info optab op op_info))
  (if (and (not (term$is_var rule_lhs))
	   (consp rule_rhs) ;CMU
	   (not (term$is_var rule_rhs))
	   (eq (term$head rule_lhs) (term$head rule_rhs)))
      (rule_ring$!adjoin_rule
       (operator_info$rules_with_same_top op_info) rule)
    (setf (operator_info$rules_with_different_top op_info)
      (rule$!adjoin_rule
       rule (operator_info$rules_with_different_top op_info)))
    )
  )
  nil)
