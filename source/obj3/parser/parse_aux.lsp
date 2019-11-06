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

;; $Id: parse_aux.lsp,v 205.2.1.1 2003/09/23 13:50:32 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                    Parser Interfacing
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Tim Winkler ;;;; Created: 7/8/86 modified 7/29/86

;;;;;;;;;;;;;;;; Summary ;;;;;;;;;;;;;;;;
; omitted

; op opvar$syntactic_type : Operator+Variable -> Synactic_Type
;  Synactic_Type: variable antefix latefix juxtaposition
; name: term_parser$syntactic_type
(defun term_parser$syntactic_type (e)
  (let ((typ (type-of e)))
    (if (eq 'operator typ)
	(operator$syntactic_type e)
      'variable)
  ))

(defun operator$parse_category (op)
  (operator$syntactic_type_from_name (operator$name op)))
(defun operator$syntactic_type_from_name (op_name)
  (cond
   ((equal "_" (car op_name))
    (cond
     ((equal "_" (cadr op_name)) 'juxtaposition)
     (t 'latefix)))
   (t 'antefix)))
(defun operator$compute_syntactic_type (op)
  (operator$syntactic_type_from_name (operator$name op)))

(defvar obj$current_module)

(defvar *parse$raw_parse* 'none)

(defvar saved_ambiguous nil)

(defun parse$parse (module preterm &optional (sort *obj$sort_Universal*))
  (if (null preterm) (progn
    (princ "Empty input -- no parse") (terpri)
    (term$make_built_in_constant obj_ERR$sort_Err '(empty)))
  (let ((obj$current_module module))
  (let ((res
	   (parser$parser preterm module parser$max_precedence sort)))
  (let ((final_well-defined (mapcan #'(lambda (e)
                                        (when (and (null (cdr e))
                                                   (not (term$ill_defined
                                                         (caar e))))
                                              (list (caar e))))
                              res)))
  (let ((final (if final_well-defined final_well-defined
                 (mapcan #'(lambda (e)
                             (when (null (cdr e)) (list (caar e))))
                   res))))
    (if (null final)
      (progn
	(princ "No successful parse for the input: ") (terpri)
	(parser$diagnose module preterm sort)
	(term$make_built_in_constant obj_ERR$sort_Err preterm))
    (let ((raw_parse
	       (if (null (cdr final)) (car final)
		 (progn
		   (princ "Ambiguous term, two parses are:") (terpri)
		   (term$print (car final)) (princ " -versus-") (terpri)
		   (term$print (cadr final)) (terpri)
		   (princ "differences are:") (terpri)
		   (parse$show_diff (car final) (cadr final)) (terpri)
		   (princ "Arbitrarily taking the first as correct.") (terpri)
		   (setq saved_ambiguous final)
		   (car final)))))
      (setq *parse$raw_parse* raw_parse)
      (parse$convert raw_parse)
     )
    )))))))

(defun parser$diagnose (module preterm sort)
  (if (null preterm)
    (progn (princ "Input is emtpy--no parse") (terpri))
  (progn
  ;(princ "No successful parse for the input: ") (terpri)
  (print$simple_princ_open preterm) (terpri)
  (let ((prefix nil) (suffix preterm)
	(flags (make-list (length preterm)))
	(len (length preterm)))
    (when obj$verbose
      (princ "partial parses are: ") (terpri))
    (loop
     (when (null suffix) (return))
     (when obj$verbose
       (princ "--- ")
       (when prefix (print$simple_princ_open prefix))
       (princ " _|_ ")
       (when suffix (print$simple_princ_open suffix))
       (terpri))
     (let ((res (parser$parser suffix module parser$max_precedence
			       sort)))
       (mapc #'(lambda (e)
		 (let ((lenp (length prefix)))
		   (dotimes (i (- len (+ lenp (length (cdr e)))))
		     (setf (nth (+ lenp i) flags) t)
		     ))
		 (when obj$verbose
		   (princ "    ==> ")
		   (when prefix
		     (print$simple_princ_open prefix) (princ " ___ "))
		   (let ((tm (caar e)))
		     (if (and (null (term$subterms tm))
			      (eq 'constant (car
					     (operator$name (term$head tm)))))
		       (term$print tm)
		       (progn
			 (princ "(")
			 (term$print tm)
			 (princ ").")
			 (print$sort_name obj$current_module
					  (term$sort tm)))))
		   (when (cdr e)
		     (princ " ___ ")
		     (print$simple_princ_open (cdr e)))
		   (terpri)))
	     res)
       )
     (setq prefix (append prefix (list (car suffix))))
     (setq suffix (cdr suffix))
     )
    (princ "partial descriptions: ") (terpri)
    (dotimes (i len)
      (if (nth i flags)
	(princ " _")
	(progn
	  (princ " ")
	  (princ (nth i preterm)))))
    (terpri)
    (dotimes (i len)
      (if (and (null (nth i flags))
	       (or (= 0 i)
		   (nth (1- i) flags)))
	(princ " _[")
	(if (and (not (= 0 i))
		 (nth i flags)
		 (null (nth (1- i) flags)))
	  (princ "]_ ")
	  (princ " ")))
      (princ (nth i preterm))
      (print$chk))
    (when (null (nth (1- len) flags)) (princ "]_"))
    (terpri)
    )
  )))

(defun parse$parse_ground (id module preterm
			   &optional (sort *obj$sort_Universal*))
  (let ((trm (parse$parse module preterm sort)))
    (when (term$is_containing_var trm)
      (princ "Warning: in ")
      (princ id)
      (princ " term contains a variable: ")
      (term$print trm) (terpri))
    trm
    )
  )

; added for use in parser$parses
(defun parse$convert_bi (term)
  ;; convert built-in constants to appropriate form
  (cond
   ((term$is_var term) term)
   (t (let ((hd (term$head term)))
    (cond
     ((and (null (term$subterms term))
           (eq 'constant (car (operator$name hd))))
      (term$make_built_in_constant (operator$coarity hd)
                                   (cadr (operator$name hd))))
     (t (if (term$ill_defined term)
	  (term$make_directly_ill_term hd
	    (mapcar #'parse$convert_bi (term$subterms term)))
	  (term$make_term hd
	    (mapcar #'parse$convert_bi (term$subterms term)))))
     )))
   ))

(defun parse$convert (term)
  ;; convert built-in constants to appropriate form
  (cond
   ((term$is_var term) term)
   ((term$is_built_in_constant term) term)
   (t (let ((hd (term$head term)))
    (cond
     ((and (null (term$subterms term))
           (eq 'constant (car (operator$name hd))))
      (term$make_built_in_constant (operator$coarity hd)
                                   (cadr (operator$name hd))))
     (t (term$make_term_with_retracts hd
          (mapcar #'parse$convert (term$subterms term))))
     ))) 
   ))

(defun term$make_term_with_retracts (hd subs)
  (let ((mod (if obj$current_module obj$current_module
	       (operator$module hd))))
  (let ((so (module$sort_order mod)))
    (term$make_term_with_sort_check hd
      (mapcar #'(lambda (tm s) (term$retract_if_needed so tm s))
	subs (operator$arity hd)))
  )))

(defun term$retract_if_needed (so tm s)
  (let ((stm (term$sort tm)))
  (if (sort_order$is_included_in so stm s) tm
    (term$make_term (operator$make_retract stm s) (list tm)))
  ))

(defun parse$show_diff (t1 t2)
  (if (term$is_var t1)
      (if (term$is_var t2)
	  (unless (and (equal (variable$name t1) (variable$name t2))
		       (eq (variable$initial_sort t1)
			   (variable$initial_sort t2)))
	    (term$print t1) (princ " -versus-") (terpri)
	    (term$print t2) (terpri))
	(progn
	  (term$print t1) (princ " -versus-") (terpri)
	  (term$print t2) (terpri)))
  (if (term$is_var t2)
	(progn
	  (term$print t1) (princ " -versus-") (terpri)
	  (term$print t2) (terpri))
  (if (and (operator$is_same_operator (term$head t1) (term$head t2))
       (let ((so (module$sort_order obj$current_module)))
       (or
	(and
	 (sort$$list_included_in so
	  (operator$arity (term$head t1)) (operator$arity (term$head t2)))
	 (sort_order$is_included_in so
	  (operator$coarity (term$head t1)) (operator$coarity (term$head t2))))
	(and
	 (sort$$list_included_in so
	  (operator$arity (term$head t2)) (operator$arity (term$head t1)))
	 (sort_order$is_included_in so
	  (operator$coarity (term$head t2)) (operator$coarity (term$head t1))))
	)))
    (do ((lst1 (term$subterms t1) (cdr lst1))
	 (lst2 (term$subterms t2) (cdr lst2)))
      ((null lst1))
      (parse$show_diff (car lst1) (car lst2))
    )
   (unless (or (eq 'constant (car (operator$name (term$head t1))))
	       (eq 'constant (car (operator$name (term$head t2)))))
      (print$name (term$head t1))
      (princ " -versus- ") (print$name (term$head t2)) (terpri)
      (when (or (not (null (term$subterms t1)))
		(not (null (term$subterms t2))))
	(princ "in") (terpri)
        (term$print t1) (princ " -versus-") (terpri)
        (term$print t2) (terpri)
       )
    )
  ))))

; produces a list of initial complete parses given all parses as arg
(defun parser$complete_parses (module parselist)
  (declare (ignore module))
;  (let ((obj$current_module module)) ...)  ;believe not necessary
  (mapcan #'(lambda (x) (if (null (cdr x))
			    (list (parse$convert_bi (caar x)))
			  nil))
	  parselist))

; produces a list of initial complete parses; nil for error
(defun parser$parses (module preterm
			   &optional (sort *obj$sort_Universal*))
  (if (null preterm) nil
  (let ((obj$current_module module))
  (let ((val (parser$parser preterm module parser$max_precedence sort)))
    (mapcan #'(lambda (x) (if (null (cdr x))
			      (list (parse$convert_bi (caar x)))
			    nil))
      val)))))

; takes list of first parses and a sort and comes back with
; nil -- none; or a list of possible parses
; (any well-defined preferred to ill-defined)
(defun parser$find_parse (module parses sort)
  (let ((obj$current_module module))
  (let ((so (module$sort_order module))
	(any nil) (ill nil) (well nil))
  (dolist (tm parses)
    (if (sort_order$is_included_in so (term$sort tm) sort)
      (if (term$ill_defined tm)
	(when (null well) (push tm ill))
	(push tm well)
	)
      (when (and (null (or ill well))
		 (sort_order$is_in_same_connected_component so
		   (term$sort tm) sort))
	  (push tm any))
      )
    ) ; dolist
  (if well well
    (if ill ill
      any))
  )))

; takes list of first parses and a sort and comes back with
; nil -- none; or a list of possible parses
; (any well-defined preferred to ill-defined)
; very similar to above, but is required to directly satisfy sort restriction
(defun parser$find_parse_strict (module parses sort)
  (let ((obj$current_module module))
  (let ((so (module$sort_order module))
	(ill nil) (well nil))
  (dolist (tm parses)
    (if (sort_order$is_included_in so (term$sort tm) sort)
      (if (term$ill_defined tm)
	(when (null well) (push tm ill))
	(push tm well)
	)
      )
    ) ; dolist
  (if well well
    (if ill ill
      nil))
  )))

; takes list of first parses and a list of sorts and comes back with
; nil -- none; or a list of possible parses
; (any well-defined preferred to ill-defined)
; very similar to above, but is required to directly satisfy sort restriction
(defun parser$find_parse_strict_sorts (module parses sorts)
  (let ((obj$current_module module))
  (let ((so (module$sort_order module))
	(ill nil) (well nil))
  (dolist (tm parses)
    (if (member (term$sort tm) sorts
		:test #'(lambda (x y) (sort_order$is_included_in so x y)))
      (if (term$ill_defined tm)
	(when (null well) (push tm ill))
	(push tm well)
	)
      )
    ) ; dolist
  (if well well
    (if ill ill
      nil))
  )))

; given list of parses of lhs and rhs (as from parser$parses) looks
; for compatible pair
(defun parser$find_rule_pair (module lhslst rhslst)
  (let ((obj$current_module module))
  (let ((so (module$sort_order module))
	(ok nil) (retr nil) (ill nil) (bad nil))
  (dolist (lhs lhslst)
    (let ((sl (term$sort lhs)))
    (dolist (rhs rhslst)
      (let ((sr (term$sort rhs)))
      (if (term$ill_defined lhs)
	  (if (or (sort_order$is_included_in so sr sl)
		  (sort_order$is_in_same_component_safe so sl sr))
	    (push (list lhs rhs) bad)
	    nil ;nothing
	    )
      (if (sort_order$is_included_in so sr sl)
	(if (term$ill_defined rhs)
	  (push (list lhs rhs) ill)
	  (push (list lhs rhs) ok))
      (if (sort_order$is_in_same_component_safe so sl sr)
	(if (term$ill_defined rhs)
	  (push (list lhs rhs) ill)
	  (push (list lhs rhs) retr))
	;nothing
	))))
    )))
  (if ok ok
    (if retr retr
      (if ill ill
	(if bad (cons 'bad-lhs bad)
	  nil))))
  )))

; used in modexp$compute_op_mapping
(defun parse$quiet_parse (module preterm &optional (sort *obj$sort_Universal*))
  (if (null preterm)
    (term$make_built_in_constant obj_ERR$sort_Err '(empty))
  (let ((obj$current_module module))
  (let ((res (parser$parser preterm module parser$max_precedence sort)))
  (let ((final_well-defined (mapcan #'(lambda (e)
                                        (when (and (null (cdr e))
                                                   (not (term$ill_defined
                                                         (caar e))))
                                              (list (caar e))))
                              res)))
  (let ((final (if final_well-defined final_well-defined
                 (mapcan #'(lambda (e)
                             (when (null (cdr e)) (list (caar e))))
                   res))))
    (if (null final)
	(term$make_built_in_constant obj_ERR$sort_Err preterm)
    (let ((raw_parse (car final)))
      (parse$convert raw_parse)
     )
    )))))))

;;;; &&&& should eliminate this
;(defun sort$is_included_in (s1 s2 so)
;  (or (eq s1 s2)
;      (sort_order$is_included_in so s1 s2)))

; op parse$!update_parse_information : Module
;       SET[Operator] SET[Variable] -> {cur_mod}
; create parse_dictionary; juxtaposition for module
;   and update attributes of operators
(defun parse$!update_parse_information (module)
  (let ((operators (module$operators module))
	(variables (module$variables module)))
  (when (null (module$parse_dictionary module))
	(module$!init_parse_dictionary module))
  (let ((mod_dict (module$parse_dictionary module)))
  (dolist (s (module$sorts module))
    (when (sort$is_built_in s)
      (dictionary$!add_built_in_sort
       mod_dict
       (let ((lisp_info (sort$info s)))
	 (list* (car lisp_info) s (cdr lisp_info))))))
  (dolist (op operators)
    (let ((syn_typ (operator$compute_syntactic_type op)))
    (operator$!replace_syntactic_type op syn_typ)
    (case syn_typ
      (antefix (dictionary$!add_info_on_token
		 mod_dict
		 (car (operator$name op))
		 op))
      (latefix (dictionary$!add_info_on_token
		 mod_dict
		 (cadr (operator$name op))
		 op))
      (juxtaposition
       (module$!add_juxtaposition module op))
      (otherwise (break "SNARK: parse$!update_parse_information")))
    ))
  (dolist (var variables)
    (dictionary$!add_info_on_token
      mod_dict
      (variable$name var)
      var))
  (parse$compress_overloaded_operators module mod_dict)
  (module$!replace_juxtaposition module
    (operator$compress_overloaded_set (module$sort_order module)
      (module$juxtaposition module)))
  )))

(defun parse$compress_overloaded_operators (module dict)
  (let ((obj$current_module module))
  (let ((sort_order (module$sort_order module))
	(table (dictionary$table dict)))
  (maphash #'(lambda (ky val)
	       (setf (gethash ky table)
                 (operator$compress_overloaded_set
		  sort_order
		  val)))
    table)
  ;&&&& 25 Aug 87 this is just a temporary patch
  (mapcar #'(lambda (op)
	      (let* ((nm (operator$name op))
		     (ky (if (equal "_" (car nm)) (cadr nm) (car nm)))
		     (val (gethash ky table)))
		(unless (or (null val) (null (cdr val)))
		  (setf (gethash ky table)
			(operator$compress_overloaded_set
			 sort_order
			 (gethash ky table))))))
    (module$operators module))			   
  )))

;;; this has side-effect of updating overloading info on operators
(defun operator$compress_overloaded_set (sort_order items)
  (let ((ops nil) (res nil))
    (dolist (i items)
      (if (not (and (typep i 'operator)
		    (operator$arity i)))
	(push i res)
	(push i ops)))
    (dolist (op ops)
       (pushnew (operator$select_most_general_version_of sort_order op ops)
	     res :test #'eq))
    res
  ))

;;; side-effect (for now) update via operator$!mark_strictly_overloaded
;;; largest arity and smallest coarity
(defun operator$select_most_general_version_of (sort_order op ops)
  (let ((res_op op))
    (dolist (op2 ops)
      (when (operator$is_instance_of sort_order res_op op2)
	(setq res_op op2)))
    (when (not (eq op res_op))
	  (operator$!mark_strictly_overloaded res_op)) ;side-effect
    res_op
  ))

;;; same name; larger coarity; smaller arity smaller coarity too
(defun operator$is_instance_of (sort_order op1 op2)
  (and
   (equal (operator$name op1) (operator$name op2))
   (or ;7 Jun 88
    (not (eq (operator$coarity op1) (operator$coarity op2)))
    (not (operator$top_eq (operator$arity op1) (operator$arity op2))))
   (sort_order$is_included_in sort_order ;7 Jun 88 added strictly
     (operator$coarity op1) (operator$coarity op2))
   (sort$$list_included_in sort_order
     (operator$arity op1) (operator$arity op2))
   ))

(defun operator$top_eq (l1 l2) ;7 Jun 88
  (or (eq l1 l2)
      (do ((lst1 l1 (cdr lst1))
	   (lst2 l2 (cdr lst2)))
	((or (null lst1) (null lst2)) (and (null lst1) (null lst2)))
	(unless (eq (car lst1) (car lst2)) (return nil))))
  )

(defun sort$in_same_connected_component (s1 s2 so)
  (sort_order$is_in_same_connected_component so s1 s2))

(defun term$ill_defined (tm)
  (and (consp tm)
       (consp (car tm))
       (eq 'ill-defined (cdr (car tm)))))
(defun term$make_directly_ill_term (head subterms)
  (cons (cons head 'ill-defined) subterms))
(defun term$make_inheritedly_ill_term (head subterms)
  (cons (cons head 'ill-defined) subterms))

;;;; generalized sort relationship
;;;;   for sort_order use "current module"
;@@@
;(defun sort$is_included_in (x y)
;  (sort_order$is_included_in (module$sort_order obj$current_module)
;    x y))
