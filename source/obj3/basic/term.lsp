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

;; $Id: term.lsp,v 206.1 2003/09/23 13:41:13 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;           Terms as they are used in OBJ
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Claude Kirchner ;;;; created: June 6, 86 ;;;;

;;;;;;;;;;;;;;;; Summary ;;;;;;;;;;;;;;;;
;;;; First the functions provided
; op term$make_term : operator list[term] -> term
; op term$make_term_with_sort_check : operator list[term] -> term
; op term$is_var : term -> bool
; op term$is_constant : term -> bool
; op term$is_reduced : term -> bool
; op term$is_containing_var : term -> bool
; op term$is_not_well_lowest_parsed : term -> bool
; op term$subterms  : term -> list[term]; signals(is_a_variable)
; op term$arg_1 : term -> term; signals(no_subterm)
; op term$arg_2 : term -> term; signals(no_subterm)
; op term$arg_n : term occurrence -> term; signals(no_such_subterm)
; op term$sort : term -> sort
; op term$lowest_sort : term -> sort
; op term$head : term -> operator; signals(is_variable)
; op term$!change_head_operator: Term Operator -> Term .
; op term$!replace : term term -> term
; op term$!mark_as_reduced : term -> term
; op term$!mark_as_lowest_parsed : term -> term
; op term$!mark_as_not_lowest_parsed_on_top : term -> term
; op term$!update_lowest_parse_on_top term : term -> term
; op term$!normalize_for_morphism_rules: Term -> Term 
; op term$clean_term : term -> term
; op term$equal : term term -> bool
; op term$similar : term term -> bool
; op term$equational_equal : term term -> bool
; op term$!print : term -> void [output]
; op term$right_associative_normal_form : term -> term
; op term$list_assoc_subterms : term operator -> list[term]
; op term$make_right_assoc_normal_form : operator list[term] -> term
; op term$make_right_assoc_normal_form_with_sort_check : 
;                                        operator list[term] -> term
; op term$copy_and_returns_list_variable: Term -> Term List[Variable]
; op term$copy_using_variable: Term List[Variable] -> Term 
; op term$make_built_in_constant : Sort Value -> Term
; op term$built_in_value : Term -> Value
; op term$is_built_in_constant : Term -> Bool
; op term$vars : Term -> SET[Variable]
; op term$equal_built_in : Term Term -> Bool
; op term$have_same_top : Term Term -> Bool


;;;;;;;;;;;;;;;; REPRESENTATION ;;;;;;;;;;;;;;;;

; --- the problem of the representation of terms is not easy ...
; In the following, we take the following representation:
;               . 
;              / \
;             .  list of subterms
;            / \
;           op status
;
; * op is the top operator of term
; * status contains information about the term it can be:
;   + an indication that the term is well lowest parsed (status = 1)
;   + an indication that the term is in normal form (status = 2)
;   + an indication that the term is not well lowest parsed (status = 0)
;
; a variable is represented by
;        .
;       / \
;      x   .
;         / \
;      gen   .
;           / \
;       sorts work

; See the variable cluster

; It is not the same in obj2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; op term$is_var = term -> bool
;(defun term$is_var (term)
;  (atom (car term))
;  )
(defmacro term$is_var (term)
  `(atom (car ,term)))

; op term$is_constant : term -> bool
(defmacro term$is_constant (term)
  `(null (cdr ,term)))

;% returns a term f(t1,...,tn) from the operator "f" and the
;% list of terms (t1,...,tn).
;% It is very important to notice that f is supposed to be THE operator
;% such that f(t1,...tn) is well lowest parsed.
;% --- "free%maketerm" in obj2

; op term$make_term : operator {subterms}list[term] -> term
(defmacro term$make_term (f subterms)
  `(cons (cons ,f 1) ,subterms))

;% returns the top operator of a term if it is a non variable one.
;% --- "%top" in obj2

; op term$head : term -> operator; signals(is_variable)
;(defun term$head (term)
;  (caar term)
;  )
(defmacro term$head (term)
  `(caar ,term))

;% returns the list of subterms of the term t. For example
;% if t = f(t1,...,tn) then it returns the list t1,..., tn.

; op term$subterms  : term -> list[term]; signals(is_a_variable)
;(defun term$subterms (term)
;  (cdr term)
;  )
(defmacro term$subterms (term)
  `(cdr ,term))

;% returns the term f(t1, t2)

; op term$make_binary_term : operator term term -> term

; Returns a new term built on the subterms in "subterms" which
; are supposed to be lowest parsed and on the operator "low_f"
; which correspond to the lowest operator such that the term
; f(t1,..,tn) is lowest parsed.

; op term$make_term_with_sort_check : operator {subterms}list[term] -> term
(defun term$make_term_with_sort_check (f subterms)
  (cond
   ((every2 #'(lambda (x y) (eq x (term$sort y)))
        (operator$arity f) subterms)
    (term$make_term f subterms))
   ((operator$is_a_retract f)
    (if (sort_order$is_included_in
	 (module$sort_order
	  (if obj$current_module obj$current_module
	    (sort$module (operator$coarity f))))
	 (term$sort (car subterms))
	 (operator$coarity f))
	(car subterms)
      (term$make_term f subterms)))
   (t (term$make_term
       (let ((res (operator_table$lowest_operator f ;@@ 12/10/91
	              (mapcar #'term$sort subterms))))
	 (if (eq 'none res) f res))
       subterms) )
   )
  )

; specialization of the above
(defun term$make_term_with_sort_check_binary (f subterms)
  (let ((s1 (term$sort (car subterms)))
	(s2 (term$sort (cadr subterms))))
  (if (let ((ar (operator$arity f)))
	(and (eq (car ar) s1) (eq (cadr ar) s2)))
      (term$make_term f subterms)
    (term$make_term
        (operator$lowest_operator f (list s1 s2)
            (if obj$current_module obj$current_module (operator$module f)))
	subterms)
    ))
  )

;% returns the first son of the term t. Example if t = f(t1,...,tn)
;% then it returns t1

; op term$arg_1 : term -> term; signals(no_subterm)
(defmacro term$arg_1 (term)
  `(cadr ,term)
  )
  
;% Same thing with the second component

; op term$arg_2 : term -> term; signals(no_subterm)
(defmacro term$arg_2 (term)
  `(caddr ,term)
  )


; op term$arg_3 : term -> term; signals(no_subterm)
(defun term$arg_3 (term)
  (caddr term)
  )

; op term$arg_n : term occurrence -> term; signals(no_subterm)
(defun term$arg_n (term occ)
  (nth (1- occ) (term$subterms term))
  )
  
;% returns the sort of the top symbol of "t".
;% Since in OBJ3 the terms are supposed to be always lowest sorted
;% "sort" and "lowest_sort" are the same.
;% --- "sort%of" en obj2

; op term$sort : term -> sort
(defun term$sort (term)
  (if (term$is_var term)
      (variable$initial_sort term)
    (operator$coarity (term$head term))
    )
  )

;% returns the lowest sort of the term "t"
;% --- "lowest%sort" in obj2

; op term$lowest_sort : term -> sort
(defun term$lowest_sort (term)
  (declare (ignore term))
  (print "term$lowest_sort is not implemented in OBJ3")
  )

;% returns true iff the term is already reduced or is a variable
;% --- "%reduced" in obj2

; op term$is_reduced : term -> bool

(defun term$is_reduced (term)
 (or (term$is_var term)
     (let ((status (cdar term)))
       (or (eql 2 status)
	   (consp status)))
     ;(term$is_built_in_constant term)
     )
 )

; Returns true iff the term contains at least a variable.

; op term$is_containing_var : term -> bool
(defun term$is_containing_var (term)
  (or (term$is_var term)
      (dolist (subt (term$subterms term) nil) ;default nil
	(when (term$is_containing_var subt)
	  (return t))
	)))

; returns true iff the term is well lowest parsed

; op term$is_well_lowest_parsed : term -> bool
(defun term$is_well_lowest_parsed (term)
  (or (term$is_var term) 
      (not (eql 0 (cdar term)))
      ;(eql 1 (cdar term))
      ;(eql 2 (cdar term))
      ;(term$is_built_in_constant term)
      )
  )

; returns true iff the term is NOT well lowest parsed

; op term$is_not_well_lowest_parsed : term -> bool
(defun term$is_not_well_lowest_parsed (term)
  (and (not (term$is_var term))
       (or (eql 0 (cdar term))
	   (consp (cdar term)))) ; subsorts on bi's
  )

;% returns the set of variables in "t"
;% --- "setvar%term in obj2

; op term$vars : Term -> SET[Variable]
(defun term$vars (term)
  (cond
   ((term$is_var term) (list term))
   (t (let ((res nil))
	(dolist (st (term$subterms term))
	  (setq res (union (term$vars st) res :test #'eq)))
	res))))

; Change the top operator of term to be the new one. It suppose consistancy!!
; The reduce flag is cleaned (i.e. put to false)

; op term$!change_head_operator: Term Operator -> Term~
(defun term$!change_head_operator (term op)
  (rplaca (car term) op)
  )

;; returns nothing but modify "t1" on place by "t2"
;; It allows to reduce "on place" (or by side effect) a term.
;; In some sense it is an affectation

; op term$!replace : term term -> term
(defun term$!replace (t1 t2)
  (rplaca t1 (car t2))
  (rplacd t1 (cdr t2))
  t1
  )

;; mark the term as all ready in normal form
;; the term is modified (marked)

; op term$!mark_as_reduced : term
(defun term$!mark_as_reduced (t1)
  (unless (term$is_built_in_constant t1) ;&&&&
   (setf (cdar t1) 2))
  t1
  )

;; mark the term as not well lowest parsed on top

; op term$!mark_as_not_lowest_parsed_on_top : term
(defun term$!mark_as_not_lowest_parsed_on_top (term)
  (alt ;&&&&
   :obj3
   (progn
     (setf (cdar term) 0)
     term)
   :obj3-devel
   (progn
     (when (term$is_built_in_constant term)
       (break "SNARK: term$!mark_as_not_lowest_parsed_on_top on bi constant"))
     (setf (cdar term) 0)
     term)
   )
  )

;; Returns a new term which is not annotated, that is such that the
;; "status" field is set to "1".

; op term$clean_term : Term -> Term

(defun term$clean_term (term)
  (if (or (term$is_var term)
	  (term$is_built_in_constant term))
      term
    (term$make_term 
     (caar term)
     (mapcar #'term$clean_term (cdr term)))
    )
  )

;; mark the term as well lowest parsed (but not necessary in normal form)

; op term$!mark_as_lowest_parsed : Term -> Term

(defun term$!mark_as_lowest_parsed (term)
  (unless (term$is_built_in_constant term)
    (setf (cdar term) 1)
    )
    term
  )

; Modifies "term" in such a way that the sort of the top operator
; is the lowest possible with respect to the sort of the subterms and the 
; current morphism rules, that is:
;   1) the "usual" morphism rules, induced by the disambiguation
;   2) the extensions of the usual mophism rules for associativity, if
;      the top symbol is associative (at least)
;   3) the morphism rules induced by the sort constraints and their extensions
;      if the symbol is associative (at least).
;   4) the retract elimination: if sort(t) <= s then r_s(t) = t fi
; Z: "term" is supposed to be a non variable and non constant term.
; NOTE that it will be usefull to call this function only when it is sure that
; it is needed.

; op term$!update_lowest_parse_on_top: Term -> Term~

; term is returned as a value
; as well as being modified by side-effect
(defun term$!update_lowest_parse_on_top (term)
  (unless (term$is_var term)
  ;(unless (term$is_built_in_constant term) ;&&&& necessary? ;2 Jun 88 out
  (block the_end
  (let* ((head (term$head term))
	 (son nil)
	 (t1 nil)
	 (t2 nil)
	 (mod (if obj$current_module obj$current_module
		(if (term$is_var term)
		    (sort$module (variable$initial_sort term))
	       (operator$module head))))
	 (sort_order (module$sort_order mod)))
    (when (term$is_built_in_constant term)
      (let ((so (module$sort_order
		 (if obj$current_module obj$current_module
		   (sort$module (operator$coarity head)))))
	    (isrt (term$sort term))
	    (val (term$built_in_value term)))
      (let ((subs (sort_order$lower_sorts so isrt))
	    (srt isrt))
	(dolist (s subs)
	  (if (and (sort_order$is_included_in so s srt)
		   (sort$info s)
		   (funcall (cadddr (sort$info s)) val))
	    (setq srt s))
	)
	(unless (eq isrt srt)
	  (rplaca (car term) (sort$constructor srt)))
      ))
      (return-from the_end));; when
    ;; first possible retracts:
    (when (operator$is_a_retract head)
	  (when (sort_order$is_included_in
		   (module$sort_order
		    (if obj$current_module obj$current_module
		      (sort$module (operator$coarity head))))
		 (term$sort (setq son (term$arg_1 term)))
		 (operator$coarity head))
		(term$!replace term son)
		);; when
	  (return-from the_end)
	  );; when
    ;; "standard" morphism rules
    (rplaca (car term) (operator_table$lowest_operator
			head
			(mapcar #'term$sort (term$subterms term))
			)
	    )
    ;; extensions for associativity: if s and s' are sorts s.t. s < s' then
    (when (operator$is_associative head)
	  ;; &&&& the following transformation tends to put term
	  ;; term into standard form even when sort doesn't decrease
	  (when (and
		 (not (term$is_var (setq son (term$arg_1 term))))
		 (operator$is_associative_restriction_of ;&&&&
		  (term$head son) head)
		 ;(eq (term$head (setq son (term$arg_1 term))) head) &&&&
		 (eq (term$sort (setq t1 (term$arg_2 son)))
		     (term$sort (setq t2 (term$arg_2 term))))
		 (sort_order$is_included_in sort_order
		     (term$sort t2) (term$sort (term$arg_1 son)))
		 )
		;; we are in the following configuration
		;;              fs'   ->    fs'
		;;          fs'    s     s'     fs
		;;       s'    s              s   s
		;; so:
		(rplacd term
			(list (term$arg_1 son)
			      (term$!update_lowest_parse_on_top
			       (term$make_term head (list t1 t2)))
			          ;&&&& list was omitted
			      )
			)
		);; when
	  ;;&&&& would only like to do the following if the
	  ;; sort really decreases
	  (when 
	        (and
		 ;(and (eq head (term$head (setq son (term$arg_2 term)))) &&&&)
		 (not (term$is_var (setq son (term$arg_2 term))))
		 (operator$is_associative_restriction_of
		  (term$head son) head)
		 (eq (term$sort (setq t1 (term$arg_1 term)))
		     (term$sort (setq t2 (term$arg_1 son))))
		 (sort_order$is_included_in sort_order
		  (term$sort t1) (term$arg_2 son))
		 )
		;&&&& don't you want to require that fs not-= fs'??
		;; we are in the following configuration
		;;              fs'       ->       fs'
		;;            s     fs'         fs     s'
		;;                s    s'     s   s
		;; so:
		(rplacd term
			(list (term$!update_lowest_parse_on_top
			       (term$make_term head (list t1 t2)))
			          ;list was omitted &&&&
			      (term$arg_2 son) ;&&&& was arg_1
			      )
			)
		);; when
	  );; when
    ;;  necesary to have true lowest parse
    (when (operator$is_commutative head)
      (let* ((t1 (term$arg_1 term))
	     (t2 (term$arg_2 term))
	     (alt_op (operator_table$lowest_operator head
		       (list (term$sort t2) (term$sort t1)))))
	(when (not (eq alt_op head))
	  (term$!replace term (term$make_term alt_op (list t2 t1)))
	) ;; when
      )) ;; let*/when
    ) ;; let
  );; block
  ;);; unless ;2 Jun 88 out
  term
  ))

; op term$!normalize_for_morphism_rules: Term -> Term 

; normalize the term for the current morphism rules.
; It uses the previous function in a bottom up fashion.
; Z: The term is modified "in place" i.e. by side effect. 
;    The root must remain the same
; U: Used in rule_gen$add_specialization

(defun term$!normalize_for_morphism_rules (term)
  (cond ((term$is_var term) term)
	((term$is_constant term) term)
	(t (dolist (subterm (term$subterms term))
		   (term$!normalize_for_morphism_rules subterm)
		   ) ;; dolist
	   (term$!update_lowest_parse_on_top term)
	   (term$!mark_as_lowest_parsed term)
	   )
	) ;; cond
  )

;% returns true iff "t1" and "t2" are the same.

;&&&& not used?
; op term$equal : term term -> bool
(defun term$equal (t1 t2)
 (eq t1 t2)
  )

;% returns true iff "t1" and "t2" are similar
;% --- "equal%term" in obj2

; op term$similar : term term -> bool
(defun term$similar (t1 t2)
  (cond ((term$is_var t1)  ; was (eq t1 t2) &&
	 (and (term$is_var t2) (variable$is_equal t1 t2)))
	((term$is_var t2) nil)
	((term$is_built_in_constant_fast t1)
	 (term$equal_built_in_fast t1 t2))
	((term$is_built_in_constant_fast t2) nil)
	((eq (term$head t1) (term$head t2))
;	 (term$$similar_subterms (term$subterms t1) (term$subterms t2))
	 (let ((subs1 (term$subterms t1)) (subs2 (term$subterms t2)))
	   (loop
	    (when (null subs1) (return (null subs2)))
	    (unless (term$similar (car subs1) (car subs2)) (return nil))
	    (setq subs1 (cdr subs1)  subs2 (cdr subs2))
	   )
	 ))
	(t nil))
  )

;;; help the previous one

;(defun term$$similar_subterms (term_list1 term_list2)
;  (cond ((null term_list1)(null term_list2))
;	((term$similar (car term_list1) (car term_list2)) 
;	 (term$$similar_subterms (cdr term_list1) (cdr term_list2)))
;	(t nil))
;  )

;% theory-equal returns true iff "t1" and "t2" are E-equal 
;%in the theory E considered.

; op term$equational_equal : term term -> bool
; t1,t2 both taken "literally"
(defun term$equational_equal (t1 t2)
  (cond
;   ((or (term$is_var t1) (term$is_var t2)) (eq t1 t2))
   ((term$is_var t1) (and (term$is_var t2) (variable$is_equal t1 t2)))
   ((term$is_var t2) nil)
   ((or (term$is_built_in_constant_fast t1)
	(term$is_built_in_constant_fast t2))
    (term$equal_built_in_fast t1 t2))
   (t (let ((f1 (term$head t1))
	    (f2 (term$head t2)))
      (and (operator$is_same_operator_fast f1 f2)
	(theory$E_equal_in_theory (operator$theory f1) t1 t2)
	)))))

;% It is very important to realize that the associative normal
;% form of a term must be a correct term in order to use the standard
;% operations of replacement and "parcours". For example the associative 
;% normal form of 1+((2+2)+1) must be (1+(2+(2+1))) and NOT (+ (1 2 2 1))
;% which represent the associative class.

; op term$right_associative_normal_form : term -> term
(defun term$right_associative_normal_form (t1)
						;p (term$!print t1)
  (cond ((term$is_constant t1) t1)
	((term$is_var t1) t1)
	( t (let ((h_op (term$head t1)))
	      (cond
	       ((theory$contains_associativity (operator$theory h_op))
		(term$make_right_assoc_normal_form 
		 h_op 
		 (mapcar 'term$right_associative_normal_form 
			 (term$list_assoc_subterms t1 h_op))))
	       (t (term$make_term 
		   h_op 
		   (mapcar 'term$right_associative_normal_form 
			   (term$subterms t1))))
	       )
	      ))
	)
  )

;% returns the list of subterms of "t1" which are not beginning
;% with the same Associative operator than "op". For example,
;% if f and g are Assoc then f(f(x,g(a,b)),f(a,h(c))) --> (x, g(a,b), a, h(c))
; Same thing if f is AC, i.e. is f contains A.
; op is suppose to be at least associative

; op term$list_assoc_subterms : term operator -> list[term]
(defun term$list_assoc_subterms (term op)
  (term$list_assoc_subterms_aux term op nil))

(defun term$list_assoc_subterms_aux (term op lst)
  (cond ((term$is_var term) (cons term lst))
	((operator$is_same_operator (term$head term) op)
	 ; no variations in attributes
	 ;(operator$is_associative_restriction_of (term$head term) op)
	 ;  2/26/87 -- following discussions with CK
	 ;(equal op (term$head term)) &&&& original
	 ;; then the operator is binary of course
	 (term$list_assoc_subterms_aux (term$arg_1 term) op
	     (term$list_assoc_subterms_aux (term$arg_2 term) op lst)))
	(t (cons term lst))))

; derived from the above function
(defun term$list_AC_subterms (term op)
  (term$list_AC_subterms_aux term op nil))

(defun term$list_AC_subterms_aux (term op lst)
  (cond ((term$is_var term) (cons term lst))
        ((operator$is_AC_restriction_of (term$head term) op)
         ;; then the operator is binary of course
         (term$list_AC_subterms_aux (term$arg_1 term) op
             (term$list_AC_subterms_aux (term$arg_2 term) op lst)))
        (t (cons term lst))))

;% build a new term which is the right associative form of
;% the term "term$make_term(op, subterms)" where op is associative
;% and all the term occuring in subterms have not "op" as head.

; op term$make_right_assoc_normal_form : operator list[term] -> term
(defun term$make_right_assoc_normal_form (op subterms)
  (if (= (length subterms) 2)
      (term$make_term op subterms)
      (term$make_term 
	op
	(list (pop subterms)
	      (term$make_right_assoc_normal_form op subterms))
	))
  )

; Same thing with sort check. *ck19may87*
; op term$make_right_assoc_normal_form_with_sort_check : 
;    operator list[term] -> term
(defun term$make_right_assoc_normal_form_with_sort_check (op subterms)
  (if (and (cdr subterms) (null (cddr subterms)))
      (term$make_term_with_sort_check_binary op subterms)
      (term$make_term_with_sort_check_binary
	op
	(list (car subterms) ;15 Apr 88 was (pop subterms)
	      (term$make_right_assoc_normal_form_with_sort_check
	       op (cdr subterms)))
	      ;15 Apr 88 was just term$make_right_assoc_normal_form
	))
  )

; Same thing with sort check and handle case of length 1. 19 Feb 88 TCW
; op term$make_right_assoc_normal_form_with_sort_check : 
;    operator list[term] -> term
(defun term$make_right_assoc_normal_form_with_sort_check_1 (op subterms)
  (if (= (length subterms) 1)
      (car subterms)
  (if (= (length subterms) 2)
      (term$make_term_with_sort_check_binary op subterms)
      (term$make_term_with_sort_check_binary
	op
	(list (car subterms)
	      (term$make_right_assoc_normal_form_with_sort_check
	       op (cdr subterms)))
	)))
  )

; Returns a copy of the term and the association list linking
; the new variables introduced and the old ones.
; This list of variable is intended to be used by term$copy_using_variable
; in order to share the variables between lhs and rhs and condition of rules.

; op term$copy_and_returns_list_variable: Term -> Term List[Variable]

(defun term$copy_and_returns_list_variable (term)
  (multiple-value-bind (result list_new_var)
		       ($$copy_list_term_using_list_var (list term) nil)
      (values (car result) list_new_var)
      )
  )

; Local function which returns a copied list of terms with shared new
; variables which are stored with the correspondance with the old variables
; in the associative list "list_new_var". 

;op $$copy_list_term_using_list_var: 
;     List[Term] List[Variable] -> List[Term] List[Variable] .

(defun $$copy_list_term_using_list_var (term_list list_new_var)
  (let ((pair nil)
	(list_copied_term nil)
	)
    (values 
     (mapcar
      #'(lambda (term)
	  (cond ((term$is_var term) 
		 (if (setq pair (assoc term list_new_var))
		     (cdr pair)
		   (let ((new_var (variable$copy term)))
		     (setq list_new_var (acons term new_var list_new_var))
		     new_var)
		   );; if
		 )
		((term$is_built_in_constant term) term)
		(t (multiple-value-setq (list_copied_term list_new_var)
					($$copy_list_term_using_list_var
					 (term$subterms term)
					 list_new_var)
					)
		   (term$make_term (term$head term)
				   list_copied_term)
		   )
		);; cond
	  );; lambda
      term_list);; mapcar
     list_new_var)
    );; let
  )

; Returns a copy of "term" using the association list of Old, New variables
; build by "term$copy_and_returns_list_variable". 

; op term$copy_using_variable: Term List[Variable] -> Term 
(defun term$copy_using_variable (term list_new_var)
  (multiple-value-bind (result list_new_var_res)
		       ($$copy_list_term_using_list_var 
			(list term) 
			list_new_var)
      (declare (ignore list_new_var_res))
      (car result)
      )
  )

; "TERM" is modified by side effect, the second argument returned is the 
; gathered condition while descending. It is used for the right hand side
; of equations only.
; I: In the LISP implementation, only the consition and the signal are 
;    returned.

; op term$intended_parse: ErrTerm Sort -> Term~ Term, signals(no_issue)
(defun term$intended_parse (TERM SORT)
  (declare (ignore term sort))
#|
  (block the_end
     (if (term$is_var TERM)
	 (unless (sort_order$is_included_in
		  (module$sort_order (if obj$current_module obj$current_module
				       (sort$module (variable$initial_sort
						     TERM))))
		  (car (variable$sorts TERM)) SORT)
		 (return-from the_end (values nil t))
		 )
       (let ((OP
	      (operator_table$highest_operator (term$head TERM)
					       SORT)))
	  (if (null OP)
	      ;; then check if a sort constraint can be applied
	      (if ;@@@there is a constraint returning
		  ;@@@(OP_CONSTRAINT COND) satisfying ((coarity OP_CONSTRAINT)
						      ;@@@ < SORT)
		  ;; then
		  (term$change_head TERM OP_CONSTRAINT)
		  (multiple-value-bind
		   (COND NO_ISSUE)
		   (term$$!apply_ip_on_subterms TERM OP_CONSTRAINT)
		   (if NO_ISSUE
		       (return-from the_end (values nil t))
		     (return-from the_end (values ;@@@ alll the conditions
						  nil))
		     );; if
		   );; bind
		  ;; else
		  (return-from the_end (values nil t))
		  );; if (the exists a constraint)
	    ;; else one can work without constraint
	    (progn ;&&&&
	    (term$change_head TERM OP)
	    (multiple-value-bind
	     (COND NO_ISSUE)
	     (term$$!apply_ip_on_subterms TERM OP_CONSTRAINT)
	     (if NO_ISSUE
		 (return-from the_end (values nil t))
	       (return-from the_end (values ;@@@ alll the conditions
					    nil))
	       );; if
	     );; bind
	    );; progn
	    );; if
	  );; let
       );; if
     );; block
|#
  )

; help the previous one



; op term$$!apply_ip_on_subterms: Term Operator -> Term~ Term, 
;                                                      signals(no_issue)

(defun term$$!apply_ip_on_subterms (TERM OP)
  (declare (ignore term op))
#|
  (block the_end
     (let ((COND nil));; the list of conditions to be satisfied
       (do* ((STERMS (term$subterms TERM))
	     (ARITY  (operator$arity OP_CONSTRAINT))
	     (ST (pop STERMS) (pop STERMS))
	     (AR (pop ARITY) (pop ARITY))
	     )
	    ((not ARITY))
	    (multiple-value-bind
	     (NEW_COND NO_ISSUE)
	     (term$!intended_parse ST AR)
	     (when NO_ISSUE
		   (return-from the_end (values nil t))
		   )
	     )
	    );; do
       (return-from the_end (values ; @@@ the conjunction of all 
				    ; @@@the conditions in COND
				    nil))
       );; let
     ) ;; block
|#
  )

; Compute the lowest parse of the intended parse of "term"

; op term$!lowest_parse_of_ip: Term -> Term~ Term, signals(no_issue)

(defun term$!lowest_parse_of_ip (term sort)
  (declare (ignore term sort))
#|
  (multiple-value-bind (cond no_issue)
		       (term$!intended_parse term sort)
      (if no_issue
	  (values nil t)
	  (term$lowest_parse_without_constraint term)
	  )
      )
|#
  )

; op term$lowest_parse_without_constraint: Term -> Term~ 
; apply morphism rules but never push the sort up, only down
; &&&& missing
(defun term$lowest_parse_without_constraint (term)
  (declare (ignore term))
  )

; op term$make_built_in_constant : Sort Value -> Term
(defun term$make_built_in_constant (sort value)
  (cons (cons (sort$constructor sort) (cons value nil)) nil))

; op term$make_built_in_constant : Sort Value -> Term
(defun term$make_built_in_constant_with_sort_check (sort value)
  (term$!update_lowest_parse_on_top
   (cons (cons (sort$constructor sort) (cons value nil)) nil)))

; op term$built_in_value : Term -> Value
(defmacro term$built_in_value (term)
  `(car (cdar ,term)))

; op term$is_built_in_constant : Term -> Bool
(defun term$is_built_in_constant (term)
  (and (consp (car term))
       (consp (cdar term))))

; macro version of the above
(defmacro term$is_built_in_constant_fast (term)
  `(and (consp (car ,term))
	(consp (cdar ,term))))

; op term$equal_built_in : Term Term -> Bool
; terms are assumed to be two built-in constants; or at least one of them
(defun term$equal_built_in (t1 t2)
;  (and (eq (term$head t1) (term$head t2))
;       (equal (cdar t1) (cdar t2)))
  (let ((v1 (car t1)) (v2 (car t2)))
    (and (eq (car v1) (car v2))
	 (equal (cadr v1) (cadr v2))))
  )

(defmacro term$equal_built_in_fast (t1 t2)
  `(let ((v1 (car ,t1)) (v2 (car ,t2)))
    (and (eq (car v1) (car v2))
	 (equal (cadr v1) (cadr v2))))
  )

;1 Jun 88 from dictionary$$make_built_in_constant
; built-ins embedded in ops
(defun term$make_built_in_constant_op (sort token)
  (let ((info (sort$info sort)))
  (let ((value (funcall (cadr info) token)))
    (operator$create_intrinsic
      (list 'constant value) nil sort (sort$module sort)
      theory$the_empty_theory nil nil nil t
      (rule_ring$create nil) nil
      0 (list (cons 'token token)) 'antefix)
  )))

(defun term$is_built_in_constant_op (op)
  (eq 'constant (car (operator$name op))))

; op term$convert_built_in_constant_op : Operator -> Term
(defun term$convert_built_in_constant_op (op)
  (term$make_built_in_constant (operator$coarity op) (cadr (operator$name op)))
  )

; op term$have_same_top : Term Term -> Bool
(defun term$have_same_top (t1 t2)
  (if (or (term$is_built_in_constant t1) (term$is_built_in_constant t2))
      (term$equal_built_in t1 t2)
  (if (term$is_var t1)
    (and (term$is_var t2) (variable$is_equal t1 t2))
;      (eq t1 t2) ;&&&& 26 Aug 87 eq??
  (if (term$is_var t2)
    nil
    (operator$is_same_operator (term$head t1) (term$head t2))))))

; op term$congruent : term term -> bool
(defun term$congruent (t1 t2)
  (cond ((term$is_var t1)
	 (or (eq t1 t2)
	     ;&&&& 20 Aug 87 added these
	     (and (term$is_var t2)
		  (equal (variable$name t1) (variable$name t2))
		  (eq (variable$initial_sort t1)
		      (variable$initial_sort t2)))))
	((term$is_var t2) nil)
	((term$is_built_in_constant t1)
	 (term$equal_built_in t1 t2))
	((term$is_built_in_constant t2) nil)
	((operator$is_same_qual_operator (term$head t1) (term$head t2))
	 (term$$congruent_subterms (term$subterms t1) (term$subterms t2)))
	(t nil))
  )

(defun term$$congruent_subterms (term_list1 term_list2)
  (cond ((null term_list1)(null term_list2))
	((term$congruent (car term_list1) (car term_list2)) 
	 (term$$congruent_subterms (cdr term_list1) (cdr term_list2)))
	(t nil))
  )

; extension to strategies for on-demand evaluation
(defvar *on-demand* 3)

(defun term$is_on_demand (term)
  (and (not (term$is_var term))
       (eq *on-demand* (cdar term))))

(defun term$!mark_as_on_demand (term)
  (unless (or (term$is_reduced term) ;includes built-in constants and vars
	      (term$is_built_in_constant term))
    (rplacd (car term) *on-demand*)))

(defun term$is_empty_theory_term (term)
  (or
   (term$is_var term)
   (let ((sb (term$subterms term)))
   (or
    (null sb)
    (and
     ;top is ok
     (or
      (null (cdr sb))
      (cddr sb) ;i.e. (< 2 (length sb))
      (theory_name$is_empty_for_matching (theory$name (operator$theory
	  (term$head term)))))
     ;subterms are ok
     (dolist (st sb t)
       (unless (term$is_empty_theory_term st)
	 (return nil))))))))

; op term$make_term_check_op : operator {subterms}list[term] -> term
; check if f is a built_in_constant_op
 
(defun term$make_term_check_op (f subterms)
  (if (and (null subterms) (term$is_built_in_constant_op f))
      (term$convert_built_in_constant_op f)
    (cons (cons f 1) subterms))
  )

; op term$make_term_check_op_with_sort_check :
;     operator {subterms}list[term] -> term
; check if f is a built_in_constant_op
(defun term$make_term_check_op_with_sort_check (f subterms)
  (cond
   ((and (null subterms) (term$is_built_in_constant_op f))
      (term$convert_built_in_constant_op f))
     ;&&&& probably need to try and do a sort check here
   ((every2 #'(lambda (x y) (eq x (term$sort y)))
        (operator$arity f) subterms)
    (term$make_term f subterms))
   ((operator$is_a_retract f)
    (if (sort_order$is_included_in
	 (module$sort_order
	  (if obj$current_module obj$current_module
	    (sort$module (operator$coarity f))))
	 (term$sort (car subterms))
	 (operator$coarity f))
	(car subterms)
      (term$make_term f subterms)))
   (t (term$make_term
       (operator_table$lowest_operator f
           (mapcar #'term$sort subterms))
       subterms) )
   )
  )

; new functions associated with built-in identity matching
(defun term$is_zero_for_op (term op)
  (let* ((th (operator$theory op))
	 (zero (car (theory$zero th))))
    (term$similar term zero)))

(defun term$make_zero (op)
  (let ((zero (car (theory$zero (operator$theory op)))))
    (if zero zero
      (let* ((highest
 	      (operator_info$highest
	       (optable$operator_info
		(module$operator_table
		 (operator$$chk obj$current_module))
		op))))
      (dolist (o highest)
	(let ((zero (car (theory$zero (operator$theory o)))))
	(when zero (return zero))))
 ))))

(defun term$list_assoc_id_subterms (term op)
  (term$list_assoc_subterms_aux term op nil))

(defun term$list_assoc_id_subterms_aux (term op lst)
  (cond ((term$is_var term) (cons term lst))
	((term$is_zero_for_op term op) ; pdl - zero case
	 lst)
	((operator$is_same_operator (term$head term) op)
	 (term$list_assoc_subterms_aux (term$arg_1 term) op
	     (term$list_assoc_subterms_aux (term$arg_2 term) op lst)))
	(t (cons term lst))))

(defun term$list_ACZ_subterms (term op)
  (term$list_ACZ_subterms_aux term op nil))

(defun term$list_ACZ_subterms_aux (term op lst)
  (cond ((term$is_var term) (cons term lst))
	((term$is_zero_for_op term op) ; pdl - zero case
	 lst)
        ((operator$is_AC_restriction_of (term$head term) op)
         ;; then the operator is binary of course
         (term$list_ACZ_subterms_aux (term$arg_1 term) op
             (term$list_ACZ_subterms_aux (term$arg_2 term) op lst)))
        (t (cons term lst))))

(defun term$right_associative_id_normal_form (t1)
  (cond ((term$is_constant t1) t1)
	((term$is_var t1) t1)
	( t (let ((h_op (term$head t1)))
	      (cond
	       ((theory$contains_AZ (operator$theory h_op))
		(term$make_right_assoc_normal_form 
		 h_op 
		 (mapcar 'term$right_associative_id_normal_form 
			 (term$list_assoc_id_subterms t1 h_op))))
	       ;; note this is only top-level normalization.
	       (t t1))))))

(defun term$id_normal_form (t1)
  (cond ((term$is_constant t1) t1)
	((term$is_var t1) t1)
	(t (let ((h_op (term$head t1)))
	      (cond
	       ((term$is_zero_for_op (term$arg_1 t1) h_op)
		(term$id_normal_form (term$arg_2 t1)))
	       ((term$is_zero_for_op (term$arg_2 t1) h_op)
		(term$id_normal_form (term$arg_1 t1)))
	       (t t1))))))

(defun term$make_right_assoc_id_normal_form (op subterms)
  (term$make_right_assoc_normal_form op (term$$filter-zero op subterms)))

(defun term$$filter-zero (op subterms)
  (when subterms
	(if (term$is_zero_for_op (car subterms) op)
	    (term$$filter-zero op (cdr subterms))
	    (cons (car subterms)
		  (term$$filter-zero op (cdr subterms))))))
