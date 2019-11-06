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

;; $Id: match_methods.lsp,v 205.2.1.1 2003/09/23 14:09:52 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Simplified pattern matching methods
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; Tim Winkler ;;;; Created: 21 Dec 87

;;;;;;;;;;;;;;;; Summary

; op match$empty_direct : Term Term -> GlobalState Substitution
;                                    signals(no_match, E_equal)
; op match$next_fail : GlobalState -> GlobalState Substitution, 
;                                     signals(no_match) .

(eval-when (eval compile)
;(defmacro term$is_var (x) `(atom (car ,x)))
; the following is okay only because of the simplicity of the uses of
; this function below
; % makes sure that this definition will only be used where chosen
(defmacro term$%is_built_in_constant(x)
  `(and (consp (car ,x))
	(consp (cdar ,x))))
;(defmacro term$head (x) `(caar ,x))
;(defmacro term$subterms (x) `(cdr ,x))
)

;&&&& delete failed
;(eval-when (eval)
;(defun substitution$%lookup (teta term)
;  (let ((val (assoc term teta)))
;    (if val (cdr val) nil)))
;)

(eval-when (compile eval)
(defmacro substitution$%lookup (teta term)
  (let ((temp (gensym)))
  `(let ((,temp (assoc ,term ,teta)))
     (if ,temp (cdr ,temp) nil))))
)

; This simply always fails
; op match$next_fail : GlobalState -> GlobalState Substitution, 
;                                     signals(no_match) .
(defun match$next_fail (gst)
  (declare (ignore gst))
  (values nil nil t))

;----------------------------------------------------------------------
; empty case matching

(defvar *match$empty_direct_subst* nil)

; op match$empty_direct : Term Term -> GlobalState Substitution
;                                    signals(no_match, E_equal)
; global state will always be nil
(defun match$empty_direct (t1 t2)
  (let ((*match$empty_direct_subst* (substitution$new)))
    (let ((val (match$empty_direct_subst t1 t2)))
      (values nil *match$empty_direct_subst* (null val) nil))))

; op match$empty_direct_subst : Term Term -> Bool
(defun match$empty_direct_subst (t1 t2)
  (cond ((term$is_var t1)
	 (let ((sl (substitution$%lookup *match$empty_direct_subst* t1)))
	   (if sl
	       (term$equational_equal sl t2) ;>< term$congruent use eventually
	     (if (sort_order$is_included_in ;&&&& could check most general
		   ;>< not sure this is correct, cf. match_equation @2594
		   (module$sort_order obj$current_module)
		   (term$sort t2)
		   (variable$initial_sort t1))
	       (progn
		 (setq *match$empty_direct_subst*
		       (substitution$add *match$empty_direct_subst*
		         t1 t2))
		 t)
	       nil))))
	((term$is_var t2) nil)
	((term$%is_built_in_constant t1)
	 (term$equal_built_in t1 t2))
	((term$%is_built_in_constant t2) nil)
	((operator$is_same_operator (term$head t1) (term$head t2))
	 (do* ((lst1 (term$subterms t1) (cdr lst1))
	       (i1 (car lst1) (car lst1))
	       (lst2 (term$subterms t2) (cdr lst2))
	       (i2 (car lst2) (car lst2)))
	      ((null lst1) (null lst2))
	      (unless (match$empty_direct_subst i1 i2) (return nil))))
	(t nil))
  )

; op match$is_empty_direct_ok : Lhs Condition -> Bool
; cases:
;   only empty ops and vars
;   x op y or fn(x1,...,xn) and unconditional
(defun match$is_empty_direct_ok (lhs cond)
  (or (term$is_empty_theory_term lhs)
      (and (obj_BOOL$is_true cond)
	   (match$is_general_pattern lhs)))
  )

(defun match$is_general_pattern (term)
  (or
   (term$is_var term)
   (and
     (every #'(lambda (x y) (and (term$is_var x)
				 (eq (variable$initial_sort x)
				     y)))
       (term$subterms term)
       (operator$arity (term$head term))
       )
     (do* ((lst (term$subterms term) (cdr lst))
	   (elt (car lst) (car lst)))
       ((null lst) t)
       (when (member elt (cdr lst)) (return nil)))
     )
   ))

;----------------------------------------------------------------------
; methods for idempotent like rules
;   x + x = r
;   (x + x) + e = r + e  -- extension
;  rule is unconditional
;  op is AC
;  variable is "general"
; 19 Feb 88

;&&&& Would like to use sorting to perform list2multi_set_list when
;  the lists are long

; Note: if order rules right may not need the simple case at all
; the extension version succeeds except when the input is of length 2
; That is might be able to tremendously simplify match$idem

; op match$idem : Term Term -> GlobalState Substitution
;                               signals(no_match, E_equal)
; global state will always be nil
; x op x unconditionally
; t1 is the pattern
(defun match$idem (t1 t2)
  (let* ((op (term$head t2))
	 (subs (term$list_AC_subterms t2 op)))
  (if (oddp (length subs))
      (values nil nil t nil)
  (let ((var (car (term$subterms t1)))
	(ms_tm (list2multi_set_list subs)))
  (if (dolist (x ms_tm t) (when (oddp (cdr x)) (return nil)))
      ;if all even
      (values
       nil
       (list
	(cons
	 var
	 (term$make_right_assoc_normal_form_with_sort_check_1
	  (term$head t1)
	  (multi_set_list2set
	   (mapcar #'(lambda (x) (cons (car x) (truncate (cdr x) 2)))
		   ms_tm)))))
       nil nil)
    (values nil nil t nil)
    )
  ))))

; op match$idem_ext : Term Term -> GlobalState Substitution
;                               signals(no_match, E_equal)
; global state will always be nil
; (x op x) op e unconditionally
; t1 is the pattern
(defun match$idem_ext (t1 t2)
  (let* ((op (term$head t2))
	 (subs (term$list_AC_subterms t2 op)))
  (if (< (length subs) 3)
      (values nil nil t nil)
  ; assume that the rules is actually created in the form e + (x + x)
  (let* ((t1subs (term$subterms t1))
	 (evar (car t1subs))
	 (var (car (term$subterms (cadr t1subs))))
	 (ms_tm (list2multi_set_list subs)))
  ; if any odds that one from each goes in evar
  ; if no odds then must put two in evar (evar has to match something)
  ; if all 1, then fail (nothing to match var against)
  (let ((tl ms_tm)
	(singletons nil) (evens nil) (odds nil)
	(n nil) (fr nil) (it nil))
  ; split into singletons evens and odds (just categorize)
  (loop
   (when (null tl) (return))
   (setq fr tl  tl (cdr tl)  it (car fr)  n (cdr it))
   (if (= 1 n) (progn (rplacd fr singletons) (setq singletons fr))
   (if (oddp n) (progn (rplacd fr odds) (setq odds fr))
     (progn (rplacd fr evens) (setq evens fr))))
  ) ; loop
  (if (and (null evens) (null odds)) (values nil nil t nil)
  (progn
    ; change form of singletons to simple list of terms
    (if (and (null singletons) (null odds))
      (let ((fe (car evens)))
	(setq singletons (list (car fe) (car fe)))
	(let ((n (cdr fe)))
	  (if (= 2 n) (setq evens (cdr evens))
	    (rplacd fe (- n 2))))
      ) ; let
      ; else
      (let ((lst singletons))
	(loop (when (null lst) (return))
	  (rplaca lst (caar lst))
	  (setq lst (cdr lst))))
    ) ; if
    ; transfer odds to singletons and evens
    (loop
     (when (null odds) (return))
     (setq fr odds  odds (cdr odds)  it (car fr))
     (setq singletons (cons (car it) singletons)) ; new cons
     ; know that repetition count is 3 or larger
     (rplacd fr evens)  (setq evens fr)  (decf (cdr it))
    ) ; loop
    (values
     nil ; global state
     (list
      ; evens
      (cons var
	    (term$make_right_assoc_normal_form_with_sort_check_1
	     (term$head t1)
	     (multi_set_list2set
	      (mapcar #'(lambda (x) (cons (car x) (truncate (cdr x) 2)))
		evens))))
      ; singletons
      (cons evar
	    (term$make_right_assoc_normal_form_with_sort_check_1
	     (term$head t1)
	     singletons))
     ) ; list
     nil nil) ;error indications
  ))
  ) ; let
  ))))

(defun match$is_idem_ok (lhs cond)
  (and
   (obj_BOOL$is_true cond)
   (not (term$is_var lhs))
   (let ((op (term$head lhs)))
   (and
    (operator$is_associative op)
    (operator$is_commutative op)
    (let* ((arity (operator$arity op))
	   (subs (term$subterms lhs))
	   (t1 (car subs))
	   (t2 (cadr subs)))
      (and
       (term$is_var t1)
       (term$is_var t2)
       (eq t1 t2)
       (eq (variable$initial_sort t1) (car arity))
       (eq (variable$initial_sort t2) (cadr arity))
       )
    ))
   ))
  )

; assume that the rules is actually created in the form e + (x + x)
(defun match$is_idem_ext_ok (lhs cond)
  (and
   (obj_BOOL$is_true cond)
   (not (term$is_var lhs))
   (let ((op (term$head lhs)))
   (and
    (operator$is_associative op)
    (operator$is_commutative op)
    (let* ((arity (operator$arity op))
	   (subs (term$subterms lhs))
	   (t1 (car subs))
	   (t2 (cadr subs)))
    (and
      (term$is_var t1)
      (let ((vs (variable$initial_sort t1)))
      (or
       (eq vs *obj$sort_Universal*)
       (and (eq vs (car arity)) (eq vs (cadr arity)))))
      (match$is_idem_ok t2 cond)
      (not (eq t1 (car (term$subterms t2)))) ;31 May 88 added
      )))))
  )

;----------------------------------------------------------------------
; method for unconditional non-left-linear AC rules that split into an
; independent and a DEPendent part
; Note: if the condition i successful, then the LHS is re-ordered
(defun match$is_dep_ok (lhs cond)
  (and
   (obj_BOOL$is_true cond)
   (not (term$is_var lhs))
   (let ((op (term$head lhs)))
   (and
    (operator$is_associative op)
    (operator$is_commutative op)
    (if (null obj$current_module)
	(progn
	  (princ "### unexpected dependent matching failure -- no module")
	  (terpri)
	  nil) ; not ok
    (let ((subs (term$list_AC_subterms lhs op))
	  (lst nil)
	  (indep nil)
	  (accumvars nil) (allvars nil)
	  (vars nil) (duplicatedvar nil)
	  (depvars nil) (indepvars nil)
	  (so (module$sort_order obj$current_module))
	  cur)
      ; first separate out the variables
      (dolist (tm subs)
	(if (term$is_var tm)
	    (push tm vars)
	  (push tm lst)))
      (setq lst (nreverse lst))
      (dolist (x lst)
	(setq allvars (union allvars (term$vars x))))
      ;split nonvars into indep and dep parts
      (setq accumvars nil)
      (if (null allvars)
	(setq indep lst  lst nil)
      (loop
       (when (or (null lst) (subsetp allvars accumvars)) (return))
       (setq cur (car lst))
       (setq lst (cdr lst))
       (push cur indep)
       (setq accumvars (union accumvars (term$vars cur)))
      ) ;loop
      ) ;if
      ;split vars into indep and dep parts
      (dolist (v vars)
	(if (member v accumvars) (push v depvars)
	(if (member v indepvars)
	    (progn (setq duplicatedvar t) (return))
	  (push v indepvars))))
 ;(when (and $$debug (< 5 $$debug_level))
 ;(princ "###is dep ok") (terpri)
 ;(print$struct (list 'indep indep 'lst lst 'allvars allvars
 ;    'depvars depvars 'indepvars indepvars 'duplicatedvar duplicatedvar))
 ;(terpri))
      (if duplicatedvar nil ;not ok
        (progn
	  (setq indepvars
              (topo-sort indepvars
			 #'(lambda (x y)
			     (let ((xs (variable$initial_sort x))
				   (ys (variable$initial_sort y)))
			       (and (not (eq xs ys))
				    (sort_order$is_included_in so xs ys))))))
	  (setq depvars (nreverse depvars))
 ;(when (and $$debug (< 5 $$debug_level))
 ;(princ "### topo indep vars") (terpri)
 ;(print$struct indepvars) (terpri))
	  (if (and
	       indep
	       ; there are dependent
	       ; or there are a few easy matching cases
	       (or lst
		   depvars
		   (< 1 (length indepvars)))
	       ;indep vars must be linearly ordered by sort
	       (or (null (cdr indepvars))
		   (do ((cur (car indepvars) nxt)
			(nxt (cadr indepvars) (car lst))
			(lst (cddr indepvars) (cdr lst)))
		       ((null nxt) t) ;a bit ugly
		       (unless (sort_order$is_included_in so
				(variable$initial_sort cur)
				(variable$initial_sort nxt))
			 (return nil))))
	       );and
	      ;restructure lhs of rule (not so nice)
	      (let ((genvar nil)
		    ;(gensort nil)
		    )
		(when indepvars
		  (setq genvar (car (last indepvars)))
		  ;(setq gensort (variable$initial_sort genvar))
		  )
		(term$!replace lhs
		  (term$make_right_assoc_normal_form_with_sort_check
		   op
		   (append
		    (list
		     (term$make_right_assoc_normal_form_with_sort_check_1
		      op
		      indep))
		    lst
		    depvars
		    indepvars
		    )))
		t) ; is ok
	    nil) ; not ok
	  ))));let subs
      ))))

(defvar *match$dep_var* nil)

; op match$dep : Term Term -> GlobalState Substitution
;                               signals(no_match, E_equal)
; global state will always be nil
; t1 is the pattern
(defun match$dep (t1 t2)
  (let ((subs (term$subterms t1)))
  (let ((indep (car subs))
	(op (term$head t1)))
  (let ((newpat (term$make_term op (list indep *match$dep_var*)))
	(coar (operator$coarity op)))
 ;(when (and $$debug (< 5 $$debug_level))
 ;(princ "### match dep") (terpri)
 ;(print$struct (list 'subs subs 'indep indep 'op op 'newpat newpat))
 ;(terpri))
  (multiple-value-bind
       (global_state subst no_match E_equal)
       (match$first_match newpat t2)
   (if (or no_match E_equal)
     (values nil nil no_match E_equal)
   (let ((dep (term$list_AC_subterms (cadr subs) op)) ;&&&& replace by simpler?
	 (so (module$sort_order obj$current_module)))
   (loop
    ; try to finish if succeed return subst.
    (let ((ok t)
	  (rest (term$list_AC_subterms (cdr (assoc *match$dep_var* subst)) op))
	  (lst dep) x)
 ;(when (and $$debug (< 5 $$debug_level))
 ;(princ "### match loop") (terpri)
 ;(dolist (i lst) (princ " ") (print$term i)) (terpri)
 ;(print$struct subst) (terpri))
    (when (< (length rest) (length dep))
      (return (values nil nil t nil))) ;quit whole matching process
    (block finish_match
    (loop
     (when (null lst) (return))
     (when (null rest) (setq ok nil) (return))
     (setq x (car lst)  lst (cdr lst))
 ;(when (and $$debug (< 5 $$debug_level))
 ;(princ "### --- ") (print$term x) (princ " --")
 ;(if (null rest) (princ " NULL")
 ;(dolist (i rest) (princ " ") (term$print i)))
 ;(terpri))
     (if (term$is_var x)
	 (let ((val (cdr (assoc x subst))))
	   (if val
	       ;remove value if find or fail; rest is not nil
	       ;tricky point: if bound value of term has op at top
	       ;  need to treat its term$list_ac_subterms individually
	       (dolist (tm
			(if (and (not (term$is_var val))
				 (operator$is_AC_restriction_of
				  (term$head val) op))
			    (term$list_AC_subterms val op)
			  (list val)))
		 (let ((prev nil) (cur rest))
		 (loop
		  (when (null cur) (setq ok nil) (return-from finish_match))
		  (when (term$equational_equal tm (car cur))
		    (if (null prev)
			(setq rest (cdr rest))
		      (rplacd prev (cdr cur)))
		    (return))
		  (setq prev cur  cur (cdr cur))
		  )))
	     ;find term for var and bind; if last done
	     (if (null lst)
		 (if (null (cdr rest))
		     (if (sort_order$is_included_in so
			  (term$sort (car rest))
			  (variable$initial_sort x))
			 (progn
			   (push (cons x (car rest)) subst)
			   (setq rest nil))
		       (progn (setq ok nil) (return)))
		   (if (sort_order$is_included_in so
			 coar (variable$initial_sort x))
		       (progn
			 (push (cons x
			   (term$make_right_assoc_normal_form_with_sort_check
			    op rest)) subst)
			 (setq rest nil))
		     (progn (setq ok nil) (return))))
	       (let ((varsort (variable$initial_sort x))
		     (prev nil) (cur rest))
	       (loop
		(when (null cur) (setq ok nil) (return-from finish_match))
		(when (sort_order$is_included_in so
			(term$sort (car cur)) varsort)
		  (if (null prev)
		      (setq rest (cdr rest))
		    (rplacd prev (cdr cur)))
		  (push (cons x (car cur)) subst)
		  (return))
		(setq prev cur  cur (cdr cur))
		))
	     )))
       ;instantiate and find or fail; rest is not nil
       (let ((instx (substitution$image subst x))
	     (prev nil) (cur rest))
       (loop
	(when (null cur) (setq ok nil) (return-from finish_match))
	(when (term$equational_equal instx (car cur))
	  (if (null prev)
	      (setq rest (cdr rest))
	    (rplacd prev (cdr cur)))
	  (return))
	(setq prev cur  cur (cdr cur))
       )))
    ))
    (when (and ok (null rest)) (return (values nil subst nil nil)))
    )
    (multiple-value-setq
         (global_state subst no_match)
         (match$next_match global_state))
    (when no_match
      (return (values nil nil t nil)))
   )))
  )))))

;----------------------------------------------------------------------

(defvar *match$empty_method* '(match$empty_direct match$next_fail))
(defvar *match$idem_method* '(match$idem match$next_fail))
(defvar *match$idem_ext_method* '(match$idem_ext match$next_fail))
(defvar *match$dep_method* '(match$dep match$next_fail))
(defvar *match$id_A_method* '(match$id_A_match match$next_fail))
(defvar *match$id_AC_method* '(match$id_AC_match match$next_fail))
(defvar *match$id_A_dep_method* '(match$id_A_dep_match match$next_fail))
(defvar *match$id_AC_dep_method* '(match$id_AC_dep_match match$next_fail))
(defvar *match$id_gen_method* '(match$id_gen_match match$next_fail))

(defvar *match$methods* (list
  (cons *match$empty_method* "empty")
  (cons *match$idem_method* "idem")
  (cons *match$idem_ext_method* "idem-ext")
  (cons *match$dep_method* "dependent")
  (cons *match$id_A_method* "id_A")
  (cons *match$id_AC_method* "id_AC")
  (cons *match$id_A_dep_method* "id_A_dep")
  (cons *match$id_AC_dep_method* "id_AC_dep")
  (cons *match$id_gen_method* "id_gen")
 ))

; the automatically generate rule method
(defun match$choose_rule_method (lhs cond knd)
  (cond
   ((and (or (eq 'id_theory knd) (eq 'id_ext_theory knd))
	 (theory$contains_identity (operator$theory (term$head lhs))))
    (let ((op (term$head lhs)))
    (if (operator$is_associative op)
      (if (match$is_dep_ok lhs cond)
	(if (operator$is_commutative op)
	  *match$id_AC_dep_method*
	  *match$id_A_dep_method*)
	(if (operator$is_commutative op)
	  *match$id_AC_method*
	  *match$id_A_method*))
      *match$id_gen_method*)))
   ((match$is_empty_direct_ok lhs cond) *match$empty_method*)
   ((match$is_idem_ok lhs cond) *match$idem_method*)
   ((match$is_idem_ext_ok lhs cond) *match$idem_ext_method*)
   ((match$is_dep_ok lhs cond)
    (when (null *match$dep_var*)
      (setq *match$dep_var* (variable$make_var "REST" *obj$sort_Universal*)))
    ;(when (and $$debug (< 5 $$debug_level)) ;><
    ;  (princ "### dep method") (terpri)
    ; (term$print lhs) (terpri))
    *match$dep_method*)
   (t nil) ; the default
  ))

(defun match$method_name (m)
  (cdr (assoc m *match$methods*)))

; must be accurate when answer is false
; assume that t1,t2 are not a variables (this is a slight optimization)
(defun match$possibly_matches_nonvar (t1 t2)
  (let ((op1 (term$head t1))
	(op2 (term$head t2)))
    (if (not (operator$is_same_operator op1 op2))
	;@@ built-in identity matching requires a change here
        ;  somehow the usage of theories seems messy here
      (if (test-flag Z_flag (theory_info$code
	      (operator$theory_name_for_matching op1)))
	; @@ too costly?
	(or (match$possibly_matches (term$arg_1 t1) t2)
	    (match$possibly_matches (term$arg_2 t1) t2))
	nil)
    (if (null obj$current_module) t ;8 Jul 88 cautious
      (let ((chk1 (theory_name$is_empty_for_matching
		   (operator$theory_name_for_matching op1))) ;@@@
	    (chk2 (theory_name$is_empty_for_matching
		   (operator$theory_name_for_matching op2)))) ;@@@
	(if (and chk1 chk2)
	    (let ((subs1 (term$subterms t1))
		  (subs2 (term$subterms t2))
		  (ok t))
	    (loop
	     (when (null subs1) (return))
	     (unless (match$possibly_matches (car subs1) (car subs2))
	       (setq ok nil) (return))
	     (setq subs1 (cdr subs1)  subs2 (cdr subs2))
	    )
	    ok
	    );let
	  t)
      )))
  ))

; could improve on this
(defun match$possibly_matches (t1 t2)
  (cond
   ((term$is_var t1) t)
   ((term$is_var t2) nil)
   (t (match$possibly_matches_nonvar t1 t2))))

;----------------------------------------------------------------------
(defun match$id_A_match (t1 t2)
  (match$first_match_theory the_A_property t1 t2)
  )

(defun match$id_AC_match (t1 t2)
  (match$first_match_theory the_AC_property t1 t2)
  )

(defun match$id_A_dep_match (t1 t2)
  (match$dep_theory the_A_property t1 t2)
  )

(defun match$id_AC_dep_match (t1 t2)
  (match$dep_theory the_AC_property t1 t2)
  )

(defun match$id_gen_match (t1 t2)
  (let ((th_name (theory$name (operator$theory (term$head t1)))))
  (match$first_match_theory
   (theory$code_to_info (logandc1 Z_flag (theory_info$code th_name)))
   t1 t2)
  ))

; incorporate these into the originals? @@@

(defun match$first_match_theory (th_name t1 t2)
  (multiple-value-bind (m_sys no_match) 
;      (match_system$dec_merg_theory th_name (match_system$new t1 t2))
      (match_system$dec_merg (match_system$new t1 t2))
    ; note that is the two terms are similar then "m_sys" is empty,
    ; In the current code it is not signaled "E_equal", 
    ; it must be corrected (ck-04/11/86)
    (if no_match 
	(values nil nil t nil)
	(let ((gst (global_state$new)))
	  (cond ((system$is_empty (match_system$system m_sys))
		 (values gst 
			 (match_system$match_system2substitution m_sys) 
			 nil nil))
		((match_system$E_equal m_sys) 
		 (values nil nil nil t))
		(t (multiple-value-bind 
		     (sys th_name_ign) (match_system$extract_one_system m_sys)
		     (declare (ignore th_name_ign))
		     ;; the matching system is not modified,
		     ;; thus we create a new match_system
		     (multiple-value-bind (th_st no_match)
			 (theory_state$initialize th_name sys
						  ;; [ck: Nov 29 87]
						  (match_system$env m_sys))
		       (if no_match
			   (values nil nil t nil)
			   (multiple-value-bind (new_gst subst no_match)
			       (match$next_match 
				 (global_state$push 
				  gst 
				  (state$create 
				   (match_system$modif_m_sys m_sys sys)
				   sys
				   th_name
				   th_st)))
			     (values new_gst subst no_match nil))))
		     )))
	  ) ;; let
	) ;; if
    )
  )

; remainder not used
(defun match_system$dec_merg_theory (th_name m_sys)
  (block no_match
    (let ((sys (match_system-sys m_sys))
	   (env (match_system-env m_sys))
	   (new_env (environment$new) )
	   (new_sys (system$new))
	   )
      (dolist (eq (system$list_eq sys))
	(multiple-value-bind (eq_list clash_of_symbol)
	    (match_equation$decomposition_theory th_name eq)
	  (if clash_of_symbol
	      (return-from no_match (values nil t))
	      (when (environment$insert_if_coherent_with 
		     new_env 
		     env 
		     new_sys 
		     eq_list)
		    (return-from no_match (values nil t))
		    ) ;; when
	      ) ;; if
	  ) ;; mult
	) ;; do
      (values (match_system$create 
		new_env
		new_sys)
	      nil)
      )
    )
  )

(defun match_equation$decomposition_theory (th_name eq)
  ; the following curious initialisation allows to modify "list_dec_eq"
  ; inside match_equation$$!decomposition
  (let ((list_dec_eq (cons nil nil))
	(no_match nil))
    (setq no_match (match_equation$$!decomposition_theory
		    th_name
		    (match_equation$t1 eq)
		    (match_equation$t2 eq)
		    list_dec_eq))
    (values (cdr list_dec_eq) no_match)
    )
  )

(defun match_equation$$!decomposition_theory (th_name t1 t2 l_result)
  (block no_match
    (cond
      ((term$is_var t1)
       ;; test of the sort of the variable,
       ;; NO test is done if the variable is general
       (unless 
	   (or (and (not *match$check_variables*)
		    (variable$is_general_variable t1))
	       (let ((ts (term$sort t2)))
		 (if obj$current_module
		   (let ((so (module$sort_order obj$current_module)))
		     (dolist (vs (variable$sorts t1) nil)
		       (when (sort_order$is_included_in so ts vs)
			 (return t))))
		   (dolist (vs (variable$sorts t1) nil)
		     (when (sort_order$is_included_in
			    (module$sort_order (sort$module vs))
			    ts vs)
		       (return t)))
		   ))
	       )
	 (match_equation$$!decomposition_on_demand t1 t2 l_result))
       (push (match_equation$create t1 t2) (cdr l_result))
       nil)
      ((term$is_built_in_constant t1)
       (if (term$equal_built_in t1 t2) nil
	 (return-from no_match t)))
      ((term$is_var t2)
       (return-from no_match 'no_match_to_var))
      (t (let ((t1_top (term$head t1)))
        (cond
	  ((not (theory_name$is_empty_for_matching th_name))
	   (push (match_equation$create t1 t2) 
		 (cdr l_result))
	   (return-from no_match nil))
	  (t (if (term$is_var t2)
	       (match_equation$$!decomposition_on_demand
		t1 t2 l_result)
	       (let ((t2_top (term$head t2)))
	       (if ;; since it is OS-matching, we only
		  ;; test the name of the operator. vs OBJ2
		(operator$is_same_operator
		 t1_top t2_top)
		 (let ((t1_subterms (term$subterms t1))
		       (t2_subterms (term$subterms t2)))
		 (do ((tt1)
		      (tt2))
		     ((not t1_subterms))
		   (setq tt1 (pop t1_subterms) 
			 tt2 (pop t2_subterms))
		   (let ((not_ok 
			  (match_equation$$!decomposition 
			   tt1 tt2 l_result)))
		     (when (and not_ok
				(match_equation$$!decomposition_on_demand
				 t1 t2 l_result))
		       (return-from no_match t))
		     ) ;; let 
		   ) ;;do
		 (return-from no_match nil))
		 (match_equation$$!decomposition_on_demand_theory
		  th_name
		  t1 t2 l_result)
		 ))
	       ))
	  )))
      )
    );; block no_match
  )

(defun match_equation$$!decomposition_on_demand_theory (th_name t1 t2 l_result)
  (if (and *rew$perform_on_demand_reduction*
	   (term$is_on_demand t2))
    (progn
      (term$!mark_as_lowest_parsed t2)
      (if (rew$!normalize_with_memo t2)
	t
	(match_equation$$!decomposition_theory th_name t1 t2 l_result)
	)
      )
    t))

; want this for id_A and id_AC variants
; should try and integrate with above code
; note: match$next_match uses the theory already determined
(defun match$dep_theory (th_name t1 t2)
  (let ((subs (term$subterms t1)))
  (let ((indep (car subs))
	(op (term$head t1)))
  (let ((newpat (term$make_term op (list indep *match$dep_var*)))
	(coar (operator$coarity op)))
 ;(when (and $$debug (< 5 $$debug_level))
 ;(princ "###@ match dep") (terpri)
 ;(print$struct (list 'subs subs 'indep indep 'op op 'newpat newpat))
 ;(terpri))
  (multiple-value-bind
       (global_state subst no_match E_equal)
       (match$first_match_theory th_name newpat t2)
   (if (or no_match E_equal)
     (values nil nil no_match E_equal)
   (let ((dep (term$list_AC_subterms (cadr subs) op)) ;&&&& replace by simpler?
	 (so (module$sort_order obj$current_module)))
   (loop
    ; try to finish if succeed return subst.
    (let ((ok t)
	  (rest (term$list_AC_subterms (cdr (assoc *match$dep_var* subst)) op))
	  (lst dep) x)
 ;(when (and $$debug (< 5 $$debug_level))
 ;(princ "###@ match loop") (terpri)
 ;(dolist (i lst) (princ " ") (print$term i)) (terpri)
 ;(print$struct subst) (terpri))
    (when (< (length rest) (length dep))
      (return (values nil nil t nil))) ;quit whole matching process
    (block finish_match
    (loop
     (when (null lst) (return))
     (when (null rest) (setq ok nil) (return))
     (setq x (car lst)  lst (cdr lst))
 ;(when (and $$debug (< 5 $$debug_level))
 ;(princ "###@ --- ") (print$term x) (princ " --")
 ;(if (null rest) (princ " NULL")
 ;(dolist (i rest) (princ " ") (term$print i)))
 ;(terpri))
     (if (term$is_var x)
	 (let ((val (cdr (assoc x subst))))
	   (if val
	       ;remove value if find or fail; rest is not nil
	       ;tricky point: if bound value of term has op at top
	       ;  need to treat its term$list_ac_subterms individually
	       (dolist (tm
			(if (and (not (term$is_var val))
				 (operator$is_AC_restriction_of
				  (term$head val) op))
			    (term$list_AC_subterms val op)
			  (list val)))
		 (let ((prev nil) (cur rest))
		 (loop
		  (when (null cur) (setq ok nil) (return-from finish_match))
		  (when (term$equational_equal tm (car cur))
		    (if (null prev)
			(setq rest (cdr rest))
		      (rplacd prev (cdr cur)))
		    (return))
		  (setq prev cur  cur (cdr cur))
		  )))
	     ;find term for var and bind; if last done
	     (if (null lst)
		 (if (null (cdr rest))
		     (if (sort_order$is_included_in so
			  (term$sort (car rest))
			  (variable$initial_sort x))
			 (progn
			   (push (cons x (car rest)) subst)
			   (setq rest nil))
		       (progn (setq ok nil) (return)))
		   (if (sort_order$is_included_in so
			 coar (variable$initial_sort x))
		       (progn
			 (push (cons x
			   (term$make_right_assoc_normal_form_with_sort_check
			    op rest)) subst)
			 (setq rest nil))
		     (progn (setq ok nil) (return))))
	       (let ((varsort (variable$initial_sort x))
		     (prev nil) (cur rest))
	       (loop
		(when (null cur) (setq ok nil) (return-from finish_match))
		(when (sort_order$is_included_in so
			(term$sort (car cur)) varsort)
		  (if (null prev)
		      (setq rest (cdr rest))
		    (rplacd prev (cdr cur)))
		  (push (cons x (car cur)) subst)
		  (return))
		(setq prev cur  cur (cdr cur))
		))
	     )))
       ;instantiate and find or fail; rest is not nil
       (let ((instx (substitution$image subst x))
	     (prev nil) (cur rest))
       (loop
	(when (null cur) (setq ok nil) (return-from finish_match))
	(when (term$equational_equal instx (car cur))
	  (if (null prev)
	      (setq rest (cdr rest))
	    (rplacd prev (cdr cur)))
	  (return))
	(setq prev cur  cur (cdr cur))
       )))
    ))
    (when (and ok (null rest)) (return (values nil subst nil nil)))
    )
    (multiple-value-setq
         (global_state subst no_match)
         (match$next_match global_state))
    (when no_match
      (return (values nil nil t nil)))
   )))
  )))))
