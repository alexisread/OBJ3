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

;; $Id: modexp_eval2.lsp,v 205.2.1.1 2003/09/23 14:06:51 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                    modexp_eval (part 2)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Tim Winkler ;;;; Created: 10/2/91
;;;; (the second half of modexp_eval.lsp)

;;;; module-expression evaluation
; this routined eliminates identity mappings
; map is a list with entries of the form (sort s1 s2) or (op op1 op2)
; A.Socorro 21/4/93

(defun rem-id (map)
  (if (null map)
      nil
    (if (not (equal (second (car map)) (third (car map))))
	(cons (car map) (rem-id (cdr map)))
      (rem-id (cdr map))
      )))

(defun modexp_eval$rename_canonicalize (modexp)
  (cond
   ((and (consp (cadr modexp)) (eq '* (car (cadr modexp))))
    (modexp_eval$rename_canonicalize
     (modexp$make_rename (cadr (cadr modexp))
       (modexp_eval$compose_renames
	(caddr modexp) (caddr (cadr modexp))))))
   (t
    (modexp_eval$rename_push (modexp_eval$$canon (cadr modexp))
			     ;&&&&
      ; **** added call to rem-id ****
      (rem-id (modexp_eval$canonicalize_rename (caddr modexp)))))
  ))

; note: want to push renames into view expressions
;   problem: what is the canonical form of a named view with a rename?
; the processing of renames takes place before module evaluation --
;   is based solely on "names"
; Description: this process is rather complex
; -- if the renaming is "bijective", then simply push all the way in
;   ultimately combine multiple renamings into one and canonically order
; -- if the renaming is not bijective, then would like (for each view)
;   to check if can commute, unfortunately, at this point we have not
;   evaluated the views and so don't really know very much about their
;   structure
; -- if get stuck, i.e. have a view and a rename and cannot see that
;   it is valid to commute, then evaluate the enclosed module expressions
;   find the structure of the view and then check to see if you can
;   commute; if you can, then need to properly canonicalize the module
;   expression, and create the right final module (is it possible to
;   use the partial results?); if cannot commute then the module
;   expression is in normal form and obtain the final result by actually
;   performing the renaming (not done here)
; note: normalization of renaming thus calls module evaluation in cases
(defun modexp_eval$rename_push (modexp ren)
  (cond
   ((null ren) modexp)
   ((typep modexp 'module)
    (let ((nm (module$name modexp)))
      (if (modexp_eval$is_parameter_theory nm)
	  modexp
	(modexp_eval$rename_push (module$name modexp) ren))))
   ((or (atom modexp)
	(and (consp modexp) (eq 'name (car modexp))))
    (let ((mod (modexp_eval$eval modexp)))
    (when (modexp_eval$is_error mod)
      (princ "Cannot evaluate: ") (print$modexp modexp) (terpri)
      (obj3-to-top))
    (let ((redren (modexp_eval$reduce_rename mod ren)))
      (if (null redren) mod
        (modexp$make_rename mod redren))
    )))
   ;@@ new (specn x) (op x) (sort x) (qual x y) add cases ? @@more
   ((eq '+ (car modexp))
    (modexp_eval$canonicalize
     (modexp$make_plus
      (modexp_eval$rename_push (cadr modexp) ren)
      (modexp_eval$rename_push (caddr modexp) ren))))
   ((eq '* (car modexp))
    (modexp_eval$rename_push (cadr modexp)
      (modexp_eval$compose_renames ren (caddr modexp))))
   ((eq '*view (car modexp))
    (modexp$make_view_rename
     (modexp_eval$rename_push (cadr modexp) ren)
     (modexp_eval$compose_renames ren (caddr modexp))))
   ((not (modexp_eval$is_rename_injective ren))
    (modexp$make_rename modexp ren))
   ((eq '|:| (car modexp))
    (modexp_eval$canonicalize
     (modexp$make_instantiation
      (modexp_eval$rename_push (cadr modexp) ren)
      (mapcar #'(lambda (x)
		  (modexp_eval$rename_push
		   (modexp_eval$insert_view_rename x)
		   ren))
	(caddr modexp)))))
   ((eq 'view (car modexp))
    (let ((canview (modexp_eval$canonicalize modexp)))
      (modexp_eval$rename_push canview ren))
    )
   ((eq 'view_from (car modexp))
    (modexp_eval$canonicalize
     (modexp$make_view_from (cadr modexp)
       (modexp_eval$rename_push (caddr modexp) ren)
       (modexp_eval$rename_view_map (caddr modexp) (cadddr modexp) ren))))
   ((modexp_eval$is_parameter_theory modexp)
    (modexp_eval$rename_push (modexp_eval$eval modexp) ren))
   (t (break "modexp_eval$rename_push: unexpected form"))
  ))

(defun modexp_eval$is_simple_name (x)
  (or (stringp x)
      (modexp_eval$is_parameter_theory x)))

;&&&& probably not used
(defun modexp_eval$is_simple_named_module (mod)
  (let ((nm (module$name mod)))
  (or (stringp nm)
      (and (consp nm) (eq 'name (car nm))
	   (modexp_eval$is_simple_named_module (cadr nm))))
  ))

(defun modexp_eval$rename_view_map (targetmod map ren)
  (declare (ignore targetmod ren))
  map
  )

(defun modexp_eval$insert_view_rename (x)
  (if (and (consp x) (eq '*view (car x)))
      x
    (modexp$make_view_rename x nil)))

; composing renames: ren2 is applied first, then ren1
; case not considered: (compose (a->b) (a->c)) = a->c  (a->b second no a)
(defun modexp_eval$compose_renames (ren1 ren2)
  (let ((ops1
	 (mapcar #'(lambda (x) (cons (cadr x) (caddr x)))
	 (remove-if-not #'(lambda (x) (eq 'op (car x))) ren1)))
	(sorts1
	 (mapcar #'(lambda (x) (cons (cadr x) (caddr x)))
	 (remove-if-not #'(lambda (x) (eq 'sort (car x))) ren1)))
	(sorts2
	 (mapcar #'(lambda (x) (cons (cadr x) (caddr x)))
	 (remove-if-not #'(lambda (x) (eq 'sort (car x))) ren2))))
  (let ((ren2map
	 (mapcar
	  #'(lambda (x)
	      (if (eq 'sort (car x))
		  `(sort ,(cadr x)
			 ,(modexp_eval$image_rename_sort sorts1 (caddr x)))
		`(op ,(cadr x)
		     ,(modexp_eval$image_rename_op ops1 (caddr x)))
	      ))
	  ren2)
	 )
	(ren1red (mapcan #'(lambda (x) ;19 Jan 88
		   (if (member (cadr x) ren2 :test #'(lambda (x y)
			           (equal x (cadr y))))
		     nil
		     (list (modexp_eval$ops_inverse_image_sorts sorts2 x)))
		   )
		   ren1)))
   ; **** added call to rem-id ****
    (rem-id (append ren2map ren1red))
  )))

(defun modexp_eval$image_rename_sort (ren x)
  (let ((val (assoc x ren :test #'modexp_eval$sort_match)))
    (if val (cdr val) x)
  ))

;&&&& more
(defun modexp_eval$image_rename_op (ren x)
  (let ((val (assoc x ren :test #'equal)))
    (if val (cdr val) x)
  ))

(defun modexp_eval$inverse_image_rename_sort (ren x)
  (let ((val (rassoc x ren :test #'modexp_eval$sort_match)))
    (if val
	(if (and (consp val) (null (cdr val))) (caar val) (car val))
    x)
  ))

(defun modexp_eval$ops_inverse_image_sorts (sorts x)
  (if (eq 'sort (car x)) x
  (if (eq 'op (car x))
    (let ((src (cadr x)) (tgt (caddr x)))
    (let* ((pos1 (position ":" src :from-end t :test #'equal))
	   (pos2 (position "->" src :from-end t :test #'equal)))
    (if (and pos1 pos2)
	(let ((opname (subseq src 0 pos1))
	      (ar (subseq src (1+ pos1) pos2))
	      (coar (nth (1+ pos2) src)))
	  `(op
	    ,(append
	      opname '(":")
	      (mapcar #'(lambda (u) (modexp_eval$inverse_image_rename_sort
				     sorts u))
		      ar)
	      '("->") (list (modexp_eval$inverse_image_rename_sort
			     sorts coar)))
	    ,tgt
	    ))
	  
      x)))
    x)
  ))

;@@ need to consider (specn x) (op x) (sort x) (qual x y)
(defun modexp_eval$reduce_rename (mod ren)
  (let ((sort_res nil) (op_res nil) (lst (reverse ren)) x)
  (loop
    (when (null lst) (return))
    (setq x (car lst) lst (cdr lst))
    (cond
     ((eq 'sort (car x))
      (when (mod_eval$$find_qual_sort_in mod (cadr x))
	(push x sort_res)))
     ((eq 'op (car x))
      (let ((opname (if (consp (cadr x)) (cadr x) (list (cadr x)))))
      (when (let ((val (mod_eval$find_operator_named_in mod opname)))
	      (and val
		   (not (and
		    (eq 'object (module$kind mod))
		    (eq 'theory (module$kind (operator$module val)))))))
        (push (list 'op opname
	            (if (atom (caddr x)) (list (caddr x)) (caddr x)))
	 op_res))))
     )
  )
  (append (sort sort_res #'ob<) (sort op_res #'ob<))
  ))

;;; the following should be able to assume that component
;;; module expressions have already be evaluated in the process
;;; of nomalization
;;; note: need to correctly create the names of the modules
(defun modexp_eval$create (modexp)
  (cond
   ((atom modexp)
    (cons 'error modexp))
   ((eq '+ (car modexp)) ; right associate and re-order &&&&
    ;&&&& bad approach -- collect set of tree-of-+'s args and do them all
    ;  together; just call create_plus on the whole
    (modexp_eval$create_plus
     (modexp_eval$eval_whole (cadr modexp))
     (modexp_eval$eval_whole (caddr modexp))))
   ((eq '* (car modexp))
    (modexp_eval$create_rename
     (modexp_eval$eval (cadr modexp))
     (caddr modexp)))
   ((eq '*view (car modexp)) ;12 Jan 88
    (break "modexp_eval$create: missing case *view"))
   ((eq '|:| (car modexp))
    (let ((modpar (modexp_eval$eval (cadr modexp)))) ;&&&&
    (when (not (typep modpar 'module))
       (princ "Unknown parameterized module in instantiation: ")
       (if (and (consp modpar) (eq 'error (car modpar)))
	   (princ (cdr modpar))
	 (print$name modpar))
      (obj3-to-top))
    (when (eq obj$current_module modpar)
       (princ "Warning: module ") (print$name obj$current_module)
       (princ " instantiates itself") (terpri)
       (obj3-module-error))
    (let ((args
	   (do ((i 0 (1+ i))
		(r (caddr modexp) (cdr r))
		(res nil))
	       ((null r) (nreverse res))
	     (push
	      (modexp_eval$evaluate_view_arg i (car r) modpar)
	      res))))
    (modexp_eval$create_instantiation
     modpar
     args))))
   ((eq 'view (car modexp)) ;&&&&
    (modexp_eval$view_can_defaults modexp))
   ((eq 'under (car modexp)) ;&&&&
    (modexp_eval$view_can modexp))

   ((eq 'view_from (car modexp)) ; the canonicalized version
    modexp) ; assume that it is in canonical form
   (t (break "modexp_eval$create: bad modexp form"))
  ))

; a plus like an object that protects only of the components
(defun modexp_eval$create_plus (mod1 mod2)
  (when (modexp_eval$is_error mod1)
      (progn
	(princ "Warning: first argument to + in module expression") (terpri)
	(princ "is undefined: ") (print$simple_princ_open (cdr mod1)) (terpri)
	(setq mod1 nil)))
  (when (modexp_eval$is_error mod2)
      (progn
	(princ "Warning: second argument to + in module expression") (terpri)
	(princ "is undefined: ") (print$simple_princ_open (cdr mod2)) (terpri)
	(setq mod2 nil)))
  (if (and mod1 mod2)
      (let ((mod_name (modexp$make_plus mod1 mod2)))
      (let ((newmod (module$create mod_name (module$kind mod1) nil)))
      (let ((obj$current_module newmod))
	(mod_eval$$!import_module newmod 'protecting mod1)
	(mod_eval$$!import_module newmod 'protecting mod2)
	(mod_eval$$!process_final newmod) ;@
	newmod
      )))
    (or mod1 mod2
	(progn
	  (princ "Giving up") (terpri)
	  (obj3-to-top)))
    ))

; note: mapping$apply must use memo tables since
; mapping may affect sub-objects (e.g. with "protecting A[X]",
; "X" a parameter)
(defun modexp_eval$create_instantiation (mod args)
  (let ((mappg (modexp_eval$!obtain_mapping_from_views mod args))
	(mod_name (modexp$make_instantiation mod args)))
  (let ((module (mapping$apply mod_name mappg mod)))
    (modexp_eval$!add_defn mod_name module)
    module
  )))

;;;&&&& using mod in argument so can get the views used in defintion of module
(defun modexp_eval$!obtain_mapping_from_views (mod vws)
  (let ((parms (module$parameters mod)))
  (when (not (= (length parms) (length vws)))
    (princ "Error: number of actual parameters incorrect for parameterized ")
    (princ "object:") (terpri)
    (print$name mod)
    (princ "[") (prin1 (length vws)) (princ " actual; ")
      (prin1 (length parms)) (princ " required]") (terpri)
    (obj3-to-top))
  (let ((mppg (modexp_eval$convert_view_to_mapping
	       (cdar parms)
	       (car vws))))
    (let (vw ath)
    (do ((vl (cdr vws) (cdr vl))
	 (tl (cdr parms) (cdr tl)))
	((null vl))
      (setq vw (car vl) ath (cdar tl))
      (let ((next_mppg (modexp_eval$convert_view_to_mapping ath vw)))
      (setq mppg (mapping$merge mppg next_mppg)))))
    mppg
  )))

; view is assumed already evaluated (canonicalized)
; ath -- the actual theory in the parameterized object
;&&&& believe that should substitute the actual theory (ath) for
;  the "generic" theory used in the view
;&&&& can the following be simplified? -- assuming view canonicalized?
(defun modexp_eval$convert_view_to_mapping (ath avw)
  (setq avw (modexp_eval$$simplify_view avw))
  (cond
   ((eq 'view_from (car avw))
    (let ((vw (modexp$make_view_from (cadr avw)
		(modexp_eval$eval (caddr avw))
		(cadddr avw))))
    (let ((modm (acons
		 ath
		 vw ; needs to be the view itself to allow proper
		    ;   composition &&&&
		 nil))
	  (sortm
	    ;&&&& consider remove-if cat = cdr
	    (mapcar #'(lambda (x)
			(let ((v (cdr x)))
			  (cons ;&&&& "by name" is this satisfactory?
			        (mod_eval$$find_qual_sort_in
				    ath (sort$name (car v)))
				(cadr v))))
		    ;@@ up
	    (remove-if-not #'(lambda (x) (eq 'sort (car x))) (cadddr vw))))
	  (opm ;&&&&
	   (mapcar #'(lambda (x)
		       (cons (term$head (car x))
			     `(replace ,(term$subterms (car x))
				       ,(cdr x))))
	   (mapcar #'(lambda (x)
		       (let ((v (cdr x)))
			 (cons (let ((op (term$head (car v))))
				 (term$make_term_check_op
				  (mod_eval$$find_qual_operator_in ;@@
				    ath (operator$name op)
				    (mod_eval$$recreate_sorts ath
				      (operator$arity op))
				    (mod_eval$$recreate_sort ath
			              (operator$coarity op)))
				  (term$subterms (car v))))
			       (cadr v))))
	    (remove-if-not #'(lambda (x) (eq 'op (car x))) (cadddr vw))))))
       (mapping$make `(map ,ath ,vw) sortm opm modm)
       )
    ))
   (t (break "modexp_eval$convert_view_to_mapping: illegal form; incomplete"))
  ))

; create module and then rename -- conceivably could try and optimize the
; case when module is a non-trivial expression
(defun modexp_eval$create_rename (mod ren)
  ; create new module and throw in pieces at the same time renaming
  (let ((mod_name (modexp$make_rename mod ren)))
  (let ((newmod (module$create mod_name (module$kind mod) nil)))
  (let ((sortmap nil) (opmap nil) (modmap (list (cons mod newmod))))
  (let ((map (mapping$make mod_name sortmap opmap modmap)))
  (setq sortmap
    (mapcar #'(lambda (x)
		(let ((sort (mod_eval$$find_qual_sort_in mod (cadr x)))) ;@@
		  (unless sort
		    (princ "sort not found for rename: ")
		    (princ (cadr x)) (terpri))
		  (cons
		       sort
		     ;&&&& non-bijective case
		     (let ((res (mod_eval$$find_qual_sort_in ;@@
				 newmod (caddr x))))
		       (if res res
			 (let ((dmod (modexp_eval$$create_dummy_mod
				      map (sort$module sort)
				      (list 'sort (cadr x) (caddr x)))))
			 (push (cons (sort$module sort) dmod) modmap)
			 (let ((new (sort$create
				     (if (and (consp (caddr x))
					      (null (cdr (caddr x))))
					 (car (caddr x)))
				     dmod)))
			   (when (sort$info sort)
			     (setf (sort$constructor new)
			       (mod_eval$create_built_in_constructor new))
			     (setf (sort$info new) (sort$info sort)))
			   (module$!add_sort newmod new)
			   new))))
		     )))
    (remove-if-not #'(lambda (x) (eq 'sort (car x))) ren)))
  (setf (mapping$sort map) sortmap)
  (setf (mapping$module map) modmap)
  (setq opmap
    (mapcan #'(lambda (x)
	       (let ((ops (mod_eval$find_all_qual_operators_named_in
			  mod (cadr x))))
	       (unless ops
		 (princ "operator not found in rename: ")
		 (princ (cadr x))
		 (terpri))
	       (let ((res nil))
	       (dolist (op ops)
		 ;&&&& exists
		 (let ((newop (modexp_eval$$rename_op map
				  mod
				  newmod sortmap op (caddr x))))
		   (push (cons op (cons 'rename newop)) res)
		 ))
	         res)))
    (remove-if-not #'(lambda (x) (eq 'op (car x))) ren)))
  (setf (mapping$op map) opmap)
  ;&&&&check case of sub-renames
  (mapping$build map mod newmod)
  )))))

;&&&& produce name of operator for moment -- temp
(defun modexp_eval$$op_refer (opr)
  (if (consp opr)
    (if (util$check_enclosing_parens opr) (butlast (cdr opr)) opr)
    (list opr))
  )

(defun modexp_eval$$rename_sort (map oldmod newmod sortmap sort)
  (declare (ignore newmod))
  (let ((val (or (assoc sort sortmap)
		 (assoc sort (mapping$sort map)))))
  (if (and (null val)
	   (not (eq oldmod (sort$module sort))))
      sort
  (if (typep (cdr val) 'sort) (cdr val)
    (let ((mod (sort$module sort)))
    (let ((modim (cdr (assoc mod (if map (mapping$module map) nil)))))
    (let ((res (if modim ;&&&&
		(find sort (module$sorts modim) ;@@><
		  :test #'(lambda (x y) (and
			    (equal (sort$name x) (sort$name y))
			    (eq (sort$module x) (sort$module y))))))))
    (if res res
    (let ((dmod (if modim modim
		  (modexp_eval$$create_dummy_mod
		   map (sort$module sort) (list 'sort sort)))))
    (let ((newsort (mod_eval$$recreate_sort dmod sort)))
      (if val (rplacd val newsort)
	(if map (push (cons sort newsort) (mapping$sort map))
	  (break "modexp_eval$$rename_sort: cannot map sort")))
      (module$!add_sort dmod newsort)
      newsort)
    )))))))))

(defun modexp_eval$$rename_sorts (map oldmod newmod sortmap sorts)
  (mapcar #'(lambda (x) (modexp_eval$$rename_sort map oldmod newmod sortmap x))
    sorts))

(defun modexp_eval$$create_dummy_mod (map mod info)
  (let ((val (assoc mod (mapping$module map))))
    (if val (cdr val)
      (progn
	(let ((newmod (module$create "DUMMY" 'theory nil)))
	(setf (getf (module$status newmod) 'dummy) t)
	(setf (getf (module$status newmod) 'rename_mod)
	      (cons mod info))
	newmod)))))

(defun modexp_eval$is_dummy_mod (mod)
  (and
   (typep mod 'module)
   (equal "DUMMY" (module$name mod))
   (getf (module$status mod) 'dummy)))

;derived from mapping$$recreate_operator
(defun modexp_eval$$rename_op (map oldmod newmod sortmap operator op_name)
  (let ((oldmod
	 (if (eq oldmod (operator$module operator))
	     oldmod
	   (let ((amod (cdr (assoc (operator$module operator)
				     (mapping$module map)))))
	       (if amod (setq newmod amod)
		 (progn
		   (setq newmod (modexp_eval$$create_dummy_mod
			 map
			 (operator$module operator)
			 (list 'op (operator$name operator) op_name)))
		   (push (cons (operator$module operator) newmod)
		       (mapping$module map))))
	       (operator$module operator)))))
  (let ((opnm (if (util$check_enclosing_parens op_name)
		(butlast (cdr op_name))
		op_name)))
  (let ((op_arity (modexp_eval$$rename_sorts map oldmod newmod sortmap
		    (operator$arity operator)))
	(op_coarity (modexp_eval$$rename_sort map oldmod newmod sortmap
		      (operator$coarity operator))))
  (let ((val (mod_eval$$find_qual_operator_in
	      newmod opnm op_arity op_coarity)))
    ;@@up
    (if val val
      (let* ((val (modexp_eval$$rename_recreate_op_form
		      map oldmod newmod sortmap (operator$form operator) opnm))
	     (op_form (car val)) (is_standard (cadr val)))
      (let ((newop
	     (let ((obj$current_module oldmod))
	     (operator$!update_intrinsic
	     (operator$create opnm op_arity op_coarity newmod
		       (modexp_eval$$recreate_theory
			(operator$theory operator))
		       (module$operator_rew_strategy oldmod operator)
		       (module$operator_user_rew_strategy oldmod operator)
		       (operator$is_memo operator)
		       (operator$error_strategy operator)
		       (rule_ring$create nil) ; rules wst
		       nil ; rules wdt
		       (operator$precedence operator)
		       op_form
		       (operator$syntactic_type_from_name opnm)
		       is_standard
		       (operator$polymorphic operator)
		       (operator$is_marked_strictly_overloaded operator)
		       )
		(operator$intrinsic operator)
	     ))))
	(module$!add_operator newmod newop)
	newop
	))))
  ))))

(defun modexp_eval$$recreate_theory (thy)
  (theory$create (theory$name thy)
    (if (theory$zero thy) `(to_rename ,(theory$zero thy)) nil))
  )

; op modexp_eval$$rename_recreate_op_form : Module Operator-Form ->
;     Operator-Form
;&&&&
(defun modexp_eval$$rename_recreate_op_form
           (map oldmod module sortmap op_frm op_name)
  (let ((is_standard (not (member "_" op_name :test #'equal)))
	(res1
	 (mapcan #'(lambda (i)
		(if (eq 'argument (car i))
		    (list (list* 'argument (cadr i)
			    (modexp_eval$$rename_sort map oldmod module
			      sortmap (cddr i))
		  ))
		  nil))
	    op_frm)))
  (let ((res nil)
	(apat (if is_standard (mod_eval$build_standard_pattern op_name res1)
		op_name)))
  (dolist (i apat)
    (if (equal "_" i) (push (pop res1) res)
      (push (cons 'token i) res)))
  (list (nreverse res) is_standard)
  )))

; note: mod is evaluated
(defun modexp_eval$evaluate_view_arg (argno vw mod)
  (cond
   ((modexp_eval$is_error mod)
    (princ "Cannot evaulate: ") (print$name mod) (terpri)
    (obj3-to-top))
   ((atom vw)
    (modexp_eval$view_can vw)) ;28 Apr 87 ;><
   ;@@ new (specn x) (op x) (sort x) (qual x y) like the above -- pass on?
   ;@@ more
   ((or (eq 'view_from (car vw)) (eq 'view_mapping (car vw)))
    (modexp_eval$$simplify_view
     vw)) ; assume that it is in canonical form
   ((eq 'view (car vw))
    (let ((real_view (modexp_eval$view_amplify argno vw mod)))
    (let ((view
	   (modexp_eval$$simplify_view ;&&&&
	     (modexp_eval$$view_canon_defaults ;## defaults
	      (cadr real_view)
	      (caddr real_view)
	      (cadddr real_view)
	      )
	    )))
    (let ((aview (modexp_eval$find_in_view_env view)))
      (if aview (cdr aview)
	(progn
	  (modexp_eval$!add_view_defn real_view view)
	  (modexp_eval$!add_view_defn view view)
	  view
      ))))))
   ((eq 'under (car vw))
    (modexp_eval$evaluate_view_arg argno vw mod)) ;&&&&
   ((member (car vw) '(|:| + *))
    (modexp_eval$eval vw)) ;&&&&
   (t (break "modexp_eval$evaluate_view_arg: bad modexp form"))
  ))

; third argument here must be evaluated
(defun modexp_eval$view_amplify (argno vw mod)
  (cond
   ((eq 'under (car vw))
    (break "modexp_eval$view_amplify: `under' not yet"))
   ((eq 'view (car vw))
    (cond
     ((eq 'none (cadr vw))
      (let* ((arg (nth argno (module$parameters mod)))
	     (arg_name (module$name (cdr arg))))
	(modexp$make_view arg_name (caddr vw) (cadddr vw))))
     (t (let* ((arg (nth argno (module$parameters mod)))
	       (arg_name (module$name (cdr arg)))
	       (vw_th (cadr vw))
	       (new_th
		(if (and (modexp_eval$is_parameter_theory arg_name)
			 (equal vw_th (module$name (caddr arg_name))))
		    arg_name
		  vw_th)))
	  (modexp$make_view new_th (caddr vw) (cadddr vw))
	  ))))
   (t vw) ;&&&& check in env?
  ))

(defun modexp_eval$nth_param (argno mod)
  (module$name (cdr (nth argno (module$parameters mod))))
  )

(defun modexp_eval$principal_sort (mod)
  (let ((val (module$principal_sort mod)))
    (if val val
      (let ((val2 (modexp_eval$primary_sort mod)))
	(if val2 val2
	  nil
	  ;*obj_BOOL$sort_Bool*
	  )))))

(defun modexp_eval$theory_principal_sort (mod)
  (let ((val (module$principal_sort mod)))
    (if (and val (not (eq 'object (module$kind (sort$module val)))))
      val
      (let ((val2 (modexp_eval$primary_sort mod)))
	(if (and val2 (not (eq 'object (module$kind (sort$module val2)))))
	  val2
	  nil
	  ;*obj_BOOL$sort_Bool*
	  )))))

(defun modexp_eval$primary_sort (mod)
  (let ((sorts (reverse (module$sorts mod)))
	(res nil))
    (dolist (s sorts)
      (when (eq (sort$module s) mod)
	;(setq res s)
	(return)))
    (if res res
      (dolist (m (reverse (module$sub_modules mod)) nil)
        (let ((val (modexp_eval$primary_sort (car m))))
	  (when val (return val))))
    )
  ))

(defun modexp_eval$top_level_view_eval (nm vwex)
  (let ((vwpars (modexp_parse$parse_view vwex)))
  (let ((val (modexp_eval$find_in_view_env vwpars)))
  (if val (cdr val)
  (let ((vw
	 ;(modexp_eval$view_defaults (modexp_eval$view_can vwpars)) ;##defaults
	 (modexp_eval$view_can_defaults vwpars)
	 ))
    (modexp_eval$!add_view_defn nm vw) ;<>??
    vw
  )))))

;&&&& not used
(defun modexp_eval$view_parse (vwex)
  (when (not (eq 'view (car vwex))) (break "modexp_eval$view_parse bad view"))
  `(view ,(let ((val (cadr vwex)))
	    (modexp_parse$parse (if (atom val) (list val) val)))
	 ,(let ((val (caddr vwex)))
	    (modexp_parse$parse (if (atom val) (list val) val)))
	 ,(cadddr vwex))
  )

;&&&&
(defun modexp_eval$is_rename_injective (ren)
  (let ((srts nil) (ops nil))
    (dolist (x ren)
      (let ((v (cdr x)))
      (let ((pr (cons (car v) (cadr v))))
      (if (eq 'sort (car x))
	(push pr srts)
	(push pr ops)))))
    (do ((lst srts (cdr lst)))
	((null lst))
      (when (rassoc (cdar lst) (cdr lst) :test #'equal)
	(return-from modexp_eval$is_rename_injective nil)))
    (do ((lst ops (cdr lst)))
	((null lst))
      (when (rassoc (cdar lst) (cdr lst) :test #'equal)
	(return-from modexp_eval$is_rename_injective nil)))
    t
  ))

(defun modexp_eval$view_can_defaults (vw)
  (setq vw (modexp_eval$$simplify_view vw)) ;&&&&
  (cond
   ((atom vw)
    (let ((val (modexp_eval$find_in_view_env vw)))
      (if val (cdr val)
	vw)))
   ((member (car vw) '(specn op sort qual)) vw) ;@@ new
   ((eq 'view (car vw))
    ; getting theory from context
    (when (eq 'none (cadr vw)) (cond
      (*modexp_eval$abstr_mod*
       (setq *modexp_eval$abstr_mod*
	     (modexp_eval$eval *modexp_eval$abstr_mod*))
       (rplaca (cdr vw) (module$name (cdr (nth *modexp_eval$arg_pos*
			  (module$parameters *modexp_eval$abstr_mod*)))))
       )
      (t
       (princ "Cannot omit theory from view in this context") (terpri)
       (princ "view: ") (print$name vw) (terpri)
       (obj3-to-top)
       )
      ))
    (modexp_eval$$view_canon_defaults
     (modexp_eval$canonicalize (cadr vw))
     (modexp_eval$canonicalize (caddr vw))
     (cadddr vw)))
   ((eq 'view_from (car vw)) vw)
   ((member (car vw) '(|:| + *))
    (modexp_eval$canonicalize vw))
   ((eq '*view (car vw)) ;&&&&
    (modexp$make_view_rename
     (modexp_eval$view_can (cadr vw))
     (caddr vw)))
   (t (break "modexp_eval$view_can: not yet"))))

;@@@ not used in practice?
(defun modexp_eval$$view_canon_defaults (th mod vw_map)
  (when (modexp$is_view mod) ;&&&&
    (let ((simp (modexp_eval$$simplify_view
		 (modexp$make_view_from th mod vw_map))))
     (setq th (cadr simp))
     (setq mod (caddr simp))
     (setq vw_map (cadddr simp))))
  (let ((src_mod (modexp_eval$eval_whole th))
	(dst_mod (modexp_eval$eval mod)))
  (when (or (modexp_eval$is_error src_mod)
	    (modexp_eval$is_error dst_mod))
    (princ "Cannot evaluate module in view:")
    (when (modexp_eval$is_error src_mod) (princ " ") (print$modexp th))
    (when (modexp_eval$is_error dst_mod)
      (when (modexp_eval$is_error src_mod) (princ " and"))
      (princ " ") (print$modexp mod))
    (terpri)
    (obj3-to-top))
  (let ((sort_maps
	 (remove-if-not #'(lambda (x) (eq 'sort (car x))) vw_map))
	(var_decls
	 (remove-if-not #'(lambda (x) (eq 'var (car x))) vw_map))
	(op_maps
	 (remove-if-not #'(lambda (x) (eq 'op (car x))) vw_map))
	(pr (modexp_eval$theory_principal_sort src_mod)))
  (let ((sort_mapping ; an alist
	  (mapcan
	   #'(lambda (x)
	       (let ((srcs
		      (mod_eval$$find_qual_sort_in src_mod (cadr x))) ;@@
		     (tgts
		      (mod_eval$$find_qual_sort_in dst_mod (caddr x)))) ;@@
	       (if (or (null srcs) (null tgts))
		 (progn
		   (when (null srcs)
		     (princ "Warning: in view from ")
		     (print$simple_princ_open th) (terpri)
		     (princ "to ")
		     (print$simple_princ_open mod)
		     (princ ", source sort not recognized")
		     (princ " ")
		     (print$simple_princ_open (cadr x)) (terpri)
		     (obj3-to-top))
		   (when (null tgts)
		     (princ "Warning: in view from ")
		     (print$simple_princ_open th) (terpri)
		     (princ "to ")
		     (print$simple_princ_open mod)
		     (princ ", target sort not recognized")
		     (princ " ")
		     (print$simple_princ_open (caddr x)) (terpri)
		     (obj3-to-top))
		   nil)
		 (list (cons srcs tgts)))))
	   sort_maps)))
  (dolist (s (module$sorts src_mod))
    (when (eq src_mod (sort$module s))
	  (unless (modexp_eval$$view_map_item_sort_pre s sort_mapping)
	    (let ((val (mod_eval$$find_qual_sort_in dst_mod
		          (sort$name s)))) ;@@
  ;;principal sort ; Jan 15 92
	      (if (eq s pr) ;is this the principal sort?
		  (let ((val (modexp_eval$principal_sort dst_mod)))
		  (unless (eq pr val)
		    (when (and pr val) ;@@
		      (push (cons pr val) sort_mapping))))
  ;;same name sort
	      (if val
		  (push (cons s val) sort_mapping)
		(progn
		  (princ "Warning: view incomplete for sort ")
		  (print$name s) (terpri)
		  (princ "View from ") (print$name src_mod) (princ " to ")
		  (print$name dst_mod) (terpri)
		  (obj3-to-top)
		)))))))
  (let ((sortmap
	  (mapcar #'(lambda (x) `(sort ,(car x) ,(cdr x)))
	    sort_mapping)))
  (let* ((src_vars
	  (mapcan #'(lambda (x)
		      (let ((sort (mod_eval$$find_qual_sort_in
				   src_mod (caddr x))))
			(when (null sort)
			  (princ "Sort not found in view variable")
			  (princ "declaration:")
			  (print$simple_princ_open (caddr x))
			  (terpri))
			;@@ up
			(mapcar #'(lambda (y)
				    (variable$make_var y sort))
			  (cadr x))
		      ))
	   var_decls))
	 (dst_vars
	  (mapcar #'(lambda (x)
		      (let ((val (cdr (assoc (variable$initial_sort x)
				      sort_mapping))))
			(if val (variable$make_var (variable$name x) val)
			  x)))
	    src_vars))
	 (op_mapping (modexp$compute_op_mappings
		    sortmap
                    src_mod src_vars
		    dst_mod dst_vars
		    op_maps)))

  ;; handle constants last
      (let ((ops nil) (consts nil) thisop)
      (dolist (o (module$operators src_mod))
	(if (null (operator$arity o)) (push o consts)
	  (push o ops)))
      (setq ops (nconc ops consts))
  ;;same name operator
      (dolist (o (module$operators src_mod))
	(when (eq src_mod (operator$module o))
	  (unless (setq thisop (modexp_eval$$view_map_item_op o op_mapping))
	    ;&&&& in the following: what if more than one?
	    (let ((val (mod_eval$$find_qual_operator_in dst_mod ;@@
			 (operator$name o)
			 (modexp_eval$$view_map_image_sorts dst_mod
			  (operator$arity o) sortmap)
			 (modexp_eval$$view_map_image_sort dst_mod
			  (operator$coarity o) sortmap))))
	      (setq thisop val)
	      (if val
		  (let ((vars (modexp$make_psuedo_vars_from_sorts
			       (operator$arity o))))
		    (push `(op ,(term$make_term_check_op o vars)
			       ,(term$make_term_check_op val vars))
			  op_mapping))
  ;;unique matching rank and attributes
		(let ((val (modexp_eval$$find_some_operator_in ;@@><
			    dst_mod
			    (modexp_eval$$view_map_image_sorts dst_mod
			     (operator$arity o) sortmap)
			    (modexp_eval$$view_map_image_sort dst_mod
			     (operator$coarity o) sortmap)
			    (module$operator_equational_theory src_mod o))))
		  (setq thisop val)
		  (if val
		      (let ((vars (modexp$make_psuedo_vars_from_sorts
				   (operator$arity o))))
			(push `(op ,(term$make_term_check_op o vars)
				   ,(term$make_term_check_op val vars))
			      op_mapping))
		    (progn
		      (princ "Warning: view incomplete for operator ")
		      (print$name o) (princ " of ") (print$name src_mod)
		      (terpri)
		      (princ "View from ") (print$name src_mod) (princ " to ")
		      (print$name dst_mod) (terpri)
		      (obj3-to-top)
		      ))))))
	  (when thisop
            (let ((o2 (if (consp thisop) (term$head (caddr thisop))
			thisop)))
	    (let ((othy (module$operator_equational_theory src_mod o))
		  (o2thy (module$operator_equational_theory dst_mod o2)))
	    (let ((oz (car (theory$zero othy)))
		  (o2z (car (theory$zero o2thy))))
	      (when (and oz
			 ;&&&&><next
			 (not (modexp_eval$$view_map_item_op oz op_mapping))
			 o2z
			 (term$is_constant oz) (term$is_constant o2z))
		  (push `(op ,oz ,o2z) op_mapping))
	  ))))
	  ))
      )
    ; final result:
    ;&&&& >< could overwrite old view
    (modexp$make_view_from src_mod dst_mod
        (append
	 (sort sortmap
	       #'ob<
	       :key #'(lambda (x)
			(if (typep (cadr x) 'sort)
			    (sort$name (cadr x))
			 (break "modexp_eval$$view_canon_defaults: bad var"))))
	                  ;&&&&
	 (sort op_mapping
	       #'ob<
	       :key #'(lambda (x)
			(let ((val (term$head (cadr x))))
			  (if (symbolp val) val
			    (operator$name val)))))
	 ))
  ))))))

;@@? not used; see next -- rename
(defun modexp_eval$$view_map_item_sort (x vw_map)
  (find-if #'(lambda (v) (eq x (cadr v))) vw_map))

;@@new
(defun modexp_eval$$view_map_item_sort_pre (x vw_map)
  ;(find-if #'(lambda (v) (eq x (car v))) vw_map)
  (assoc x vw_map)
  )

;@@?
(defun modexp_eval$$view_map_image_sort (mod x vw_map)
  (let ((val (find-if #'(lambda (v) (eq x (cadr v))) vw_map)))
  (if val (caddr val)
  (if (member x (module$sorts mod)) x
  (let ((val2 (mod_eval$$find_qual_sort_in mod (sort$name x))))
  (if val2 val2 x))))))

(defun modexp_eval$$view_map_image_sorts (mod l vw_map)
  (mapcar #'(lambda (x) (modexp_eval$$view_map_image_sort mod x vw_map))
    l))

;@@?
(defun modexp_eval$$view_map_item_op (x vw_map)
  (find-if #'(lambda (v) (eq x (term$head (cadr v)))) vw_map))

(defun modexp_eval$$find_some_operator_in (module arity coarity theory) ;@@
  (let ((val
	 (remove-if-not
	  #'(lambda (op)
	      (and (modexp_eval$$same_sort
		    coarity (operator$coarity op))
		   (= (length arity) (length (operator$arity op)))
		   (every #'modexp_eval$$same_sort arity (operator$arity op))
		   (modexp_eval$is_included_in_theory
		    theory (module$operator_equational_theory module op))
		   ))
	  (module$operators module))))
    (if val
      (if (null (cdr val)) (car val)
	(let ((newval (remove-if #'operator$polymorphic val)))
	  (if newval
	    (if (null (cdr newval)) (car newval)
	      nil)
	    nil)))
      nil)))

(defun modexp_eval$$same_sort (s1 s2)
  (and s1 s2 ;&&&&
       (equal (sort$name s1) (sort$name s2)))
  )

(defun modexp_eval$is_included_in_theory (th1 th2)
  (and
   (if (theory$contains_associativity th1)
       (theory$contains_associativity th2) t)
   (if (theory$contains_commutativity th1)
       (theory$contains_commutativity th2) t)
;   (if (theory$contains_idempotency th1) ;ignore idempotence
;       (theory$contains_idempotency th2) t)
   (if (theory$contains_identity th1)
       (theory$contains_identity th2) t)
  ))

; used in mapping.lsp
(defun modexp_eval$target_of_view_arg (vw)
  (cond
   ((atom vw) vw)
   ((member (car vw) '(view view_from view_mapping)) (caddr vw))
   (t (break "modexp_eval$target_of_view_arg: unknown view argument"))
  ))

(defun modexp_eval$is_parameter_theory (e)
  (and (consp e) (equal "::" (cadr e)) (equal 3 (length e))))

; check for view of a view
(defun modexp_eval$$simplify_view (me)
  (if (and (modexp$is_view me) (modexp$is_view (caddr me)))
      (modexp_eval$$compose_views me)
    me))

; compose a view of a view
(defun modexp_eval$$compose_views (vw)
  (let ((src1 (cadr vw))
	(tgt1 (caddr vw))
	(map1 (cadddr vw)))
  (let ((src2 (cadr tgt1))
	(tgt2 (caddr tgt1)))
    (list ;&&&&
     (car vw)
       src1
       ;&&&& was (modexp$make_plus src1 src2)
     tgt2
     ; compose the mappings
     (let ((ath (modexp_eval$eval_whole src2)))
     (let ((map (modexp_eval$convert_view_to_mapping
		 ath tgt1)))
     (mapping$reconstruct_view_mapping tgt1 map map1) ;><
     ))
     )
  )))

(defun modexp_eval$is_sub_module (x y)
  (if (consp y)
      (and (eq 'view_from (car y))
	   (or
 	    ; following modexp_eval$eval's should eval_whole's but
 	    ; for compatibility ...
	    (modexp_eval$is_sub_module x (modexp_eval$eval (cadr y)))
	    (modexp_eval$is_sub_module x (modexp_eval$eval (caddr y)))))
   (or
    (member x (module$sub_modules y))
    (let ((nm (module$name y)))
      (and (consp nm)
	   (or (eq '|:| (car nm)) (eq '* (car nm)))
	   (eq x (cadr nm)))
       )
    (some #'(lambda (sm)
	      (or (eq x (car sm))
		  (modexp_eval$is_sub_module x (car sm))))
	  (module$sub_modules y))
    )))

;; This procedure replaced by patch below Apr 14 1992
;; (defun modexp_eval$delete_module (x)
;;   (setq *modexp_eval$canon_env*
;;     (mapcan #'(lambda (e)
;; 		(unless
;; 		 (and (not (modexp_eval$is_simple_name (car e)))
;; 		      (let ((val (modexp_eval$find_in_env (cdr e))))
;; 			(and val
;; 			     (modexp_eval$is_sub_module x (cdr val)))))
;; 		 (list e)))
;;       *modexp_eval$canon_env*))
;;   (setq *modexp_eval$env*
;;     (mapcan #'(lambda (e)
;; 		(unless
;; 		 (and
;; 		  (not (modexp_eval$is_simple_name (car e)))
;; 		  (modexp_eval$is_sub_module x (cdr e)))
;; 		 (list e)))
;;       *modexp_eval$env*))
;;   nil
;;   )

(defun modexp_eval$delete_module (x)
  (setq *modexp_eval$canon_env*
    (mapcan #'(lambda (e)
		(unless
		 (and (not (modexp_eval$is_simple_name (car e)))
		      (let ((val (modexp_eval$find_in_env (cdr e))))
			(and val
			     (modexp_eval$is_sub_module x (cdr val)))))
		 (list e)))
      *modexp_eval$canon_env*))
  (setq *modexp_eval$view_env*
    (mapcan #'(lambda (e)
		(unless
		 (modexp_eval$is_sub_module x (cdr e))
		 (list e)))
      *modexp_eval$view_env*))
  (setq *modexp_eval$env*
    (mapcan #'(lambda (e)
		(unless
		 (and
		  (not (modexp_eval$is_simple_name (car e)))
		  (modexp_eval$is_sub_module x (cdr e)))
		 (list e)))
      *modexp_eval$env*))
  nil
  )

; 6 Dec 90 modified to delete simple named module itself
(defun modexp_eval$delete_module_all (x)
  (setq *modexp_eval$canon_env*
    (mapcan #'(lambda (e)
		(unless
		    (let ((val (modexp_eval$find_in_env (cdr e))))
		      (and val
			   (or (eq x (cdr val))
			       (modexp_eval$is_sub_module x (cdr val)))))
		  (list e)))
      *modexp_eval$canon_env*))
  (setq *modexp_eval$env*
    (mapcan #'(lambda (e)
		(unless
		    (or (eq x (cdr e))
			(modexp_eval$is_sub_module x (cdr e)))
		  (list e)))
      *modexp_eval$env*))
  nil
  )

(defvar *mod_eval$local_vars*)

;----------------
; top-level rename only
(defun mod_eval$create_renamed_module (mod nm)
  (let ((newmod (module$create nm (module$kind mod)
		    (module$parameters mod)
		    )))
  (let ((obj$current_module newmod))
  (let ((*mod_eval$local_vars* nil)) ;&&&
  ;; parameters
  (dolist (x (module$sub_modules mod))
    (module$!add_imported_module newmod (cdr x) (car x))
    (mod_eval$$!incorporate_module newmod (cdr x) (car x)))
  ;; at this point have already got a lot of sorts and operators (etc.)
  ;;   from the incorporated modules
  ;; :sorts
  (module$!replace_sort_order newmod (sort_order$new))
  (dolist (x (reverse (module$sorts mod)))
	  ;; reverse because want to preserve the original order
    (when (eq mod (sort$module x))
      (module$!add_sort newmod (mod_eval$$recreate_sort newmod x)))
    )
  ;; :principal_sort
  (let ((modprs (module$principal_sort mod)))
  (when modprs
  (module$!update_principal_sort newmod
      (if (eq mod (sort$module modprs))
	  (mod_eval$$find_sort_in newmod (sort$name modprs))
	modprs))))
  ;; :sort_relation
  (dolist (pr (module$sort_relation mod))
    (module$!adjoin_sort_relationship newmod
        (mod_eval$$find_sort_in newmod (sort$name (car pr)))
        (mod_eval$$find_sort_in newmod (sort$name (cdr pr)))))
        ;&&&& sort$name's
  (module$!replace_sort_order newmod
    (mod_eval$$recreate_sort_order newmod (module$sort_order mod)))
  ;; :operators
  (dolist (op (reverse (module$operators mod)))
	  ;; want to preserve the original order of operators
    (when (eq mod (operator$module op))
      (module$!adjoin_operator newmod
              (mod_eval$$recreate_operator mod newmod op))))
  (dolist (op (module$operators newmod))
	(mod_eval$$update_poly_rules newmod op)
	(mod_eval$$update_theory newmod op)
	)
  ;; :variables
      ;nil
  ;; :sort_constraints
      ;nil
  ;; :equations 
  (setf (module$equations newmod)
      (mapcar #'(lambda (r)
                  (mod_eval$$recreate_rule newmod r))
	      (module$equations mod)))
  ;; :sort_order
       ;nil at this point
  ;; :rules
    ;nil
  ;; :status
  (setf (module$status newmod) (copy-list (module$status mod)))
  (setf (getf (module$status newmod) 'is_compiled) nil)
  (setf (getf (module$status newmod) 'is_parse_setup) nil) ;@@
  (push (list 'renamed (module$name mod))
	(getf (module$status newmod) 'history))
  ;; :parse_dictionary :juxtaposition -- recompute from scratch
  ;; -- put in history
  (module$!replace_sort_order newmod ;&&&&
    (mod_eval$$recreate_sort_order newmod
     (module$sort_order mod)))
  ;; create parsing information
  (let ((obj$current_module newmod))
  (operator_table$!update_operator_info newmod)
  (parse$!update_parse_information newmod)
  ) ;let
  (modexp_eval$!add_defn nm newmod)
  (modexp_eval$!add_canon nm nm)
  newmod
  ;omit rule generation?
  ))))
