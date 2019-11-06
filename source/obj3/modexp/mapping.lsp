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

;; $Id: mapping.lsp,v 206.1 2003/09/29 12:46:23 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                    mapping -- unification of rename and view
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; renames map sort to sort, op to op
; views map sort to sort, op to op or op to expression
; further: in parameterization: map parameter sub-object to actual sub-object

;;;;;;;;;;;;;;;; Summary ;;;;;;;;;;;;;;;;
; defstruct mapping
; op assoc_image : Assoc-list Value -> Value
; op mapping$apply : Name Mapping Module -> Module
; op mapping$recreate_rule :
;        Module Sort-Mapping Operator-Mapping Mod-Mapping Rule -> Rule
; op mapping$$recreate_term :
;        Module Sort-Mapping Op-Mapping Module-Mapping Term -> Term
; op mapping$apply_op_mapping : Module Op-Mapping Term -> Term
; op mapping$image : List[Term] Term -> Term
; op mapping$merge : Mapping Mapping -> Mapping

;;;;;;;;;;;;;;;;; Representation

(defstruct (mapping
	     (:conc-name mapping$)
	     (:constructor mapping$make (name sort op module))
	     (:type list)
	     :named
	     )
	"a module mapping, unifies renaming and view application"
  (name "anonymous") ; name of mapping; e.g. name of view taken from
      ; may be more than one view, of course
  (sort nil) ; sort mapping (assoc list)
  (op nil) ; operator mapping (assoc list)
    ; image is of form (rename . image-value) -or-
    ;     (replace vars image-value)
    ;   flag = rename -- case of operator rename
    ;     -- image-value is an operator
    ;   flag = replace -- case of complex operator image in a view
    ;     -- the list of vars is for the substitution process (1/9/87)
    ;        (&&&& can eliminate the list of vars?)
    ;     -- image-value is a term with variables whose names are
    ;     indices into the subterms of the initial operator
    ;     (f a0 a1) with ((2 * (var 0)) + (var 1)) => ((2 * a0) + a1)
  (module nil)
    ; module replacement (assoc list) of (module . module)
  )

(defvar obj$current_module) ; used in optable stuff (external reference)

(defun mapping$is_rename (map)
  (let ((nm (mapping$name map)))
    (and (consp nm) (eq '* (car nm)))))

; op assoc_image : Assoc-list Value -> Value
(defun assoc_image (assoc_list x)
  (let ((val (assoc x assoc_list))) ; uses eql by default
    (if val (cdr val) x)))

; op assoc_images : Assoc-list Value-list -> Value-list
; cf. assoc_image
(defun assoc_images (assoc_list lst)
  (mapcar #'(lambda (x) (let ((val (assoc x assoc_list)))
			  (if val (cdr val) x)))
    lst))

;;; apply a mapping creating a new object
; op mapping$apply : Module-Name Mapping Module -> Module
; nm is name for resulting object
; in cases where not mapped; must recreate
; op mapping$apply : Name Mapping Module -> Module
(defun mapping$apply (nm map mod)
  ;; :name :kind :parameters -- creating component by component
  ;; :parameters nil (by default)
  (let ((newmod (cdr (assoc mod (mapping$module map)))))
    (if newmod
	(progn
	  (setf (module$name newmod) nm)
	  (setf (module$kind newmod) (module$kind mod))
	  (remf (module$status newmod) 'dummy))
      (progn
	(setq newmod (module$create nm (module$kind mod) nil))
	(push (cons mod newmod) (mapping$module map)))
      )
  (let ((obj$current_module newmod))
  (mapping$build map mod newmod))))

; same as above but doesn't rebind obj$current_module
(defun mapping$apply_internal (nm map mod)
  ;; :name :kind :parameters -- creating component by component
  ;; :parameters nil (by default)
  (let ((newmod (cdr (assoc mod (mapping$module map)))))
    (if newmod
	(progn
	  (setf (module$name newmod) nm)
	  (setf (module$kind newmod) (module$kind mod))
	  (remf (module$status newmod) 'dummy))
      (progn
	(setq newmod (module$create nm (module$kind mod) nil))
	(unless (assoc mod (mapping$module map))
	  (push (cons mod newmod) (mapping$module map))))
      )
  (mapping$build map mod newmod)))

(defun mapping$build (map mod newmod)
  (let ((*mapping$local_vars* nil))
  (let ((val (assoc mod (mapping$module map))))
    (if val
	(when (null (cdr val)) (rplacd val newmod))
      (push (cons mod newmod) (mapping$module map))))
  ; the following code may look very odd, but what it
  ; is doing is "delayed" identification of the module for a sort
  ; or operator; Note: thus a sort may be created and used before
  ; its module has properly been identified
  (when (mapping$is_rename map)
    (setf (module$parameters newmod) (module$parameters mod))
    (let ((modmap (mapping$module map)))
    (dolist (sm (mapping$sort map))
      (let ((s1 (car sm)) (s2 (cdr sm)))
      (let* ((mod1 (sort$module s1))
	     (a1 (assoc mod1 modmap)))
	(if (and a1 (not (eq (cdr a1) (sort$module s2))))
	    (progn
	      (setf (sort$module s2) (cdr a1))
	      (module$!adjoin_sort newmod s2))
	(if (mapping$mod_is_rename_dummy_for mod1 mod)
	    (progn
	      (setf (sort$module s2) newmod)
	      (module$!adjoin_sort newmod s2))
	))
      )))
    (dolist (om (mapping$op map))
      (let ((op1 (car om)) (op2 (cddr om))) ;19 Mar 87 cddr was caddr
	;rename case only
      (let* ((mod1 (operator$module op1))
	     (a1 (assoc mod1 modmap)))
	(if (and a1 (cdr a1) (not (eq (cdr a1) (operator$module op2))))
	    (progn
	     (setf (operator$module op2) (cdr a1))
	     (mapping$!check_rank newmod mod map op2)
	     (module$!adjoin_operator newmod op2)
	     (module$!transfer_operator newmod mod op2))
	(if (mapping$mod_is_rename_dummy_for mod1 mod)
	    (progn
	      (setf (operator$module op2) newmod)
	      (mapping$!check_rank newmod mod map op2)
	      (module$!adjoin_operator newmod op2)
	      (module$!transfer_operator newmod mod op2))
	))
      )))
    ))
  (let ((sortmap (mapping$sort map))
	(opmap (mapping$op map))
	(modmap (mapping$module map)))
  ;; :sub_modules
  ; need to consider sub-module-instantiation
  ;  also apply mapping; want to memo-ize appropriately;
  ;  in some sense must always apply the mapping to sub-objects
  ; idea: if sub-object contains parameter as sub-object then
  ;  map it (should always be directly there); the other source
  ;  of information is the name of the object; if is instantiated
  ;  then can see if the name contains a use of the parameter
  (module$!replace_sort_order newmod (sort_order$new))
  (mapping$import_submodules mod newmod map mod)
  ;; at this point have already got a lot of sorts and operators (etc.)
  ;;   from the incorporated modules
  ;; :sorts
  ; after have created sub-modules need to "fix" renaming
  ; Note this is very redundant with code at beginning for is_rename
  (when (mapping$is_rename map)
    (let ((modmap (mapping$module map)))
    (dolist (sm (mapping$sort map))
      (let ((s1 (car sm)) (s2 (cdr sm)))
      (let* ((mod1 (sort$module s1))
	     (a1 (assoc mod1 modmap)))
	(when (and a1 (cdr a1) (not (eq (cdr a1) (sort$module s2))))
	  (setf (sort$module s2) '"RENAME ERROR")
	  (setf (module$sorts newmod) (remove s2 (module$sorts newmod)))
	  (let ((srt (mapping$$find_sort_in (cdr a1) (sort$name s2)))) ;@@s
	    (module$!adjoin_sort newmod srt)
	    (rplacd sm srt))
	)
      )))
  ))
  (setq sortmap (mapping$sort map))
  (setq opmap (mapping$op map))
  (setq modmap (mapping$module map))
 (when (and $$debug (< 20 $$debug_level))
 (princ "### build ###") (terpri)
 (let ((*print$explicit* t))
 (print$struct map) (terpri))
 (print$module mod) (terpri)
 (print$module newmod) (terpri))
  (dolist (x (reverse (module$sorts mod)))
	  ;; reverse because want to preserve the original order
    (let ((sortmapval (assoc x sortmap)))
    (if sortmapval
	; should only arise in the case of renamings
      (let ((ims (cdr sortmapval)))
	(unless (member ims (module$sorts newmod))
	  (module$!adjoin_sort newmod ims)
	  (module$!replace_sort_order newmod
	    (sort_order$adjoin_sort (module$sort_order newmod) x))
	)
      )
      (if (eq mod (sort$module x))
	  (let ((sortim (mapping$$recreate_sort newmod modmap sortmap x)))
	  (unless (eq x sortim)
	    (push (cons x sortim) sortmap)
	    (setf (mapping$sort map) sortmap)
	    (setq x sortim)
	    )
	  (module$!adjoin_sort newmod sortim)
	  (module$!replace_sort_order newmod
	    (sort_order$adjoin_sort (module$sort_order newmod) sortim)))
      (let ((modv (assoc (sort$module x) modmap)))
      (when modv
	(let ((sortim (mapping$$recreate_sort newmod modmap sortmap x)))
	  (unless (eq x sortim)
	    (push (cons x sortim) sortmap)
	    (setf (mapping$sort map) sortmap)
	  )
       )))
      )
      )))
  (let ((modprs (module$principal_sort mod)))
  (when modprs
  (module$!update_principal_sort newmod
      (mapping$sort_image newmod sortmap modprs))))
  ;; :sort_relation
  (dolist (pr (module$sort_relation mod))
    (let ((s1 (mapping$sort_image newmod sortmap (car pr))) ;&&&& should exist
	  (s2 (mapping$sort_image newmod sortmap (cdr pr))))
    (module$!adjoin_sort_relationship newmod s1 s2)
    ))
  (let ((selfso (mapping$$recreate_sort_order newmod mod
		  modmap sortmap (module$sort_order newmod))))
  (module$!replace_sort_order newmod
    (sort_order$transitive_closure1
     (sort_order$merge
	 (mapping$$recreate_sort_order newmod mod
	   modmap sortmap (module$sort_order mod))
	 selfso
         ))))
  ;; :operators
  ; after have created sub-modules need to "fix" renaming for operators too
  (when (mapping$is_rename map)
    (let ((modmap (mapping$module map)))
    (dolist (om (mapping$op map))
      (let ((op1 (car om)) (op2 (cddr om)))
	;rename case only
      (let* ((mod1 (operator$module op1))
	     (a1 (assoc mod1 modmap)))
	(if (and a1 (cdr a1) (not (eq (cdr a1) (operator$module op2))))
	  (progn
	    (when (typep op2 'operator)
	      (setf (operator$module op2) '"RENAME ERROR"))
	    (setf (module$operators newmod)
		(remove op2 (module$operators newmod)))
	    (let ((opr (mapping$$find_operator_named_in (cdr a1) ;@@o?
			  (if (and (consp op2) (eq 'rename (car op2)))
			    (cdr op2)
			    (operator$name op2)))))
	    (when opr
	      (module$!adjoin_operator newmod opr)
	      (module$!transfer_operator newmod mod opr)
	      (rplacd (cdr om) opr)
	      ))
	    )
	  (when (or (member op2 (module$operators newmod))
		    (eq mod (operator$module op2)))
	    (mapping$!check_rank newmod mod map op2)
	    )
      )))
  ))) ;when is_rename
  (let ((polys nil))
  (dolist (op (reverse (module$operators mod)))
	  ;; want to preserve the original order of operators
    (when  (and (eq mod (operator$module op))
		(null (assoc op opmap)))
      ;if mapped comes from image (may even be "complex")
      (let ((newop (mapping$$recreate_operator mod newmod sortmap op)))
	(when (module$operator_polymorphic mod op)
	  (push (cons op newop) polys))
	(module$!adjoin_operator newmod newop))
    )
  )
  ;At this point all operators should exist; term recreation is possible
  ; The following changes allow make_term_with_sort_check to work
  (mod_eval$!update_sort_order newmod)
  (operator_table$!update_operator_info newmod)
  (mapping$!recreate_poly_rules mod newmod sortmap opmap modmap polys)
  (dolist (op (module$operators newmod))
    (when (eq newmod (operator$module op))
      (mapping$$update_theory newmod map op))
    )
  ) ;let polys
  ;; :variables
      ; should be nil at this point
  ;; :sort_constraints
      ; nil
  ;; :equations 
  (setf (module$equations newmod)
    (append
      (mapcar #'(lambda (r)
                  (mapping$recreate_rule newmod sortmap opmap modmap r))
	      (module$equations mod))
      (module$equations newmod)))
  ;; :sort_order
       ; recomputed above
  ;; :rules
    ; should be nil at this point; will be computed below
  ;; :assertions
    ; should be nil; since will be generated by rule-generation done later
  ;; :status
  (setf (module$status newmod) (copy-list (module$status mod)))
  (setf (getf (module$status newmod) 'is_compiled) nil)
  (setf (getf (module$status newmod) 'is_parse_setup) nil) ;@@
  (push (list '|:| map) (getf (module$status newmod) 'history))
  ;; :parse_dictionary :juxtaposition -- recompute from scratch
  ;; -- put in history
  (let ((obj$current_module newmod))
  (mod_eval$$!process_final newmod)) ; redoes rule generation ;@
      ; this depends on: rules (rule_gen$rule_generation) strategy-info
      ;   variables operators
      ; updates: dictionary:
 (when (and $$debug (< 20 $$debug_level))
 (princ "### build result ###") (terpri)
 (print$module newmod) (terpri))
  newmod ;the final result
  )))

(defun mapping$mod_is_rename_dummy_for (mod1 mod)
  (if (and (equal "DUMMY" (module$name mod1))
	   (getf (module$status mod1) 'dummy))
    (let* ((info (getf (module$status mod) 'rename_mod))
	   (oldmod (car info)))
    (eq oldmod mod))
  ))

(defun mapping$import_submodules (mod newmod map amod)
  (dolist (sbmd (reverse (module$sub_modules amod)))
    (mapping$import_submodule mod newmod map (cdr sbmd) (car sbmd))))

; first compute the submodule
(defun mapping$import_submodule (mod newmod map mode submod)
 (when (and $$debug (< 30 $$debug_level))
 (princ "### build adding ###") (terpri)
 (let ((*print$explicit* t))
 (print$struct map) (terpri))
 (prin1 mode) (princ " ") (print$name submod) (terpri)
 (print$module newmod) (terpri))
  (let ((modmap (mapping$module map)))
  (let ((val (assoc submod modmap))) ; is it mapped directly?
  (let ((submodim
    (if val
      (if (or (null (cdr val))
	      (modexp_eval$is_dummy_mod (cdr val)))
	  ;case of renaming
	  (mapping$map_submodule map mod submod)
	;associated value is a view in general
	(modexp_eval$target_of_view_arg (cdr val)))
    (if (mapping$$submodule_is_mapped modmap submod)
	(mapping$map_submodule map mod submod)
      submod))))
    (if (eq 'using mode)
	(mapping$import_submodules mod newmod map submodim)
      (mod_eval$$!import_module newmod mode submodim))))))

;op mapping$$recreate_sort : Module Modmap SortMap Sort -> Sort
; based on mod_eval$$recreate_sort
(defun mapping$$recreate_sort (mod modmap sortmap sort)
  (declare (ignore sortmap))
  (let ((sort_name (sort$name sort))
	(smod (sort$module sort)))
  (let ((mmod (cdr (assoc smod modmap))))
  (let ((themod (if mmod mmod mod)))
  (let ((asort (find sort_name (module$sorts mod)
		 :test #'(lambda (n s)
			   (and (equal n (sort$name s))
				(eq themod (sort$module s)))))))
  (if asort asort
    (let ((newsort (sort$create sort_name themod)))
    (when (sort$info sort)
      (sort$!replace_info newsort (sort$info sort))
      (let ((*mod_eval$$current_module* mod))
      (sort$!replace_constructor newsort
	(mod_eval$create_built_in_constructor newsort))
      ))
    newsort)
  ))))))

; op mapping$$recreate_sort_order : Module SortMap Sort-Order -> Sort-Order
;&&&& want to try and avoid mapping back to names
(defun mapping$$recreate_sort_order (module oldmod modmap sortmap sort_order)
  (let ((res nil))
  (dolist (x sort_order)
    (let ((srt (mapping$sort_image_create
		module oldmod modmap sortmap (car x)))
	  (lss (mapping$$set_reduce
		(mapping$sorts_image_create
		 module oldmod modmap sortmap (cadr x))))
	  (gtr (mapping$$set_reduce
		(mapping$sorts_image_create
		 module oldmod modmap sortmap (cddr x)))))
      (let ((rel (assoc srt res)))
	(if rel
	    (let ((cdr_rel (cdr rel)))
	      (rplaca cdr_rel (union lss (car cdr_rel)))
	      (rplacd cdr_rel (union gtr (cdr cdr_rel))))
	  (push (cons srt (cons lss gtr)) res)))))
  res))

(defun mapping$$set_reduce (x)
  (let ((res nil))
    (dolist (e x) (when (not (member e res)) (push e res)))
    (nreverse res)
  ))

(defun mapping$sort_image_create (module oldmod modmap sortmap sort)
  (let ((s1 (assoc_image sortmap sort)))
  (if (not (eq s1 sort)) s1
  (if (and (equal "DUMMY" (module$name (sort$module sort)))
	   (getf (module$status (sort$module sort)) 'dummy))
      (let* ((mod (sort$module sort))
	     (info (getf (module$status mod) 'rename_mod))
	     (oldmod (car info))
	     (modim (cdr (assoc oldmod modmap))))
	(if modim
	    (let ((val
		   (let ((asrt
			  (mapping$$find_sort_in modim (sort$name sort))))
		     ;@@s up
		     (if asrt asrt
		       (if (or (eq sort *obj$sort_Universal*)
			       (eq sort *obj_BOOL$sort_Bool*)
			       (eq sort obj_ERR$sort_Err))
			   sort
			 nil)))))
 (when (and $$debug (< 50 $$debug_level))
 (princ "image: ") (print$name val) (terpri))
	      (if val val
		(progn
		  (setf (sort$module sort) modim)
		  (module$!adjoin_sort modim sort)
		  sort)))
	  sort)
	)
  (if (not (eq oldmod (sort$module sort)))
      sort
    (let ((newsort (mapping$$recreate_sort module modmap sortmap sort)))
      (module$!adjoin_sort module newsort)
      newsort))))))

(defun mapping$sorts_image_create (module oldmod modmap sortmap sortlist)
  (mapcar #'(lambda (x) (mapping$sort_image_create
			 module oldmod modmap sortmap x))
    sortlist))

;sort should alread exist
(defun mapping$sort_image (module sortmap sort)
  (let ((s1 (assoc_image sortmap sort)))
  (if (member s1 (module$sorts module)) s1
    (let ((val (mod_eval$$find_sort_in module (sort$name s1)))) ;@@s
      (if val val
	(if (or (eq sort *obj$sort_Universal*)
		(eq sort *obj_BOOL$sort_Bool*)
		(eq sort obj_ERR$sort_Err))
	    sort
	  (progn
	    (princ "mapping$sort_image: sort not found: ")
	         (princ (sort$name s1)) (terpri)
	    sort)
	))))))

(defun mapping$sorts_image (module sortmap sortlist)
  (mapcar #'(lambda (x) (mapping$sort_image module sortmap x))
    sortlist))

;;; *** The following are derived from similar things in mod_eval.lsp

(defvar *mapping$local_vars* nil)

; op mapping$recreate_rule :
;        Module Sort-Mapping Operator-Mapping Mod-Mapping Rule -> Rule
(defun mapping$recreate_rule (module sortmap opmap modmap rul)
  (let ((obj$current_module module))
  (if (rule$is_built_in rul)
      (rule$!update_labels
      (rule$make_bi_rule
       (mapping$$recreate_term module sortmap opmap modmap
			       (rule$lhs rul))
       (mapping$recreate_bi_term module sortmap modmap (rule$rhs rul))
       (if (eq *obj_BOOL$true* (rule$condition rul))
	   *obj_BOOL$true*
	 (mapping$$recreate_term module sortmap opmap modmap
				 (rule$condition rul))))
       (rule$labels rul))
      (rule$!update_labels
      (rule$!update_kind
      (rule$make_standard_rule
       (mapping$$recreate_term module sortmap opmap modmap
			       (rule$lhs rul))
       (mapping$$recreate_term module sortmap opmap modmap
			       (rule$rhs rul))
       (if (eq *obj_BOOL$true* (rule$condition rul))
	   *obj_BOOL$true*
	  (mapping$$recreate_term module sortmap opmap modmap
				  (rule$condition rul)))
        )
      (rule$kind rul))
      (rule$labels rul))
  )))

(defun mapping$recreate_bi_term (module sortmap modmap tm)
  (if (consp tm)
      (cons (mapping$recreate_bi_term module sortmap modmap (car tm))
	    (mapping$recreate_bi_term module sortmap modmap (cdr tm)))
      (if (typep tm 'sort)
          (mapping$sort_image module sortmap tm)
          (if (typep tm 'module)
              (let ((val (assoc tm modmap)))
                (if val (cdr val)
                    tm))
              ;; @todo kiniry 25 Sept 2003 - Should CLISP use the latter?
              #-CMU tm
              #+CMU (if (and (typep tm 'function)
                             (function-lambda-expression tm))
                        (make_function
                         (mapping$recreate_bi_term module sortmap modmap
                                                   (function-lambda-expression tm)))
                        tm)))))

; op mapping$$recreate_term :
;        Module Sort-Mapping Op-Mapping Mod-Mapping Term -> Term
(defun mapping$$recreate_term (module sortmap opmap modmap term)
 (cond
  ((term$is_an_error term) term)
  ((term$is_built_in_constant term)
   (let ((ts (operator$coarity (term$head term))))
   (term$make_built_in_constant
    (mapping$sort_image module sortmap ts)
    (if (eq obj_BUILT-IN$sort_Built-in ts)
	(mapping$recreate_bi_term module sortmap modmap
            (term$built_in_value term))
      (term$built_in_value term)))))
  ((term$is_var term)
   ;; &&&& the operator should always be found
   (let ((var_name (variable$name term)))
     (let ((img-sort (mapping$sort_image module sortmap
		       (variable$initial_sort term))))
       (let ((val2 (find-if #'(lambda (x)
				(and
				 (equal var_name (variable$name x))
				 (eq img-sort (variable$initial_sort x))))
		     *mapping$local_vars*)))
	 (if val2 val2
	   (let ((new_var (variable$make_var var_name img-sort)))
	     (push new_var *mapping$local_vars*)
	     new_var))))
   ))
  ;; &&&& what about retracts?
  (t (let ((head (term$head term)))
     ;; look in the mapping
     (let ((val (assoc head opmap)))
       (when val
	 (setq term (mapping$apply_op_mapping module (cdr val) term))
	 (setq head (term$head term))
       )
       (when (operator$is_a_retract head)
	 (let ((s1 (mapping$sort_image module sortmap
		     (car (operator$arity head))))
	       (s2 (mapping$sort_image module sortmap
		     (operator$coarity head))))
	 (unless (and (eq s1 (car (operator$arity head)))
		      (eq s2 (operator$coarity head)))
	   (setq head (operator$make_retract s1 s2))
	 )
       ))
     (if (term$is_built_in_constant term)
	 term
     (let ((new_head 
	    (if val
		head
	    (let ((aval (assoc (operator$module head) modmap)))
	    (if (not aval)
		head
	      (let ((lookmod
		     (if (typep (cdr aval) 'module) (cdr aval)
		     (if (and (consp (cdr aval)) (eq 'view_from (cadr aval)))
			 (modexp$view_from_tgt (cdr aval))
		       (cdr aval)))))
	      (mapping$$find_operator_in ;@@? 
		      lookmod ;module
		      (operator$name head)
		      (mapping$sorts_image lookmod sortmap
		          (operator$arity head))
		      (mapping$sort_image lookmod sortmap
		          (operator$coarity head))
		      )
	      ))))))
       (when (null new_head) (setq new_head head))
       (let ((obj$current_module module))
       (term$make_term_check_op_with_sort_check new_head
	 (mapcar #'(lambda (tm) (mapping$$recreate_term
				 module sortmap opmap modmap tm))
		 (term$subterms term)))
       )
     ))))
   )
  ))

; op mapping$$find_operator_in : Module Name Arity Coarity -> Operator
; derived from  mod_eval$$find_operator_in (module op_name arity coarity)
(defun mapping$$find_operator_in (module op_name arity coarity) ;@@?
  (let ((len (length arity)))
  (find-if
   #'(lambda (op)
       (and (equal op_name (operator$name op))
	    (eq coarity (operator$coarity op))
	    (= len (length (operator$arity op)))
	    (every #'eq arity (operator$arity op))
	    ;(util$outer_equal modnm (module$name (operator$module op)))
	    (eq module (operator$module op))
	    ))
   (module$operators module))))
;    (if res1 res1
;      (if (and (null arity) (sort$is_built_in coarity))
;	  (mod_eval$$find_built_in_operator_in module coarity op_name)
;	nil))

;;; How to represent op_mapping -- given that want to be able to
;;; have a canonical representation; idea -- change the vars that
;;; are give to 0 1 2 3 etc. based on their argument position
;;; in the original operator
;;; Note: the op_mapping is a single mapping element not an alist
; op mapping$apply_op_mapping : Op-Mapping Term -> Term
(defun mapping$apply_op_mapping (module op_mapping term)
  (if (eq 'rename (car op_mapping))
      (term$make_term_check_op_with_sort_check
       (cdr op_mapping) (term$subterms term))
    (let ((obj$current_module module))
    (mapping$image (term$subterms term) (caddr op_mapping))))
  )

(defun mapping$apply_op_map (module op_map term)
  (let ((val (assoc (term$head term) op_map)))
  (if val
      (mapping$apply_op_mapping module (cdr val) term)
    term)))

;;; derived from substitution$image
;;; variables occuring in term are assumed to have numbers as names
;;; that are indices into term_list; var "1" get replaced by (nth 1 term_list)
;;; indices are 0-origin
; op mapping$image : List[Term] Term -> Term
(defun mapping$image (term_list term)
  (cond
   ((term$is_var term)
    (let ((nm (variable$name term)))
     (if (integerp nm) (nth nm term_list)
       (progn
	 (princ "mapping$image: illegal variable") (terpri)
	 (princ "var: ") (print$brief term) (terpri)
	 (break "mapping$image: illegal variable") 
	 ))))
   ((term$is_constant term) term)
   (t (term$make_term_check_op_with_sort_check
       (term$head term)
        (mapcar #'(lambda (st) (mapping$image term_list st))
		(term$subterms term)))
    )))

; op mapping$merge : Mapping Mapping -> Mapping
(defun mapping$merge (m1 m2)
  (let ((nm1 (mapping$name m1))
	(nm2 (mapping$name m2)))
    (mapping$make
     ;name will need to be used for memoization
     ; the assumption here is that basic mappings have names like:
     ;     (map th vw)
     ; and that other names are create by this routine
     (append (if (atom (car nm1)) (list nm1) nm1)
	     (if (atom (car nm2)) (list nm2) nm2))
     (mapping$merge_assoc (mapping$sort m1) (mapping$sort m2))
     (mapping$merge_op_assoc (mapping$op m1) (mapping$op m2))
     (mapping$merge_assoc (mapping$module m1) (mapping$module m2)))
  ))

(defun mapping$merge_assoc (a1 a2)
  (let ((res a2))
    (dolist (m a1)
      (let ((im (assoc (car m) a2)))
      (if im
	(unless (eq (cdr im) (cdr m))
	  (princ "Warning: in instantiation of module, combined view has")
	      (terpri)
	  (princ "inconsistent mappings for: ") (print$name (car m)) (terpri)
	  (princ "images are: ") (print$name (cdr m)) (terpri)
	  (princ "and: ") (print$name (cdr im)) (terpri)
	  )
	(push m res))
      ))
    res
  ))

(defun mapping$merge_op_assoc (a1 a2)
  (let ((res a2))
    (dolist (m a1)
      (let ((im (assoc (car m) a2)))
      (if im
	(unless (mapping$same_op_image (cdr im) (cdr m))
	  (princ "Warning: in instantiation of module, combined view has")
	      (terpri)
	  (princ "inconsistent mappings for operator: ")
	      (print$name (car m)) (terpri)
	  (princ "images are: ") (print$name (cdr m)) (terpri)
	  (princ "and: ") (print$name (cdr im)) (terpri)
	  )
	(push m res))
      ))
    res
  ))

;cases on cdrs: (rename . op)
(defun mapping$same_op_image (im1 im2)
  (if (and (consp im1) (eq 'rename (car im1)))
    (and (consp im2) (eq 'rename (car im2))
	 (eq (cdr im1) (cdr im2)))
  (if (and (consp im1) (eq 'replace (car im1)))
    (and (consp im2) (eq 'replace (car im2))
	 (term$equational_equal (caddr im1) (caddr im2)))
    (break "SNARK: illegal operator mapping")))
  )

(defun mapping$$module_is_mapped (modmap mod)
  (assoc mod modmap) ;&&&& this needs to be improved
  )

(defun mapping$$submodule_is_mapped (modmap mod)
  (some #'(lambda (x)
	    (or (mapping$$module_is_mapped modmap (car x))
		(mapping$$submodule_is_mapped modmap (car x))))
      (module$sub_modules mod))
  )

; op mapping$$recreate_operator : Module Sort-Mapping Operator -> Operator
(defun mapping$$recreate_operator (oldmodule module sortmap operator)
  (let ((op_name (operator$name operator))
	(op_arity (mapping$sorts_image module sortmap
		      (operator$arity operator)))
	(op_coarity (mapping$sort_image module sortmap
		        (operator$coarity operator))))
  (let ((val (mapping$$find_operator_in module op_name op_arity op_coarity)))
    ;@@ up
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
		       nil ; rules wdt
		       (operator$precedence operator)
		       (mapping$$recreate_op_form
			module sortmap (operator$form operator))
		       (operator$syntactic_type operator)
		       (operator$is_standard operator)
		       (operator$polymorphic operator)
		       (operator$is_marked_strictly_overloaded operator)
		       )
        (operator$intrinsic operator))))))
            ; intrinsic info is independent of context
  )

; op mapping$$recreate_op_form : Module Operator-Form -> Operator-Form
(defun mapping$$recreate_op_form (module sortmap frm)
  (mapcar #'(lambda (i)
	      (if (eq 'argument (car i))
		  (list* 'argument (cadr i)
			 (mapping$sort_image module sortmap (cddr i)))
		i))
    frm)
  )

(defun mapping$$update_theory (mod map op)
  (let ((thy (module$operator_equational_theory mod op)))
  (module$!operator_update_equational_theory mod op
	(theory$create (theory$name thy)
	    (let ((idn (theory$zero thy)))
	      (when idn
              (let ((srtmap (mapping$sort map))
		    (opmap (mapping$op map))
		    (modmap (mapping$module map))
		    (idinf (if (and idn (eq 'to_rename (car idn)))
			       (cadr idn) idn)))
		(cons (mapping$$recreate_term mod srtmap opmap modmap
		        (car idinf))
		      (cdr idinf))
		))))))
  )

(defun mapping$!check_rank (newmod oldmod map op)
  (let ((modmap (mapping$module map))
	(sortmap (mapping$sort map))
	(ar (operator$arity op))
	(coar (operator$coarity op)))
    (setf (operator$arity op) (mapping$sorts_image_create newmod oldmod
			        modmap sortmap ar))
    (setf (operator$coarity op) (mapping$sort_image_create newmod oldmod
			        modmap sortmap coar))
    (setf (operator-form op)
	(mapping$$recreate_op_form newmod sortmap (operator$form op)))
  ))

; smod is a sub-module of mod that itself has a mapped sub-module
; (i.e., a parameter); thus a sub-instantiation
; -- must do appropriate memoization
;   to memoize must construct a module expression from the name of
;   smod and the name of the relevant parameter (which in general
;   may be a view)
; constructing the appropriate module expression is rather delicate
;  the name of module will be the canonicalized module expression
;  not all cases arise; are somewhat restricted.  In this context,
;  don't allow a rename that affects the parameter. cases:
;  + (act on sub-components); instantiation: check for occurrence
;  of module mappped by the mapping, if occurs then rebuild;
;  rename (leaf / internal non-bijective) assume mapping not affected
;  by the renaming
;  for instantiation: occurrence of some parameter must occur
;  in some view argument in the instantiation; it will be a canonicalized
;  view so the forms of the views will be very regular;
;  -- what is being "inserted" is also a view; therefore must
;    compose the views
;    (apply view to view = composition)
(defun mapping$map_submodule (map mod smod)
  (let ((nm (mapping$construct_name map (module$name smod))))
  ; The following is a kind of evaluate without much effort and
  ; could perhaps be separated out
  (let* ((me (modexp_eval$canonicalize nm))
	 (val (modexp_eval$find_in_env me)))
    (if val
	(progn
	  (let ((pair (assoc smod (mapping$module map)))
		(nmod (cdr val)))
	    (if pair
	      (progn
		;(push (cons (cdr pair) (cdr val)) (mapping$module map))
		(rplacd pair (cdr val)))
	      (push (cons smod (cdr val)) (mapping$module map)))
	    (setf (mapping$module map)
	      (append (mapping$compute_submodule_mappings map mod smod)
		      (mapping$module map)))
	    nmod))
      (let ((newmod (mapping$apply_internal nm map smod)))
	  (progn
	    (modexp_eval$!add_canon me me) ;@@@@@@
	    (modexp_eval$!add_defn me newmod) ;@@ this is critical?
	    (setf (mapping$module map)
		  (cons (cons smod newmod)
			  (mapping$module map)))
	    newmod)))))
  )

(defvar *mapping$expanded* nil)

(defun mapping$construct_name (map smod)
  (cond
   ((mapping$is_rename map)
    (modexp_eval$rename_canonicalize
     (modexp$make_rename
      (if (typep smod 'module) (module$name smod) smod)
      (caddr (mapping$name map)))))
   (t 
    (let ((*mapping$expanded* nil))
    (let ((val (mapping$reconstruct_name map
	          (if (typep smod 'module) (module$name smod) smod))))
      (if (modexp$is_view val)
	  (let ((val2 (modexp_eval$target_of_view_arg val)))
	    (if (typep val2 'module) (module$name val2) val2)
	    )
	val))))
   ))

; want result in canonical form
(defun mapping$reconstruct_name (map me)
  (cond
   ((atom me)
    (let ((modval (modexp_eval$eval me))
	  (modmap (mapping$module map)))
    (let ((im (assoc modval modmap)))
      (if im 
	(if (member modval *mapping$expanded*)
	    modval
	  (progn
	    (push modval *mapping$expanded*)
	    (mapping$reconstruct_name map (cdr im))))
	(if (typep me 'module)
	    (mapping$reconstruct_name map (module$name me))
	  me)))))
   ((eq 'name (car me))
    me
;@@@@@
;    `(name ,(mapping$reconstruct_name map (cadr me)) ,(caddr me))
    )
   ((eq '+ (car me))
    (modexp_eval$plus_canonicalize
     (modexp$make_plus
      (mapping$reconstruct_name map (cadr me))
      (mapping$reconstruct_name map (caddr me)))))
   ((eq '* (car me))
    (modexp_eval$canonicalize
      (modexp$make_rename
       (mapping$reconstruct_name map
	 (if (typep (cadr me) 'module) (module$name (cadr me)) (cadr me)))
       (if (typep (caddr me) 'module) (module$name (caddr me)) (caddr me)))))
   ((eq '*view (car me))
    (break "mapping$reconstruct_name: missing case for *view"))
   ((eq '|:| (car me))
    (let* ((modpar (cadr me))
	   (modparnm (if (typep modpar 'module) (module$name modpar) modpar)))
    (modexp$make_instantiation
     (mapping$reconstruct_name map modparnm)
     (do ((i 0 (1+ i))
	  (r (caddr me) (cdr r))
	  (res nil))
	 ((null r) (nreverse res))
	 (push (mapping$reconstruct_view_arg
		i (car r) map modpar)
	       res)))))
   ((eq 'view_from (car me))
    (modexp$make_view_from ;&&&&
      (cadr me)
      (mapping$reconstruct_name map (caddr me))
      (cadddr me)))
   (t (break "mapping$reconstruct_name: missing case"))
  ))

(defun mapping$reconstruct_view_arg (i vw map modpar)
  (declare (ignore i modpar))
  (let* ((tgt (modexp_eval$target_of_view_arg vw))
	 (modmap (mapping$module map))
	 (val (assoc tgt modmap)))
    (if (null val)
	(modexp_eval$$simplify_view
	(modexp$make_view_from
	 (cadr vw)
	 (mapping$reconstruct_name map (caddr vw))
	 (mapping$reconstruct_view_mapping
	  (progn ;&&&& need to think about this
	    (when $$debug
	      (princ "### Warning: using BOOL in reconstruct_view_arg")
	      (terpri))
	    *obj_BOOL$module*
	    )
	  map (cadddr vw))
	 ))
      (let ((actual (cdr val)))
	(let ((tmod (if (typep actual 'module)
			actual
		      (modexp_eval$target_of_view_arg actual))))
	  (modexp$make_view_from
	   (cadr vw)

	   tmod
	   (mapping$reconstruct_view_mapping tmod map (cadddr vw))
	   )))
      )
    )
  )

(defun mapping$reconstruct_view_mapping (mod map vwmap)
  (mapcar #'(lambda (i) (mapping$reconstruct_view_item mod map i))
    vwmap)
  )

(defun mapping$reconstruct_view_item (mod map vwitem)
  (cond
   ((eq 'sort (car vwitem))
    `(sort ,(cadr vwitem) ,(assoc_image (mapping$sort map) (caddr vwitem))))
   ((eq 'op (car vwitem))
    `(op ,(cadr vwitem)
	 ,(mapping$apply_op_map_2 mod map (mapping$op map) (caddr vwitem))))
  (t (break "mapping$reconstruct_view_item"))
  ))

(defun mapping$image_2 (map term_list term)
  (cond
   ((term$is_var term)
    (let ((nm (variable$name term)))
     (if (integerp nm) (nth nm term_list)
       (progn
	 (princ "mapping$image: illegal variable") (terpri)
	 (princ "var: ") (print$brief term) (terpri)
	 (break "mapping$image: illegal variable") 
	 ))))
   ((term$is_constant term) term)
   (t (term$make_term_check_op_with_sort_check
       (let* ((op (term$head term))
	      (om (operator$module op))
	      (as (assoc om (mapping$module map))))
	 (if (and as (cdr as) (not (eq (cdr as) om))
		  (typep (cdr as) 'module))
	     ;(mod_eval$$find_operator_named_in ;@@ use new fcn
	     ; (cdr as) ;@@
	     ; (operator$name op))
	      (mapping$$find_operator_in ;@@
		      (cdr as)
		      (operator$name op)
		      (mapping$sorts_image (cdr as) (mapping$sort map)
		          (operator$arity op))
		      (mapping$sort_image (cdr as) (mapping$sort map)
		          (operator$coarity op))
		      )
	     op)
       )
       (mapcar #'(lambda (st) (mapping$image_2 map term_list st))
	       (term$subterms term)))
    )))

(defun mapping$apply_op_mapping_2 (module map op_mapping term)
  (if (eq 'rename (car op_mapping))
      (term$make_term_check_op_with_sort_check
       (cdr op_mapping) (term$subterms term))
    (let ((obj$current_module module))
    (mapping$image_2 map (term$subterms term) (caddr op_mapping))))
  )

(defun mapping$apply_op_map_2 (module map op_map term)
  (let ((val (assoc (term$head term) op_map)))
  (if val
      (mapping$apply_op_mapping_2 module map (cdr val) term)
    (let* ((op (term$head term)) ;@@
	   (om (operator$module op))
	   (as (assoc om (mapping$module map))))
      (if (and as (cdr as) (not (eq (cdr as) om)))
	  (let ((obj$current_module module))
	  (mapping$image_2 map (term$subterms term) term))
	term)))))

(defun mapping$compute_submodule_mappings (map mod smod)
  (let ((res nil) (modmap (mapping$module map)))
  (dolist (smp (module$sub_modules smod))
    (let ((sm (car smp)))
    (if (and (not (eq 'using (cdr smp)))
	       (mapping$$submodule_is_mapped modmap sm))
      (setq res
	    (cons (cons sm
		   (modexp_eval$eval
		    (mapping$construct_name map (module$name sm))))
		  (append
		   (mapping$compute_submodule_mappings map mod sm)
		   res)))
    (let ((aval (assoc sm modmap)))
      (when (and aval
		 (modexp_eval$is_dummy_mod (cdr aval)))
	(let ((newm (modexp_eval$eval
		     (mapping$construct_name map (module$name sm)))))
          (push (cons (cdr aval) newm) (mapping$module map)) ;><
	  (rplacd aval newm)
	)
      )
    ))
  ))
  res
  ))

(defun modexp_eval$mod_occurs_in (mod1 mod2)
  (or (eq mod1 mod2)
      (if (consp mod2) (or (modexp_eval$mod_occurs_in mod1 (car mod2))
			   (modexp_eval$mod_occurs_in mod1 (cdr mod2)))
	(if (typep mod2 'module)
	    (modexp_eval$mod_occurs_in mod1 (module$name mod2))
	  nil)))
  )

;;;;&&&& not sure where this should go
(defun term$is_an_error (x)
   (and (consp x)
	(consp (car x))
	(or
	 (null (caar x))
	 (eq (caar x) 'error)
	 (not (typep (caar x) 'operator))
	 (eq obj_ERR$sort_Err (term$sort x)))))

(defun mapping$!recreate_poly_rules (mod newmod sortmap opmap modmap polys)
  (let ((obj$current_module newmod))
  (dolist (i polys)
    (let ((opm (cons (list* (car i) 'rename (cdr i)) opmap)))
    (dolist (r
	     (mapcar #'(lambda (r)
			 (mapping$recreate_rule newmod sortmap opm modmap r))
		     (module$all_rules mod (car i))))
        (module$!add_rule_to_op newmod (cdr i) r)
      )))
  ))

; not used
(defun mod_eval$$find_mapped_operator_in (module op_name arity coarity modmap)
  (declare (ignore modmap)) ;@@
  (let ((len (length arity)))
  (let ((res1
	 (find-if
	  #'(lambda (op)
	      (and (equal op_name (operator$name op))
		   (eq coarity (operator$coarity op))
		   (= len (length (operator$arity op)))
		   (every #'eq arity (operator$arity op))
		   ))
	  (module$operators module))))
    (if res1 res1
      (if (and (null arity) (sort$is_built_in coarity))
	  (mod_eval$$find_built_in_operator_in module coarity op_name)
	nil)))))

; op mapping$$find_sort_in : Module Sort-Name -> Sort
(defun mapping$$find_sort_in (module sort_name)
; derived from op mod_eval$$find_sort_in : Module Sort-Name -> Sort
  (let ((res (find sort_name (module$sorts module)
	       :test #'(lambda (n s)
			 (and (equal n (sort$name s))
			      (eq module (sort$module s)))))))
  (if res res nil)
  ))

(defun mapping$$find_operator_named_in (module op_name)
  (let ((res1
	 (find-if
	  #'(lambda (op)
	      (or (equal op_name (operator$name op))
		  (and (operator$is_standard op)
		       (if (atom op_name)
			   (equal op_name (car (operator$name op)))
			 (and (null (cdr op_name))
			   (equal (car op_name) (car (operator$name op)))))
		       (eq module (operator$module op))
		       )))
	  (module$operators module))))
    (if res1 res1
      (dolist (srt (module$sorts module) nil)
	(if (sort$is_built_in srt)
	  (let ((res (mod_eval$$find_built_in_operator_in module srt op_name)))
	    (if res (return res)))))
      )
    ))
