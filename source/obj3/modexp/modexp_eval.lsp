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

;; $Id: modexp_eval.lsp,v 205.2.1.1 2003/09/23 14:06:40 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                    modexp_eval (part 1)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Tim Winkler ;;;; Created: 10/16/86 from 7/8/86 version

;;;; module-expression evaluation

;;;;;;;;;;;;;;;; Summary ;;;;;;;;;;;;;;;;
; op modexp_eval$eval : Module-Expression -> Module
; op modexp_eval$!add_defn : Module-Expression Module ->

; var *modexp_eval$canon_env* ; assoc from ME to  canon ME
; var *modexp_eval$env* ; assoc from canon ME to Modules

; the original notion was canonicalization followed by evaluation,
; but canonicalization necessarily requires evaluation; the
; two processes go hand-in-hand

(defvar obj$current_module)
(defvar *obj$allow_uninstantiated* nil)

(defun modexp_eval$top_level_eval (modexp &optional (module nil))
  (let ((meparse (modexp_parse$parse modexp)))
    (if (equal "THE-LAST-MODULE" meparse)
      (if *mod_eval$$last_module* *mod_eval$$last_module*
	(progn
	  (princ "Error: no last module") (terpri)
	  (obj3-to-top)))
    (if (and (consp meparse) (eq 'with (car meparse)))
	(progn
	  (princ "Error: module expression ...with...and... not allowed")
	  (terpri)
	  (obj3-to-top))
      (modexp_eval$eval meparse module))
  )))

(defun modexp_eval$top_level_eval_whole (modexp &optional (module nil))
  (let ((meparse (modexp_parse$parse modexp)))
  (let ((val
    (if (equal "THE-LAST-MODULE" meparse)
      (if *mod_eval$$last_module* *mod_eval$$last_module*
	(progn
	  (princ "Error: no last module") (terpri)
	  (obj3-to-top)))
    (if (and (consp meparse) (eq 'with (car meparse)))
      (progn
	(princ "Error: module expression ...with...and... not allowed")
	(terpri)
	(obj3-to-top))
      (modexp_eval$eval meparse module))
    )))
    (when (and (null *obj$allow_uninstantiated*)
		(not (modexp_eval$is_error val))
	       (typep val 'module)
	       (module$parameters val))
      (princ "Module used without instantiation: ")
      (print$name val) (terpri)
      (obj3-to-top))
    val
  )))

; evaluate modexp with check that is instantiated when done
(defun modexp_eval$eval_whole (modexp &optional (module nil))
  (let ((val (modexp_eval$eval modexp module)))
    (when (and (not (modexp_eval$is_error val))
	       (typep val 'module)
	       (module$parameters val))
      (princ "Module used without instantiation: ")
      (print$name val) (terpri)
      (obj3-to-top))
    val
    )
  )

; evaluate a non-canonicalized module expression
(defun modexp_eval$eval (modexp &optional (module nil))
  (declare (ignore module))
  ;; Process:
  ;; canonicalization of module expression
  ;; (1) push down renaming
  ;; (2) evaluate leaves
  ;; (3) simplify renaming (drop irrelevant parts)
  ;; (4) compute default views (put in canonical form)
  ;; (5) create canonicalized module expression
  ;; note: the optional argument is not currently used
  ;;   and can only be relevant to view processing
  ;; for instantiation must evaluate args before canonicalizing whole in gen.
  ;; (6) look for module in environment; if not found build and enter
  ;; *modexp_eval$canon_env* is a "memoization" table that short-cuts
  ;; the above process 
  ;; when build module, are given module expression in canonical form
  ;; -- i.e. renames pushed down and views computed in canonical form
  ;; terms in view are parsed in normalized view -- which requires
  ;; that appropriate modules be evaluated before normalizing the
  ;; view
  ; sketch: normalize expression ; lookup in table, if there use it;
  ;         otherwise build and enter
  (if (typep modexp 'module) modexp
  (let ((me (modexp_eval$canonicalize
	     ;(if (atom modexp) (list (string modexp)) modexp)
	     (if (and (consp modexp)
		      (null (cdr modexp))
		      (stringp (car modexp)))
		 (car modexp)
	       modexp)
	     )))
  (let ((val (modexp_eval$find_in_env me)))
    (if val (cdr val)
      (let ((newmod (modexp_eval$create me)))
	(if (modexp_eval$is_error newmod) newmod
	  (progn
	    (modexp_eval$!add_defn me newmod)
	    newmod))))))
  ))

(defun modexp_eval$is_error (val)
  (and (consp val) (eq 'error (car val))))

(defvar *modexp_eval$canon_env* nil)
  ; alist mapping: partially canonicalized module expr's -> canon'd me's
(defvar *modexp_eval$env* nil)
  ; alist mapping: canonicalized module expressions -> modules or views
(defvar *modexp_eval$local_env* nil)
  ; "local" extension to *modexp_eval$env* used for parameters of p'd mods
  ; alist mapping: canonicalized module expressions -> modules or views
(defvar *modexp_eval$view_env* nil)
  ; alias mapping: canonicalized view expressions -> views

; could, perhaps, redo the following assoc-lists in terms of hashtab
(defun modexp_eval$!add_defn (modexp value)
  (let ((def (modexp_eval$assoc_env modexp *modexp_eval$env*)))
  (unless (and def (eq (cdr def) value))
    (push (cons modexp value) *modexp_eval$env*))))

(defun modexp_eval$!add_local_defn (modexp value)
  (push (cons modexp value) *modexp_eval$local_env*))

; used in module_eval -- case of $add_pr that creates sub-module
(defun modexp_eval$!update_name (modexp modval)
  (let ((pr (rassoc modval *modexp_eval$env*)))
  (if pr (rplaca pr modexp)
    (break "modexp_eval$!update_name: no such module")
  )))

(defun modexp_eval$equal_env (x y)
  (if (stringp x) (equal x y)
    (eq x y)))

(defun modexp_eval$assoc_env (x env)
  (dolist (p env nil) ;default to nil
    (when (modexp_eval$equal_env x (car p)) (return p)))
  )

(defun modexp_eval$find_in_env (me)
  (or
   (modexp_eval$assoc_env me *modexp_eval$local_env*)
   (modexp_eval$assoc_env me *modexp_eval$env*)
   ))
   
;;; The intent is that the following be used with uncanonicalized module
;;; name's.; case in particular -- creation of parameters for module
(defun modexp_eval$find_equiv_in_env (me)
  (assoc me *modexp_eval$env* :test #'modexp_eval$equal_top_level))

(defun modexp_eval$equal_top_level (x y)
  (cond
   ((stringp x) (equal x y))
   ((atom x) (eql x y))
   ((atom y) nil)
   (t (and (modexp_eval$equal_top_level (car x) (car y))
		     (modexp_eval$equal_top_level (cdr x) (cdr y))))))

(defun modexp_eval$!add_canon (modexp value)
  (let ((def (modexp_eval$find_in_canon_env modexp)))
  (unless (and def (eq (cdr def) value))
    (push (cons modexp value) *modexp_eval$canon_env*))))

(defun modexp_eval$find_in_canon_env (me)
  (dolist (p *modexp_eval$canon_env* nil) ;default to nil
    (when (modexp_eval$same_top_level me (car p)) (return p)))
  )

; op modexp_eval$same_top_level : Module-Expression Module-Expression -> Bool
; are two module expressions the same assuming that they have
;   been canonicalized below the top level?
; cannot expect all parts of module expressions that have been put in
; normal form to be eq
(defun modexp_eval$same_top_level (me1 me2)
  (cond
   ((eq me1 me2) t)
   ((typep me1 'module) (modexp_eval$same_top_level (module$name me1) me2))
   ((typep me2 'module) (modexp_eval$same_top_level me1 (module$name me2)))
   ((atom me1)
    (if (stringp me1) (equal me1 me2)
      (eq me1 me2)))
   ((atom me2) (eq me1 me2))
   ((eq '* (car me1)) ;rename
    (and (eqmod (cadr me1) (cadr me2))
	 (modexp_eval$same_rename (caddr me1) (caddr me2))))
   ((eq '*view (car me1)) ;renamed view 12 Jan 88
    (and (eqmod (cadr me1) (cadr me2))
	 (modexp_eval$same_rename (caddr me1) (caddr me2))))
   ((or (eq 'view_from (car me1)) (eq 'view_mapping (car me1)))
    (and (eqmod (cadr me1) (cadr me2))
	 (eqmod (caddr me1) (caddr me2))
	 (modexp_eval$same_view_mapping (cadddr me1) (cadddr me2))))
   ((eq '|:| (car me1))
    (and (eq '|:| (car me2))
	 (eqmod (cadr me1) (cadr me2))
	 (= (length (caddr me1)) (length (caddr me2)))
	 (every #'eq (caddr me1) (caddr me2))))
   ((eq '+ (car me1))
    (and (eq '+ (car me2))
	 (eqmod (cadr me1) (cadr me2))
	 (eqmod (caddr me1) (caddr me2))))
   ((or (atom me2) (not (listp me1)) (not (listp me2))
	(not (= (length me1) (length me2))))
    nil)
   ((or (eq 'view (car me1)) (eq 'view (car me2)))
    ;(equal me1 me2) ; not very good
    (and (eqmod (cadr me1) (cadr me2))
	 (eqmod (caddr me1) (caddr me2))
	 (modexp_eval$same_view_mapping (cadddr me1) (cadddr me2))))
   ((or (member (car me1) '(prim_view under))
	(member (car me2) '(prim_view under)))
    (break "modexp_eval$same_top_level: illegal form"))
   (t (and (= (length me1) (length me2)) (every #'eq me1 me2)))
   ))

(defun eqmod (x y)
  (or (eq x y)
      (and (stringp x) (equal x y))
      (and (typep x 'module) (eqmod (module$name x) y))
      (and (typep y 'module) (eqmod x (module$name y)))))

(defun modexp_eval$same_rename (r1 r2)
  (util$outer_equal r1 r2) ;&&&& more
  )

(defun modexp_eval$same_view_mapping (v1 v2)
  (util$outer_equal v1 v2) ;&&&& more
  )

;&&&& where
(defun util$outer_equal (x y)
  (cond
   ((stringp x) (equal x y))
   ((atom x) (eq x y)) ;note this includes structures
   ((consp x)
    (and (consp y)
	 ;(every #'util$outer_equal x y)
	 (do ((xl x (cdr xl))
	      (yl y (cdr yl))
	      (flag t))
	     ((or (when (or (atom xl) (atom yl)) (setq flag (eq xl yl)) t)
		  (when (not (util$outer_equal (car xl) (car yl)))
		    (setq flag nil) t))
	      flag))
	 ))
   (t nil)
  ))

;;; view environment functions
(defun modexp_eval$!add_view_defn (vw value)
  (let ((def (modexp_eval$find_in_view_env vw)))
  (unless (and def (eq (cdr def) value))
    (push (cons vw value) *modexp_eval$view_env*))))

(defun modexp_eval$find_in_view_env (vw)
  (dolist (p *modexp_eval$view_env* nil) ;default to nil
    (when (modexp_eval$same_top_level vw (car p)) (return p)))
  )

; canonicalize a module expression
;   this involves computing views (parsing terms appearing in them)
;   generally requires the evaluation of proper sub-objects
;   doesn't create the object itself
;   should canonicalize in the list sense too
(defun modexp_eval$canonicalize (modexp)
  (if (and (consp modexp) (eq 'name (car modexp)))
      (let ((val (modexp_eval$find_in_canon_env modexp)))
	(if val (cdr val)
	  ;(break "SNARK SNARK #N")
	  modexp ; this must be a module in process of creation
	  ))
  (if (and (consp modexp) (eq 'is (car modexp)))
      (let ((can2 (modexp_eval$canonicalize (caddr modexp))))
	(modexp_eval$!add_canon (cadr modexp) can2)
	can2)
  (if (modexp_eval$is_parameter_theory modexp)
      (cdr (modexp_eval$find_in_canon_env modexp))
  (let ((me1 (modexp_eval$sub_canonicalize modexp)))
  (let ((val (modexp_eval$find_in_canon_env me1)))
  (if val (cdr val)
  (let ((res (modexp_eval$$canon modexp)))
  (let ((me2 (modexp_eval$sub_canonicalize res)))
  (let ((val2 (modexp_eval$find_in_canon_env me2)))
  (let ((res2
          (if val2 (cdr val2)
	    (progn (modexp_eval$!add_canon me2 res) res))))
    (when (not (modexp_eval$same_top_level me1 me2))
      (modexp_eval$!add_canon me1 res2))
    res2
  )))))))))))

; when canonicalizing a view keeps track of the parameterized module
(defvar *modexp_eval$abstr_mod* nil)
; when canonicalizing a view keeps track of its arg position
(defvar *modexp_eval$arg_pos* nil) ; 0-origin

; canonicalize below the top-level
(defun modexp_eval$sub_canonicalize (me)
  (cond
   ((atom me) me)
   ;@@ new (specn x) (op x) (sort x) (qual x y) add cases
   ((member (car me) '(op sort specn qual))
    `(,(car me) ,(cadr me) ,(modexp_eval$canonicalize (caddr me))))
   ((eq '* (car me))
    (cond
     ((and (consp (cadr me)) (eq '* (car (cadr me))))
      (modexp_eval$sub_canonicalize
       (modexp$make_rename (cadr (cadr me))
	 (modexp_eval$compose_renames (caddr me) (caddr (cadr me))))))
     (t
      (modexp$make_rename (modexp_eval$canonicalize (cadr me))
			  (modexp_eval$canonicalize_rename (caddr me))))))
   ((eq '*view (car me))
    (cond
     ((and (consp (cadr me)) (eq '*view (car (cadr me))))
      (modexp_eval$sub_canonicalize
       (modexp$make_view_rename (cadr (cadr me))
	 (modexp_eval$compose_renames (caddr me) (caddr (cadr me))))))
     (t
      (modexp$make_view_rename (modexp_eval$canonicalize (cadr me))
			  (modexp_eval$canonicalize_rename (caddr me))))))
   ((eq '|:| (car me))
    ; hooks for "view to" abbreviation
    (let ((*modexp_eval$abstr_mod* (modexp_eval$canonicalize (cadr me))))
    (modexp$make_instantiation *modexp_eval$abstr_mod*
     (let ((res nil) (*modexp_eval$arg_pos* 0))
       (dolist (a (caddr me))
	 (push (modexp_eval$view_can_defaults a) res)
	 (incf *modexp_eval$arg_pos*)
       )
       (nreverse res)
     ))))
   ((eq 'view (car me))
    (list 'view_from
	  (modexp_eval$canonicalize (cadr me))
	  (modexp_eval$canonicalize (caddr me))
	  (modexp_eval$canonicalize_view_mapping (cadddr me))
	  ))
   ((or (eq 'view_from (car me)) (eq 'view_mapping (car me))) me)
   ((member (car me) '(prim_view under))
    (break "modexp_eval$sub_canonicalize: not yet"))
   (t (cons (car me) (mapcar #'modexp_eval$canonicalize (cdr me))))
   ))

(defun modexp_eval$canonicalize_rename (r)
  (mapcar #'(lambda (x)
        (if (eq 'sort (car x)) x
	(if (eq 'op (car x))
	  (list 'op (modexp_eval$$op_refer (cadr x))
		    (modexp_eval$$op_refer (caddr x)))
	  x)
        )
      )
    r)
  )

(defun modexp_eval$canonicalize_view_mapping (vwm)
  vwm
  )

; perform canonicalization on mod-exp. known not to be in canonical form
; ops + * : view under
(defun modexp_eval$$canon (modexp)
  (cond
   ((or (atom modexp)
	(and (consp modexp) (null (cdr modexp)) (atom (car modexp))))
    (if (atom modexp) modexp (car modexp)))
   ((and (consp modexp) (eq 'name (car modexp)))
    (list 'name (modexp_eval$$canon (cadr modexp)) (caddr modexp)))
   ((eq '+ (car modexp)) ; right associate and re-order
    (modexp_eval$plus_canonicalize modexp))
   ((or (eq '* (car modexp)) (eq '*view (car modexp)))
    (modexp_eval$rename_canonicalize modexp))
   ((eq '|:| (car modexp))
    (let ((modpar (modexp_eval$canonicalize (cadr modexp)))
	  (modparval (modexp_eval$eval (cadr modexp))))
    (when (not (typep modparval 'module))
       (princ "Unknown parameterized module in instantiation: ")
       (if (and (consp modparval) (eq 'error (car modparval)))
	   (princ (cdr modparval))
	 (print$name modparval))
       (terpri)
      (obj3-to-top))
    (let ((args
	   (do ((i 0 (1+ i))
		(r (caddr modexp) (cdr r))
		(res nil))
	       ((null r) (nreverse res))
	     (push
	      (modexp_eval$canonicalize_view_arg i (car r) modparval)
	      res))))
    (modexp$make_instantiation
     modpar args))))
     ;; need to have corresponding theory to be able to interpret view
   ((eq 'view (car modexp))
    ;(modexp_eval$view_defaults (modexp_eval$view_can modexp)) ;## defaults
    (modexp_eval$view_can_defaults modexp)
    )
   ((eq 'under (car modexp)) ;&&&& eliminate
    (break "modexp_eval$$canon: `under` case not handled yet")
    )
   ((eq 'view_from (car modexp)) ; the canonicalized version
    modexp) ; assume that it is in canonical form
   ((eq 'view_mapping (car modexp))
    modexp) ; as above
   ((modexp_eval$is_parameter_theory modexp) modexp)
   (t (break "modexp_eval$$canon: bad modexp form"))
  ))

; canonicalizing a view:
; this is written with the assumption that the view is complete, i.e.
;     no defaults have to be supplied
; component modules have been canonicalized but not evaluated
; scan view for variable declarations; create parsing contexts
; -- find sort<s; parse terms; locate operators
;   (if is mapping given by name of operator
;   case of mapping given by terms, make easy to tell if the same)
; -- eliminate variables from the terms in the operator mappings
; -- split into separate categories and order
; -- join the categories back together in a canonical order (want a structure?)
;    (variables have been eliminated; only sorts and ops left)
;@@@ not used in practice?
(defun modexp_eval$$view_canon (th mod vw_map)
  (when (modexp$is_view mod)
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
    (when (modexp_eval$is_error dst_mod) (princ " ") (print$modexp mod))
    (terpri)
    (obj3-to-top))
  (let ((sort_maps (remove-if-not #'(lambda (x) (eq 'sort (car x))) vw_map))
	(var_decls (remove-if-not #'(lambda (x) (eq 'var (car x))) vw_map))
	(op_maps (remove-if-not #'(lambda (x) (eq 'op (car x))) vw_map)))
  (let* ((sort_mapping ; an alist
	  (mapcar #'(lambda (x)
		        (cons
			 (mod_eval$$find_qual_sort_in src_mod (cadr x)) ;@@
			 (mod_eval$$find_qual_sort_in dst_mod (caddr x)) ;@@
			 ))
	    sort_maps))
	 (sortmap
	  (mapcar #'(lambda (x) `(sort ,(car x) ,(cdr x)))
	    sort_mapping))
	 (src_vars
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
    ; final result:
    ;&&&& >< could overwrite old view
    (modexp$make_view_from src_mod dst_mod
        (append
	 (sort sortmap
	       #'ob<
	       :key #'(lambda (x) (sort$name (cadr x))))
	 (sort op_mapping
	       #'ob<
	       :key #'(lambda (x)
			(let ((val (term$head (cadr x))))
			  (if (symbolp val) val
			    (operator$name val)))))
	 ))
  ))))

;&&&& replace uses of the function above?
(defun modexp_eval$view_can (vw)
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
    (modexp_eval$$view_canon
     (modexp_eval$canonicalize (cadr vw))
     (modexp_eval$canonicalize (caddr vw))
     (cadddr vw)))
   ((eq 'view_from (car vw)) vw)
   ((member (car vw) '(|:| + *))
    (modexp_eval$canonicalize vw))
   ((eq '*view (car vw))
    (modexp$make_view_rename
     (modexp_eval$view_can (cadr vw))
     (caddr vw)))
   (t (break "modexp_eval$view_can: not yet"))))

; There is a tricky point here which is that want to allow both
; the use of operator names and expressions.  Rule is: if can
; be considered an expression do that; otherwise try as operator name
(defun modexp$compute_op_mappings (sortmap src_mod src_vars
				      dst_mod dst_vars op_maps)
  (mapcar #'(lambda (opmap)
	      (modexp$compute_op_mapping
	       sortmap  src_mod src_vars
	               dst_mod dst_vars opmap))
    op_maps))

;This is needed for the following function
(defvar *parser$variables*)

; probably don't need the sortmap as an argument
(defun modexp$compute_op_mapping (sortmap src_mod src_vars
				      dst_mod dst_vars op_map)
  (declare (ignore sortmap))
  (let ((src_sort (car (cadr op_map)))
	(src_opex (cadr (cadr op_map)))
	(dst_sort (car (caddr op_map)))
	(dst_opex (cadr (caddr op_map))))
  (let ((src_done nil) src_is_op src_pars src_op src_varbs try_op
	(dst_done nil) dst_is_op dst_pars (psuedo_vars nil))
    (setq src_is_op (typep src_opex 'operator)) ;@@
    (unless src_is_op
	(setq src_pars (let ((*parser$variables*
			      (mapcar #'(lambda (x)
					  (cons (variable$name x) x))
				      src_vars)))
	  (parse$quiet_parse src_mod src_opex
		       (if (eq 'universe src_sort) *obj$sort_Universal*
			 (if (typep src_sort 'sort) src_sort
			   (mod_eval$$find_qual_sort_in
			    src_mod src_sort))))))
	;@@ up
	(setq src_op (term$head src_pars)
	      src_varbs (term$subterms src_pars))
	; should do a validity check of src_varbs ... all vars, no-rep's &&&&
	(unless (term$is_an_error src_pars)
	  (setq src_done t)
	  (setq psuedo_vars (modexp$make_psuedo_vars src_varbs)))
	)
    (unless src_done
      (if (typep src_opex 'operator) ;@@
	  (setq try_op src_opex)
	(setq try_op (mod_eval$$find_qual_operator_named_in
		      src_mod src_opex)))
      ;@@ up
      (when (null try_op)
	(setq try_op
	      (mod_eval$$find_qual_operator_named_in
	       src_mod 
	       (modexp_parse$parse_op_name_specn src_opex))))
         ;@@ record case "somehow"?
      (if (null try_op)
	  (progn
	    (princ "Couldn't find operator ")
	    (print$simple_princ_open src_opex)
	    (princ " appearing as source in view") (terpri)
	    (princ "Attempting to continue") (terpri))
	(progn
	  (setq src_is_op t)
	  (setq src_op try_op)
	  (setq src_vars (modexp$make_psuedo_vars_from_sorts
			  (operator$arity src_op)))
	  (setq psuedo_vars src_vars))
	))
    (setq dst_is_op (typep dst_opex 'operator))
    (unless (or dst_is_op src_is_op)
      (setq dst_pars (let ((*parser$variables* 
			    (mapcar #'(lambda (x)
					(cons (variable$name x) x))
				    dst_vars)))
		       (let ((obj$current_module *obj_BOOL$module*))
			 (substitution$simple_image ;@@
			  (mapcar #'cons
			      (mapcar #'(lambda (x)
			          (assoc (car x) dst_vars :test #'equal))
				src_varbs)
			      psuedo_vars)
			  (parse$quiet_parse dst_mod dst_opex
			      (if (eq 'universe dst_sort) *obj$sort_Universal*
				(if (typep dst_sort 'sort) dst_sort
				  (mod_eval$$find_qual_sort_in
				   dst_mod dst_sort))))))))
      (unless (term$is_an_error dst_pars)
	(setq dst_done t))
      )
    (unless dst_done
	; should check that ops are compatible
	(let ((dst_op
	       (if (typep dst_opex 'operator) ;@@
		   dst_opex
		 (mod_eval$$find_qual_operator_named_in ;@@
		  dst_mod
		  dst_opex)
		 )))
	  (when (null dst_op)
	    (setq dst_op
		  (mod_eval$$find_qual_operator_named_in
		   dst_mod 
		   (modexp_parse$parse_op_name_specn dst_opex))))
	  ;@@ record case "somehow"?
	  (if (null dst_op)
	      (progn
		(princ "Couldn't find operator ")
		(print$simple_princ_open dst_opex)
		(princ " appearing as target in view") (terpri)
		(princ "Attempting to continue") (terpri))
	    (progn
	      (setq dst_is_op t)
	      (setq dst_pars (term$make_term_check_op dst_op psuedo_vars)))
	    )
      ))
    ;@@up
    ;check to perform -- the set of vars in dst should be subset of
    ; those in src
    (when (or (and src_is_op (not dst_is_op))
	      (and (not src_is_op) dst_is_op))
      (if src_is_op (princ "source is op") (princ "source is not op"))
      (princ "; ")
      (if dst_is_op (princ "target is op") (princ "target is not op")) (terpri)
      (princ "source operator: ") (print$name src_op) (terpri)
      (princ "target term: ") (print$term dst_pars) (terpri)
      (obj3-module-error))
    ;; the sort of a view mapping is always going to be a term
    `(op ,(term$make_term_check_op src_op psuedo_vars) ,dst_pars)
  )))

;@@ probably will no longer be used -- probably isn't
; Is an operator expression the name of an operator (as opposed to being
;   an expression)?
(defun modexp$is_op_name (x) ;@@ new cases qual
  (and (consp x)
       (not (equal "(" (car x)))
       (or
	(member "_" x :test #'equal)
	(and (null (cdr x)) (atom (car x))))))

; makes sequence of variables with canonical names (natural numbers)
(defun modexp$make_psuedo_vars (l)
  (do ((r l (cdr r))
       (i 0 (1+ i))
       (res nil (cons (variable$make_var i (variable$initial_sort (car r)))
		      res)))
      ((null r) (nreverse res)))
  )

; makes sequence of variables with canonical names (natural numbers)
(defun modexp$make_psuedo_vars_from_sorts (l)
  (do ((r l (cdr r))
       (i 0 (1+ i))
       (res nil (cons (variable$make_var i (car r)) res)))
      ((null r) (nreverse res)))
  )

; parameterize module-expression vw as a view/argument to parameterized
; object mod
; special cases:
;   module name --> default computation
;   sort name --> default view with TRIV theory
; need to have index of view so that can find theory associated with
;   this view from parameterized module
; note: mod is evaluated
(defun modexp_eval$canonicalize_view_arg (argno vw mod &optional (ren nil))
  (let ((ovw vw))
  (when (and (consp vw) (eq '*view (car vw)))
    (setq ren (modexp_eval$compose_renames ren (caddr vw)))
    (setq vw (cadr vw)))
  (cond
   ;sort, object, view name, view proper
   ((or (atom vw) (member (car vw) '(|:| + *)))
    (let ((modval (modexp_eval$eval_whole vw)))
    (if (modexp_eval$is_error modval)
	(let ((srt (if (and obj$current_module
			    (or (stringp vw) ;&&&& intro function
				(and (consp vw)
				     (eq 'qual (car vw)))))
		       (mod_eval$$find_qual_sort_in obj$current_module vw))))
	  (if srt
	      (let* ((pushed (eq (sort$module srt) obj$current_module))
		     (tgt (if pushed
			      (modexp_eval$create_pushed_module
			       obj$current_module)
			    (sort$module srt)))
		     (newsrt
		      (if pushed
			  (mod_eval$$find_qual_sort_in obj$current_module vw)
			srt)))
	        (modexp_eval$canonicalize_view_arg argno
		  (let* ((thnm (modexp_eval$nth_param argno mod))
			 (thprs (modexp_eval$theory_principal_sort
				 (caddr thnm)))
			 (tprs (if pushed
				 (mod_eval$$find_sort_in
				  tgt (sort$name newsrt))
				 (if newsrt (sort$name newsrt) nil))))
		  `(view ,thnm
			 ,tgt
			 ,(if (and thprs tprs)
			     `((sort ,(sort$name thprs) ,tprs))
			     (progn
			       (when thprs
				 (princ "Sort ") (print$name thprs)
				 (princ " has no image in default view")
				 (terpri)
				 (obj3-to-top))
			       nil))))
		  mod
		  ren
		  ))
	  (let ((val (modexp_eval$find_in_view_env ovw)))
	    (if val (cdr val)
	      (progn
		(princ "Unrecognized module parameter: ")
		(print$modexp vw) (terpri)
                (obj3-to-top))
	      )))
	)
      ;&&&& the following should be replaced by a general default computation
      (modexp_eval$canonicalize_view_arg
       argno
       (let* (;(thy (caddr (modexp_eval$nth_param argno mod)))
              (thy (cdr (nth argno (module$parameters mod))))
	      (thy_srt (modexp_eval$theory_principal_sort thy))
	      (tgt_srt (let ((asrt (if thy_srt
				     (mod_eval$$find_qual_sort_in modval
				         (sort$name thy_srt))
				     nil)))
		       (if (and asrt
				(let ((m (sort$module asrt)))
				(or (not (eq 'theory (module$kind m)))
				    (not (modexp_eval$is_parameter_theory
					  (module$name m))))))
			 (sort$name asrt)
			 (let ((aprs (modexp_eval$principal_sort modval)))
			   (if aprs (sort$name aprs) nil))))))
	 `(view ,(module$name thy) ;&&&&
	   ,vw
	   ,(if (and thy_srt tgt_srt)
	       `((sort ,(sort$name thy_srt) ,tgt_srt))
	       (progn
		 (when thy_srt
		   (princ "Sort ") (print$name thy_srt)
		   (princ " has no image in default view")
		   (terpri)
		   (obj3-to-top))
		 nil))
	     ))
       mod
       ren
      ))))
   ;&&&& note difference below from that above -- use of the real theory
   ((member (car vw) '(specn op sort qual))
    (let ((avw (if (eq 'qual (car vw)) `(specn ,vw) vw)))
    (let ((srt
	   (if (and (not (eq 'op (car avw)))
		    (or
		     (eq 'sort (car avw))
		     (let ((nam (cadr avw)))
		       (and (consp nam) (null (cdr nam))))))
	     (mod_eval$$find_qual_sort_in
	      obj$current_module (cadr avw))
	     nil)))
      (if srt
	  (let* ((pushed (eq (sort$module srt) obj$current_module))
		 (tgt
		  (if pushed
		      (modexp_eval$create_pushed_module obj$current_module)
		    (sort$module srt)))
		 (newsrt (if pushed
			     (mod_eval$$find_qual_sort_in
			      obj$current_module (cadr avw))
			   srt)))
	  (modexp_eval$canonicalize_view_arg argno
	    (let ((thnm (modexp_eval$nth_param argno mod))
		  (psrt (modexp_eval$theory_principal_sort
			     ;(caddr thnm)
			     (cdr (nth argno (module$parameters mod)))
			    )))
	      ;cf. version above
	      `(view ,thnm
		     ,tgt
		     ,(if (and psrt newsrt)
			 `((sort (qual
			   (,(sort$name psrt))
			   ,thnm
			   )
			  ,newsrt
;			    (if pushed
;			       (mod_eval$$find_sort_in tgt (sort$name srt))
;			     srt)
			  ))
			 (progn
			   (when psrt
			     (princ "Sort ") (print$name psrt)
			     (princ " has no image in default view")
			     (terpri)
			     (obj3-to-top))
			   nil))))
	    mod ren)
	)
      ;must be an operator
      (let* ((op (mod_eval$$find_qual_operator_named_in
		  obj$current_module (cadr avw)))
	     (pushed (eq (operator$module op) obj$current_module))
	     (tgt (if pushed
		      (modexp_eval$create_pushed_module obj$current_module)
		    (operator$module op)
		    ))
	     (newop (if pushed
			(mod_eval$$find_qual_operator_named_in
			 obj$current_module (cadr avw))
		      op)))
	(modexp_eval$canonicalize_view_arg argno
	  (let* ((thnm (modexp_eval$nth_param argno mod))
		 (thy (modexp_eval$eval thnm)))
	  (when (null newop)
	    (princ "Error: in default view specified by operator: operator")
	    (terpri)
	    (princ "not found; view from ")
	    (print$name thy) (princ " to ")
	    (print$qual_op_name (cadr avw)) (terpri)
	    (obj3-to-top))
	  (let ((srcop (modexp_eval$appropriate_op
			thy obj$current_module newop)))
	    (when (or (null srcop)
		      (not (= (length (operator$arity srcop))
			      (length (operator$arity newop)))))
	      (princ "Error: no appropriate operator in theory") (terpri)
	      (princ "for default view to operator ") (terpri)
	      (princ "in view from ") (print$name thy) (princ " to ")
	      (print$name newop) (terpri)
	      (obj3-to-top))
	    `(view ,thnm ,tgt
		 ((op (universe ,srcop) (universe ,newop)))
		 )))
	  mod ren)
	))
    )))
   ((eq 'view (car vw))
    (setq vw (modexp_eval$$simplify_view vw))
    (let ((real_view (modexp_eval$view_amplify argno vw mod)))
    (let ((view
	    (modexp_eval$$view_canon_defaults ;## defaults
	     (cadr real_view)
	     (caddr real_view)
	     (cadddr real_view)
	     )
	  ))
    (setq view (modexp_eval$apply_rename_to_view view ren))
    (let ((aview (modexp_eval$find_in_view_env view)))
      (if aview (cdr aview)
	(progn
	  (modexp_eval$!add_view_defn real_view view)
	  (modexp_eval$!add_view_defn view view)
	  view
      ))))))
   ((eq 'view_from (car vw))
    (let ((newview (modexp_eval$apply_rename_to_view vw ren)))
    (let ((aview (modexp_eval$find_in_view_env newview)))
      (if aview (cdr aview)
	(progn
	  (modexp_eval$!add_view_defn vw newview)
	  (modexp_eval$!add_view_defn newview newview)
	  newview
      )))))
   (t (break "modexp_eval$canonicalize_view_arg: not yet"))
  ))
  )

(defun modexp_eval$create_pushed_module (mod)
  (let ((newmod
	 (mod_eval$create_renamed_module
	  mod
	  (mod_eval$create_variant_name mod mod))))
    (module$clear mod)
    (if (module$parameters newmod)
	(let ((newmod_inst
	       (modexp_eval$eval
		(modexp$make_instantiation
		 newmod
		 (mapcar #'cdr (module$parameters mod))
		 ))))
	  (let ((allsubs (module$sub_modules mod)))
	    (setf (module$sub_modules mod) nil)
	    (dolist (x allsubs)
	      (when (rassoc (car x) (module$parameters mod))
		(module$!add_imported_module mod (cdr x) (car x))
		(mod_eval$$!incorporate_module mod (cdr x) (car x)))))
	  (mod_eval$$!import_module mod 'protecting newmod_inst)
	  newmod_inst)
      (progn
	(mod_eval$$!import_module mod 'protecting newmod)
	(dolist (x (module$sub_modules mod))
		(mod_eval$$!incorporate_module mod (cdr x) (car x)))
	newmod))
    ))

(defun modexp_eval$appropriate_op (srcmod module op)
  (when (or (null module)
	    (not (member op (module$operators module))))
    (setq module (operator$module op)))
  (let ((arnum (length (operator$arity op)))
	(theory (module$operator_equational_theory module op)))
  (let ((val
	 (remove-if-not
	  #'(lambda (opr)
	      (and (= arnum (length (operator$arity opr)))
		   (modexp_eval$is_included_in_theory
		    (module$operator_equational_theory srcmod opr) theory)
		   (eq srcmod (operator$module opr))
		   ))
	  (module$operators srcmod))))
    (if val
	(if (null (cdr val)) (car val)
	  nil)
      nil))))

;&&&& not used
(defun modexp_eval$principal_op (mod)
  (dolist (op (reverse (module$operators mod)) (car (module$operators mod)))
    (when (eq mod (operator$module op)) (return op))
  ))

;&&&& note, this will have to work with either raw views or eval'd
; in following: should be checking sorts on image and can do that
;   by fixing the inverse images at this point
(defun modexp_eval$apply_rename_to_view (vw ren)
  (if (null ren) vw ;@@ moved up from between let/map and let/srtm
  (let ((mod (modexp_eval$eval (caddr vw))))
  (let ((map (modexp_eval$recreate_rename_mapping_info
		 mod ren)))
    (let ((srtm (mapping$sort map))
	  (opm (mapping$op map))
	  (src (cadr vw))
	  (tgt (caddr vw))
	  (vwmap (cadddr vw))
	  (resmap nil))
      (dolist (i vwmap)
	(if (eq 'sort (car i))
	  (push 
	   `(sort ,(cadr i)
		  ,(modexp_eval$image_view_rename_sort mod ren srtm (caddr i)))
	   resmap)
	(if (eq 'op (car i))
	  (push 
	   `(op ,(cadr i)
		  ,(modexp_eval$image_view_rename_op srtm opm (caddr i)))
	   resmap)
	  (break "modexp_eval$apply_rename_to_view: incomplete")
        ))
      )
      (modexp$make_view_from src tgt (nreverse resmap)) ;21 Jan 88 nreverse
      ))
  )))

(defun modexp_eval$image_view_rename_sort (mod ren srtm srt)
  (let ((val (assoc (sort$name srt) srtm
		    :test #'modexp_eval$sort_match)))
    (if val
	(cdr val)
    (if (member srt (module$sorts mod)) srt
    (if (typep srt 'sort)
	(modexp_eval$recreate_renamed_sort mod ren srt)
      srt)))))

(defun modexp_eval$recreate_renamed_sort (mod ren srt)
  (let ((num 0) (srtnm (sort$name srt)) (im nil))
  (dolist (s (module$sorts mod))
    (when (equal srtnm (sort$name s))
      (unless im (setq im s))
      (incf num)))
  (if (= 1 num) im
  (let ((renmod (modexp_eval$eval
		 (modexp$make_rename (module$name (sort$module srt))
				     ren))))
    (dolist (s (module$sorts renmod))
      (when (and (eq renmod (sort$module s))
		 (equal srtnm (sort$name s)))
	(return s)))
  ))))

;&&&& probably should pass sort instead of just name
(defun modexp_eval$sort_match (srtnm srtpat)
  (or (equal srtnm srtpat)
      (and (consp srtpat) (null (cdr srtpat)) (equal srtnm (car srtpat))))
  )

(defun modexp_eval$image_view_rename_op (srtm opm trm)
; from mapping$$recreate_term (module sortmap opmap term)
(cond
  ((term$is_an_error trm) trm)
  ((term$is_built_in_constant trm) trm) ;&&&& for the moment
  ((term$is_var trm) trm) ; same as in source of view
  ;; what about retracts? &&&&
  (t (let ((head (term$head trm)))
     ; (let ((opnm (operator$name head))) ...)
     (let ((val
	    (find-if #'(lambda (rn)
			 (modexp_eval$is_renamed_op srtm head rn))
	      opm)
	    )) ;&&&& needs change
       (term$make_term_check_op
	(if val (cdr val) head)
	(mapcar #'(lambda (tm)
		    (modexp_eval$image_view_rename_op srtm opm tm))
	  (term$subterms trm))))))))

;&&&& much room for simplification and optimization
;@@ new (specn x) (op x) (sort x) (qual x y) -- new cases here @@more
(defun modexp_eval$is_renamed_op (srtm op ren)
  (declare (ignore srtm))
  (let ((opn (car ren)))
  (let ((pat (if (util$check_enclosing_parens opn) (butlast (cdr opn)) opn)))
    (if (member "->" (member ":" pat :test #'equal) :test #'equal)
      (let* ((pos1 (position ":" pat :from-end t :test #'equal))
	     (pos2 (position "->" pat :from-end t :test #'equal))
	     (opname (subseq pat 0 pos1))
	     (ar (subseq pat (1+ pos1) pos2))
	     (coar (nth (1+ pos2) pat)))
	(and (equal (operator$name op) opname)
	     (equal coar (sort$name (operator$coarity op)))
	     (every2len #'(lambda (x y)
			    (equal x (sort$name y)))
	       ar
	       (operator$arity op))
	     )
	)
      (equal (operator$name op) pat)))))

; what about parameter modules?
(defun modexp_eval$recreate_rename_mapping_info (mod ren)
  ; derived from modexp_eval$rename_create (mod ren)
  (let ((sortmap nil) (opmap nil) (modmap nil))
  (let ((map (mapping$make "Anon." sortmap opmap modmap)))
  (setq sortmap
    (mapcan #'(lambda (x)
		(let ((sort (cadr x)))
		  (unless sort
		    (princ "sort not found for rename: ")
		    (princ (cadr x)) (terpri))
		(let ((res (mod_eval$$find_qual_sort_in ;@@
			    mod (caddr x))))
		  ;><non-bijective case
		  (if res (list (cons sort res))
		    (progn
		      (when $$debug
		      (princ "modexp_eval$recreate_rename_mapping_info:")
		      (terpri)
		      (princ "cannot complete view with rename for sort ")
		      (princ (caddr x)) (terpri)
		      (princ "which is the image of ") (princ sort)
		      (terpri)
		      (princ "in view to module ") (print$name mod)
		      (terpri))
		      nil)))))
    (remove-if-not #'(lambda (x) (eq 'sort (car x))) ren)))
  (setf (mapping$sort map) sortmap)
  (setf (mapping$module map) modmap)
  (setq opmap
    (mapcan #'(lambda (x)
	       (let ((op (cadr x)))
	       (let ((res nil))
		 (let ((imageop
			(modexp_eval$find_renamed_qual_operator_named_in
			 sortmap
			 mod (caddr x))))
		   ;@@ up
		   (when (and $$debug (null imageop))
		     (princ "### modexp_eval$recreate_rename_mapping_info")
		     (terpri)
		     (princ op) (terpri)
		     (print$name mod) (terpri)
		     (princ (caddr x)) (terpri))
		   (when imageop
		     (push (cons op imageop) res))) ;&&&& simplify
	         res)))
    (remove-if-not #'(lambda (x) (eq 'op (car x))) ren)))
  (setf (mapping$op map) opmap)
  map
  )))

;@@ new version of this @@more
(defun modexp_eval$find_renamed_operator_named_in (srtmap mod opn)
  (let ((opnm (if (util$check_enclosing_parens opn) (butlast (cdr opn)) opn)))
    (if (member "->" (member ":" opnm :test #'equal) :test #'equal)
	(let* ((pos1 (position ":" opnm :from-end t :test #'equal))
	       (pos2 (position "->" opnm :from-end t :test #'equal))
	       (opname (subseq opnm 0 pos1))
	       (ar (subseq opnm (1+ pos1) pos2))
	       (coar (nth (1+ pos2) opnm)))
	(let ((val (mod_eval$find_operator_from_rank
		    mod
		    (append
		     opname '(":")
		     (mapcar #'(lambda (x)
				 (sort$name (modexp_eval$image_rename_sort
					     srtmap x)))
			     ar)
		     '("->")
		     (sort$name (modexp_eval$image_rename_sort srtmap coar)))
		    )))
	  (when val (list val))))
      (mod_eval$$find_operator_named_in mod opn)))
  )

(defun modexp_eval$find_renamed_qual_operator_named_in (srtmap module opn)
  (if (and (consp opn)
	   (eq 'qual (car opn)))
      (let ((qual (mod_eval$fix_qualification module (caddr opn))))
	(if qual
	    (if (typep qual 'module)
		(modexp_eval$find_renamed_operator_named_in
		 srtmap qual (cadr opn))
	    (if (typep qual 'sort)
		(modexp_eval$$find_renamed_operator_named_in_sort
		 srtmap module (cadr opn) qual)
	      nil))
	nil
	))
  (modexp_eval$find_renamed_operator_named_in srtmap module opn)
  ))

;&&&& ><
(defun modexp_eval$$find_renamed_operator_named_in_sort
       (srtmap module sort opn)
  (declare (ignore srtmap module sort opn))
  (break "modexp_eval$$find_renamed_operator_named_in_sort: not yet")
  )

; collect args to tree of plus's at top; canonicalize; cannot re-order
; build a right-associated tree (delete duplications)
(defun modexp_eval$plus_canonicalize (modexp)
  ;&&&& more
  (let ((a1 (modexp_eval$canonicalize (cadr modexp)))
	(a2 (modexp_eval$canonicalize (caddr modexp))))
    (let ((newplus (modexp$make_plus a1 a2)))
    (if (or (modexp$is_view a1) (modexp$is_view a2)) ;&&&& simplify
	(modexp_eval$lift_views_in_plus newplus)
      newplus))))

; (view A to B map1) + (view C to D map2) => (view A+B to C+D map1+map2)
; (view A to B map1) + C => (view A to B+C map1)
; for the moment assume that we have view_from's
(defun modexp_eval$lift_views_in_plus (me)
  (let ((a1 (cadr me))
	(a2 (caddr me)))
  (if (and (modexp$is_view a1) (modexp$is_view a2))
      (modexp_eval$view_plus a1 a2)
  (if (modexp$is_view a1)
      (if (eq 'view_from (car a1))
	  (modexp_eval$make_lifted_view_plus
	   (cadr a1) (cadddr a1) (caddr a1) a2)
	(break "modexp_eval$lift_views_in_plus: only view_from"))
  (if (modexp$is_view a2)
      (if (eq 'view_from (car a1))
	  (modexp_eval$make_lifted_view_plus
	   (cadr a2) (cadddr a2) a1 (caddr a2))
	(break "modexp_eval$lift_views_in_plus: only view_from"))
    me)))))

(defun modexp_eval$make_lifted_view_plus (th map m1 m2)
  (modexp$make_view_from
   th
   (modexp_eval$eval
    (modexp$make_plus
     (modexp_eval$eval_whole m1)
     (modexp_eval$eval_whole m2)))
   map))

(defun modexp_eval$view_plus (a1 a2)
  (unless (and (eq 'view_from (car a1))
	       (eq 'view_from (car a2)))
    (break "modexp_eval$view_plus: only view_from"))
  (let ((th1 (cadr a1))
	(m1 (caddr a1))
	(map1 (cadddr a1))
	(th2 (cadr a2))
	(m2 (caddr a2))
	(map2 (cadddr a2)))
  (modexp$make_view_from
    (modexp_eval$eval (modexp$make_plus th1 th2))
    (modexp_eval$eval (modexp$make_plus m1 m2))
    (mapping$merge map1 map2))))
