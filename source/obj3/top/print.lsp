;; OBJ3 version 2
;; Modified 5/30/2001 by Lutz Hamel for TRIM support
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

;; $Id: print.lsp,v 205.2.1.1 2003/09/23 14:17:00 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                    print
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Tim Winkler ;;;; Created: 10/2/91
;;;; based on original obj_print

;;; object printing functions -- user visible printing functions 
;;; more debugging oriented output appears in obj_print.lsp

;;;;;;;;;;;;;;;; Summary ;;;;;;;;;;;;;;;;
; omitted

; lhh - make sure we continue with comments for TRIM output
(defvar *print$trim-comments-contin* nil) 

(defvar *print$indent* 0)
(defvar *print$indent_increment* 2)
(defvar *print$explicit* nil) ;if t then give more detail on sorts, etc.
(defvar *print$abbrev_mod* nil) ; abbreviate module names
(defvar *print$abbrev_num* 0)
(defvar *print$abbrev_table* nil)
(defvar *print$operator_table* nil)
(defvar *print$flag_module_values* nil)
(defvar *print$indent_contin* t)
(defvar *print$mode* nil)
(defvar *print$all_eqns* nil)
(defvar *print$ignore_mods* nil)

(defun print$indent (&optional (n *print$indent*))
  (dotimes (i (* n *print$indent_increment*)) (princ " ")))
(defun print$next (&optional (n *print$indent*))
  (terpri) (print$indent n))

; lhh -- make sure we respect the TRIM comment mode
(defun print$chk ()
  (when (< *print$line_limit* (filecol *standard-output*))
	(terpri)
        (when *print$trim-comments-contin* (princ "--- "))
	(when *print$indent_contin* (princ "    "))))

; lhh -- make sure we respect the TRIM comment mode
(defun print$chkn (n)
  (when (< *print$line_limit* (+ (filecol *standard-output*) n))
	(terpri)
        (when *print$trim-comments-contin* (princ "--- "))
	(when *print$indent_contin* (princ "    "))
	t))
  
(defvar *obj3-print-level* 5)
(defvar *obj3-print-length* 100)

(defun pr (x)
  (let ((*print-level* *obj3-print-level*)
	(*print-length* *obj3-print-length*))
    (prin1 x))
  'done)

(defun print$simple (x)
  (if (is-illegal-type x) (progn (princ "illegal#") (print$addr x))
  (cond
   ((atom x) (prin1 x))
   (t (let ((flag nil) (tail x))
     (princ "(")
     (loop
      (when (not (consp tail)) (return))
      (if flag (princ " ") (setq flag t))
      (print$simple (car tail))
      (setq tail (cdr tail))
     )
     (when tail (princ " . ") (prin1 tail))
     (princ ")")
   )))))

;to eliminate
(defun print$simple-princ (x) (print$simple_princ x))

(defun print$simple_princ (x)
  (print$chk)
  (if (is-illegal-type x) (progn (princ "illegal#") (print$addr x))
  (cond
   ((atom x) (princ x))
   (t (let ((flag nil) (tail x))
     (princ "(")
     (loop
      (when (not (consp tail)) (return))
      (if flag (princ " ") (setq flag t))
      (print$simple_princ (car tail))
      (setq tail (cdr tail))
     )
     (when tail (princ " . ") (prin1 tail))
     (princ ")")
   )))))

(defun print$simple_princ_open (x)
  (print$chk)
  (if (is-illegal-type x) (progn (princ "illegal#") (print$addr x))
  (cond
   ((atom x) (princ x))
   (t (let ((flag nil) (tail x))
     (loop
      (when (not (consp tail)) (return))
      (if flag (princ " ") (setq flag t))
      (print$simple_princ (car tail))
      (setq tail (cdr tail))
     )
     (when tail (princ " ... ") (prin1 tail))
   )))))

(defun print$safe (x)
  (if (is-illegal-type x) (progn (princ "illegal#") (print$addr x))
  (cond
   ((or (numberp x) (symbolp x) (stringp x)) (princ x))
   ((and (consp x) (null (cdr (last x)))
	 (every #'stringp x))
    (princ "(")
    (let ((flg nil))
    (dolist (i x)
      (if flg (princ " ") (setq flg t))
      (princ i)))
    (princ ")"))
   (t (print$name x))
  )))

(defun print$mod_name (arg)
  (if (typep arg 'module)
  (let ((x (if (and *mod_eval$open_module*
		    (eq arg *mod_eval$open_module*)
		    *mod_eval$last_before_open*
		    (equal "%" (module$name arg)))
	     *mod_eval$last_before_open*
	     arg)))
  (progn
    (when *print$flag_module_values* (princ "{"))
    (let ((modname (module$name x)))
    (if *print$abbrev_mod*
	(if (or (stringp modname)
		(and (consp modname) (equal "::" (cadr modname))))
	    (print$mod_name_internal modname)
	(let ((ab (assoc x *print$abbrev_table*)))
	  (if ab (progn (princ "MOD-") (princ (cdr ab)))
	    (progn
	      (push (cons x *print$abbrev_num*) *print$abbrev_table*)
	      (princ "MOD-") (princ *print$abbrev_num*)
	      (incf *print$abbrev_num*)
	      ))))
    (if (and (equal "DUMMY" modname)
	     (getf (module$status x) 'dummy))
	(let ((info (getf (module$status x) 'rename_mod)))
	  (print$mod_name (car info))
	  (princ "*DUMMY")
	  (when *print$explicit*
	    (if (eq 'sort (cadr info))
		(progn (princ "(sort ") (print$name (caddr info)) (princ ")"))
	      (progn
		(princ "(op ") (print$jammed (caddr info))
		(princ " ") (print$jammed (cadddr info)) (princ ")")))))
      (print$mod_name_internal modname))))
    (when *print$flag_module_values* (princ "}"))))
  (progn
    (princ "##") (print$struct arg)
    )))

(defun print$short_module_name (x)
  (if (typep x 'module)
  (let ((modname (module$name x)))
    (if (or (stringp modname)
	    (and (consp modname) (equal "::" (cadr modname)))) ;14 Jul 87&&&&
	(print$mod_name_internal modname)
      (print$mod_name_internal
        (list* (car modname) (cadr modname)
          (mapcar #'(lambda (x) (declare (ignore x)) "_") (cddr modname))))
      ))
  (progn
    (princ "##") (print$struct x)
    )))

(defun print$mod_name_internal (val)
  (if (stringp val) (princ val)
  (if (consp val)
      (if (equal "::" (cadr val))
	  (progn (princ (car val)) (princ "::") (print$name (caddr val)))
	(print$modexp val)
	)
    (print$name val))))

; m is module context or nil if indeterminate
; currently not used -- goal is to use to resolve ambiguities
(defun print$sort_name (m s)
  (declare (ignore m))
  (cond
   ((not (typep s 'sort)) (print$name s))
   (*print$explicit*
    (princ (sort$name s))
    (princ ".")
    (print$name (sort$module s))
    )
   (t (let ((val (sort$name s)))
	(if (and obj$current_module
		 (dolist (x (module$sorts obj$current_module) nil)
		   (when (and (not (eq x s))
			      (equal val (sort$name x)))
		     (return t))))
	    (progn
	      (princ val)
	      (princ ".")
	      (print$qualn (sort$module s)))
	  (princ val))))
  ))

(defun print$short_sort_name (s)
  (cond
   ((not (typep s 'sort)) (print$name s))
   (*print$explicit*
    (princ (sort$name s))
    (princ ".")
    (print$short_module_name (sort$module s))
    )
   (t (princ (sort$name s)))
  ))

(defun print$sort_list_open (mod lst)
  (let ((flag nil))
  (dolist (s lst)
    (if flag (princ " ") (setq flag t))
    (print$sort_name mod s)))
  )

; @@@@ improve this
(defun print$theory_brief (thy)
  (let ((th_name (theory$name thy))
	(flag nil)
	(val (theory$zero thy)))
  (when (or (theory_name$is_AC th_name)
	    (theory_name$is_A th_name)
	    (theory_name$is_AI th_name)
	    (theory_name$is_AZ th_name)
	    (theory_name$is_AIZ th_name)
	    (theory_name$is_ACI th_name)
	    (theory_name$is_ACZ th_name)
	    (theory_name$is_ACIZ th_name))
    (setq flag t)
    (princ "assoc"))
  (when (or (theory_name$is_AC th_name)
	    (theory_name$is_C th_name)
	    (theory_name$is_CI th_name)
	    (theory_name$is_CZ th_name)
	    (theory_name$is_CIZ th_name)
	    (theory_name$is_ACI th_name)
	    (theory_name$is_ACZ th_name)
	    (theory_name$is_ACIZ th_name))
    (if flag (princ " ") (setq flag t))
    (princ "comm"))
  (when (or (theory_name$is_I th_name)
	    (theory_name$is_IZ th_name)
	    (theory_name$is_CI th_name)
	    (theory_name$is_CIZ th_name)
	    (theory_name$is_AI th_name)
	    (theory_name$is_AIZ th_name)
	    (theory_name$is_ACI th_name)
	    (theory_name$is_ACIZ th_name))
    (if flag (princ " ") (setq flag t))
    (princ "idem"))
  (when val
    (if flag (princ " "))
    (if (null (cdr val)) (princ "id: ") (princ "idr: "))
    (print$term (car val)))
  ))

; check if bu strategy: 1 2 3 .. n [ 0 ]
; probably would like to improve on this
(defun print$check_bu (op l)
  (let ((iota (util$make_list_1_n (length (operator$arity op)))))
  (or (equal l iota)
      (equal l (append iota '(0))))))

(defun print$sort_brief_mod (mod srt)
  (let* ((so (module$sort_order mod))
	 (prs (module$principal_sort mod))
	 (low (sort_order$lower_sorts so srt))
	 (hi (sort_order$greater_sorts so srt)))
    (print$chk) (princ "sort ") (print$sort_name mod srt) (princ " .")
    (when (eq srt prs)
      (print$chk) (princ " psort ") (print$chk)
      (print$sort_name mod srt)  (princ " ."))
    (when (or low hi)
      (print$chk)
      (princ " subsorts ")
      (print$chk)
      (when low
	(print$sort_list_open mod low)
	(princ " < ") (print$chk))
      (print$sort_name mod srt)
      (print$chk)
      (when hi
	(princ " < ")
	(print$chk)
	(print$sort_list_open mod hi))
      (princ " .")
      )
  ))

(defun print$sort_brief (srt)
  (print$sort_brief_mod obj$current_module srt)
  )

;; lhh -- print operator declarations suitable for the Eq compiler.
(defun print$op_brief-trim (op)
  (princ "op ")
;  (print$simple_princ_open (operator$name op))
  (dolist (i (operator$name op))
	  (princ i)
  )
  (princ " : ")
  (when (operator$arity op)
    (print$sort_list_open obj$current_module (operator$arity op))
    (princ " "))
  (princ "-> ")
  (print$sort_name obj$current_module (operator$coarity op))
  )

(defun print$op_brief (op)
  (princ "op ")
  (print$simple_princ_open (operator$name op))
  (princ " : ")
  (when (operator$arity op)
    (print$sort_list_open obj$current_module (operator$arity op))
    (princ " "))
  (princ "-> ")
  (print$sort_name obj$current_module (operator$coarity op))
  ;  print "attributes" -- for the moment ignore purely syntactic
  ;  -- i.e. precedence and gather
  (let ((strat
	 (let ((val (module$operator_rew_strategy obj$current_module op)))
	   (if (print$check_bu op val) nil val)))
	(memo (module$operator_memo obj$current_module op))
	(thy (module$operator_equational_theory obj$current_module op))
	(poly (module$operator_polymorphic obj$current_module op)))
    (when (or strat memo (not (eq (theory$name thy) the_empty_property)) poly)
      (let ((flag nil)
	    (*print$indent* (1+ *print$indent*)))
      (princ " [")
      (when (not (eq (theory$name thy) the_empty_property))
	 (setq flag t)
	 (print$theory_brief thy))
      (print$check)
      (when strat
	(if flag (princ " ") (setq flag t))
	(princ "strat ") (print$simple strat))
      (print$check)
      (when memo
	(if flag (princ " ") (setq flag t))
	(princ "memo"))
      (when poly
	(unless (print$chkn 40)
	  (if flag (princ " ")))
	(princ "polymorphic ") (prin1 poly))
      (princ "]")))
  ))

(defun print$term (term)
  (if (is-illegal-type term) (progn (princ "illegal#") (print$addr term))
  (if *print$mode* (print$pterm term)
    (term$print term))))

(defun print$rule_brief (rul)
  (when (rule$labels rul) (print$rule_labels rul) (princ " "))
  (print$rule_brief_no_label rul))

(defun print$rule_brief_no_label (rul)
  (let ((bi (rule$is_built_in rul))
	(cnd (not (term$similar *obj_BOOL$true* (rule$condition rul))))
	(rul_rhs (rule$rhs rul)))
    (if bi
	(if (eq 'obj$general_invoke
		(car (caddr (function_lambda_expression rul_rhs))))
	  (if cnd (princ "cbeq ")
	    (princ "beq "))
	  (if cnd (princ "cbq ")
	    (princ "bq ")))
      (if cnd (princ "cq ")
	(princ "eq ")))
    (print$term (rule$lhs rul)) (princ " = ")
    (if bi
      (print$bi_rhs (function_lambda_expression rul_rhs))
      (print$term (rule$rhs rul)))
    (when (or cnd
	      (and obj$verbose (rule$id_condition  rul)))
      (princ " if ")
      (when cnd
	(print$term (rule$condition rul)))
      (when obj$verbose
	(when (and cnd (rule$id_condition rul)) (princ " and "))
	(when (rule$id_condition rul)
	  (print$id_condition (rule$id_condition rul))))
      )
  ))

; lhh -- print the rules in Eq format
(defun rule$print-trim (rul old-stream)
  (prog ()
    (let ((bi (rule$is_built_in rul))
	  (cnd (not (term$similar *obj_BOOL$true* (rule$condition rul))))
	  (rul_rhs (rule$rhs rul))
          (save-ostream *standard-output*)
         )

      ; create prologue to equation: eq or cq (not built-ins allowed)
      (if bi
        ; then
        (let ()
          (setq *standard-output* old-stream)
          (terpri)
          (princ "Warning: built-in equations are not supported -- ignored") 
          (terpri)
          (princ "equation: ")
          (print$rule_brief rul)
          (terpri)
          (setq *standard-output* save-ostream)
          (return)
          (if (eq 'obj$general_invoke
		(car (caddr (function_lambda_expression rul_rhs))))
            (if cnd
              (princ "cbeq ")
              (princ "beq "))
	    (if cnd 
              (princ "cbq ")
	      (princ "bq "))
          )
        ) 
        ; else if not built-in
        (if cnd 
          (princ "cq ") 
          (princ "eq "))
      )

      ; print left hand side
      (term$print-trim (rule$lhs rul) old-stream) 

      (princ " => ")

      ; print the right hand side
      (if bi
        (print$bi_rhs (function_lambda_expression rul_rhs))
        (term$print-trim (rule$rhs rul) old-stream)
      )

      ; print the condition
      (when (or cnd
	      (and obj$verbose (rule$id_condition  rul)))
        (princ " if ")
        (when cnd
	  (term$print-trim (rule$condition rul) old-stream))
        (when obj$verbose
	  (when (and cnd (rule$id_condition rul)) (princ " and "))
	  (when (rule$id_condition rul)
	    (print$id_condition (rule$id_condition rul))))
      )
      (princ " .")
    ) ; end let
  ) ; end prog
)

(defun print$bi_rhs (rhs)
  (let ((lam (function_lambda_expression rhs)))
  (if (atom lam)
      (if #-CMU (typep lam 'compiled-function) #+CMU (typep lam 'function)
	  (prin1 lam)
	(print$name lam))
  (if (eq 'lambda (car lam))
      (if (equal '(subst) (cadr lam))
	(print$name
	 (print$find_rhs
	  (function_lambda_expression (cadr (cadr (caddr lam))))))
      (if (equal '(compn) (cadr lam))
	  (progn
	    (print$bi_rhs (cadr (cadr (caddr lam))))
	    (princ " where")
	    (let ((flag nil))
	    (dolist (i (cadr (cadr (caddr (caddr lam)))))
	      (princ " ")
	      (if flag (princ ", ") (setq flag t))
	      (term$print (car i))
	      (princ " = ")
	      (term$print (cdr i))))
	    )
	(print$name lam)))
    (print$name lam)))))

(defun print$find_rhs (x)
  #-CMU (caddr x)
  #+CMU (if (eq 'module (caddr x)) ; dummy use of "module"
	  (cadddr x)
	  (caddr x))
  )

(defun print$merged (mod)
  (and *mod_eval$open_module*
       (eq mod *mod_eval$open_module*)
       (equal "%" (module$name mod))
       *mod_eval$last_before_open*))

(defun module$print (mod &optional (*print$indent* *print$indent*))
  (let ((omit *print$ignore_mods*)
	(obj$current_module (if obj$current_module obj$current_module
			      mod))
	(merged (print$merged mod)))
  (print$indent)
  (if (eq 'object (module$kind mod)) (princ "obj ") (princ "theory "))
  (print$mod_name mod)
  (when (module$parameters mod)
    (princ "[")
    (let ((flag nil))
    (dolist (x (module$parameters mod))
      (if flag (princ ", ") (setq flag t))
      (let ((nm (module$name (cdr x))))
	(princ (car nm))
	(princ " :: ")
	(print$name (caddr nm))
	)
      ))
    (princ "]"))
  (princ " is")
  (when merged (princ " *** open"))
  (let ((*print$indent* (1+ *print$indent*)))
  (when (module$sub_modules mod)
  (let ((skip (if merged
		(cons (cons 'implicit *mod_eval$last_before_open*)
		      (module$parameters mod))
		(module$parameters mod))))
  (dolist (sb (reverse (module$sub_modules mod)))
    (when (not (rassoc (car sb) skip))
      (print$next)
      (princ (cdr sb)) (princ " ") (print$name (car sb))
      (princ " .")))))
  (when (module$sorts mod)
    (if obj$verbose
      (let ((modprs (module$principal_sort mod)))
	(when modprs
	  (print$next)
	  (print$sort_brief modprs))
	(dolist (s (reverse (module$sorts mod)))
	  (unless (or (eq s *obj$sort_Universal*) (eq s modprs))
	    (print$next)
	    (print$sort_brief s))))
      (let ((modprs (module$principal_sort mod))
	    (srts (reverse (module$sorts mod))))
	(print$next)
	(let ((cnt 0))
	  (dolist (s srts)
	    (unless (eq s *obj$sort_Universal*) (incf cnt)))
	  (when (< 0 cnt)
	    (if (< 1 cnt)
	      (princ "sorts ")
	      (princ "sort "))
	    (let ((*print$indent* (1+ *print$indent*)))
	      (when modprs (print$sort_name mod modprs))
	      (dolist (s srts)
		(unless (or (eq s *obj$sort_Universal*) (eq s modprs))
		  (print$chk) ; needed?
		  (princ " ")
		  (print$sort_name mod s)))
	      (princ " ."))
	    ))
	(dolist (srel (reverse (module$sort_relation mod)))
	  (print$next)	
	  (princ "subsort ")
	  (print$sort_name mod (car srel))
	  (princ " < ")
	  (print$sort_name mod (cdr srel))
	  (princ " ."))
	)))
  (when (module$operators mod)
  (dolist (op (reverse (module$operators mod)))
    (unless (and (not obj$verbose)
		 (let ((opmod (operator$module op)))
		   (and (not (eq mod opmod))
			(not (eq merged opmod))
			(member opmod omit))))
      (print$next)
      (print$op_brief op) (princ " ."))))
  (when (module$sort_constraints mod)
    (print$next)
    (princ "sort constraints: ")
    (print$nested_list_as 'sort_constraint (module$sort_constraints mod)))
  (when (module$variables mod)
    (dolist (v (reverse (module$variables mod)))
      (print$next)
      (princ "var ")
      (print$safe (variable$name v))
      (princ " : ")
      (print$sort_name mod (variable$initial_sort v))
      (princ " .")))
  (if (module$is_compiled mod)
  (dolist (op (reverse (module$operators mod)))
    (unless (and (not obj$verbose)
		 (let ((opmod (operator$module op)))
		   (and (not (eq mod opmod))
			(not (eq merged opmod))
			(member opmod omit))))
      (dolist (r (print$all_rules mod op))
      (when (or *print$all_eqns*
		obj$verbose
		(or (eq 'bad_rule (rule$kind r))
		    (null (rule$kind r))))
      (print$next)
      (print$rule_brief r) (princ " .")
      (when *print$all_eqns*
	(dolist (er (rule$A_extensions r))
	  (when er (print$next) (print$rule_brief er) (princ " .")))
	(dolist (er (rule$AC_extension r))
	  (when er (print$next) (print$rule_brief er) (princ " .")))
      )))))
  (dolist (r (reverse (module$equations mod)))
      (print$next)
      (print$rule_brief r) (princ " .")))
  ) ; let indent
  (terpri) (print$indent)
  (if (eq 'object (module$kind mod)) (princ "endo") (princ "endth"))
  )
  (values)
  )

; lhh -- generate a module output suitable for the TRIM compiler
(defun module$print-trim (mod old-stream)
  (let ((obj$current_module mod)
	(curr-stream *standard-output*)
	(merged (print$merged mod)))

    (setq *print$trim-comments-contin* t) 
    (mod_eval$!setup_reduction mod)

    (when (module$parameters mod)
      (setq *standard-output* old-stream)
      (princ "Error: uninstantiated parameterized module.") (terpri)
      (recover-trim-error)
    )

    ; create a module header.
    (princ "--- Module: ") (princ (convert-to-string mod)) (terpri)

    ; list the sub modules -- what about parameters?
    (when (module$sub_modules mod)
      (let ((skip (if merged
		(cons (cons 'implicit *mod_eval$last_before_open*)
		      (module$parameters mod))
		(module$parameters mod))))
        (dolist (sb (reverse (module$sub_modules mod)))
          (when (not (rassoc (car sb) skip))
            (princ "--- ") (princ (cdr sb)) (princ " ") (print$name (car sb))
            (terpri)))))

    ; list the sorts
    (when (module$sorts mod)
;      (let ((modprs (module$principal_sort mod)))
;	(when modprs
;	  (print$sort_brief modprs))
;	(dolist (s (reverse (module$sorts mod)))
;	  (unless (or (eq s *obj$sort_Universal*) (eq s modprs))
;	    (print$next)
;	    (print$sort_brief s))))
      (let ((modprs (module$principal_sort mod))
	    (srts (reverse (module$sorts mod))))
        ; principle sort
        (princ "--- psort ") (princ (sort$name modprs)) (terpri)
        ; list all sorts
	(let ((cnt 0))
	  (dolist (s srts)
	    (unless (eq s *obj$sort_Universal*) (incf cnt)))
	  (when (< 0 cnt)
	    (if (< 1 cnt)
	      (princ "--- sorts ")
	      (princ "--- sort "))
	      (dolist (s srts)
		(unless (eq s *obj$sort_Universal*)
                  (princ (sort$name s)) (princ " ")))
              (terpri)
	    ))
        ; list the sort relations
;	(when (module$sort_relation mod)
;	  (setq *standard-output* old-stream)
;	  (princ "Warning: subsorts not supported.") (terpri)
;	  (setq *standard-output* curr-stream)
;	)
        (setq *print$trim-comments-contin* nil) 

	(dolist (srel (reverse (module$sort_relation mod)))
	  (princ "subsorts ")
	  (princ (sort$name (car srel)))
	  (princ " < ")
	  (princ (sort$name (cdr srel)))
	  (terpri))
	))

    ; list the operators
    (when (module$operators mod)
      (setq *standard-output* old-stream)
      (princ "Warning: operator attributes ignored.") (terpri)
      (setq *standard-output* curr-stream)
      (dolist (op (reverse (module$operators mod)))
        (unless (eq merged (operator$module op))
          (print$op_brief-trim op) (terpri))))

    ; handle sort constraints
    (when (module$sort_constraints mod)
      (setq *standard-output* old-stream)
      (terpri)
      (princ "Error: sort constraints are not supported.") (terpri)
      (recover-trim-error)
;      (princ "--- sort constraints: ")
;      (print$nested_list_as 'sort_constraint (module$sort_constraints mod))
    )

    ; list variables
; *NOTE* if we list variables we should list all variables which are used
;	 in the rules -- this code only lists the variables local to the
;	 last module
;    (when (module$variables mod)
;      (dolist (v (reverse (module$variables mod)))
;        (princ "--- var ")
;        (print$safe (variable$name v))
;        (princ " : ")
;        (print$sort_name mod (variable$initial_sort v))
;        (terpri)))


    ; open a TRIM module
    (princ "{") (terpri) (terpri)

    ; write out the equations
    (if (module$is_compiled mod)
      ; then
      (dolist (op (reverse (module$operators mod)))
        (unless (eq merged (operator$module op))
          (dolist (r (print$all_rules mod op))

            (setq *print$trim-comments-contin* t) 
            (princ "--- ") (print$rule_brief r) (terpri)
            (setq *print$trim-comments-contin* nil) 

            (rule$print-trim r old-stream) (terpri) (terpri)

	    (dolist (er (rule$A_extensions r))
              (when er 
                (setq *print$trim-comments-contin* t) 
                (princ "--- ") (print$rule_brief er) (terpri)
                (setq *print$trim-comments-contin* nil) 
              )
	      (when er (rule$print-trim er old-stream) (terpri) (terpri)))

	    (dolist (er (rule$AC_extension r))
              (when er 
                (setq *print$trim-comments-contin* t) 
                (princ "--- ") (print$rule_brief er) (terpri)
                (setq *print$trim-comments-contin* nil) 
              )

	      (when er (rule$print-trim er old-stream) (terpri) (terpri)))
          )
        )
      )
      ; else not compiled
      (dolist (r (reverse (module$equations mod)))
        (setq *print$trim-comments-contin* t) 
        (princ "--- ") (print$rule_brief r) (terpri)
        (setq *print$trim-comments-contin* nil) 
        (rule$print-trim r old-stream) (terpri) (terpri)
      )
    )  

    ; close the TRIM module
    (princ "}") (terpri)
  )
  (values)
)

(defun print$module_sorts (mod)
  (when (module$sorts mod)
  (let ((obj$current_module mod)
	(modprs (module$principal_sort mod)))
  (when modprs
    (print$sort_brief modprs))
  (dolist (s (reverse (module$sorts mod)))
    (unless (or (eq s *obj$sort_Universal*) (eq s modprs))
      (print$next)
      (print$sort_brief s)))
  (terpri))))

(defun print$module_ops (mod)
  (let ((obj$current_module mod))
  (when (module$operators mod)
  (let ((obj$current_module mod))
  (dolist (op (reverse (module$operators mod)))
    (print$op_brief op) (princ " .") (terpri))))
  (when (module$sort_constraints mod)
    (princ "sort constraints: ")
    (dolist (sc (module$sort_constraints mod))
      (print$sort_constraint sc) (terpri))
    )))

(defun print$module_eqs (mod)
  (let ((obj$current_module mod)))
  (if (module$is_compiled mod)
  (dolist (op (reverse (module$operators mod)))
    (dolist (r (print$all_rules mod op))
      (print$rule_brief r) (princ " .") (terpri)))
  (dolist (r (reverse (module$equations mod)))
      (print$rule_brief r) (princ " .") (terpri))))

(defun print$module_submodules (mod)
  (let ((skip (if (and *mod_eval$open_module*
		       (eq mod *mod_eval$open_module*)
		       *mod_eval$last_before_open*
		       (equal "%" (module$name mod)))
		(cons (cons 'implicit *mod_eval$last_before_open*)
		      (module$parameters mod))
		(module$parameters mod))))
  (dolist (sb (reverse (module$sub_modules mod)))
    (when (not (rassoc (car sb) skip))
      (princ (cdr sb)) (princ " ") (print$name (car sb))
      (princ " .") (terpri)))))

(defun print$module_parameters (mod)
  (when (module$parameters mod)
    (let ((flag nil))
    (dolist (x (module$parameters mod))
      (if flag (princ ", ") (setq flag t))
      (print$name (cdr x))))
    (terpri)
    ))

(defun print$module_sort (mod srt)
  (print$sort_brief_mod mod srt)
  (when (and obj$verbose (sort$info srt))
    (terpri) (princ "  built-in: ") (prin1 (sort$info srt)))
  )

(defun print$qual_op_name (qop)
  (if (and (consp qop) (eq 'qual (car qop)))
      (progn
	(print$simple_princ (cadr qop))
	(princ ".")
	(print$qual_sort_name (caddr qop)))
  (if (consp qop)
      (let ((flag nil))
      (dolist (x qop)
	(if flag (princ " ") (setq flag t))
	(if (consp x) (print$qual_sort_name x)
	  (princ x))
      ))
    (print$simple_princ_open qop)
    )))

(defun print$qual_sort_name (qs)
  (if (and (consp qs) (eq 'qual (car qs)))
      (let ((nm (cadr qs)) (mod (caddr qs)))
	(if (and (consp nm) (null (cdr nm)))
	    (print$simple_princ (car nm))
	  (print$simple_princ nm))
	(princ ".")
	(if (typep mod 'module)
	    (print$mod_name mod)
	  (print$modexp mod)
	  ))
    (print$simple_princ_open qs)
    ))

(defun print$op_parse (op)
  (let ((fm (operator$form op))
	(prec (operator$precedence op))
	(std (operator$is_standard op)))
    (if std
	(princ "standard")
      (progn
	(princ "precedence: ") (prin1 prec)
	(princ "  form:")
	(dolist (i fm)
	  (princ " ")
	  (if (eq 'token (car i))
	      (princ (cdr i))
	    (progn
	      (princ "(")
	      (print$sort_name obj$current_module (cddr i))
	      (princ "/")
	      (prin1 (cadr i))
	      (princ ")")
	      )
	    )
	)))
  ))

(defun print$module_variables (mod)
  (when (module$variables mod)
    (dolist (v (reverse (module$variables mod)))
      (princ "var ")
      (print$safe (variable$name v))
      (princ " : ")
      (print$sort_name mod (variable$initial_sort v))
      (princ " .")
      (print$next))
  ))

(defun print$rule_labels (rul)
  (princ "[")
  (let ((flg nil))
  (dolist (x (rule$labels rul))
    (if flg (princ ",") (setq flg t))
    (print$simple_princ_open x))
  (princ "]")
  ))

(defun print$modexp (me)
  (print$chk)
  (cond
   ((atom me)
    (cond
     ((stringp me) (princ me))
     ((typep me 'module) (print$mod_name me))
     (t (print$brief me))
    ))
   ((eq 'name (car me))
    (print$modexp (cadr me))
    (princ "<") (prin1 (caddr me)) (princ ">"))
   ((eq '+ (car me)) ;(+ me me)
    (print$modexp (cadr me)) (princ " + ") (print$modexp (caddr me)))
   ((eq '* (car me)) ;(* me rename)
    (print$modexp (cadr me)) (princ " * (")
    (print$rename (caddr me)) (princ ")"))
   ((eq '*view (car me)) ;(*view view rename) ;12 Jan 88
    (print$modexp (cadr me)) (princ " *-view (")
    (print$rename (caddr me)))
   ((eq '|:| (car me)) ;(: me (vw ...))
    (if (or (stringp (cadr me))
	    (and (typep (cadr me) 'module) (stringp (module$name (cadr me)))))
	  (print$modexp (cadr me))
      (progn
	(princ "(")
	(print$modexp (cadr me))
	(princ ")")
	))
    (princ "[")
    (let ((flg nil))
    (dolist (vw (caddr me))
      (if flg (princ ",") (setq flg t))
      (print$view vw)
    ))
    (princ "]"))
   ((eq 'view (car me)) ;(view me-th me-mod pre-vw-mapping)
    (print$chk)
    (princ "view")
    ;(princ " from ") (print$modexp (cadr me))
    (print$chk)
    (princ " to ") (print$modexp (caddr me))
    (print$chk)
    (princ " is")
    (print$view_mapping (cadddr me))
    (print$chk)
    (princ " endv"))
   ((eq 'prim_view (car me)) ;(prim-view x y)
    (princ "print$modexp: prim_view: ") (print$name me))
   ((eq 'under (car me)) ;(under me view)
    (print$modexp (cadr me)) (princ " :: ")
    (print$view (caddr me)))
   ((eq 'view_from (car me)) ;(view-from me-th-can me-mod-can vw-mapping)
    (print$chk)
    (princ "view")
    ;(princ " from ") (print$modexp (cadr me))
    (print$chk)
    (princ " to ") (print$modexp (caddr me))
    (print$chk)
    (princ " is")
    (print$view_mapping (cadddr me))
    (print$chk)
    (princ " endv"))
   ((eq 'view_mapping (car me))
      ;(view-mapping me-th-can me-mod-can vw-mapping mapping)
    (princ "view")
    ;(princ " from ") (print$modexp (cadr me))
    (princ " to ") (print$modexp (caddr me))
    (print$chk)
    (princ " is")
    (print$view_mapping (cadddr me))
    (princ "--- mapping: ")
    (print$mapping (nth 4 me))
    (print$chk)
    (princ " endview"))
   ((and (consp me)
	 (null (cdr me))
	 (stringp (car me)))
    (print$modexp (car me)))
   (t (princ "#") (print$name me))
  ))

(defun print$rename (rn)
  (print$chk)
  (cond
   ((null rn) (princ " EMPTY "))
   ((atom rn) (princ rn))
   (t
    (let ((flg nil))
    (dolist (re rn)
      (if flg (princ ", ") (setq flg t))
      (if (eq 'sort (car re))
	  (progn
	    (print$chk)
	    (princ "sort ")
	    (print$chk)
	    (print$qual_sort_name (cadr re))
	    (print$chk)
	    (princ " to ")
	    (print$qual_sort_name (caddr re)))
	  (progn
	    (print$chk)
	    (princ (car re)) (princ " ")
	    (print$qual_op_name (cadr re))
	    (print$chk)
	    (princ " to ")
	    (print$qual_op_name (caddr re))))
    ))
    )))

(defun print$view (vw)
  (cond
   ((atom vw) (princ vw))
   ((member (car vw) '(view view-from prim_view view_from view_mapping))
    (print$modexp vw)
    )
   ((stringp (car vw)) (princ vw))
   (t (print$modexp vw))
  ))

(defun print$view_mapping (vwmap)
  (cond
   ((or (stringp (car vwmap))
	(and (consp (car vwmap))
	     (or (stringp (caar vwmap))
		 (and (consp (caar vwmap))
		      (stringp (caaar vwmap))))))
    (princ vwmap))
   (t
    (dolist (m vwmap)
      (print$chk)
      (princ " ")
      (princ (car m)) (princ " ")
      (if (eq 'sort (car m))
	(progn
	  (print$chk)
	  (print$name (cadr m)) ;print$qual_sort_name is here
	  (print$chk)
	  (princ " to ")
	  (print$name (caddr m)))
	(progn
	  (print$chk)
	  (print$term (cadr m))
	  (print$chk)
	  (princ " to ")
	  (print$term (caddr m))))
      (princ " .")
    ))
   ))

(defun print$all_rules (module operator)
  (let ((res (module$rules_with_different_top module operator))
	(ring (module$rules_with_same_top module operator)))
    (do ((rule (rule_ring$initialize ring) (rule_ring$next ring)))
	((rule_ring$end_test ring))
	(push rule res))
    (when (eq 'theory (module$kind module))
    (dolist (r (module$rules module))
      (when (and (eq 'bad_rule (rule$kind r))
		 (not (term$is_var (rule$lhs r)))
		 (operator$is_same_operator operator (term$head (rule$lhs r))))
	(setq res (append res (list r))))))
    res
    ))
