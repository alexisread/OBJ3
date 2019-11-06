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

;; $Id: misc.lsp,v 206.1 2003/09/29 12:46:22 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                    Miscellaneous
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Tim Winkler ;;;; Created: 7/8/86

;;; miscellaneous top-level command-loop functions

;;;;;;;;;;;;;;;; Summary ;;;;;;;;;;;;;;;;
; omitted

; lhh: specify where the TRIM images live
;;(defvar *trim-dir*)
;;(setq *trim-dir* "/home/faculty/hamel/bin/")

(defvar *obj$try* nil) ; for experimental features
(defvar reduce-conditions)
(defvar obj$verbose nil)
(defvar module_all_rules_every)

(defvar $$complain_theory nil)

;;;; lhh -- top level compile command for TRIM
(defun misc$compile (inp)
  (let ((cmd-dat (cadr inp)) 
        (cmd-keyword-list (list "verbose" "noopt" "keep"))
	(old-stream *standard-output*)
        (mod-name-string "")
        (eq-file-name "")
        (temp-file-name *obj-unique-filename*)
        (compile-verbose nil)
        (compile-keep nil)
        (compile-opt t)
;;        (trimcc-cmd-line (concatenate 'string *trim-dir* "trimcc "))
        (trimcc-cmd-line "trimcc ")
       )

    (when (not (equal (car inp) "compile"))
      (princ "Error: unknown command in `compile' code.")
      (terpri)
      (obj3-to-top)
    )

    (dolist (k cmd-dat) ; -- process keywords in the command line
      (when (equal k "verbose")
        (setq compile-verbose t)
        (setq cmd-dat (cdr cmd-dat))
      )
      (when (equal k "keep")
        (setq compile-keep t)
        (setq cmd-dat (cdr cmd-dat))
      )
      (when (equal k "noopt")
        (setq compile-opt nil)
        (setq cmd-dat (cdr cmd-dat))
      )
      (when (not (member k cmd-keyword-list :test 'equal))
        (return)
      )
    )

    ; assemble command lines and spawn processes...
    (let ((mod (eval_mod_ext cmd-dat))) ; see if we have a valid module here.

      (setq mod-name-string (convert-to-string mod))

      (if compile-keep
	  (setq eq-file-name (concatenate 'string mod-name-string ".eq"))
	  (setq eq-file-name (concatenate 'string temp-file-name ".eq"))
      )

      (setq *standard-output* (open eq-file-name :direction :output))
      (module$print-trim mod old-stream)
      (close *standard-output*)
      (setq *standard-output* old-stream)

      (when compile-verbose
        (setq trimcc-cmd-line (concatenate 'string trimcc-cmd-line "-v "))
      )

      (when compile-opt
        (setq trimcc-cmd-line (concatenate 'string trimcc-cmd-line "-O "))
      )

      ; command line for the TRIM compiler
      (setq trimcc-cmd-line (concatenate 'string trimcc-cmd-line " -o "))
      (setq trimcc-cmd-line 
        (concatenate 'string trimcc-cmd-line mod-name-string))
      (setq trimcc-cmd-line (concatenate 'string trimcc-cmd-line " -n "))
      (setq trimcc-cmd-line 
        (concatenate 'string trimcc-cmd-line mod-name-string))
      (setq trimcc-cmd-line (concatenate 'string trimcc-cmd-line " "))
      (setq trimcc-cmd-line (concatenate 'string trimcc-cmd-line eq-file-name))

      (when compile-verbose
        (princ trimcc-cmd-line) (terpri)
      )

      ; start the process
      (if (not (equal
                #+GCL (system trimcc-cmd-line)
                #+(or CMU CLISP) (ext:run-program trimcc-cmd-line nil)
                0))
        (obj3-to-top)
      )

      (if (not compile-keep) (delete-file eq-file-name))
    )
  )
)

;;;; called from top.lsp command interpreter
; "show" "sh" "mod" "set" "do" "please"
; what is "mod" ?
(defun top$commands (inp)
  (let ((tag (car inp)) (dat (cadr inp)))
  (cond 
   ((or (equal "show" tag) (equal "sh" tag) (equal "select" tag))
     (let ((it (if (equal "select" tag) tag (car dat)))) (cond
       ((equal "mod" it) (print_mod (cdr dat)))
       ((equal "sorts" it) (print_sorts (cdr dat)))
       ((equal "sort" it) (print_sort (cdr dat)))
       ((or (equal "principal-sort" it)
	    (equal "psort" it))
	(print_principal_sort (cdr dat)))
       ((equal "ops" it) (print_ops (cdr dat)))
       ((equal "op" it) (print_op (cdr dat)))
       ((equal "select" it)
	  (eval_mod_ext
	   (if (equal tag "select") dat (cdr dat))))
       ((equal "sign" it) (print_sign (cdr dat)))
       ((equal "vars" it) (print_vars (cdr dat)))
       ((equal "eqs" it) (print_eqs (cdr dat)))
       ((equal "subs" it) (print_subs (cdr dat)))
       ((equal "name" it) (print_name (cdr dat)))
       ((equal "abbrev" it) (print_abbrev_name (cdr dat)))
       ((equal '("term") dat)
	(if (and $$term (not (eq 'void $$term)))
	  (let ((obj$current_module *mod_eval$$last_module*)
		(*mod_eval$$current_module* *mod_eval$$last_module*))
	    (print$sort_name obj$current_module (term$sort $$term))
	    (princ ": ")
	    (print$term $$term)
	    (terpri))
	  (progn
	    (princ "No current term") (terpri))))
       ((and (equal "sub" it)
	     (cadr dat)
	     (parse-integer (cadr dat) :junk-allowed t))
	(let ((no (read-from-string (cadr dat))))
	  (show_sub (cddr dat) (1- no))
	  ))
       ((equal "params" it) (print_params (cdr dat)))
       ((and (equal "param" it)
	     (cadr dat)
	     (parse-integer (cadr dat) :junk-allowed t))
	(let ((no (read-from-string (cadr dat))))
	  (show_param (cddr dat) (1- no))
	  ))
       ((equal "time" it)
	(let ((val (timer)))
	  (format t "~8,3f cpu    ~8,3f real" (car val) (cadr val))
	(terpri)))
       ((equal "rules" it) (print_rules (cdr dat)))
       ((and (equal "all" it) (equal "rules" (cadr dat)))
	(let ((obj$verbose t)
	      (module_all_rules_every t))
	   (print_rules (cddr dat))))
       ((equal "modules" it) (print_modules (cdr dat)))
       ((equal "rule" it) (apply_print_rule (cadr dat)))
       ((and (equal "all" it) (equal "rule" (cadr dat)))
	(let ((obj$verbose t))
	   (apply_print_rule (caddr dat))))
       ((equal '("pending") dat) (print_pending))
       ((equal '("modes") dat) (show_modes nil))
       ((equal '("all" "modes") dat) (show_modes t))
       ((equal "?" it)
	(princ "  show [sorts|psort|ops|vars|eqs|params|subs|name]")
	(princ "[<module-expression>] .") (terpri)
	(princ "  show [sign|all|mod] [<module-expression>] .") (terpri)
	(princ "  show select <module-expression> .") (terpri)
	(princ "  show sort <sort-reference> .") (terpri)
	(princ "  show op <operator-reference> .") (terpri)
	(princ "  show [param|sub] <number> [<module-expression>] .") (terpri)
	(princ "  show [all] rules [<module_expression>].") (terpri)
	(princ "  show abbrev [<module-expression>] .") (terpri)
	(princ "  show [all] rule <RuleSpec> .") (terpri)
	(princ "  show modules .") (terpri)
	(princ "  show [all] modes .") (terpri)
	(princ "  show time .") (terpri)
	(princ "  show term .") (terpri)
	(princ "  show [pending] .") (terpri)
	(princ "  show can be abbreviated to sh") (terpri)
        (princ "  can precede <module-expression> by ")
	(princ "sequence of `sub k' and `param k'") (terpri))
       ((equal "all" it)
	(let ((obj$verbose t))
	  (print_mod (cdr dat))))
       (t (print_mod dat))
       )))
   ((equal "set" tag)
    (let* ((parity (car (last dat)))
	   (which
	    (if (or (equal "on" parity) (equal "off" parity))
	      (butlast dat)
	      dat)))
    (catch 'parity-error
    (cond
       ((equal '("trace") which)
	(if (check_parity parity) (trace-on) (trace-off)))
       ((equal '("blips") which)
	(if (check_parity parity) (blips-on) (blips-off)))
       ((equal '("gc" "show") which)
	(if (check_parity parity)  (gc-trace-on) (gc-trace-off)))
       ((equal '("clear" "memo") which)
	(if (check_parity parity) (memo-clean-on) (memo-clean-off)))
       ((equal '("obj2") which) (setq *obj$obj2_mode* (check_parity parity)))
       ((equal '("show" "retracts") which)
	(setq *show-retracts* (check_parity parity)))
       ((equal '("display" "retracts") which) ;for back compat
	(setq *show-retracts* (check_parity parity)))
       ((equal '("print" "with" "parens") which)
	(setq *fancy-print* (not (check_parity parity))))
       ((equal '("include" "BOOL") which)
	(setq *obj$include_BOOL* (check_parity parity)))
       ((equal '("stats") which)
	(setq *obj$show_stats* (check_parity parity)))
       ((equal '("trace" "whole") which)
	(if (check_parity parity) (trace-whole-on) (trace-whole-off)))
       ((equal '("try") which)
	(setq *obj$try* (check_parity parity)))
       ((equal '("all" "eqns") which)
	(setq *print$all_eqns* (check_parity parity)))
       ((equal '("abbrev" "quals") which)
	(setq *abbrev_quals* (check_parity parity)))
       ((equal '("show" "var" "sorts") which)
	(setq *show_var_sorts* (check_parity parity)))
       ((equal '("old") which)
	;(setq module_all_rules_new nil)
	;(setq *mod_eval$default_prec* 1 *mod_eval$default_unary_prec* 1)
	)
       ((equal '("reduce" "conditions") which)
	(setq reduce-conditions (check_parity parity)))
       ((equal '("all" "rules") which)
	(setq module_all_rules_every (check_parity parity)))
       ((equal '("verbose") which)
	(setq obj$verbose (check_parity parity)))
       ((equal '("?") which)
	(princ "  set [trace|blips|gc show] [on|off] .") (terpri)
	(princ "  set [print with parens|show retracts|abbrev quals]")
	(princ " [on|off] .")
	(terpri)
	(princ "  set [include BOOL|clear memo|stats|trace whole]")
        (princ " [on|off] .") (terpri)
	(princ "  set [all eqns|all rules|show var sorts|reduce conditions]")
	(princ " [on|off] .") (terpri)
        (princ "  set [verbose] [on|off] .") (terpri))
       (t (princ "Not recognized") (terpri))
       ))))
   ((equal "do" tag) (cond
       ((equal '("gc") dat)
	#+GCL (gbc t)
	#+(or LUCID CMU CLISP) (gc)
	)
       ((equal '("clear" "memo") dat) (memo-clean))
       ((equal "save" (car dat)) (obj3_save_status (cdr dat)))
       ((equal "restore" (car dat)) (obj3_restore_status (cdr dat)))
       ((equal '("?") dat)
	(princ "  ") (princ tag) (princ " [gc|clear memo] .") (terpri)
	(princ "  ") (princ tag) (princ " [save|restore] <Name> .") (terpri)
	)
       (t (princ "Not recognized") (terpri))
       ))
  )))

(defun check_parity (x)
  (if (equal "on" x) t
  (if (equal "off" x) nil
    (progn
      (princ "Specify on or off.") (terpri)
      (throw 'parity-error nil)))))

(defun eval_mod (toks)
  (if (null toks)
      (if *mod_eval$$last_module* *mod_eval$$last_module*
	(progn
	  (princ "No last module") (terpri)
	  (obj3-to-top)))
  (if (equal '("open") toks)
      (if *mod_eval$open_module*
	(progn
	  (setq *mod_eval$$last_module* *mod_eval$open_module*)
	  *mod_eval$open_module*)
	(progn
	  (princ "No module is open") (terpri)
	  (obj3-to-top)))
    (let ((val (modexp_eval$top_level_eval toks)))
      (if (modexp_eval$is_error val)
	  (if (and (null (cdr toks))
		   (<= 4 (length (car toks)))
		   (equal "MOD" (subseq (car toks) 0 3)))
	      (let ((val (read-from-string (subseq (car toks) 3))))
		(if (integerp val)
		    (setq *mod_eval$$last_module* (print$nth_mod val))
		  (progn
		    (princ "Undefined module?") (terpri)
		    (obj3-to-top))))
	    (progn
	      (princ "Undefined module?") (terpri)
	      (obj3-to-top)))
	(progn
	  (setq *mod_eval$$last_module* val)
	  val))))))

(defun eval_mod_ext (toks)
  (let ((it (car toks)))
  (cond
   ((and (equal "sub" it)
	 (cadr toks)
	 (parse-integer (cadr toks) :junk-allowed t))
    (let* ((no (read-from-string (cadr toks)))
	   (mod (eval_mod_ext (cddr toks)))
	   (sub (nth_sub (1- no) mod)))
      (if sub
	  (setq *mod_eval$$last_module* sub)
	(progn (princ "No such sub-module") (terpri)))))
   ((and (equal "param" it)
	 (cadr toks)
	 (parse-integer (cadr toks) :junk-allowed t))
    (let* ((no (read-from-string (cadr toks)))
	   (mod (eval_mod_ext (cddr toks)))
	   (params (module$parameters mod))
	   (param (nth (1- no) params)))
      (if param
	  (setq *mod_eval$$last_module* (cdr param))
	(progn (princ "No such parameter") (terpri)))))
   (t (eval_mod toks))
   )))

(defun print_mod (toks)
  (let ((mod (eval_mod_ext toks)))
  (let ((obj$current_module mod))
  (mod_eval$!setup_reduction mod)
  (module$print mod)
  (terpri))))
(defun print_sorts (toks) (print$module_sorts (eval_mod_ext toks)))
(defun print_ops (toks) (print$module_ops (eval_mod_ext toks)))
(defun print_sign (toks)
  (let ((mod (eval_mod_ext toks)))
    (print$module_sorts mod)
    (print$module_ops mod)))
(defun print_vars (toks) (print$module_variables (eval_mod_ext toks)))
(defun print_eqs (toks)
  (let ((mod (eval_mod_ext toks)))
  (mod_eval$!setup_reduction mod)
  (print$module_eqs mod)))
(defun print_rules (toks)
  (let ((mod (eval_mod_ext toks)) (i 1))
  (let ((obj$current_module mod))
  (mod_eval$!setup_reduction mod)
  (dolist (r (module_all_rules mod)) ; from apply, was module$rules
    (prin1 i) (princ ": ") (print$rule_brief r) (terpri)
    (setq i (1+ i))
  )
  )))
(defun print_subs (toks) (print$module_submodules (eval_mod_ext toks)))
(defun print_params (toks) (print$module_parameters (eval_mod_ext toks)))
(defun print_name (toks)
  (let ((mod (eval_mod_ext toks)))
  (print$name mod)
  (when (print$merged mod) (princ " *** open"))
  (terpri)))
(defun print_abbrev_name (toks)
  (let ((mod (eval_mod_ext toks)))
  (let ((num (print$mod_num mod)))
    (if (= 0 num)
      (princ  "(...)")
      (progn (princ "MOD") (prin1 num))
    )
    (princ " is ")
    (let ((*print$abbrev_mod* nil))
      (print$mod_name mod))
    (terpri))))
(defun print_principal_sort (toks)
  (let ((mod (eval_mod_ext toks)))
  (let ((obj$current_module mod))
    (print$sort_brief_mod mod (module$principal_sort mod))
    (terpri)
  )))

(defun print_sort (toks)
  (let ((mod (if *mod_eval$$last_module* *mod_eval$$last_module*
	       (if obj$current_module obj$current_module
	       *obj_BOOL$module*))))
  (let ((obj$current_module mod))
  (let ((parsedsrt (modexp_parse$parse_sort_specn toks)))
  (let ((srt (mod_eval$$find_qual_sort_in mod parsedsrt)))
    (if srt
	(let* ((mod
		(if (member srt (module$sorts mod)) mod
		  (sort$module srt)))
	       (obj$current_module mod))
	  (setq *mod_eval$$last_module* mod)
	  (print$module_sort mod srt) (terpri))
      (progn (princ "No such sort") (terpri))
    ))))))

(defun print_op (toks)
  (let ((mod (if *mod_eval$$last_module* *mod_eval$$last_module*
	       (if obj$current_module obj$current_module
	       nil))))
  (unless mod
    (princ "No last module") (terpri)
    (obj3-to-top))
  (mod_eval$!setup_reduction mod)
  (let ((obj$current_module mod))
  (let ((parsedop (modexp_parse$parse_op_name_specn toks)))
  (let ((ops (mod_eval$find_all_qual_operators_named_in mod parsedop)))
    (if ops
	(dolist (op ops)
	  (let* ((mod
		  (if (member op (module$operators mod)) mod
		    (operator$module op)))
		 (obj$current_module mod))
	    (setq *mod_eval$$last_module* mod)
	    (print$op_brief op) (terpri)
	    (princ "        strategy: ")
	        (prin1 (module$operator_rew_strategy mod op))
		(terpri)
	    (princ "        ") (print$op_parse op) (terpri)
	    (dolist (r (module$all_rules mod op))
	      (print$rule_brief r) (terpri))
	    ))
      (progn (princ "No such operator") (terpri))
    ))))))

(defun show_sub (toks no)
  (let* ((mod (eval_mod_ext toks))
	 (sub (nth_sub no mod)))
  (if sub
    (progn
      (setq *mod_eval$$last_module* sub)
      (let ((obj$current_module sub))
      (module$print sub)
      (terpri)))
    (progn
      (princ "No such sub-module") (terpri)))
  ))

(defun nth_sub (no mod)
  (let ((lst nil)
	(params (module$parameters mod)))
  (dolist (i (module$sub_modules mod))
   (when (not (rassoc (car i) params)) (push (car i) lst)))
  (nth no lst)
  ))

(defun show_param (toks no)
  (let* ((mod (eval_mod_ext toks))
	 (params (module$parameters mod))
	 (param (nth no params)))
  (if param
    (progn
      (setq *mod_eval$$last_module* (cdr param))
      (let ((obj$current_module (cdr param)))
      (module$print (cdr param))
      (terpri)))
    (progn
      (princ "No such parameter") (terpri)))
  ))

(defun print_modules (x)
  (declare (ignore x))
  (let ((*print$indent_contin* nil)
	(lst *modexp_eval$env*) m)
  (loop
   (when (null lst) (return))
   (setq m (cdr (car lst)))
   (setq lst (cdr lst))
   (unless (rassoc m lst)
     (print$check)
     (when (< 0 (filecol *standard-output*))
       (princ "  "))
     (print$name m)
     (if obj$verbose (terpri) (print$check))
     )
  ))
  (fresh-line)
  )

(defun show_mode (flg val tag)
  (if flg
    (progn
      (print$check) (princ tag) (princ " is ")
      (princ (if val "on" "off")) (terpri))
    (progn
      (print$check)
      (when val (princ tag))
      )))

(defun show_modes (flg)
  (let ((*print$line_limit* 62))
  (show_mode flg $$trace_rewrite "  trace")
  (show_mode flg $$rew_blips "  blips")
  (show_mode flg *gc_tracing* "  gc show")
  (show_mode flg *obj$include_BOOL* "  include BOOL")
  (show_mode flg *show-retracts* "  show retracts")
  (show_mode flg *abbrev_quals* "  abbrev quals")
  (show_mode nil *obj$obj2_mode* "  obj2")
  (show_mode flg (not *fancy-print*) "  print with parens")
  (show_mode flg *obj$show_stats* "  stats")
  (show_mode flg rew$clean_memo_in_normalize "  clear memo")
  (show_mode flg $$trace_rewrite_whole "  trace whole")
  (show_mode flg *print$all_eqns* "  all eqns")
  (show_mode flg module_all_rules_every "  all rules")
  (show_mode flg *show_var_sorts* "  show var sorts")
  (show_mode flg reduce-conditions "  reduce conditions")
  (show_mode flg obj$verbose "  verbose")
  (fresh-line)
  ))

;;;; user helpers
(defvar $$trace_rewrite nil)
(defvar $$trace_rewrite_whole nil)

(defun trace-on ()
  (setq $$trace_rewrite t))

(defun trace-off ()
  (setq $$trace_rewrite nil))

(defun trace-whole-on ()
  (setq $$trace_rewrite_whole t))

(defun trace-whole-off ()
  (setq $$trace_rewrite_whole nil))

(defvar *real-time* 0)
(defvar *run-time* 0)

(defun timer ()
  (let ((real *real-time*) (sys *run-time*))
    (setq *real-time* (get-internal-real-time))
    (setq *run-time* (get-internal-run-time))
    (list (float (/ (- *run-time* sys) internal-time-units-per-second))
          (float (/ (- *real-time* real) internal-time-units-per-second)))))

(defmacro call-that (x)
 `(progn (setq ,x (term$copy_and_returns_list_variable $$norm)) 'done))

(defun termcopy (x) (term$copy_and_returns_list_variable x))

(defvar $$rew_blips nil)

(defun blips-on () (setq $$rew_blips t))
(defun blips-off () (setq $$rew_blips nil))

#+GCL
(defun more-room (&optional (x 0) (y 0))
  (allocate 'cons (+ 400 x))
  (allocate-relocatable-pages (+ 100 y))
  'ok)

(defvar *gc_tracing* nil)

(defun gc-trace-on ()
  (setq *gc_tracing* t)
  (gc-trace))

(defun gc-trace-off ()
  (setq *gc_tracing* nil)
  (gc-no-trace))

;for following; see also rew.lsp
(defvar rew$clean_memo_in_normalize nil)

(defun memo-clean-on ()
  (setq rew$clean_memo_in_normalize t))

(defun memo-clean-off ()
  (setq rew$clean_memo_in_normalize nil))

(defun memo-clean ()
  (setq *memo_rew$table* (memo_rew$clean_memo_table *memo_rew$table*)))

(defun show ()
  (when (and $$term (not (eq 'void $$term)))
  (print$term $$term))
  (values)
  )

(defvar obj3_status_env nil)

(defun obj3_save_status (name)
  (push (cons name 
	      (list *modexp_eval$canon_env* *modexp_eval$env*
		    *modexp_eval$view_env* *mod_eval$$last_module*))
	obj3_status_env)
  )

(defun obj3_restore_status (name)
  (let ((val (cdr (assoc name obj3_status_env :test #'equal))))
  (if (null val)
      (progn (princ "Unrecognized name") (terpri))
    (progn
      (setq *modexp_eval$canon_env* (nth 0 val))
      (setq *modexp_eval$env* (nth 1 val))
      (setq *modexp_eval$view_env* (nth 2 val))
      (setq *mod_eval$$last_module* (nth 3 val))
      (princ "Done") (terpri))
  )))

(defun get-time-string ()
  (multiple-value-bind
   (sec min hr day month year weekday daysav zone)
   (get-decoded-time)
   (declare (ignore daysav zone))
   (format nil "~d ~a ~d ~a ~d:~2,'0d:~2,'0d"
     year
     (case month (1 "Jan") (2 "Feb") (3 "Mar") (4 "Apr") (5 "May") (6 "Jun")
         (7 "Jul") (8 "Aug") (9 "Sep") (10 "Oct") (11 "Nov") (12 "Dec"))
     day
     (case weekday (0 "Mon") (1 "Tue") (2 "Wed") (3 "Thu") (4 "Fri")
         (5 "Sat") (6 "Sun"))
     hr min sec)
  ))
