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

;; $Id: apply.lsp,v 206.1 2003/09/26 13:03:50 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;        Apply feature
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; rules labels cannot contain . or start with a digit

(defvar applied nil)
(defvar action-stack nil)
(defvar reduce-conditions nil)
(defvar inside-apply-all nil)
(defvar inside-apply-with-extensions nil)
(defvar apply-ignore-mods nil)

;(setq $$complain_theory nil)

(defvar module_all_rules_seen nil)

(defvar module_all_rules_every nil)

(defun module_all_rules (mod)
  (if module_all_rules_every
    (let ((module_all_rules_seen nil))
      (module_all_rules_aux mod))
    (let ((res (module$rules mod))
	  (allsubs (module$all_sub_modules mod)))
      (dolist (m allsubs)
	(let ((sm (car m)))
	  (unless (or (member sm *print$ignore_mods*)
		      (eq 'using (cdr m)))
	    (setq res (append res (module$rules sm))))))
      res
      )
  ))

(defun module_all_rules_aux (mod)
  (unless (member mod module_all_rules_seen)
  (push mod module_all_rules_seen)
  (let ((res (module$rules mod))
	(allsubs (module$all_sub_modules mod)))
    (dolist (m allsubs)
      (let ((sm (car m)))
	(unless (member sm apply-ignore-mods)
	  (setq res (union res (module_all_rules_aux sm) :test #'eq)))))
    res
    ))
  )

(defun copy_term (term)
  (if (term$is_var term)
    (copy-list term)
  (if (term$is_built_in_constant term)
    (list (list (caar term) (term$built_in_value term)))
    (term$make_term
     (caar term)
     (mapcar #'copy_term (cdr term)))
    )
  ))

(defun rulenum (mod n)
  (mod_eval$!setup_reduction mod)
  (let ((res (nth (1- n) (module_all_rules mod)))) ; was module$rules
    (if (null res)
	(progn
	  (princ "Error: rule selected doesn't exist") (terpri)
	  (obj3-to-top))
      res)
    ))

(defun allruleslabelled (mod l)
  (mod_eval$!setup_reduction mod)
  (let ((res nil))
    (dolist (rul (module_all_rules mod))
      (when (member l (rule$labels rul) :test #'equal)
	(push rul res))
      )
    res
    ))

(defun rulelabelled (mod l)
  (let ((val (allruleslabelled mod l)))
    (if (null val)
      (progn
	(princ "No rule with label: ") (princ l) (terpri)
	(obj3-to-top))
    (if (and val (null (cdr val)))
      (car val)
      (progn
	(princ "no unique rule with label: ") (princ l) (terpri)
	(obj3-to-top)))
    )))

(defun rule$is_special (x)
  (consp x))

(defun matcher (pat trm)
  (if (term$is_var pat)
    (if
     (sort_order$is_included_in (module$sort_order obj$current_module)
	 (term$sort trm) (variable$initial_sort pat))
      (values nil (list (cons pat trm)) nil nil)
      (values nil nil t nil)
      )
    (match$first_match pat trm))
  )

(defun test-rule-special (rul trm)
  (if (eq 'retract (car rul)) ; is retract and can be simplified
    (and (not (term$is_var trm))
	 (let* ((head (term$head trm)))
	   (and (operator$is_a_retract head)
		(sort_order$is_included_in
		 (module$sort_order
		  (if obj$current_module obj$current_module
		    (sort$module (operator$coarity head))))
		 (term$sort (term$arg_1 trm))
		 (operator$coarity head))
		)))
  (if (eq '-retract (car rul)) ; same connect component
    (let* ((srt (term$sort trm))
	   (so (module$sort_order
		(if obj$current_module obj$current_module
		  (sort$module srt)))))
      (and
       (sort_order$is_in_same_connected_component so srt (cadr rul))
       (not (sort_order$is_included_in so srt (cadr rul)))))
  (break "SNARK: illegal special rule in apply")
  )))

(defun apply-one-special-rule (rul srt trm)
  (if (eq 'retract (car rul)) ; is retract and can be simplified
    (when
     (and (not (term$is_var trm))
	  (let* ((head (term$head trm)))
	    (and (operator$is_a_retract head)
		 (sort_order$is_included_in
		  (module$sort_order
		   (if obj$current_module obj$current_module
		     (sort$module (operator$coarity head))))
		  (term$sort (term$arg_1 trm))
		  (operator$coarity head))
		 )))
      (setq applied t)
      (term$!replace-dd-no-retract trm
          (add_retract_if_needed srt (term$arg_1 trm))))
  (if (eq '-retract (car rul))
    (let* ((trmsrt (term$sort trm))
	   (so (module$sort_order
		(if obj$current_module obj$current_module
		  (sort$module trmsrt)))))
      (when
	  (and
	   (sort_order$is_in_same_connected_component so trmsrt (cadr rul))
	   (not (sort_order$is_included_in so trmsrt (cadr rul))))
	(setq applied t)
	(term$!replace-dd-no-retract trm
            (add_retract_if_needed srt
	        (term$make_term (operator$make_retract trmsrt (cadr rul))
		    (list (copy_term trm)))))))
    (break "SNARK: illegal special rule in apply")
  )))

(defun test-rule-direct (rul trm)
  (multiple-value-bind (gs sub no eeq) (matcher (rule$lhs rul) trm)
    (declare (ignore gs sub eeq))
    (null no))
 )

(defun test-rule (rul trm)
  (if (rule$is_special rul)
    (test-rule-special rul trm)
    (multiple-value-bind (gs sub no eeq) (matcher (rule$lhs rul) trm)
      (declare (ignore gs sub eeq))
      (if (and no
	       (and inside-apply-with-extensions
		    (not (term$is_var trm))
		    (operator$is_same_operator
		     (term$head (rule$lhs rul)) (term$head trm))))
	(test-rule-extensions rul trm)
	(null no)
	)
  )))

(defun test-rule-extensions (rul trm)
  (let ((top (term$head (rule$lhs rul))))
  (if (operator$is_associative top)
      (dolist (r
		(if (operator$is_commutative top)
		  (rule$!compute_AC_extension rul top)
		  (rule$!compute_A_extensions rul top))
	       nil)
	(when (and r (test-rule-direct r trm))
	  (return t))
      )
    nil
  )))

(defun add_retract_if_needed_in (mod srt trm)
  (let ((so (module$sort_order mod)))
    (if (sort_order$is_included_in so (term$sort trm) srt)
	trm
      (term$make_term
       (operator$make_retract (term$sort trm) srt)
       (list trm)))))
(defun add_retract_if_needed (srt trm)
  (add_retract_if_needed_in obj$current_module srt trm))

(defvar self)

(defun apply-one-rule (rul srt trm)
  (let ((self trm))
  (if (rule$is_special rul)
    (apply-one-special-rule rul srt trm)
  (let ((cond (rule$condition rul)))
  (if (or reduce-conditions
	  (obj_BOOL$is_true cond))
  (let ((lhs (rule$lhs rul)))
  (if (term$is_var lhs)
      (multiple-value-bind (gs sub no eeq) (matcher lhs (copy-list trm))
	(declare (ignore gs))
      (when eeq (setq sub nil))
      (unless (or no
		  (and (not (obj_BOOL$is_true cond))
		       (not (obj_BOOL$is_true
			     (rew$!normalize_term
			      (copy_term
			       (substitution$image sub cond)))))))
      (setq applied t)
      (term$!replace-dd-no-retract trm
	(copy_term
	 (add_retract_if_needed srt
	 (substitution$image sub (rule$rhs rul)))))))
    (let ((copy_conditions t))
    (let ((res (rule$$!apply_one_rule_no_simplify rul trm)))
      (when res
	(term$!replace-dd-no-retract trm
	  (copy_term (add_retract_if_needed srt trm)))
	(setq applied t))
      )))
  trm
  )
  ; "recurse" on condition
  (let ((lhs (rule$lhs rul))
	(rhs (rule$rhs rul)))
  (multiple-value-bind (gs sub no eeq) (matcher lhs (copy-list trm))
    (declare (ignore gs))
  (when eeq (setq sub nil))
  (unless no
    (setq applied t)
    (princ "shifting focus to condition") (terpri)
    (let ((condinst (copy_term (substitution$image sub cond)))
	  (rhsinst
	   (add_retract_if_needed srt
	   (copy_term (substitution$image sub rhs)))))
    (setq action-stack (cons
	(list $$term trm rul condinst rhsinst srt)
	action-stack))
    (setq $$term condinst)
    (when inside-apply-all
      (princ "applying rule only at first position found: ")
      (term$print trm)
      (terpri)
      (throw 'apply-all-quit nil))
    ))
  ) ;m-v-b
  ))))))

(defun apply-rule (rul srt trm)
  (if (and inside-apply-with-extensions
	(operator$is_same_operator
	 (term$head (rule$lhs rul)) (term$head trm)))
    (apply-rule-with-extensions rul srt trm)
    (apply-one-rule rul srt trm)))

(defun apply-rule-with-extensions (rul srt trm)
  (let ((top (term$head (rule$lhs rul))))
  (if (operator$is_associative top)
      (let ((is_applied nil)
	    (is_AC (operator$is_commutative top)))
	(when (test-rule rul trm)
	  (apply-one-rule rul srt trm)
	  (setq is_applied applied))
	(unless is_applied
	(dolist (r
		  (if is_AC
		    (rule$!compute_AC_extension rul top)
		    (rule$!compute_A_extensions rul top)))
	  (when (and r (test-rule r trm))
	    (apply-one-rule r srt trm)
	    (setq is_applied applied)
	    (return))
	))
      )
    ; only hit this case if top of rule lhs wasn't associative
    (apply-one-rule rul srt trm)
  ))
  nil
  )

(defun make-rule-rev (rul)
  (if (rule$is_built_in rul) rul
  (make-rule :lhs (rule$rhs rul) :rhs (rule$lhs rul)
    :condition (rule$condition rul) :end_reduction nil
    :is_built_in nil :method nil)))

(defun make-rule-inst (rul subst)
  (when (and rul (rule$is_built_in rul))
    (princ "Error: cannot instantiate built-in rules") (terpri)
    (obj3-to-top))
  (make-rule
   :lhs (substitution$image subst (rule$lhs rul))
   :rhs (substitution$image subst (rule$rhs rul))
   :condition
     (let ((cnd (rule$condition rul)))
       (if (eq *obj_BOOL$true* cnd) cnd
	 (substitution$image subst (rule$condition rul))))
   :end_reduction nil :is_built_in nil :method nil)
  )

(defun make_right_assoc (op subs)
  (if (null (cdr subs)) (car subs)
    (term$make_right_assoc_normal_form op subs)
  ))

(defun select-a (srt trm m1 n1)
 (let ((m (1- m1)) (n (1- n1)))
 (if (term$is_var trm)
   (progn
     (princ "Warning: top operator is not associative")
     (princ "Warning: error in selection of subsequence")
     (terpri)
     (values srt trm))
 (let ((op (term$head trm)))
 (let ((lst (term$list_assoc_subterms trm op)))
 (if (and (operator$is_associative op)
	  (<= m n) (<= 1 m1) (<= n1 (length lst)))
    (if (or (< 1 m1) (< n1 (length lst)))
      (let ((res (make_right_assoc op (subseq lst m (1+ n)))))
	(term$!replace trm
	  (make_right_assoc op
	    (append (subseq lst 0 m) (list res) (subseq lst (1+ n)))))
	(values (car (operator$arity op)) res)
      )
      (values srt trm))
   (progn
     (if (not (operator$is_associative op))
	 (princ "Warning: top operator is not associative")
       (princ "Warning: error in selection of subsequence"))
     (terpri)
     (values srt trm)))
 )))))

(defun select-ac (srt trm occs)
 (if (term$is_var trm)
   (progn
     (princ "Warning: top operator is not associative and commutative")
     (terpri)
     (values srt trm))
 (let ((op (term$head trm)))
 (if (and (operator$is_associative op) (operator$is_commutative op))
    (let ((lst (term$list_AC_subterms trm op)))
    (let ((len (length lst)) (sel nil) (rest nil)
	  (err nil))
    (dolist (i occs)
      (let ((n (1- i)))
      (if (and (<= 0 n) (< n len))
	  (let ((tl (nthcdr n lst)))
	    (when (car tl) (push (car tl) sel))
	    (rplaca tl nil))
	  (setq err t))
      )
    )
    (dolist (x lst) (when x (push x rest)))
    (when err
      (princ "Warning: error in selection of subterms")
      (terpri))
    (if (null rest)
      (values srt trm)
      (let ((res (make_right_assoc op (nreverse sel))))
	(term$!replace trm
	  (make_right_assoc op (cons res (reverse rest))))
	(values (car (operator$arity op)) res)
    ))
    ))
   (progn
     (princ "Warning: top operator is not associative and commutative")
     (terpri)
     (values srt trm)))
 )))

(defun select-oc (srt trm occs)
  (let ((cursrt srt) (cur trm))
    (dolist (i occs)
      (if (and (<= 1 i) (<= i (length (term$subterms cur))))
	(progn
	  (setq cursrt (nth (1- i) (operator$arity (term$head cur))))
	  (setq cur (nth i cur))) ; this is right
	(progn
	  (princ "Warning: selection of occurrence not correct") (terpri)
	  (return))))
    (values cursrt cur)
  ))

; by side-effect
(defun apply-all (rul srt trm)
  (if (term$is_var trm)
      (when (test-rule rul trm) (apply-rule rul srt trm))
  (if (test-rule rul trm)
    (apply-rule rul srt trm)
    (mapc #'(lambda (s x) (apply-all rul s x))
      (operator$arity (term$head trm))
      (term$subterms trm))
  ))
  nil
  )

(defun misc$start (e)
  (let ((toks (cadr e)))
  (unless (top-noshow)
    (princ "start ")
    (when toks
      (print$simple_princ_open toks))
    (princ " .") (terpri))
  (if (null *mod_eval$$last_module*)
    (progn
      (princ "No last module") (terpri)
      (obj3-to-top))
    (progn
      (mod_eval$$!setup_parse *mod_eval$$last_module*)
      (let ((tm (parse$parse_ground "a start the"
		    *mod_eval$$last_module* toks *obj$sort_Universal*)))
      (if (term$is_an_error tm)
	(obj3-to-top)
	(setq $$term tm))
      )
      (when (command-final) (command-display)))
    )))

; lhh -- the run command for TRIM
(defun misc$run (inp)
  (let ((cmd-dat (cadr inp))
        (cmd-keyword-list (list "verbose" "keep" "debug"))
        (mod-name-string "")
        (write-file-name (concatenate `string *obj-unique-filename* ".wr"))
        (read-file-name (concatenate `string *obj-unique-filename* ".rd"))
        (run-verbose nil)
        (run-keep nil)
        (run-debug nil)
        (run-cmd-line "")
        (old-ostream *standard-output*)
        (old-istream *standard-input*)
        (parsed-term nil))

    (when (not (equal (car inp) "run"))
      (princ "Error: unknown command in `run' code.")
      (terpri)
      (obj3-to-top))

    (when (null *mod_eval$$last_module*)
       (princ "No last module") (terpri)
       (obj3-to-top))

    (dolist (k cmd-dat) ; -- process keywords in the command line
      (when (equal (car cmd-dat) "verbose")
        (setq run-verbose t)
        (setq cmd-dat (cdr cmd-dat)))
      (when (equal (car cmd-dat) "keep")
        (setq run-keep t)
        (setq cmd-dat (cdr cmd-dat)))
      (when (equal (car cmd-dat) "debug")
        (setq run-debug t)
        (setq cmd-dat (cdr cmd-dat)))
      (when (not (member k cmd-keyword-list :test 'equal))
        (return)))

    (setq mod-name-string (convert-to-string *mod_eval$$last_module*))
    (mod_eval$$!setup_parse *mod_eval$$last_module*)
    (let ((tm (parse$parse_ground "a start the"
	         *mod_eval$$last_module* cmd-dat *obj$sort_Universal*)))
      (if (term$is_an_error tm)
	(obj3-to-top)
	(setq $$term tm)))

    (when (not (probe-file mod-name-string))
      (princ "Error: could not find TRIM executable for module `")
      (princ mod-name-string)
      (princ "'.")
      (terpri)
      (obj3-to-top))

    ; set up command line.
    ; image name is first.
    (setq run-cmd-line mod-name-string) 
    ; force raw mode output
    (setq run-cmd-line (concatenate 'string run-cmd-line " -r "))
    (when run-verbose
      (setq run-cmd-line (concatenate 'string run-cmd-line " -v ")))
    (setq run-cmd-line (concatenate 'string run-cmd-line " < "))
    (setq run-cmd-line (concatenate 'string run-cmd-line write-file-name))
    (setq run-cmd-line (concatenate 'string run-cmd-line " > "))
    (setq run-cmd-line (concatenate 'string run-cmd-line read-file-name))

    (when run-debug
      (princ "TDL term: ")
      (term$print-trim $$term old-ostream)	
      (terpri))

    ; write term to a file in TDL format
    (setq *standard-output* (open write-file-name :direction :output))
    (term$print-trim $$term old-ostream)	
;    (princ "parse ")
;    (term$print $$term)
;    (princ " . ")
    (close *standard-output*)
    (setq *standard-output* old-ostream)

    (princ "run ")
    (princ mod-name-string)
    (princ ": ")
    (term$print $$term)
    (terpri)

    (when run-verbose
      (princ run-cmd-line) (terpri))

    ; spawn sub process
    (if (not (equal
              #+GCL (system run-cmd-line)
              #+(or CMU CLISP) (ext:run-program run-cmd-line nil)
              0))
      (obj3-to-top))

    ; get result from TRIM
    (setq *standard-input* (open read-file-name :direction :input))
    (setq parsed-term (module_parse$parse))
    (close *standard-input*)
    (setq *standard-input* old-istream)

    ; get the list of tokens from the parsed-term list: 
    ; 	("parse" (<list of tokens>) ".")
    (setq parsed-term (cdr parsed-term))
    (setq parsed-term (car parsed-term))

    ; parse the list of tokens.
    (let ((tm (parse$parse_ground "a start the"
                    *mod_eval$$last_module* parsed-term *obj$sort_Universal*)))
      (if (term$is_an_error tm)
        (obj3-to-top)
        (setq $$term tm)))

    ; print the final result
    (princ "result ")
    (print$sort_name obj$current_module (term$sort $$term))
    (princ ": ")
    (print$term $$term)
    (terpri)

    (when (not run-keep)
      (delete-file write-file-name)
      (delete-file read-file-name))))

(defun parse_rule_spec (tok)
  (let ((i 0) (f (length tok)) (rev nil) (m 0))
  (when (eql #\- (char tok 0))
    (setq rev t  i 1))
  (setq m (- f 1))
  (loop
   (when (or (< m 0) (eql #\. (char tok m)))
     (return))
   (setq m (- m 1)))
  (if (< m 0) 'err
    (list (subseq tok i m) (subseq tok (+ m 1)) rev))
  ))

(defun compute_action (tok)
  (if (or (equal "red" tok) (equal "reduction" tok))
    'reduction
  (if (equal "retr" tok)
    'retract
  (if (equal "-retr" tok)
    '-retract
  (if (equal "print" tok)
    'print
    (parse_rule_spec tok))))))

(defun compute_subst (rul substtoks)
  ; rule just for vars
  (unless (null substtoks)
  (let ((vars (union (term$vars (rule$lhs rul))
	      (union
		     (term$vars (rule$rhs rul))
		     (term$vars (rule$condition rul)))))
	(sub nil)
	varnm trmtoks avar atrm)
  (mod_eval$$!setup_parse *mod_eval$$last_module*)
  (let ((so (module$sort_order obj$current_module)))
  (loop
   (when (null substtoks) (return))
   (setq varnm (cadr substtoks))
   (setq trmtoks (nth 3 substtoks))
   (setq avar (find-if #'(lambda (x) (equal (variable$name x) varnm)) vars))
   (setq atrm (parse$parse_ground "a substitution a"
	       *mod_eval$$last_module* trmtoks *obj$sort_Universal*))
   (if (and avar
	    (not (term$is_an_error atrm)))
       (progn
	 (unless
	  (let ((s2 (term$sort atrm)))
	  (dolist (s1 (variable$sorts avar) nil)
		  (when  (sort_order$is_included_in so s2 s1)
			 (return t))))
	  (princ "Warning: term sort is incompatible with variable sort")
	  (terpri)
	  (setq atrm
		(term$make_term
		 (operator$make_retract (term$sort atrm) (term$sort avar))
		 (list atrm)))
	  )
	 (push (cons avar atrm) sub)
	 )
     (progn
       (unless avar (princ "No such variable in rule: ") (prin1 varnm)
         (terpri))
       (princ "Error: specified substitution contains an error") (terpri)
       (obj3-to-top)
       ))
   (setq substtoks (cddddr substtoks))
  ))
  sub
  )))

(defun str-to-int (x)
  (if (equal "" x) 0
    (read-from-string x)))

(defun compute_action_rule (act stoks srtnm)
  (if (eq 'reduction act) 'reduction
  (if (eq 'print act) 'print
  (if (eq 'retract act) '(retract) ; adding non-rules as "psuedo rules"
  (if (eq '-retract act)
    (let ((srt (mod_eval$$find_qual_sort srtnm)))
      (if srt
	(list '-retract srt)
	(progn
	  (princ "in a -retr, the sort: ")
	  (princ srtnm)
	  (terpri)
	  (princ "is not recognized") (terpri)
	  (obj3-to-top))))
  (progn
  (when (equal "" (cadr act))
    (princ "Error: rule number missing") (terpri)
    (obj3-to-top))
  (let ((mod (if (or (equal "" (car act))
		     (and
		      *mod_eval$$last_module*
		      (equal "%" (module$name *mod_eval$$last_module*))
		      (module$sub_modules *mod_eval$$last_module*)
		      (equal (car act)
			(module$name
			 (caar (module$sub_modules
				*mod_eval$$last_module*))))))
		 *mod_eval$$last_module*
	       (modexp_eval$eval_whole (car act))))
	rul)
    (when (modexp_eval$is_error mod)
      (let ((nxt (eval_mod (list (car act)))))
	(if (modexp_eval$is_error nxt)
	  (progn
	    (princ "Error: module undefined: ") (princ (car act)) (terpri)
	    (obj3-to-top))
	  (setq mod nxt))))
    (if (and (< 0 (length (cadr act)))
	     (digit-char-p (char (cadr act) 0)))
      (setq rul (rulenum mod (str-to-int (cadr act))))
      (setq rul (rulelabelled mod (cadr act))))
    (when (nth 2 act) (setq rul (make-rule-rev rul)))
    (when stoks (setq rul
        (make-rule-inst rul (compute_subst rul stoks))))
    rul
  )))))))

; typical arg ("{" "1" "," "4" "," "7" "}")
(defun compute-set-ocs (x)
  (let ((res nil))
  (setq x (cdr x))
  (loop
   (when (null x) (return))
   (let ((val (str-to-int (car x))))
     (unless (member val res)
       (push val res)))
   (setq x (cddr x))
  )
  (nreverse res)
  ))

(defun compsel (srt tm sel)
  (if (null sel) (values srt $$term)
  (if (or (equal '("term") (car sel)) (equal '("top") (car sel)))
      (values srt $$term)
  (if (equal "of" (car sel)) (compsel srt tm (cdr sel))
  (if (equal "(" (caar sel))
      (if (equal ")" (cadr (car sel))) (compsel srt tm (cdr sel))
	(multiple-value-bind (s1 t1) (compsel srt tm (cdr sel))
	  (select-oc s1 t1
	      (mapcar #'str-to-int (cadr (car sel))))))
  (if (equal "[" (caar sel))
      (let ((i1 (str-to-int (cadr (car sel)))))
      (multiple-value-bind (s1 t1) (compsel srt tm (cdr sel))
	 (select-a s1 t1
	     i1
	     (if (equal "]" (nth 2 (car sel)))
	       i1
	       (str-to-int (cadr (nth 2 (car sel))))))))
  (if (equal "{" (caar sel))
    (multiple-value-bind (s1 t1) (compsel srt tm (cdr sel))
       (select-ac s1 t1 (compute-set-ocs (car sel))))
      (break "SNARK: compsel"))))))))

(defun compute_selection (tm sel)
  (compsel *obj$sort_Universal*
	   tm
	   (if (null sel) nil (cons (car sel) (cadr sel))))
  )

(defun print$simple_princ_flat (x)
  (when (and (fboundp 'filecol)
	     (< *print$line_limit* (filecol *standard-output*)))
    (terpri)
    (when *print$indent_contin* (princ "    ")))
  (if (is-illegal-type x) (progn (princ "illegal#") (print$addr x))
  (cond
   ((null x))
   ((atom x) (princ x))
   (t (let ((flag nil) (tail x))
     (loop
      (when (not (consp tail)) (return))
      (if flag
	  (unless (or (null (car tail))
		      (equal "," (car tail)))
	     (princ " "))
	  (setq flag t))
      (print$simple_princ_flat (car tail))
      (setq tail (cdr tail))
     )
     (when tail (princ " ... ") (prin1 tail))
   )))))

(defun display_term (x)
  (let ((obj$current_module *mod_eval$$last_module*)
        (*mod_eval$$current_module* *mod_eval$$last_module*))
  (print$sort_name *mod_eval$$last_module* (term$sort x))
  (princ ": ")
  (let ((*show-retracts* t)) (term$print x))))

; rulespec  subst within/at selection
(defun misc$apply (e)
  (if (equal '(("?") ".") (cdr e))
      (progn
	(unless (top-noshow) (princ "apply ?") (terpri))
	(misc$apply_help))
  (if (or (null $$term) (eq 'void $$term))
    (progn
      (princ "term is not defined") (terpri)
      (obj3-to-top))
  (if (null *mod_eval$$last_module*)
    (progn
      (princ "No last module") (terpri)
      (obj3-to-top))
  (let ((obj$current_module *mod_eval$$last_module*)
        (*mod_eval$$current_module* *mod_eval$$last_module*)
	(ee (cadr e)))
  (unless (top-noshow)
    (princ "apply ") (print$simple_princ_flat ee) (princ " .") (terpri))
  (let ((action (compute_action (nth 0 ee)))
	(flg (stringp (nth 1 ee))) ; with (-retr), at or within -- no subst.
	(ri (and (equal "with" (nth 1 ee)) (equal "sort" (nth 2 ee)))))
  (if (eq 'err action)
      (progn
	(princ "Error: action specification is not meaningful: ")
	(princ (nth 0 ee))
	(terpri)
	(obj3-to-top))
  (let ((substtoks (if flg nil (nth 1 ee)))
	(kindmark (nth (if ri 4 (if flg 1 2)) ee))
	(selects (subseq ee (if ri 5 (if flg 2 3))))
	(sortnm (if ri (nth 3 ee) nil)))
  (let ((actrul (compute_action_rule action substtoks sortnm)))
  (multiple-value-bind (subtermsort subterm)
      (compute_selection $$term selects)
    (setq applied t)
    (if (eq 'reduction actrul)
	(progn
	  (mod_eval$!setup_reduction *mod_eval$$last_module*)
	  (term$!replace subterm (copy_term subterm)) ; erase reduced flags
	  (rew$!normalize subterm))
    (if (eq 'print actrul)
 	(progn (princ "term ") (display_term subterm) (terpri))
    (progn
    (setq applied nil)
    (let ((inside-apply-with-extensions
	   (and
	    (not (rule$is_special actrul))
	    (let ((arlhs (rule$lhs actrul)))
	      (and (not (term$is_var arlhs))
		   (operator$is_associative (term$head arlhs)))))))
    (if (equal "within" kindmark)
      (let ((inside-apply-all t))
	(catch 'apply-all-quit
	  (apply-all actrul subtermsort subterm)))
      (apply-rule actrul subtermsort subterm))))))
    (unless applied
      (princ "Warning: rule not applied") (terpri))
    (when (and (consp action) (nth 2 action))
      (setq $$term (copy_term $$term))) ;reset reduced flags
    (command-final)
    (command-display)
  ))))))))))

(defun command-display ()
  (if action-stack
    (progn (princ "condition(") (prin1 (length action-stack))
      (princ ") "))
    (princ "result "))
  (display_term $$term)
  (terpri)
  )

(defun command-final ()
  (when action-stack
  (if (term$similar $$term *obj_BOOL$true*)
      (progn
	(command-display)
	(princ "condition is satisfied, applying rule") (terpri)
	(princ "shifting focus back to previous context") (terpri)
	(let ((cur (car action-stack)))
	(setq $$term (caar action-stack))
	(term$!replace
	 (nth 1 cur) (add_retract_if_needed (nth 5 cur) (nth 4 cur)))
	(setq action-stack (cdr action-stack))
	)
	t)
  (when (term$similar $$term *obj_BOOL$false*)
      (progn
	(command-display)
	(princ "condition is not satisfied, rule not applied") (terpri)
	(princ "shifting focus back to previous context") (terpri)
	(setq $$term (caar action-stack))
	(setq action-stack (cdr action-stack))
	t)
      )))
  )

(defun print_pending ()
  (let ((obj$current_module *mod_eval$$last_module*)
        (*mod_eval$$current_module* *mod_eval$$last_module*))
  (princ "pending actions") (terpri)
  (if (null action-stack)
      (progn (princ "  none") (terpri))
  (let ((depth 1))
  (dolist (dact (reverse action-stack))
    (dotimes (i (- depth 1)) (princ "   "))
    (format t "~3d" depth)
    (princ "| in ") (term$print (nth 0 dact))
    (princ "  at ")
        (if (eq (nth 0 dact) (nth 1 dact))
	    (princ "top")
	    (term$print (nth 1 dact)))
    (terpri)
    (dotimes (i depth) (princ "   "))
    (princ "| rule ") (print$rule_brief (nth 2 dact))
    (terpri)
    (dotimes (i depth) (princ "   "))
    (princ "| condition ") (term$print (nth 3 dact))
    (princ "  replacement ") (term$print (nth 4 dact))
    (terpri)
    (incf depth)
  )))))

(defun misc$apply_help ()
  (princ
   "Apply a selected rule, possibly with an instantiation,")
      (princ " to selected subterm(s).")
  (terpri)
  (princ "Syntax:") (terpri)
  (princ "    apply { reduction | red | print | retr |")
      (princ " -retr with sort <Sort> |") (terpri)
  (princ "      <RuleSpec> [ with {<variable> = <expr>}... ] }") (terpri)
  (princ "      [ at | within ] ")
  (princ "<Selector> { of <Selector> } ... .") (terpri)
  (princ "    <RuleSpec> ::= [-]<ModId>.<RuleId> | [-].<RuleId>") (terpri)
  (princ "    <RuleId> ::= <Natural> | <Id>") (terpri)
  (princ "    <Selector> ::= term | top |") (terpri)
  (princ "         (<Natural>...) |   ") (terpri)
  (princ "         '[' <Natural> [ .. <Natural> ] ']' |") (terpri)
  (princ "         '{' <Natural> {, <Natural>}... '}'") (terpri)
  )

(defun apply_print_rule (x)
  ; "retr" "-retr" -A.k .k
  (let ((act (compute_action x)))
  (if (eq 'retract act)
    (progn (princ "special rule which eliminates retracts: r:B>A(X:A) = X:A")
      (terpri))
  (if (eq '-retract act)
    (progn (princ "special rule for the introduction of retracts; must")
      (terpri)
      (princ "specify sort: -retr with sort B gives X:A = r:B>A(X:A)")
      (terpri))
  (if (eq 'reduction act)
    (progn (princ "special rule for reduction of a selected subterm") (terpri))
  (if (eq 'err act)
    (progn (princ "That doesn't make sense as a rule specification.") (terpri))
  (if (eq 'print act)
    (progn (princ "special rule to print the selected subterm") (terpri))
    (let ((num (cadr act)) (mod (car act)) (rev (caddr act)))
      (princ "rule ") (princ num)
      (when rev (princ " (reversed)"))
      (if (equal "" mod)
	(princ " of the last module")
	(progn (princ " of module ") (print$name (eval_mod (list mod)))))
      (terpri)
      (let ((rul (compute_action_rule act nil nil)))
	(princ "  ")
	(print$rule_brief rul) (terpri)
	(when (and rev (rule$is_built_in rul))
	  (progn (princ "This rule cannot be applied reversed.") (terpri)))
	(when (and *mod_eval$$last_module*
		   (not (rule$is_built_in rul))
		   (not (sort_order$is_included_in
			 (module$sort_order *mod_eval$$last_module*)
			 (term$sort (rule$rhs rul))
			 (term$sort (rule$lhs rul)))))
	  (princ "(This rule rewrites up.)") (terpri))))
    )))))))
