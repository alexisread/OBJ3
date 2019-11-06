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

;; $Id: ci.lsp,v 205.2.1.1 2003/09/23 14:06:00 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                    ci -- command interpreter
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Tim Winkler ;;;; Created: 7/15/86

;;;;;;;;;;;;;;;; Summary ;;;;;;;;;;;;;;;;
; op ci$!process_definition : {Obj-State} Module -> {Obj-State}
; op ci$perform_reduction : {Obj-State} Preterm -> {output} [input-output]
; op ci$perform_test_reduction : {Obj-State} Preterm -> {output} [input-output]

; var obj$current_module -- current module in reduction process

(defvar obj$current_module nil)

(defun ci$!process_definition (def)
  (cond
   ((equal "view" (car def))
    (mod_eval$view_eval def))
   (t (mod_eval$eval_definition def))
  ))

(defvar $$mod 'void)
(defvar $$term 'void)
(defvar $$norm 'void)
(defvar $$time-red nil) ; print reduction time statistics?
(defvar $$time-parse nil) ; print parsing time statistics?
(defvar $$show-red t) ; show terms to be reduced
(defvar *show-retracts* nil)
(defvar *rew$perform_on_demand_reduction* nil) ;cf. match_equation.lsp
(defvar *rule_count* 0)
(defvar *obj$show_stats* t)
(defvar $$trials 1)

; op ci$perform_reduction : Top-Item -> {*standard-output*} [input-output]
(defun ci$perform_reduction (e)
  (setq $$trials 1)
  (let (modexp sort preterm time1 time2)
  (if (= 4 (length e))
      (let ((val (cadr (cadr e))))
	(if (and *obj$obj2_mode*
		 (equal "as" (car (last val))))
	    (setq modexp (butlast val))
	  (setq modexp val)))
    (setq modexp nil))
  (let ((mod (if modexp 
		 (modexp_eval$top_level_eval modexp)
	       *mod_eval$$last_module*)))
  (if (or (null mod) (modexp_eval$is_error mod))
      (if (null mod)
	  (progn
	    (princ "No module expression provided and no last module")
	    (terpri))
	(progn
	  (princ "Incorrect module expression: ")
	  (princ modexp) (terpri)))
  (let ((obj$current_module mod))
    (setq *mod_eval$$current_module* mod) ;@@@
    (setq *mod_eval$$last_module* mod) 
    (when (and $$complain_theory (mod_eval$has_theory_submodule mod))
      (princ "Warning: module in reduction: ")
      (print$name mod) (princ " has a theory as a sub-module") (terpri)
      (princ "term may not reduce as expected") (terpri) ;&&&& 23 Sep 87
      )
    (ci$check_bad_rules mod)
    (mod_eval$!setup_reduction mod)
    (setq $$mod obj$current_module)
    (setq sort *obj$sort_Universal*)
    (if (= 4 (length e))
	(setq preterm (nth 2 e))
      (setq preterm (nth 1 e)))
    (when $$time-parse (setq time1 (get-internal-run-time)))
    (let ((term (parse$parse_ground "a reduction the" mod preterm sort)))
      (when $$time-parse
	(setq time2 (get-internal-run-time))
        (format t "~&parsing time: ~8,3f cpu~%"
	  (/ (float (- time2 time1)) internal-time-units-per-second)))
 (when (or $$show-red (and $$debug (<= 15 $$debug_level)))
      (princ "reduce in ")
      (print$mod_name obj$current_module)
      (princ " : ")
      (term$print term)
      (terpri)
      (when (and $$debug (<= 20 $$debug_level))
	    (princ "---") (terpri) (print$struct term) (terpri))
      )
    (setq $$term term)
    (when $$time-red
      (setq $$matches 0)
      (setq time1 (get-internal-run-time)))
    (let ((*rew$perform_on_demand_reduction* t) (*rule_count* 0))
    (let ((res (rew$!normalize term)))
      (when $$time-red
	(setq time2 (get-internal-run-time))
        (format t "~&reduction time: ~8,3f cpu~%"
	  (/ (float (- time2 time1)) internal-time-units-per-second))
	(format t "match attempts: ~d~%" $$matches)
	(setq $$matches nil))
      (setq $$norm res)
      (term$set res)
 (when (and $$debug (<= 15 $$debug_level))
      (princ "rew$!normalize: ")
      (print$term res) (terpri)
      (when (and $$debug (<= 20 $$debug_level))
	    (princ "---") (terpri) (print$struct res) (terpri))
      )
    (when *obj$show_stats*
	  (princ "rewrites: ") (prin1 *rule_count*) (terpri))
    (princ "result ")
    ;(print$short_sort_name (term$sort res))
    (print$sort_name obj$current_module (term$sort res))
    (princ ": ")
    (let ((*show-retracts* t)) (term$print res))
    (terpri)
    )
  )))))))

; op ci$perform_test_reduction : Top-Item -> {*standard-output*} [input-output]
(defun ci$perform_test_reduction (e)
  (setq $$trials 1)
  (let ((red nil))
    (setq e (cdr e))
    (loop
     (when (or (null e)
	       (equal "expect:" (car e))
	       (equal "answer:" (car e)))
	   (return))
     (push (car e) red)
     (setq e (cdr e)))
    (push "." red)
    (setq red (nreverse red))
    (ci$perform_reduction red)
  (let ((pre_ans (cadr e)) (obj$current_module $$mod))
  (let ((ans (parse$parse_ground "a test reduction a"
				 $$mod pre_ans *obj$sort_Universal*)))
    (unless (term$equational_equal $$norm ans)
      (princ "*************** Inconsistent ***************") (terpri)
      (princ "expected: ") (print$simple_princ_open pre_ans) (terpri))
  ))))


(defun ci$red_loop (mod)
  (setq $$trials 1)
  (setq mod (modexp_eval$eval mod))
  (if (modexp_eval$is_error mod)
      (progn
	(princ "Undefined module")
	(terpri))
  (let (in (flag nil)
	(top-level (at-top-level)))
    (mod_eval$!setup_reduction mod)
    (loop
     (general_read$$!init)
     (let ((cur (general_read$$!set_single_reader '("[" "]" "_"))))
       (progn
	 (setq in (general_read$$seq_of_term '(|.|)))
	 (general_read$$!set_reader cur)))
     (when (null in) (return))
     (unless top-level
       (if flag
	 (progn (princ "------------------------------------------")
		(terpri))
	 (setq flag t)))
     (ci$red mod in top-level)
  )))
  'done)

(defun ci$red (mod preterm dont-print)
  (setq $$trials 1)
  (let ((obj$current_module (if (consp mod) (modexp_eval$eval mod) mod)))
  (let ((term (parse$parse_ground "a reduction the"
				  mod preterm *obj$sort_Universal*)))
    (unless dont-print (princ "reduce ") (term$print term) (terpri))
    (setq $$term term)
    (let ((res (rew$!normalize term)))
    (princ "result ")
    (print$short_sort_name (term$sort res))
    (princ ": ")
    (let ((*show-retracts* t)) (term$print res))
    (setq $$norm res)
    (term$set res)
    (terpri)
    )))
  )

(defun ci$check_bad_rules (mod)
  (when (eq 'theory (module$kind mod))
    (when
	(dolist (m (module$all_sub_modules mod) nil)
	  (when (dolist (r (module$rules (car m)) nil)
		  (when (eq 'bad_rule (rule$kind r)) (return t)))
	    (return t)))
      (princ "Warning: Module contains rule with LHS that is a variable or")
          (terpri)
      (princ "a RHS or condition with a variable not occurring in the LHS.")
          (terpri)
      (princ "These rules will be ignored during reduction.")
          (terpri)
      )))
