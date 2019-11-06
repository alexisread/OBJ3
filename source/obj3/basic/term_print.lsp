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

;; $Id: term_print.lsp,v 206.1 2003/09/23 13:51:30 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                    Term Printer
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Tim Winkler ;;;; Created: 4 August, 1986

;;;;;;;;;;;;;;;; Summary ;;;;;;;;;;;;;;;;
; op term$print : Term -> {Output} [I/O]

(defvar *show-retracts* nil)
(defvar *fancy-print* t)
(defvar *abbrev_quals* nil)
(defvar *show_var_sorts* nil)

(defun term$print (term)
  (if *fancy-print* (term$print2 term 127)
    (term$print1 term))
  )

; op term$print1 : Term -> {Output} [I/O]
(defun term$print1 (term)
  (cond
   ((is-illegal-type term) (progn (princ "illegal#") (print$addr term)))
   ((atom term) (princ "%") (prin1 term))
   ((term$is_var term)
    (if (stringp (car term))
	(princ (variable$name term))
    (if (numberp (car term))
	(progn (princ "v") (prin1 (variable$name term)))
      (progn (princ "%") (prin1 term))))
    (when *show_var_sorts*
      (princ ":")
;lhh -- we want the whole enchilada, ie., we want the sort vector
;       computed by the intended parse...
;     (print$name (variable$initial_sort term))
      (princ "[ ")
      (dolist (s (variable$sorts term))
	      (princ (sort$name s))
	      (princ " ")
      )
      (princ "]")
      )
    )
   ((eq 'error (term$head term))
    (prin1 term))
   ((null (term$head term))
    (princ "NULL(")
    (let ((flag nil))
    (dolist (i (term$subterms term))
      (term$print1 i)
      (if flag (princ ",") (setq flag t))))
    (princ ")"))
   ((term$is_built_in_constant term)
    (funcall (nth 2 (sort$info (operator$coarity (term$head term))))
      (term$built_in_value term)))
   ((and (null (term$subterms term)) ;&&&& this should be a temp; 8/4/86
	 (eq 'constant (car (operator$name (term$head term)))))
    (princ "(")
    ;@@@ case not quite right?
    (print$simple_princ_open (cdr (operator$name (term$head term))))
    (princ ").")
    (print$name (term$sort term))
    )
   ((operator$is_a_retract (term$head term))
    (let ((retr (term$head term)))
    (when *show-retracts*
    (princ "r:")
    (princ (sort$name (car (operator$arity retr))))
    (princ ">")
    (princ (sort$name (operator$coarity retr)))
    (princ "("))
    (term$print1 (term$arg_1 term))
    (when *show-retracts*
    (princ ")"))
    ))
   (t (let ((hd (term$head term)))
     (cond
      ((operator$is_standard hd)
       (let ((prv nil))
	 (dolist (i (operator$name hd))
	  (when (and prv (print$need_space prv i)) (princ " "))
	  (setq prv i)
	  (princ i)))
       (let ((subs (term$subterms term)))
	 (when subs
	   (princ "(")
	   (let ((flg nil))
	   (dolist (i subs)
	     (if flg (princ ",") (setq flg t))
	     (term$print1 i)
	     ))
	   (princ ")"))
       ))
      ; constants not standard so...
      ((let ((nm (operator$name hd)))
	 (and (null (term$subterms term))
	      (null (cdr nm)) (not (equal "_" (car nm)))))
       (princ (car (operator$name hd))))
      (t
       (princ "(") ;&&&& for the moment (can improve for associative)
       (let ((subs (term$subterms term)) (prv nil))
	 (dolist (i (operator$name hd))
	   (when (and prv (print$need_space_bars prv i))
	       (princ " "))
	   (setq prv i)
	   (cond
	    ((equal "_" i)
	     (term$print1 (car subs))
	     (setq subs (cdr subs)))
	    (t (princ i))
	 ))
	 (when subs
	   (princ "%[")
	   (let ((flg nil))
	     (dolist (i subs)
	       (if flg (princ ",") (setq flg t))
	       (print$term i)))
	   (princ "]"))
	 )
       (princ ")")
       )
    )))
  ))

; lhh -- translate the term to be reduced by TRIM into TDL.
(defun term$print-trim (term old-stream)
  (print$chk)
  (cond
   ((is-illegal-type term) (progn (princ "illegal#") (print$addr term)))
   ((atom term) (princ "%") (prin1 term))

   ; variables
   ((term$is_var term)
    (princ "(var ")
    (if (stringp (car term))
	(princ (variable$name term))
    (if (numberp (car term))
	(progn (princ "v") (prin1 (variable$name term)))
      (progn (princ "%") (prin1 term))))
    (princ " ")
;    (print$name (variable$initial_sort term))
    (princ "[ ")
    (dolist (s (variable$sorts term))
	    (princ  (sort$name s))
	    (princ " ")
    )
    (princ "]")
    (princ ")")
   )

   ; error term?!?
   ((eq 'error (term$head term))
    (prin1 term))
   ((null (term$head term))
    (setq *standard-output* old-stream)
    (terpri)
    (princ "Error: cannot handle this term: ") 
    (term$print1 term)
    (terpri)
    (recover-trim-error)
;    (princ "NULL(")
;    (let ((flag nil))
;    (dolist (i (term$subterms term))
;      (term$print-trim i old-stream)
;      (if flag (princ ",") (setq flag t))))
;    (princ ")")
   )

   ; built-in constants
   ((term$is_built_in_constant term)
    (setq *standard-output* old-stream)
    (terpri)
    (princ "Error: built-in constants are not allowed in TRIM terms.") (terpri)
    (recover-trim-error)
;    (funcall (nth 2 (sort$info (operator$coarity (term$head term))))
;      (term$built_in_value term))
   )

   ; weird looking constant?!?
   ((and (null (term$subterms term)) ;&&&& this should be a temp; 8/4/86
	 (eq 'constant (car (operator$name (term$head term)))))
    (setq *standard-output* old-stream)
    (terpri)
    (princ "Error: cannot handle this term: ") 
    (term$print1 term)
    (terpri)
    (recover-trim-error)
;    (princ "(")
;    ;@@@ case not quite right?
;    (print$simple_princ_open (cdr (operator$name (term$head term))))
;    (princ ").")
;    (print$name (term$sort term))
    )

   ; retracts -- in TDL they are unary ops with a special name prefix: 'r:'.
   ((operator$is_a_retract (term$head term))
     (setq *standard-output* old-stream)
     (terpri)
     (princ "Error: cannot handle retracts.") (terpri)
     (recover-trim-error)
;     (let ((retr (term$head term)))
;       (princ "(op 1 ")
;       (princ "r:")
;       (princ (sort$name (car (operator$arity retr))))
;       (princ ">")
;       (princ (sort$name (operator$coarity retr)))
;       (princ " ")
;       (princ (sort$name (operator$coarity retr)))
;       (princ " ")
;       (term$print-trim (term$arg_1 term) old-stream)
;       (princ ")")
;     )
   )
   ; handle general terms
   (t (let ((hd (term$head term)))
     (cond

      ; constants not standard so...
      ((let ((nm (operator$name hd)))
	 (and (null (term$subterms term))
	      (null (cdr nm)) (not (equal "_" (car nm)))))
       (princ "(op 0 ")
       (princ (car (operator$name hd)))
       (princ " ")
       (princ (sort$name (operator$coarity hd)))
       (princ ")")
      )

      ; lhh -- standard operator -- standard prefix format that is
      ; because TDL only does prefix we also have to reformat the fancy
      ; mixfix operators.

      (t 	; lhh was: (operator$is_standard hd)
       (princ "(op ")
       (princ (length (operator$arity hd)))
       (princ " ")
       (let ((prv nil))
	 (dolist (i (operator$name hd))
;	  (when (and prv (print$need_space prv i)) (princ " "))
;	  (setq prv i)
	  (princ i)
         )
       )
       (princ " ")
       (princ (sort$name (operator$coarity hd)))
       (princ " ")
       (let ((subs (term$subterms term)))
	 (when subs
	   (dolist (i subs)
	     (term$print-trim i old-stream))
         )
       )
       (princ ")")
      )
      ;;; lhh - the general case...
;      (t
;       (princ "(") ;&&&& for the moment (can improve for associative)
;       (let ((subs (term$subterms term)) (prv nil))
;	 (dolist (i (operator$name hd))
;	   (when (and prv (print$need_space_bars prv i))
;	       (princ " "))
;	   (setq prv i)
;	   (cond
;	    ((equal "_" i)
;	     (term$print-trim (car subs) old-stream)
;	     (setq subs (cdr subs)))
;	    (t (princ i))
;	 ))
;	 (when subs
;	   (princ "%.")
;	   (let ((flg nil))
;	     (dolist (i subs)
;	       (if flg (princ ",") (setq flg t))
;	       (print$term i)))
;	   (princ "."))
;	 )
;       (princ ")")
;       ) ; lhh - end general case
    )))
  ))

(defvar *print$indent_contin*)
(defvar *print$line_limit* 66)

; op term$print2 : Term -> {Output} [I/O]
(defun term$print2 (term prec)
  (print$check)
  (cond
   ((is-illegal-type term) (progn (princ "illegal#") (print$addr term)))
   ((atom term) (princ "%") (prin1 term))
   ((term$is_var term)
    (if (stringp (car term))
	(princ (variable$name term))
    (if (numberp (car term))
	(progn (princ "v") (prin1 (variable$name term)))
      (progn (princ "%") (prin1 term))))
    (when *show_var_sorts*
      (princ ":")
;lhh -- we want the whole enchilada, ie., we want the sort vector
;       computed by the intended parse...
;     (print$name (variable$initial_sort term))
      (princ "[ ")
      (dolist (s (variable$sorts term))
	      (princ (sort$name s))
	      (princ " ")
      )
      (princ "]")
      )
    )
   ((eq 'error (term$head term))
    (prin1 term))
   ((null (term$head term))
    (princ "NULL(")
    (let ((flag nil))
    (dolist (i (term$subterms term))
      (term$print2 i 127)
      (if flag (princ ",") (setq flag t))))
    (princ ")"))
   ((term$is_built_in_constant term)
    (funcall (nth 2 (sort$info (operator$coarity (term$head term))))
      (term$built_in_value term)))
   ((and (null (term$subterms term))
	 (eq 'constant (car (operator$name (term$head term)))))
    (princ "(")
    (print$simple_princ_open (cdr (operator$name (term$head term))))
    (princ ").")
    (print$name (term$sort term))
    )
   ((operator$is_a_retract (term$head term))
    (let ((retr (term$head term)))
    (when *show-retracts*
    (princ "r:")
    (princ (sort$name (car (operator$arity retr))))
    (princ ">")
    (princ (sort$name (operator$coarity retr)))
    (princ "("))
    (term$print2 (term$arg_1 term) 127)
    (when *show-retracts*
    (princ ")"))
    ))
   (t (let* ((hd (term$head term))
	    (qual (and obj$current_module
		       (term$print_qualify obj$current_module hd))))
     (when qual (princ "(")) ;19 Jun 88
     (cond
      ((operator$is_standard hd)
       (let ((prv nil))
	 (dolist (i (operator$name hd))
	  (when (and prv (print$need_space prv i)) (princ " "))
	  (setq prv i)
	  (princ i)))
       (let ((subs (term$subterms term)))
	 (when subs
	   (princ "(")
	   (let ((flg nil))
	   (dolist (i subs)
	     (if flg (princ ",") (setq flg t))
	     (term$print2 i 127)
	     ))
	   (princ ")"))
       ))
      ; constants not standard so...
      ((let ((nm (operator$name hd)))
	 (and (null (term$subterms term))
	      (null (cdr nm)) (not (equal "_" (car nm)))))
       (princ (car (operator$name hd))))
      (t (let ((prec_test (<= prec (operator$precedence hd)))
	       (assoc-test (and obj$current_module
			        (operator$is_associative hd))))
       (when prec_test
       (princ "("))
       (let ((subs (term$subterms term)) (prv nil))
	 (dolist (i (operator$form hd))
	   (cond
	    ((equal 'argument (car i))
	     (when (and prv (print$need_space_bars prv "_")) (princ " "))
	     (setq prv "_")
	     (let ((tm (car subs)))
	     (term$print2 tm
	       (if (and assoc-test
			(not (term$is_var tm))
			(operator$is_same_operator hd (term$head tm)))
		   127
		 (cadr i)))
	     (setq subs (cdr subs))))
	    (t (let ((tok (cdr i)))
	     (when (and prv (print$need_space_bars prv tok)) (princ " "))
	     (setq prv tok)
	     (princ tok))))
	 ); dolist
	 (when subs
	   (princ "%[")
	   (let ((flg nil))
	     (dolist (i subs)
	       (if flg (princ ",") (setq flg t))
	       (print$term i)))
	   (princ "]"))
	 )
       (when prec_test
       (princ ")"))
       ))
    ) ;cond
    (when qual
      (princ ").")
      (print$qualn (operator$module hd)))
    ))
  ))

(defun print$qualn (mod)
  (if (stringp (module$name mod)) (print$name mod)
  (if *abbrev_quals*
      (let ((num (print$mod_num mod)))
	(if (= 0 num)
	    (princ "(...)")
	  (progn (princ "MOD") (prin1 num))
	  ))
    (progn
      (princ "(")
      (print$name mod)
      (princ ")")))
  ))

; this is expensive
(defun term$print_qualify (mod op)
  (if (null mod) nil
  (dolist (o (module$operators mod) nil) ;default value
    (when (and (not (eq o op))
	       (not (eq (operator$module o) (operator$module op)))
	           ;26 Jun 88 not quite sure about this
	       (equal (operator$name o) (operator$name op))
	       ;(eq (operator$coarity o) (operator$coarity op))
	       ;28 Jun 88 bottom up don't know coarity
	       (every2lenfn #'eq (operator$arity o) (operator$arity op)))
      (return t))
  )))

; lhh -- make sure we respect the TRIM comment mode
(defun print$check ()
  (when (< *print$line_limit* (filecol *standard-output*))
    (terpri)
    (when *print$trim-comments-contin* (princ "--- "))
    (when *print$indent_contin* (princ "    ")))
  )

; abbreviation names for modules module numbes 1...

(defun print$nth_mod (n)
  (let ((len (length *modexp_eval$env*)))
    (if (and (<= 1 n) (<= n len))
	(cdr (nth (- len n) *modexp_eval$env*))
      (cdr (nth 0 *modexp_eval$env*)))
  ))

; 0 means not found
(defun print$mod_num (mod)
  (let ((lst *modexp_eval$env*) (i (length *modexp_eval$env*)) (res 0))
    (loop
     (when (null lst) (return))
     (when (eq mod (cdr (car lst))) (setq res i))
     (decf i)
     (setq lst (cdr lst))
     )
    res
  ))

; this assumes some stability in the reader tables
(defun print$need_space (tok1 tok2)
  (and (or (not (stringp tok1))
	   (not (reader$get_syntax (char tok1 (1- (length tok1))))))
       (or (not (stringp tok2))
	   (not (reader$get_syntax (char tok2 0)))))
  )

(defun print$need_space_to_right (tok)
  (or (not (stringp tok))
      (not (reader$get_syntax (char tok (1- (length tok))))))
  )

(defun print$need_space_to_left (tok)
  (or (not (stringp tok))
      (not (reader$get_syntax (char tok 0))))
  )

(defun print$need_space_bars (tok1 tok2)
  (or (and (equal "_" tok1)
	   (or (equal "_" tok2)
	       (print$need_space_to_left tok2)))
      (and (equal "_" tok2)
	   (print$need_space_to_right tok1))
      (print$need_space tok1 tok2))
  )

;;;; primitive term printer (terms are circular, you know)
;;;;   ugly, but at least it halts; have tried not to assume much
;;;;   about the term
#|
(defun print_term (term)
  (cond
   ((atom term) (princ "%") (prin1 term))
   ((null term) (princ "%NIL"))
   ((term$is_var term)
    (princ (variable$name term)) (princ ":")
    (let ((srt (variable$initial_sort term)))
      (if (eq 'sort (type-of srt))
	  (princ (sort$name (variable$initial_sort term)))
	(print$obj srt nil))))
   ((eq 'error (term$head term))
    (prin1 term))
   ((term$is_built_in_constant term)
    (pr (term$built_in_value term)) (princ ":")
    (princ (sort$name (operator$coarity (term$head term))))
    )
   ((and (null (term$subterms term))
	 (eq 'constant (car (operator$name (term$head term)))))
    (princ "#")
    (prin1 (cdr (operator$name (term$head term)))))
   (t
    (let* ((hd (if (consp (car term)) (term$head term) 'fail))
	   (nm (if (eq 'operator (type-of hd)) (operator$name hd) 'fail)))
      (if (eq 'fail nm) (prin1 term)
	(progn
	  (if (and (equal "_" (cadr nm)) (null (cdr nm))) (princ (car nm))
	    (dolist (i nm) (princ i)))
	  (princ "(")
	  (let ((comma_flag nil))
	  (dolist (st (term$subterms term))
	    (if comma_flag (princ ",") (setq comma_flag t))
	    (print_term st)))
	  (princ ")"))))
    )))
|#
