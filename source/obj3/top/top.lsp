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

;; $Id: top.lsp,v 206.1 2003/09/26 13:05:36 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; OBJ3 top-level
; WARNING -- this is implementation dependent
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar obj3-load-time)

(defvar *top-level-tag*
  #+GCL si::*quit-tag*
  #+(or LUCID CMU CLISP) '(*quit-tag*))

(defvar $$debug nil)
(defvar $$debug_level  1)

(defvar *obj$input_source* nil)
(defvar *obj$input_level* 0)
(defvar *obj$input_nesting_limit* 10)
(defvar *obj$input_quiet* nil)
(defvar *obj$print_errors* t)
(defvar *obj$current_labels* nil) ; for labelled things, rules in particular
(defvar *obj$prompt* "OBJ> ")
(defvar *obj$precmd_hook* nil)
(defvar *obj$postcmd_hook* nil)
(defvar *obj$prompt_hook* nil)

(defvar $)
(defvar *mod_eval$last_before_open* nil)
(defvar *obj$allow_uninstantiated* nil)

; lhh -- convert anything x into a string
(defun convert-to-string (x)
  (let ((str (make-string-output-stream)))
    (princ x str)
    (get-output-stream-string str)
  )
)

;; kiniry -- a unique string based upon the current time and a random
;; value.
(defvar *unique-string*)

; lhh -- uniqe base filename (no extension!)
(defvar *obj-unique-filename*)

(defun get-env-var (x)
  #+GCL (si:getenv x)
  #+LUCID (environment-variable x)
  #+CMU (getenv-impl x)
  #+CLISP (getenv x))

(defun sp (n)
  (dotimes (i n) (princ " ")))

(defun top-noshow ()
  (or (and (null *obj$input_source*)
	   (<= *obj$input_level* 0))
      *obj$input_quiet*)
  )

(defun at-top-level ()
  (and (null *obj$input_source*)
       (<= *obj$input_level* 0))
  )

(defvar obj3-version "2.09") ; default value

(defun obj3-greeting ()
  (unless (get-env-var "OBJ3-QUIET")
  (sp 25) (princ " \\|||||||||||||||||/") (terpri)
  (sp 24) (princ "--- Welcome to OBJ3 ---") (terpri)
  (sp 25) (princ " /|||||||||||||||||\\") (terpri)
  (sp 7) (princ "OBJ3 (with TRIM) version ") (princ obj3-version)
      (princ " built: ") (princ obj3-load-time) (terpri)
  (princ "OBJ3 2.06,2.08,2.09 Copyright (c) 2000-2003 Joseph Kiniry, Joseph Goguen") (terpri)
  (sp 10) (princ "OBJ3 2.05 (c) 2000 Sula Ma, Joseph Kiniry, Joseph Goguen") (terpri)
  (sp 12) (princ "OBJ3 2.04 Copyright 1988,1989,1991 SRI International") (terpri)
  (sp 18) (princ "TRIM Copyright (c) 1994,2001 Lutz Hamel") (terpri)
  (sp 24) (princ (get-time-string)) (terpri)
  ))

;; @todo kiniry 25 Sept 2003 - Need to add support for process-args
;; for CMU and CLISP.

(defun obj3 ()
  ;; lhh -- init some global stuff
  (setq *unique-string* (convert-to-string (get-universal-time)))
  (setq *obj-unique-filename* (concatenate 'string "/tmp/OBJ" *unique-string*))

  (obj3-init)
  #+LUCID (buffer-line)
  (obj3-greeting)
  (when (get-env-var "OBJ3-TIMING") (setq $$time-red t))
  #+LUCID (when (get-env-var "OBJ3-BATCH") (buffer-on))
  #+GCL (process-args)
  (let ((quit-flag nil))
    (loop
       (catch *top-level-tag*
         (process_input)
         (setq quit-flag t)
         )                              ;catch
       (when quit-flag (return))
       )                                ;loop
    )                                   ;let
  (finish-output)                       ; only really needed for LUCID
  )

(defun obj_input (f)
  (if (and (< 4 (length f)) (equal ".obj" (subseq f (- (length f) 4))))
      (obj_input_file f)
    (obj_input_file (concatenate 'string (string f) ".obj"))))

(defun expand_file_name (fname)
  (if (equal "~" fname) (namestring (user-homedir-pathname))
  (if (and (eql #\~ (char fname 0)) (eql #\/ (char fname 1)))
    (concatenate 'string
        (namestring (user-homedir-pathname)) (subseq fname 2))
    fname)))

(defun obj_input_file (fname)
  ; DANGER this is UNIX inspired
  (when (and (eql #\~ (char fname 0)) (eql #\/ (char fname 1)))
    (setq fname
	(concatenate 'string
	  (namestring (user-homedir-pathname))
	  (subseq fname 2))))
  (when (not (probe-file fname))
    (princ "Cannot find file: ") (princ fname) (terpri)
    (obj3-to-top))
  (let ((*obj$input_source* fname) (*obj$input_level* (1+ *obj$input_level*)))
  (with-open-file (*standard-input* fname :direction :input)
    (when (< *obj$input_nesting_limit* *obj$input_level*)
      (princ "input nesting is ") (prin1 *obj$input_level*) (terpri)
      (princ "probable input loop (can increase *obj$input_nesting_limit*)")
      (terpri))
    (process_input)
  )))

(defun load_file (fname)
  ; DANGER this is UNIX inspired
  (when (and (eql #\~ (char fname 0)) (eql #\/ (char fname 1)))
    (setq fname
	(concatenate 'string
	  (namestring (user-homedir-pathname))
	  (subseq fname 2))))
  (load fname))

(defun obj3-default-prompt (&optional top-level)
     (print$check)
     (when top-level
       (princ *obj$prompt*))
     #+LUCID (force-output) ; This helps make the buffered mode not unusable
     )
(setq *obj$prompt_hook* #'obj3-default-prompt)

(defun process_input ()
  (let ((reader$$ch 'space)
	(*general_read$$input* *general_read$$void*)
	(top-level (at-top-level)))
  ;(reader$!read_init) ;@@ (take out for obj LISP)
  (memo_rew$create_memo_table)
  (let (inp (in-in nil))
  (loop
   (catch (if top-level 'obj3-top-level-error 'obj3-main-error)
   (catch 'obj3-error
     (when (and top-level
		*obj$prompt_hook*)
       (funcall *obj$prompt_hook* top-level))

   (setq inp (module_parse$parse)) ;; lhh -- does the command parsing

   (when *obj$precmd_hook*
     (funcall *obj$precmd_hook* top-level inp))
   
   (when (or (equal '("eof") inp) (equal '("q") inp) (equal '("quit") inp)
	     (equal '(eof) inp) (equal *reader$eof_value* inp))
     (return))
   (unless (or top-level *obj$input_quiet*)
     (unless (or (equal "---" (car inp)) (equal "***" (car inp))
		 (equal "?" (car inp)) (equal "[" (car inp)))
       (fresh-line)
       (princ "==========================================") (terpri))
     (print_ident inp))
   (cond
     ((member (car inp) '("obj" "object" "th" "theory") :test #'equal)
      (ci$!process_definition inp))
     ((member (car inp) '("red" "reduce") :test #'equal)
      (ci$perform_reduction inp))
     ((member (car inp) '("ev" "eval" "evq" "eval-quiet") :test #'equal)
      (let ((val (eval (cadr inp))))
      (setq $ val)
      (unless  (or *obj$input_quiet*
		   (equal "evq" (car inp)) (equal "eval-quiet" (car inp)))
        (princ "-> ") (terpri)
	(prin1 val) (terpri))))
     ((equal "make" (car inp)) ;just an abbreviation
      (let ((flag (equal "is" (nth 2 inp))))
      (let ((name (nth 1 inp))
	    (params (if flag nil (list (nth 2 inp))))
	    (modexp (nth (if flag 3 4) inp)))
	(ci$!process_definition
	 `("obj" ,name ,@params "is" (("pr" ,modexp ".")) "endo")))))
     ((equal "view" (car inp))
      (mod_eval$view_eval inp))
     ((member (car inp) '("rl" "red-loop") :test #'equal)
      (let ((arg (cadr inp)))
      (ci$red_loop (if (equal "." arg) *mod_eval$$last_module* arg))))
     ((member (car inp) '("in" "input") :test #'equal)
      (unless (or top-level *obj$input_quiet*)
        (format t "Reading in file : ~s~%" (cadr inp)))
      (setq in-in t)
      (obj_input (cadr inp))
      (unless (or top-level *obj$input_quiet*)
        (format t "Done reading in file: ~s~%" (cadr inp)))
     )
     ((equal (car inp) "test")
      (ci$perform_test_reduction inp))
     ((equal "call-that" (car inp))
      (if *mod_eval$open_module*
	  (let ((obj$current_module *mod_eval$open_module*)
		(*mod_eval$$current_module* *mod_eval$open_module*))
	  (module$!mark_as_needs_parse_setup *mod_eval$$current_module*)
	  (mod_eval$$include_BOOL)
	  (mod_eval$$!do_let (list "let" (cadr inp) "=" nil "."))
	  (module$!mark_as_needs_parse_setup *mod_eval$$current_module*)
	  (when (module$is_compiled *mod_eval$$current_module*)
	    (module$!mark_as_needs_compiling *mod_eval$$current_module*)))
	(progn (princ "Warning: no module open") (terpri))))
     ((equal "parse" (car inp))
      (unless *obj$input_quiet*
      (if *mod_eval$$last_module*
	(progn
	  (mod_eval$$!do_parse_setup *mod_eval$$last_module*)
	  (let ((res (parse$parse *mod_eval$$last_module*
		       (obj$obj2_term (cadr inp)) *obj$sort_Universal*)))
	  (setq $$term res)
	  (unless top-level (princ "parse "))
	  (print$short_sort_name (term$sort res))
	  (princ ": ")
	  (let ((*fancy-print* nil))
	    (term$print res)
	    (terpri)
	    )))
	(progn (princ "No current module") (terpri))
       )))
     ((member (car inp) mod_elts :test #'equal)
      (if *mod_eval$open_module*
	  (let ((obj$current_module *mod_eval$open_module*)
		(*mod_eval$$current_module* *mod_eval$open_module*))
	  (mod_eval$!module_element inp)
	  )
	(progn
	  (princ "Warning: no module open") (terpri)
	  ))
      )
     ((equal "openr" (car inp))
      (setq *mod_eval$last_before_open* nil)
      (mod_eval$!open inp))
     ((equal "open" (car inp)) ;just an abbreviation
      (when *mod_eval$open_module*
	(princ "Warning: module already open: ")
	(print$name *mod_eval$open_module*) (terpri)
	(princ "Closing this module") (terpri)
	(close_module inp)
	(setq *mod_eval$$current_module* *mod_eval$$last_module*)
	)
      (when (cadr inp)
	(setq *mod_eval$$last_module*
	    (modexp_eval$top_level_eval
	     (cadr inp) *mod_eval$$current_module*)))
      (setq *mod_eval$last_before_open* *mod_eval$$last_module*)
      (let ((*obj$allow_uninstantiated* t))
      (if (eq 'object (module$kind *mod_eval$$last_module*))
	(ci$!process_definition
	 `("obj" "%" "is" (("inc" ("THE-LAST-MODULE") ".")) "endo"))
	(ci$!process_definition
	 `("th" "%" "is" (("inc" ("THE-LAST-MODULE") ".")) "endth"))))
      (let ((obj$current_module *mod_eval$$last_module*))
      (let ((*mod_eval$$last_module*
	     (caar (module$sub_modules
		    *mod_eval$$last_module*))))
      (mod_eval$$!add_vars_of '("vars-of" "."))))
      (mod_eval$!open nil)
      )
     ((equal "close" (car inp))
      (close_module inp))
;     ((equal "apply" (car inp)) ;@@
;      (ci$!apply_rule inp))
     ((equal "start" (car inp)) ;@@ put above module_elements?
      (misc$start inp))
;     ((equal "start-term" (car inp))
;      (misc$start_term))
     ((equal "apply" (car inp))
      (misc$apply inp))
;lhh -- insert the compile command
     ((equal "compile" (car inp))
      (misc$compile inp))
;lhh -- insert the run command
     ((equal "run" (car inp)) 
      (misc$run inp))
     ((member (car inp) '("show" "sh" "set" "do" "select")
	      :test #'equal)
      (unless (and
	       *obj$input_quiet*
	       (or (equal "show" (car inp)) (equal "sh"(car inp))))
      (top$commands inp))
      )
     ((equal "[" (car inp))
      (setq *obj$current_labels* (mod_eval$process_labels (cadr inp))))
     ((equal '("?") inp)
      (obj_top_level_help))
     ; DANGER these are system dependent
     ((equal "pwd" (car inp))
      #+GCL (progn (princ (namestring (probe-file "."))) (terpri))
      #+LUCID (progn (princ (working-directory)) (terpri))
      #+(or CMU CLISP) (progn (princ (namestring (default-directory))) (terpri))
      )
     ((equal "ls" (car inp))
      #+GCL (system "ls")
      #+LUCID (lucid::%execute-system-command "ls")
      #+(or CMU CLISP) (ext:run-program "ls" nil :output t)
      )
     ((equal "cd" (car inp))
      #+GCL (let ((fn (expand_file_name (cadr inp)))
                           (si::*break-enable* nil))
                       (if (probe-file (concatenate 'string fn "/"))
                           (si::chdir fn)
                           (progn (princ "Directory not found") (terpri))))
      #+LUCID (unless
		  (lucid:%set-working-directory (expand_file_name (cadr inp)))
		(princ "Directory not found") (terpri))
      #+CMU (let ((fn (expand_file_name (cadr inp))))
	      (if (probe-file (concatenate 'string fn "/"))
		(lisp::%set-default-directory fn)
		(progn (princ "Directory not found") (terpri))))
      #+CLISP (progn (princ "cd unsupported under CLISP") (terpri))
      )
     )
   (setq *obj$print_errors* t)
  ))
   (when *obj$postcmd_hook*
     (funcall *obj$postcmd_hook* top-level inp)
     )
   (when in-in
     (setq *obj$print_errors* t)
     (setq in-in nil))
   ))
  ))

(defun close_module (inp)
  (let ((saved_open *mod_eval$open_module*))
    (mod_eval$!close inp)
    (when (and saved_open (equal "%" (module$name saved_open)))
      (modexp_eval$delete_module_all saved_open)
      (setq *mod_eval$$last_module* *mod_eval$last_before_open*)
      ))
  (setq *mod_eval$$current_module* *mod_eval$$last_module*)
  (setq *mod_eval$last_before_open* nil))
  
(defvar mod_elts '(
  "dfn" "define"  "ex" "extending"   "pr" "protecting"  "us" "using"
  "inc" "including"  "sort" "sorts"  "bsort"  "psort" "principal-sort"
  "subsort" "subsorts"  "op" "ops"  "let" "var" "vars"  "vars-of"
  "as"  "op-as"  "eq"  "ceq" "cq"  "beq" "bq"  "cbeq" "cbq"
  ))

(defun print_ident (e)
  (cond
   ((or (equal "red" (car e)) (equal "reduce" (car e))))
   ((or (equal "--->" (car e)) (equal "***>" (car e)))
    (print$simple_princ_open e) (terpri))
   ((or (equal "---" (car e)) (equal "***" (car e))))
   ((or (equal "ev" (car e)) (equal "eval" (car e)))
    (princ (car e)) (princ " ") (prin1 (cadr e)) (terpri))
   ((or (equal "evq" (car e)) (equal "eval-quiet" (car e)))
    (princ (car e)) (terpri))
   ((member (car e) '("show" "sh" "mod" "set" "do" "select" "open" "openr")
	    :test #'equal)
    (princ (car e)) (princ " ")
    (when (cadr e) (print$simple_princ_open (cadr e)))
    (terpri))
   ((equal "." (car e)))
   ((or (equal "apply" (car e)) (equal "start" (car e)))
    ) ; nothing
   ((member (car e) mod_elts :test #'equal)
    (print$simple_princ_flat e) (terpri))
   ((equal "[" (car e)) ; nothing
    )
; lhh - make the compile command print nicely
   ((equal "compile" (car e)) 
    (princ (car e)) (princ " ")
    (when (cadr e) (print$simple_princ_open (cadr e)))
    (terpri)
   )
; lhh - make the run command print nicely
   ((equal "run" (car e)) 
    (princ (car e)) (princ " ")
    (when (cadr e) (print$simple_princ_open (cadr e)))
    (terpri)
   )
   (t
    (princ (car e))
    (when (cadr e) (princ " ") (princ (cadr e)))
    (terpri))))

(defun debug_setup (f l)
  (setq $$debug f  $$debug_level l))

(defun dbg ()
  (debug_setup t 30))

(defun dbn ()
  (debug_setup nil 0))

(defun obj3-init ()
  (reader$!read_init)
  (general_read$$!init))

(defun obj3-break ()
  (terpri)
  (obj3-indicate-position)
  (princ "returning to top level") (terpri)
  (throw 'obj3-top-level-error t))

(defun obj3-to-top ()
  (obj3-indicate-position)
  (princ "returning to top level") (terpri)
  (throw 'obj3-top-level-error t))

; lhh -- something wrong with our trim command:
(defun recover-trim-error ()
  (princ "returning to top level") (terpri)
  (throw 'obj3-top-level-error t))

(defun obj3-return-to-top ()
  (throw 'obj3-top-level-error t))

(defun obj3-indicate-position ()
  (when *obj$input_source* ; nil means may be from terminal
  (princ "filename: ") (princ *obj$input_source*)
  (when (file-position #-CLISP *standard-input* #+CLISP (make-stream :input))
    (princ " in top-level form ending at character position: ")
    (prin1 (file-position #-CLISP *standard-input* #+CLISP (make-stream :input))))
  (terpri)))

(defun obj3-init-files ()
  (let ((*obj$input_quiet* t))
    (if (probe-file "./.obj3")
      (obj_input_file "./.obj3")
      (let ((home (or 
		   (namestring (user-homedir-pathname))
		   (get-env-var "HOME"))))
	(when home
	  (let ((dot-obj3 (concatenate 'string home "/.obj3")))
	    (when (probe-file dot-obj3) (obj_input_file dot-obj3)))))))
  (let ((val (get-env-var "OBJ3INIT")))
    (when (and val (probe-file val)) (obj_input_file val)))
  )

; lhh -- help screen for compile command.
(defun obj_top_level_help ()
  (princ "Top-level definitional forms include: obj, theory, view, make")
      (terpri)
  (princ "The top level commands include:") (terpri)
  (princ "  q; quit --- to exit from OBJ3") (terpri)
  (princ "  show .... .  --- for further help: show ? .") (terpri)
  (princ "  set .... . --- for further help: set ? .") (terpri)
  (princ "  do .... . --- for further help: do ? .") (terpri)
  (princ "  apply .....  --- for further help: apply ? .") (terpri)
  (princ "  other commands:") (terpri)
  (princ "    in <filename>") (terpri)
  (princ "    red <term> .") (terpri)
  (princ "    run [verbose|keep] <term> .") (terpri)
  (princ "    compile [verbose|noopt|keep] [<module-expression>] .") (terpri)
  (princ "    select <module-expression> .") (terpri)
  (princ "    cd <directory>; ls; pwd") (terpri)
  (princ "    start <term> .; show term .") (terpri)
  (princ "    open [<module-expression>] .;")
      (princ " openr [<module-expression>] .; close") (terpri)
  (princ "    ev <lisp>; evq <lisp>") (terpri)
  )

#+(or LUCID CMU CLISP)
(defun lcl () nil)
#+(or LUCID CMU)
(defun bye ()
  (finish-output)
  (quit))
#+LUCID
(defun usual-output-case ()
  (and
   (eq 'synonym-stream (type-of *standard-output*))
   (eq '*terminal-io* (lucid::synonym-stream-symbol *standard-output*))
   (eq 'lucid::split-stream (type-of *terminal-io*))
   (typep (lucid::split-stream-output-stream *terminal-io*)
	  'lucid::osi-buffered-stream)
   ))
#+LUCID
(defun buffer-on ()
  (when (usual-output-case)
    (setf (lucid::osi-buffered-stream-auto-force
	   (lucid::split-stream-output-stream *terminal-io*))
	  nil)))
#+LUCID
(defun buffer-off ()
  (force-output)
  (when (usual-output-case)
    (setf (lucid::osi-buffered-stream-auto-force
	   (lucid::split-stream-output-stream *terminal-io*))
	  t)))
#+LUCID
(defun buffer-line ()
  (when (usual-output-case)
    (setf (lucid::osi-buffered-stream-auto-force
	   (lucid::split-stream-output-stream *terminal-io*))
	  :line)))
#+LUCID
(defun buffer-mode ()
  (when (usual-output-case)
    (lucid::osi-buffered-stream-auto-force
     (lucid::split-stream-output-stream *terminal-io*))))

#+GCL
(defun process-args ()
  (catch *top-level-tag* (catch 'obj3-top-level-error (catch 'obj3-error
  (let ((argc (si:argc)))
    (when (< 1 argc)
      (let ((i 1))
	(loop
	 (when (<= argc i) (return))
	 (if (equal "-in" (si:argv i))
	   (obj_input (si:argv (setq i (+ i 1))))
	 (if (equal "-inq" (si:argv i))
	   (let ((*obj$input_quiet* t))
	     (obj_input (si:argv (setq i (+ i 1)))))
	 (if (equal "-evq" (si:argv i))
	     (load_file (si:argv (setq i (+ i 1))))
	   )))
	 (setq i (+ i 1))
	 ))))
  )))
  )

;; -*- mode: lisp; font-lock-mode: nil; -*-
