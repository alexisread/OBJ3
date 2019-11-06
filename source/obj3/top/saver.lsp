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

;; $Id: saver.lsp,v 205.2.1.1 2003/09/23 14:04:33 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; LUCID or Sun Common Lisp OBJ3 state saver
; DANGER -- this is implementation dependent
; This is implementation specific
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'user)

(defmacro save-var (var)
  `(progn (defvar ,var) (setq ,var ',(symbol-value var))))

(defmacro obj3-save-vars ()
 `(progn
(save-var obj3-load-time)
(save-var obj3-version)
(save-var obj3_status_env)

(save-var *modexp_eval$canon_env*)
(save-var *modexp_eval$env*)
(save-var *modexp_eval$view_env*)

(save-var *operator$name_table*)
(save-var *operator$retract_table*)
(save-var mod_eval$creation_counter)

(save-var the_empty_property)
(save-var the_Z_property)
(save-var the_I_property)
(save-var the_IZ_property)
(save-var the_C_property)
(save-var the_CZ_property)
(save-var the_CI_property)
(save-var the_CIZ_property)
(save-var the_A_property)
(save-var the_AZ_property)
(save-var the_AI_property)
(save-var the_AIZ_property)
(save-var the_AC_property)
(save-var the_ACZ_property)
(save-var the_ACI_property)
(save-var the_ACIZ_property)
(save-var theory$the_empty_theory)

(save-var obj_BUILT-IN$keyword)
(save-var obj_LISP$keyword)
(re-install-prelude)
  ; end of save-vars
  ))

(defun obj3-save-state (file)
  (let ((filename (concatenate 'string file ".lisp"))
	(filebin (concatenate 'string file ".sbin")))
    (if (probe-file filename)
	(progn
	  (format t "Sorry the file '~a' exists.~%" filename)
	  'failed)
    (if (probe-file filebin)
	(progn
	  (format t "Sorry the file '~a' exists.~%" filebin)
	  'failed)
      (progn
	(with-open-file (*standard-output* filename :direction :output)
	  (format t "(in-package 'user)~T")
	  (format t "(obj3-save-vars)~%")
	  )
	(compile-file filename)
	(delete-file filename)
  )))))
