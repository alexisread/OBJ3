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

;; $Id: fns.lsp,v 205.2.1.1 2003/09/23 13:49:14 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;      miscellaneous low-level or debugging functions
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ev (e) (modexp_eval$eval e))

(defun l (n) (setq *print-level* n))

(defun vn () (print$name (lcl)) nil)

(defun vs () (print$struct (lcl)) nil)

(defun indx (l)
  (do ((res nil (cons (list i (car lst)) res))
       (i 0 (1+ i))
       (lst l (cdr lst)))
      ((null lst) (nreverse res))))

(defun rep ()
  (let ((+ nil) (* nil) ($+ nil) ($/ nil)
	(*standard-input* *terminal-io*))
  (loop
    (princ ">") (finish-output) ;CMU
    (setq $+ (read))
    (if (eq 'quit $+) (return 'done))
    (catch 'rep
    (setq $/ (multiple-value-list (eval $+)))
    (dolist (v $/) (print$struct v) (terpri))
    (setq + $+ * (car $/) / $/)
    );catch
    )
  ))

(defun out () (throw 'rep nil))

; Can nicely be used with LISP
(defun read$tokens (end)
  (let ((res nil) in)
  (setq reader$$ch 'space)
  (loop
    (setq in (reader$read))
    (when (or (eq *reader$eof_value* in)
	      (and end (equal (car in) end)))
      (return))
    (setq res (append res in))
  )
  (general_read$$!init)
  (setq reader$$ch 'space)
  res
  ))

(defun obj_read_term ()
  (let ((mod obj$current_module))
  (mod_eval$!setup_reduction mod)
  (general_read$$!init)
  (parse$parse_ground "a reduction the"
      mod (read$tokens ".") *obj$sort_Universal*)
  ))

(defun obj_read_term_from_file (file)
  (with-open-file (*standard-input* file) (obj_read_term)))
