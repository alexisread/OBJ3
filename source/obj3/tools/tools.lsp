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

;; $Id: tools.lsp,v 206.1 2003/09/26 13:03:36 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; some tools for working with Common Lisp
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; completely expand a function definition
(defun m-e-fn (fn)
  (cond
   ((not (fboundp fn))
    (prin1 "no function definition") (terpri))
   (t (setf (symbol-function fn)
	    (m-e-all (symbol-function fn))))))

(defun m-e-all (fm)
  (cond
   ((atom fm) fm)
   (t (macroexpand
       (cons (car fm)
	     (mapcar #'m-e-all (cdr fm)))))))

(defun me (fm)
  (do ((x (m-e-all fm) (m-e-all x))
       (l fm x))
      ((equal x l) x)))

;;; easily look at function definition
(defmacro sf (x) `(symbol-function ',x))

;;; clear screen -- specific to Sun's
(defvar cl)
(setq cl (intern (string (code-char 12))))
;;; move window to bottom -- specific to sun's
(defvar b)
(setq b (intern (concatenate 'string (string (code-char 27)) "[6t")))

;;; useful for finding location of error in a file
;;;   -- replace all defun's by %defun
;;;   -- reload to locate error
(defmacro %defun (&rest fm)
  (princ (car fm) *terminal-io*) (terpri *terminal-io*) (cons 'defun fm))

;;; evaluate with output to a file
(defmacro to-file (file &rest forms)
  `(with-open-file (*standard-output* ,file :direction :output)
      ,@forms))

;;; conditionally include a file

(defvar *files* nil)

(defun include (file)
  (when (not (member file *files* :test 'equal))
	(push file *files*)
	(load file)))

;;;; Make GCL be more nearly pure Common-Lisp

#+GCL
(eval-when (compile)
  (setq *eval-when-compile* nil))

(defun CLCL ()
  (eval-when (compile load eval)
    (setq *print-case* :upcase)))

;;;; load a file from any of several places
(defvar *directories* '())

(defun expand-file-name (fname)
  (if (and (eql #\~ (char fname 0)) (eql #\/ (char fname 1)))
    (concatenate 'string
        (namestring (user-homedir-pathname)) (subseq fname 2))
    fname))

(defun probe-load-file (file)
  (or (probe-file file)
      (probe-file (concatenate 'string file ".lsp"))
      (probe-file (concatenate 'string file
          #+GCL ".o"
	  #+(and LUCID (not SPARC)) ".lbin"
	  #+(and LUCID SPARC) ".sbin"
	  #+(and CMU SPARC) ".sparcf"
          #+(and CMU X86) ".x86f"
          #+CLISP ".fas"))))

(defun load-file (file)
  (let ((efile (expand-file-name file)))
  (if (probe-load-file efile) (load efile)
      (dolist (place *directories* 'error)
	(let ((name (expand-file-name (concatenate 'string place "/" file))))
	  (when (probe-file name)
	    (return (load name))))))))

;;;; conditionally include a file
(defvar *files* nil)

(defun load-file-if-needed (file)
  (when (not (member file *files* :test 'equal))
    (when (not (eq 'error (load-file file)))
	(push file *files*))))

;;;; echo file
(defun echo-file (file)
  (with-open-file (*standard-input* file)
    (let (inp)
    (loop
     (setq inp (read *standard-input* nil '--EOF--))
     (when (eq '--EOF-- inp) (return))
     (print-identification inp) (terpri)
    ))
  ))

(defun print-identification (form)
  (cond
   ((atom form) (prin1 form))
   ((null (cdr form)) (prin1 form))
   (t (prin1 (car form)) (princ " ") (prin1 (cadr form)))))
