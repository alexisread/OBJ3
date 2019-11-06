#!/bin/bash

#
# OBJ3 2.06,2.08,2.09 Copyright (c) 2000-2003 Joseph Kiniry, Joseph Goguen
# OBJ3 2.05 Copyright (c) 2000 Sula Ma, Joseph Kiniry, Joseph Goguen
# OBJ3 2.04 Copyright (c) 1988,1991,1993 SRI International
# TRIM Copyright (c) 1994,2001 Lutz Hamel
# All Rights Reserved
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
# 
#   o Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
# 
#   o Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
# 
#   o Neither name of the Joseph Kiniry, Joseph Goguen, Sula Ma, or SRI
#   International, nor the names of its contributors may be used to
#   endorse or promote products derived from this software without
#   specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL SRI
# INTERNATIONAL OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
# STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
# OF THE POSSIBILITY OF SUCH DAMAGE.

#
# $Id: obj3-compile.sh,v 206.1 2003/09/26 13:05:57 kiniry Exp $
#

#
# OBJ3 compile
#

#+(or LUCID CMU) (load "impl.lsp")

source obj3-config.sh

$lisp_compiler << _EOF_

(progn
#+CMU (progn
        (setq *load-verbose* t)
        (defvar *load-print-stuff* t)
        (setq *block-compile-default* nil)
        (setq *compile-verbose* t)
        (setq *compile-print* t)
        ;; (lisp::set-auto-gc-trigger 2000000)
        (gc)
        (defun check-gc (n)
          (lisp::default-gc-notify-before n)
          (when (< 30000000 n)
            (break "Compile fails with pathological GC"))
          nil)
        (setq ext:*gc-notify-before* 'check-gc))

#+CMU (defun process_defs (file)
        (let (in (eof '(eof)))
          (when *load-verbose* 
            (princ "; Processing definitions from ")
            (princ file) (terpri))
          (with-open-file (*standard-input* file)
            (loop
               (setq in (read *standard-input* nil eof))
               (when (eq eof in) (return))
               (when (consp in)
                 (if (member (car in)
                             '(defmacro
                               defconstant
                               defrepr
                               defstruct
                               defvar))
                     (eval in)
                     (when (and (eq 'eval-when (car in))
                                (member 'compile (cadr in)))
                       (eval in))))))))

#+CMU (compile 'process_defs)

(defun do-compile-file (file)
  (let* (#+(and KCL (not SPARC)) 
           (compiler::*use-buggy*
            (member file '("top/module_eval" "modexp/modexp_eval") :test #'equal))
	 (nm (concatenate 'string file ".lsp"))
	 (pn (probe-file nm)))
    (if pn
      (let* ((onm (concatenate 'string file
		    #+GCL ".o"
		    #+(and LUCID (not SPARC)) ".lbin"
		    #+(and LUCID SPARC) ".sbin"
		    #+CMU ".x86f"
		    ))
	     (opn (probe-file onm)))
	(if (or (not opn)
		(< (file-write-date onm) (file-write-date nm)))
            (progn
	      (princ "Compiling file: ") (princ file) (terpri)
	      (compile-file #-CMU file #+CMU nm)
	      (terpri))
	  (progn (princ "Already up to date: ") (princ file) (terpri)))
	'done)
      (progn (princ "No such lisp file as: ") (princ file) (terpri)
	     'err))))

#+GCL (proclaim '(optimize (speed 3) (safety 1) (space 0) (compilation-speed 0)))
#+CMU (proclaim '(optimize (speed 3) (safety 0) (space 0) (debug 0) (compilation-speed 0)))
#+CLISP (proclaim '(optimize (speed 3) (safety 0) (space 0) (debug 0) (compilation-speed 0)))

#|(|
; uncomment this for more optimized code with LUCID
#+LUCID (progn
(compiler-options :read-safety nil)
(compiler-options :write-safety nil)
(compiler-options :bounds-check nil)
(compiler-options :fast-entry t)
(compiler-options :tail-merge t)
)
|)|#

#+GCL (progn
        (defun do-compile-file (file)
          (let* ((flsp (concatenate 'string file ".lsp"))
                 (flsp_p (probe-file flsp)))   
            (if flsp_p
                (let* ((fo (concatenate 'string file ".o"))
                       (fo_p (probe-file fo)))
                  (if  (or (not fo_p)
                           (< (file-write-date fo) (file-write-date flsp)))
                       (progn (compile-file flsp) (terpri))
                       (format t "File already up to date: ~a~%" fo)))
                (progn (format t "File not found: ~a~%" flsp)
                       (bye))))))

(load "obj3-files.lsp" :verbose nil) (terpri)
(print_char 60 "%") (terpri)
(format t "Compiling version: ~a~%" +version+ )
(print_char 60 "%") (terpri) 

(dolist (file *obj3_files*)  
  #-CMU (load (concatenate 'string file ".lsp") :verbose nil)
  #+CMU (process_defs (concatenate 'string file ".lsp"))
  )

(dolist (file *obj3_files*)
  (do-compile-file file) 
  (print_char 60 "=") (terpri))
(quit)
'done
)
_EOF_

exit 0
