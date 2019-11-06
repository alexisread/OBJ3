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
# $Id: obj3.sh,v 206.2 2003/09/26 13:06:32 kiniry Exp $
#

#
# OBJ3 load
#

source obj3-config.sh

$lisp_compiler << \tio

(progn 
    #+(or LUCID CMU) (load "impl.lsp")
    (setq *print-case* :downcase)
    
    (in-package :user)

    #+GCL (setq si::*default-time-zone* -1)  ;;please set your time zone
    (load "obj3-files.lsp" :verbose nil) (terpri)
    
    (print_char 60 "%") (terpri)
    (format t "Loading version: ~a~%" +version+)
    (print_char 60 "%") (terpri) 
    
    (dolist (file *obj3_files*)
      (load file)
      (print_char 60 "=") (terpri))
    
    (defvar obj3-load-time (get-time-string))
    (setq *obj3_files* nil)
    
    #+GCL (setq si::*break-enable* t)
    (obj3-init)
    
    (let (($$debug nil))
      (obj_input "prelude/obj3sp"))

;;     #+GCL (defun si:top-level ()
;;             (sig-setup)
;;             (timer)
;;             (let ((res (catch *top-level-tag* (obj3) 'ok-exit)))
;;               (if (eq res 'ok-exit)
;;                   (bye)
;;                   (format t "OBJ3 loading error~%"))))
    
    #+GCL (progn
            (si:allocate 'cons 400)
            (si:allocate 'symbol 120)
            (si:allocate 'string 100)
            (si:allocate-relocatable-pages 300)
    
            (gbc t))

    (defun #+GCL si:top-level #+(or LUCID CMU) obj3-top-level ()
           #+GCL (sig-setup)
           #+CMU (progn (lisp::set-auto-gc-trigger 2000000)
                        (gc-no-trace))
           (timer)
           (let ((res (catch *top-level-tag* (obj3) 'ok-exit)))
             (if (eq res 'ok-exit)
                 #+GCL (bye) #+CMU (quit)
                 (progn
                   (format t "OBJ3 loading error~%")
                   (terpri)))))

    (obj3))
