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

;; $Id: gctrace.lsp,v 206.1 2003/09/26 13:02:36 kiniry Exp $

; GC tracing output.

; DANGER -- this is implementation dependent.

; Can be eliminated by defining "do nothing" functions gc-trace and
; gc-no-trace or by editing uses of these out of top/misc.

(defun gc-trace () nil)
(defun gc-no-trace () nil)
(defun gc-show (x) nil)
  
#+GCL
(defCfun "int gc_start()" :int
  "printf(\"[GC...\"); fflush(stdout);")

#+GCL
(defCfun "int gc_end()" :int
  "printf(\"GC]\"); fflush(stdout);")

#+GCL
(defCfun "object gctron()" :int
  "extern int (*GBC_enter_hook)();"
  "extern int (*GBC_exit_hook)();"
  "GBC_enter_hook = &gc_start;"
  "GBC_exit_hook = &gc_end;"
  "Creturn(Cnil);")

#+GCL
(defentry gc-trace () (object gctron))

#+GCL
(defCfun "object gctroff()" :int
  "extern int (*GBC_enter_hook)();"
  "extern int (*GBC_exit_hook)();"
  "GBC_enter_hook = NULL;"
  "GBC_exit_hook = NULL;"
  "Creturn(Cnil);")

#+GCL
(defentry gc-no-trace () (object gctroff))

#+LUCID
(defun gc-show (x)
  (case x
    (:before (princ "[GC..."))
    (:after (princ "GC]"))
    (:dynamic-expansion (princ "Dyn..."))
    (:reserved-expansion (princ "Res..."))
    (otherwise (prin1 x))))

#+LUCID
(defun gc-trace ()
  (setq *gc-silence* #'gc-show))

#+LUCID
(defun gc-no-trace ()
  (setq *gc-silence* nil))

#+CMU
(defun gc-trace ()
  (setq ext:*gc-verbose* t))

#+CMU
(defun gc-no-trace ()
  (setq ext:*gc-verbose* nil))
