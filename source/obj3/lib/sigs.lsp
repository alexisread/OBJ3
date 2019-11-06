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

;; $Id: sigs.lsp,v 206.1 2003/09/26 13:02:36 kiniry Exp $

; Signal handling.
; DANGER -- this is implementation dependent.

;; Default implementations as placeholders.
(defun signalfn (o p) nil)
(defun batr (o) nil)
(defun sig-setup () nil)
          
#+GCL (progn
        (defCfun "static object signalfn(x,y) object x,y;" :int
          "signal(fix(x),(void (*)())(fix(y)));"
          "Creturn(Cnil);")
        (defentry signalfn (object object) (object signalfn))

        (defun return-to-top-level ()
          (let ((val *top-level-tag*)) (throw val val)))

        (defCfun "static object signalhandler()" :int
          ((signal-handler-fn))
          "printf(\"Signalhandler continued -- return to top\\n\");"
          ((return-to-top-level)))
        (defun signal-handler-fn ()
          (terpri) (princ "***Fatal Error***") (terpri)
          (break))

        (defCfun "static object errorhandler()" :int
          ((error-handler-fn))
          "printf(\"Errorhandler continued -- return to top\\n\");"
          ((return-to-top-level)))
        (defun error-handler-fn ()
          (break))

        (defCfun "static object signal4fn()" :int
          "printf(\"Signal 4: Illegal instruction\\n\");"
          "errorhandler();")
        (defCfun "static object signal7fn()" :int
          "printf(\"Signal 7: EMT instruction\\n\");"
          "errorhandler();")
        (defCfun "static object signal8fn()" :int
          "printf(\"Signal 8: Floating point exception\\n\");"
          "errorhandler();")
        (defCfun "static object signal10fn()" :int
          "printf(\"Signal 10: Bus error\\n\");"
          "errorhandler();")
        (defCfun "static object signal11fn()" :int
          "printf(\"Signal 11: Segmentation violation\\n\");"
          "errorhandler();")

        ;; This was originally written with a switch statement but
        ;; that resulted in bus errors with some versions of KCL (on
        ;; SUN when -J flag is used for as bug not for cc)

        (defCfun "static object batr(x) object x;" :int
          "{ int val;"
          "val = fix(x);"
          "if (4 == val) Creturn(make_fixnum((int)signal4fn));"
          "if (7 == val) Creturn(make_fixnum((int)signal7fn));"
          "if (8 == val) Creturn(make_fixnum((int)signal8fn));"
          "if (10 == val)  Creturn(make_fixnum((int)signal10fn));"
          "if (11 == val)  Creturn(make_fixnum((int)signal11fn));"
          "Creturn(make_fixnum((int)signalhandler));"
          "}")
        (defentry batr (object) (object batr))

        (defun sig-setup ()
          (dolist (s '(4 7 8 10 11))
            (signalfn s (batr s)))))

