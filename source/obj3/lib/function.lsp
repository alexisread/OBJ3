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

;; $Id: function.lsp,v 206.1 2003/09/26 13:02:36 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Functions for Lisp Functions
; DANGER -- this is implementation dependent
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; These next two are used in module_eval.lsp for built-in RHSs.
(defun make_function (lmbd)
  #+GCL lmbd
  #+LUCID lmbd
  #+CLISP lmbd
  #+CMU (eval:make-interpreted-function lmbd)
  )

;; E.g., (function_lambda_expression (function-build 'bar '(x) '(princ x)))
;;   ==> (LAMBDA (X) (BLOCK BAR PRINC X))    ; CMU
;;       (LAMBDA-BLOCK BAR (X) PRINC X)      ; GCL
;;       (LAMBDA (X) (DECLARE (SYSTEM::IN-DEFUN BAR)) (BLOCK BAR (PRINC X))) ; CLISP

(defun function_lambda_expression (fcn)
  #+GCL fcn
  #+LUCID fcn
  #+CLISP (function-lambda-expression fcn)
  #+CMU (if (consp fcn) fcn
	  (let ((val (function-lambda-expression fcn)))
	    (if val val fcn)))
  )

;; Many of these functions are used in obj_trace.lsp

;; Defines a function named 'name' with args 'args' and body 'body'.

#+GCL
(defun function-build (name args body)
  `(lambda-block ,name ,args ,@body))

#+LUCID
(defun function-build (name args body)
  `(named-lambda ,name ,args ,@body))

#+CMU
(defun function-build (name args body)
  (eval:make-interpreted-function
   `(lambda ,args (block ,name ,@body))))

#+CLISP
(defun function-build (name args body)
  (eval `(defun ,name ,args ,body)))

(defun define-function (name def) (setf (symbol-function name) def))

#+GCL
(defun is-compiled (x)
  (if (symbolp x) (and (fboundp x) (is-compiled (symbol-function x)))
    (eq 'compiled-function (type-of x))))

#+LUCID
(defun is-compiled (x)
  (if (symbolp x) (and (fboundp x) (is-compiled (symbol-function x)))
    (zerop (logand (sys:procedure-ref x sys:procedure-flags)
		   lucid:procedure-is-interpreted-bit))))

#+CMU
(defun is-compiled (x)
  (if (symbolp x) (and (fboundp x) (is-compiled (symbol-function x)))
    (compiled-function-p x)))

#+CLISP
(defun is-compiled (x)
  (compiled-function-p x))

;; E.g., (function-code (function-build 'bar '(x) '(princ x)))
;;   ==> (LAMBDA-BLOCK BAR (X) PRINC X)          ; GCL
;;       (LAMBDA (X) (BLOCK BAR PRINC X))        ; CMU
;;       (LAMBDA (X) (DECLARE (SYSTEM::IN-DEFUN BAR)) (BLOCK BAR (PRINC X))) ; CLISP

#+GCL
(defun function-code (x)
  (if (symbolp x)
    (symbol-function x)
    x))

#+LUCID
(defun function-code (x)
  ; only if interpreted
  (if (symbolp x) (function-code (symbol-function x))
    (sys:procedure-ref x sys:procedure-symbol)))

#+CMU
(defun function-code (x)
  (if (symbolp x)
    (function-lambda-expression (symbol-function x))
    (function-lambda-expression x)))

#+CLISP
(defun function-code (x)
  (function-lambda-expression x))

;; E.g., (function-body (function-build 'bar '(x) '(princ x)))
;;   ==> (PRINC X)                  ; GCL CMU CLISP

#+GCL
(defun function-body (x)
  (if (symbolp x) (function-body (symbol-function x))
    (cdddr x)))

#+LUCID
(defun function-body (x)
  ; only if interpreted
  (if (symbolp x) (function-body (symbol-function x))
    (cdddr (sys:procedure-ref x sys:procedure-symbol))))

#+CMU
(defun function-body (x)
  (if (symbolp x) (function-body (symbol-function x))
    (cddr (caddr (function-code x)))))

#+CLISP
(defun function-body (x)
  (if (symbolp x) (function-body (symbol-function x))
    (caddr (cadddr (function-code x)))))

;; E.g., (function-args (function-build 'bar '(x) '(princ x)))
;;   ==> (X)                       ; GCL CMU CLISP

#+GCL
(defun function-args (defn)
  (if (eq 'lambda-block (car defn)) (caddr defn)
    (break "function-args")))

#+LUCID
(defun function-args (defn)
  (let ((res (sys:procedure-ref defn sys:procedure-arglist)))
    (if res res
      (break "function-args"))))

#+CMU
(defun function-args (defn)
  (let ((code (function-code defn)))
    (if (and (consp code) (eq 'lambda (car code)))
      (cadr code)
      nil)))

#+CLISP
(defun function-args (defn)
  (cadr (function-code defn)))
