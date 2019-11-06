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

;; $Id: macros.lsp,v 205.2.1.1 2003/09/23 14:12:02 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Macros
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Interpreted / Compiled variants

; if nil then in "compilation mode"
; if t then always use defun

(defvar *macro-mode* nil)

(defmacro duo (x y)
  (when (not (eq 'defun (car y)))
    (princ "#######################################") (terpri)
    (princ "Warning duo used in wrong order ")
    (prin1 (cadr y)) (terpri)
    (rotatef x y))
  (if *macro-mode*
      `(progn
	 (eval-when (compile eval) ,x) ; the macro
	 (eval-when (load) ,y) ; the defun
	 )
    y ; load for eval mode
    ))

(defmacro compile-time (x) (declare (ignore x)) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; macros for binary uses of every

(defmacro every2 (fn l1 l2)
  (let* ((lmbd (cadr fn))
	 (args (cadr lmbd))
	 (bdy (cddr lmbd)))
    `(let ((lst1 ,l1) (lst2 ,l2) ,@args)
       (loop
	(when (or (null lst1) (null lst2)) (return t))
	(setq ,(car args) (car lst1))
	(setq ,(cadr args) (car lst2))
	(unless (progn ,@bdy) (return nil))
	(setq lst1 (cdr lst1))
	(setq lst2 (cdr lst2))
	)
       )))

(defmacro every2fn (fn l1 l2)
  (let ((fcn (cadr fn)))
    `(let ((lst1 ,l1) (lst2 ,l2))
       (loop
	(when (or (null lst1) (null lst2)) (return t))
	(unless (,fcn (car lst1) (car lst2)) (return nil))
	(setq lst1 (cdr lst1))
	(setq lst2 (cdr lst2))
	)
       )))

; variant of the above that requires equal length lists for true
(defmacro every2len (fn l1 l2)
  (let* ((lmbd (cadr fn))
	 (args (cadr lmbd))
	 (bdy (cddr lmbd)))
    `(let ((lst1 ,l1) (lst2 ,l2) ,@args)
       (loop
	(when (null lst1) (return (null lst2)))
	(when (null lst2) (return (null lst1)))
	(setq ,(car args) (car lst1))
	(setq ,(cadr args) (car lst2))
	(unless (progn ,@bdy) (return nil))
	(setq lst1 (cdr lst1))
	(setq lst2 (cdr lst2))
	)
       )))

(defmacro every2lenfn (fn l1 l2)
  (let ((fcn (cadr fn)))
    `(let ((lst1 ,l1) (lst2 ,l2))
       (loop
	(when (null lst1) (return (null lst2)))
	(when (null lst2) (return (null lst1)))
	(unless (,fcn (car lst1) (car lst2)) (return nil))
	(setq lst1 (cdr lst1))
	(setq lst2 (cdr lst2))
	)
       )))

; (every #'(lambda (x) ...) lst)
(defmacro every1len (fn lst)
  (let* ((lmbd (cadr fn))
	 (arg (car (cadr lmbd)))
	 (bdy (cddr lmbd)))
    `(let ((lst1 ,lst) ,arg)
       (loop
	(when (null lst1) (return t))
	(setq ,arg (car lst1))
	(unless (progn ,@bdy) (return nil))
	(setq lst1 (cdr lst1))
	)
       )))
