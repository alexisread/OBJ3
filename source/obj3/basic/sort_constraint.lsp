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

;; $Id: sort_constraint.lsp,v 206.1 2003/09/23 13:40:40 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                    sort constraints
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Tim Winkler ;;;; Created: July 24, 86

;;;;;;;;;;;;;;;; Summary ;;;;;;;;;;;;;;;;
; op sort_constraint$create : Operator Operator LIST[Variables] Term ->
;        Sort_Constraint
; op sort_constraint$operator : Sort_Constraint -> Operator
; op sort_constraint$constrained_operator : Sort_Constraint -> Operator
; op sort_constraint$variables : Sort_Constraint -> LIST[Variables]
; op sort_constraint$condition : Sort_Constraint -> Term


; op sort_constraint$create : Operator Operator LIST[Variables] Term ->
;        Sort_Constraint
(defun sort_constraint$create (op1 op2 vars cnd)
  (list 'sort_constraint op1 op2 vars cnd))

; op sort_constraint$operator : Sort_Constraint -> Operator
(defun sort_constraint$operator (sc)
  (cadr sc))

; op sort_constraint$constrained_operator : Sort_Constraint -> Operator
(defun sort_constraint$constrained_operator (sc)
  (caddr sc))

; op sort_constraint$variables : Sort_Constraint -> LIST[Variables]
(defun sort_constraint$variables (sc)
  (cadddr sc))

; op sort_constraint$condition : Sort_Constraint -> Term
(defun sort_constraint$condition (sc)
  (nth 3 sc))

; op sort_constraint$is_sort_constraint : Universal -> Bool
(defun sort_constraint$is_sort_constraint (x)
  (and (consp x) (eq 'sort_constraint (car x))))
