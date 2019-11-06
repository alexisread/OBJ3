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

;; $Id: variable.lsp,v 206.1 2003/09/23 13:41:34 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                Variables in OBJ3
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; Claude Kirchner ;;;; Created: 10 June, 86

;;;;;;;;;;;;;;;; Summary

; op variable$make_var : Token Sort -> Variable
; op variable$make_new_var : String Sort -> Variable
; op variable$sorts: Variable -> List[Sort]
; op variable$name: Variable -> Name
; op variable$!add_inherited_sort: Variable Sort -> Variable~ .
; op variable$collect_working_sorts: Variable -> List[Sort]
; op variable$is_general_variable: Variable -> Bool .
; op variable$!make_it_general: Variable -> Variable~ .
; op variable$!put_sort_list: Variable -> Variable~ .
; op variable$copy: Variable -> Variable .
; op variable$!replace_var_by_term: Variable Term -> !Term .

;;;;;;;;;;;;;;;; Representation

; a variable is represented by
;        .
;       / \
;      x   .
;         / \
;      gen   .
;           / \
;       sorts work
;
; * x is the name of the variable (it is a token),
;     (more certainly it is an atom which printing name is the token)
; * sorts is a non empty list of sorts of the variable it must be a set
;   (i.e. no repetition)
; * gen is a boolean true iff the variable is general,
; * work is a possibly empty list of sorts used as a working area 
;   associated to the variable.

; It is not the same in obj2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Returns a new variable

; op variable$make_var: Token Sort -> Variable .

(defun variable$make_var (token sort)
  (cons token (cons nil (cons (list sort) nil)))
  )

; Returns a new variable begining with string

; op variable$make_new_var: String Sort -> Variable .

(defun variable$make_new_var (string sort)
;  (cons (gensym string) (cons nil (cons (list sort) nil)))
  (cons (concatenate 'string string "_E")
	(cons nil (cons (list sort) nil))) ;&&&& 24 Nov 87
  )

; Returns the initial sort of the variable (gived by the user)
; For the rules given by the user, the variable appearing in the left hand 
; side have only one sort.

; op variable$initial_sort Variable -> Sort .
(defmacro variable$initial_sort (var)
  `(car (variable$sorts ,var))
  )

; Returns the list of sorts of the variable "var"

; op variable$sorts: Variable -> List[Sort] .
(defmacro variable$sorts (var)
  `(caddr ,var)
  )

; op variable$name: Variable -> Name
(defmacro variable$name (var)
  `(car ,var)
  )

; Add a sort to the working SET of sorts associated to the variable.

; op variable$!add_inherited_sort: Variable Sort -> Variable~ .
(defun variable$!add_inherited_sort (var sort)
  (setf (cdddr var) (adjoin sort (cdddr var)))
  )

; Returns the list of working sorts associated to a variable

; op variable$collect_working_sorts: Variable -> List[Sort]
(defun variable$collect_working_sorts (var)
  (cdddr var)
  )

; Returns true iff var is general

; op variable$is_general_variable: Variable -> Bool .
(defun variable$is_general_variable (var)
  (cadr var)
  )

; modify the variable to make it general.

; op variable$!make_it_general: Variable -> Variable~ .
(defun variable$!make_it_general (var)
  (setf (cadr var) t)
  )

; store the list of sorts in the sorts field of var

; op variable$!put_sort_list: Variable  -> Variable~ .
(defun variable$!put_sort_list (var list_sort)
  (setf (caddr var) list_sort)
  )

; Returns a copy of the variable with the SAME printing name.
;        .
;       / \
;   name   .
;         / \
;      nil   .
;           / \
;       sorts  nil

; op variable$copy: Variable -> Variable .
(defun variable$copy (var)
  (list
   (variable$name var)
   nil
   (variable$sorts var)
   )
  )

; replace a variable by a term: modify the argument!!!
; Use the knowledge of the structure of a term: Dangerous!!!

; op variable$!replace_var_by_term: Variable Term -> !Term .
(defun variable$!replace_var_by_term (var term)
  (rplaca var (car term))
  (rplacd var (cdr term))
  var
  )

; op variable$is_same_var : Variable Variable -> Bool .
(defun variable$is_equal (x y)
  (or
   (eq x y) ; This is just a "short-cut" -- not worth it?
   (and
    (equal (variable$name x) (variable$name y))
    (eq (variable$initial_sort x) (variable$initial_sort y)))))
