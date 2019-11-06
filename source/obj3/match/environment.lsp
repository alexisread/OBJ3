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

;; $Id: environment.lsp,v 205.2.1.1 2003/09/23 14:09:51 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; An environment is a particular kind of system the equation of
; which are of the form (x == t) where x is a variable and t a term
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; Claude Kirchner ;;;; Created: April 86.

;;;;;;;;;;;;;;;; Summary

; op environment$create: Term Term -> Environment
; op environment$new: -> Environment
; op environment$list_eq: Environment -> List[Equation] .
; op environment$insert_if_coherent_with: 
;                Environment Environment System System -> Environment~,
;                                                         signals(no_coherent)
; op environment$add_eq: Environment Equation -> Environment~
; op environment$copy1: Environment -> Environment .
; op environment$environment2substitution: Environment -> Substitution
; op environment$image: Environment Variable -> Term or nil

;;;;;;;;;;;;;;;; Representation

; rep = list[equation] or (nil.list[equation))

;;;;;;;;;;;;;;;;

; op environment$create: Term Term -> Environment
(defun environment$create (t1 t2)
  (list (cons t1 t2))
  )

; op environment$new: -> Environment
(defun environment$new ()
  (cons nil nil)
  )

; op environment$list_eq: Environment -> List[Equation] .
(defun environment$list_eq (env)
  (if (null (car env))
      (cdr env)
      env)
  )
   
; insert the equation in "sys"  in
; the environment "new_env" iff they are of the form x == t and are
; not present in "test_env". 
; If x == t' is present into test_env and t' =/ t then it
; signals no_coherent.
; Then all equations present in "test_env" are added to "new_env"
; "test_env" must not be modified.
; U: used by "match_system$dec_merg" and "match_system$add"

; op environment$insert_if_coherent_with: 
;        Environment Environment System List[Equation] -> Environment~ System~
;                                                         signals(no_coherent)
(defun environment$insert_if_coherent_with (new_env test_env new_sys list_eq)
  ;; note that new_env and new_sys are both initialy of the form (nil.nil)
  ;; in order to insert without pb look at ** after
  (block the_end
     (dolist (eq list_eq)
	     (let ((t1 (match_equation$t1 eq))
		   (t2 (match_equation$t2 eq)))
	       (cond ((term$similar t1 t2) ()) ; no thing to do
		     ((term$is_var t1)
		      ;; This is point of difference with OBJ2
		      ;; check of the sort information; redundant with
		      ;; "match_equation$$!decomposition".
		      ;; test of the sort of the variable,
		      ;; NO test is done if the variable is general
		      (unless 
		       (or (variable$is_general_variable t1)
			   (let ((s2 (term$sort t2)))
			   (if obj$current_module
			       (let ((so (module$sort_order
					    obj$current_module)))
				(dolist (s1 (variable$sorts t1) nil)
				  (when  (sort_order$is_included_in so s2 s1)
					 (return t))))
			     (dolist (s1 (variable$sorts t1) nil)
			       (when (sort_order$is_included_in 
				      (module$sort_order (sort$module s1))
				      s2 s1)
				  (return t)))))
			   )
		       (return-from the_end t))
		      ;; new_env may be  modified
		      (let ((image_of_t1 (assoc t1 test_env)))
			(if image_of_t1
			    (if (not (term$equational_equal
                                         (cdr image_of_t1) t2))
				(return-from the_end t)) ; i.e  no_coherent
			  (let ((image_of_t1_in_new (assoc t1 new_env)))
			    (if image_of_t1_in_new
			      (if (not (term$equational_equal
                                           (cdr image_of_t1_in_new) t2))
				  (return-from the_end t))
			      (environment$add_eq new_env eq)))) ;; **
			) ;; let
		      )
		     (t (system$add_eq new_sys eq)))))
     ;; add now all the equation of test_env into new_env (copy test_env)
     (cond ((null (car test_env)) () )
	   ((null (car new_env))
	    (let ((l (environment$copy1 test_env)))
	      (rplaca new_env (car l))
	      (rplacd new_env (cdr l))) )
	   (t (nconc new_env test_env))
	   ) ;; cond
     nil			; i.e. the new_env is coherent
     )				; block
  )

; op environment$add_eq: Environment Equation -> Environment~
(defun environment$add_eq (env eq)
  (if (null (car env))
      (rplaca env eq)
      (rplacd env (push eq (cdr env))))
  )

; returns a copy of one level of the environment

; op environment$copy1: Environment -> Environment .
(defmacro environment$copy1 (env)
  `(copy-list ,env))

; op environment$environment2substitution: Environment -> Substitution
(defun environment$environment2substitution (env)
  env ;; was (env) [ck: Dec  2 87]
  )

; op environment$image: Environment Variable -> Term or nil
(defun environment$image (env var)
  (if (null (car env))
    (cdr (assoc var (cdr env)))
    (cdr (assoc var env)))
  )
