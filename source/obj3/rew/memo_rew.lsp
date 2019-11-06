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

;; $Id: memo_rew.lsp,v 205.2.1.1 2003/09/23 13:48:08 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;        The memo rewriting table
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Claude Kirchner ;;;; Created: June 9, 1986

;;;;;;;;;;;;;;;; Summary

;; memo_rew$get_normal_form
; op memo_rew$apply_rules : Term MemoTable Strategy -> Term .

;;;;;;;;;;;;;;;; Representation

;; Use the Hash table facility of Common Lisp.
;; See possible improvment in the text of get_normal_form.

(defvar *memo_rew$table* nil)

;; returns a memo table. 
;; The default initial size is small and is automaticaly update by the system
;; when it grows.
;; op memo_rew$create_memo_table: -> MemoTable.

(defun memo_rew$create_memo_table ()
  (when (null *memo_rew$table*)
  (setq *memo_rew$table*
	(hashtab$make 9001 #'hash$term #'term$similar) ;&&&& bigger size?
	)
  ))

(defun memo_rew$clean_memo_table (memo)
  (hashtab$clear memo)
  memo
  )

;; Returns the normal form of the term for the current environment.
;; Let "term_nu" be the term obtained from "term" by cleaning
;; all annotations like the "reduced" flag, from "term".
;; If "term_nu" is allready in the table, then the associated value is
;; returned.
;; If not, then the normal form of "term" is computed and stored in the memo
;; table under "nu_term".
;; Note that instead of computing "nu_term", it would be more efficient
;; to have a particular function determining how the two keys have to
;; be compared.
;; op memo_rew$get_normal_form: Term MemoTable Strategy -> Term .

(defun memo_rew$normal_form (term memo_table strategy)
  (let* (term_nu
         (normal_form (hashtab$get term memo_table)))
    (unless normal_form
	(setq term_nu (term$clean_term term)) ;need to save a copy
	;; compute the normal form of "term"
	(rew$!reduce term strategy nil)
	(setq normal_form term)
	;; store the normal form
	(hashtab$set term_nu memo_table normal_form)
	)
    normal_form
    )
  )

; op memo_rew$!apply_rules : Term MemoTable Strategy -> Term .
(defun memo_rew$!apply_rules (term memo_table strategy end_reductions)
  (let* (term_nu
         (normal_form (hashtab$get term memo_table)))
    (unless normal_form
	(setq term_nu (term$clean_term term)) ;need to save a copy
	;; compute the normal form of "term"
	(rew$!apply_rules term strategy end_reductions)
	(setq normal_form term)
	;; store the normal form
	(hashtab$set term_nu memo_table normal_form)
	)
    normal_form
    )
  )
