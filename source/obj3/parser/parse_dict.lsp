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

;; $Id: parse_dict.lsp,v 205.2.1.1 2003/09/23 13:50:24 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                    Parser Dictionary
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Tim Winkler ;;;; Created: 7/29/86

;;;;;;;;;;;;;;;; Summary ;;;;;;;;;;;;;;;;
; structure parse_dictionary[table : Table; builtins : List]
; op dictionary$make_empty_table : -> DictionaryHashTable
; op dictionary$make_empty : -> Dictionary
; op dictionary$info_on_token : Dictionary Token -> List[Operator+Variable]
; op dictionary$!add_info_on_token : Dictionary Token Value -> {Dictionary}
; op dictionary$!add_built_in_sort : Dictionary Info -> {Dictionary}
; op dictionary$!delete_info_on_token : Dictionary Token Value -> {Dictionary}

; op dictionary$get_token_info : Table Token -> List[Operator+Variable]
; op dictionary$!replace_token_info : Table Token List[Operator+Variable]
; op dictionary$!delete_token_info : Table Token Value -> {Table}
; op dictionary$!add_token_info : Table Token Value -> {Table}
; op dictionary$$make_built_in_constant : Token BuiltinInfo : -> Operator

; 1/6/87 Note: Needed to add global var for "additional" variables to
;     to be used in parsing; needed in handling of views (add'l vars
;     declared. This is not ideal, but prefer it at this point to
;     building a whole new parsing environment (i.e. a module)

;;;; parse dictionary
;;; dictionary is a structure -- two main parts
;;;   hash table for ordinary parsing information
;;;   list for builtin information ("intensional")

(defstruct (parse_dictionary
	    (:conc-name dictionary$)
	    (:constructor dictionary$make))
  (table (dictionary$make_empty_table))
  (built_ins nil)
  )
; built_ins is a list of: (token_predicate sort constructor token_to_value)
; parse_dictionary is extended by:
(defvar *parser$variables* nil) ;alist of vars-token/list of var's for token

; op dictionary$make_empty_table : -> DictionaryHashTable
;; should only be used in initialization above
(defun dictionary$make_empty_table ()
  (make-hash-table :test #'equal :size 50))

; op dictionary$make_empty : -> Dictionary
(defun dictionary$make_empty ()
  (dictionary$make))

; op dictionary$info_on_token : Dictionary Token -> List[Operator+Variable]
(defun dictionary$info_on_token (dictionary token)
  (let ((res (dictionary$get_token_info (dictionary$table dictionary) token)))
  (let ((res2 (assoc token *parser$variables* :test #'equal)))
    (when res2
      ;@@ now that vars are left in modules want *parser$variables* to replace
      (setq res (cons (cdr res2) (dictionary$delete_vars res))))
    (let ((pos nil))
      (dolist (bi (dictionary$built_ins dictionary))
        (when (funcall (car bi) token) (push bi pos)))
      (when pos
      (if (cdr pos)
	(let ((so (module$sort_order obj$current_module)))
	(dolist (bi pos)
	  (let ((bisrt (cadr bi)))
	  (unless (some #'(lambda (x)
			    (sort_order$is_strictly_included_in
			     so (cadr x) bisrt))
		    pos)
	    (push (dictionary$$make_built_in_constant token (cdr bi)) res))))
	);let
	(push (dictionary$$make_built_in_constant token (cdr (car pos))) res)
      ));when/if
    );let
    (let ((val (dictionary$check_retract_name token)))
      (when val (push val res)))
    res
  )))

(defun dictionary$delete_vars (lst)
  (if (dolist (e lst nil) (when (consp e) (return t)))
      (let ((res nil))
	(dolist (e lst) (unless (consp e) (push e res)))
	(nreverse res))
    lst)
  )

; op dictionary$!add_info_on_token : Dictionary Token Value -> {Dictionary}
(defun dictionary$!add_info_on_token (dictionary token value)
  (dictionary$!add_token_info (dictionary$table dictionary) token value)
  )

; op dictionary$!add_built_in_sort : Dictionary Info -> {Dictionary}
(defun dictionary$!add_built_in_sort (dictionary info)
  (let ((sort (cadr info)))
  (unless (some #'(lambda (i) (eq sort (cadr i)))
	    (dictionary$built_ins dictionary))
    (push info (dictionary$built_ins dictionary)))
  ))

; op dictionary$!delete_info_on_token : Dictionary Token Value -> {Dictionary}
(defun dictionary$!delete_info_on_token (dictionary token value)
  (dictionary$!delete_token_info (dictionary$table dictionary) token value)
  )

;;;; functions on table component of dictionary

; op dictionary$get_token_info : Table Token -> List[Operator+Variable]
(defun dictionary$get_token_info (table token)
  (gethash token table))

; op dictionary$!replace_token_info : Table Token List[Operator+Variable]
;     -> {Table}
(defun dictionary$!replace_token_info (table token values)
  (setf (gethash token table) values))

; op dictionary$!delete_token_info : Table Token Value -> {Table}
(defun dictionary$!delete_token_info (table token value)
  (let ((values (dictionary$get_token_info table token)))
  (if (member value values :test #'eq)
      (let ((new_values (remove value values :test #'eq)))
	(if (null new_values)
	    (remhash token table)
	  (dictionary$!replace_token_info table token new_values))))))

; op dictionary$!add_token_info : Table Token Value -> {Table}
(defun dictionary$!add_token_info (table token value)
  (dictionary$!replace_token_info table token
    (adjoin value (dictionary$get_token_info table token) :test #'eq)))

; op dictionary$$make_built_in_constant : Token BuiltinInfo : -> Operator
(defun dictionary$$make_built_in_constant (token info)
  (catch 'direct-value
  (let ((value (funcall (cadr info) token))
	(sort (car info)))
    (operator$create_intrinsic
      (list 'constant value) nil sort (sort$module sort)
      theory$the_empty_theory nil nil nil t
      (rule_ring$create nil) nil
      0 (list (cons 'token token)) 'antefix)
  )))

(defun dictionary$check_retract_name (token)
  (and
   (stringp token) ; defensive for a reason
   (<= 5 (length token))
   (eql #\r (char token 0))
   (eql #\: (char token 1))
   (let ((pos (position #\> token)))
   (and
    pos
    (< 2 pos)
    (let ((arnm (subseq token 2 pos))
	  (coarnm (subseq token (1+ pos))))
    (let ((ar (mod_eval$$find_sort_in obj$current_module arnm))
	  (coar (mod_eval$$find_sort_in obj$current_module coarnm)))
      (operator$make_named_retract token ar coar)
   )))))
  )
