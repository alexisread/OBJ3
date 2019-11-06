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

;; $Id: rule_ring.lsp,v 205.2.1.1 2003/09/23 13:48:22 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;         Implementation of the ring of rules
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Claude Kirchner ;;;; Created: June 3, 1986 

;;;;;;;;;;;;;;;; Summary

; op rule_ring$create: list[rule] -> rule_ring 
; op rule_ring$!add_rule: RuleRing -> RuleRing .
; op rule_ring$initialize: rule_ring -> rule .
; op rule_ring$next: rule_ring -> rule
; op rule_ring$set_mark: rule_ring -> rule_ring .
; op rule_ring$end_test: rule_ring -> Bool .
; op rule_ring$is_empty: RuleRing -> Bool .
; op rule_ring$ring2list: RuleRing -> List[Rule] .

;;;;;;;;;;;;;;;; Representation ;;;;;;;;;;;;;;;;

;; A ring of rules is represented has in JPJ's code by a circular list with
;; 2 pointeurs, one for the next rule to be returned and one for
;; the last rule which has been successfully apply.
;; Be carefull for printing! (and debugging)

(defstruct rule_ring
  ring
  current
  mark)

;;;;;;;;;;;;;;;;

; returns a new rule_ring built from a rule of rules. 
; Z: "l_rule" is modified.

; op rule_ring$create: list[rule] -> rule_ring -- the list of rule is modified
;                                              -- only if the list # nil.
(defun rule_ring$create (l_rule)
  (if l_rule
      (make-rule_ring :ring (rplacd (last l_rule) l_rule)
		      :current l_rule
		      :mark l_rule)
      (make-rule_ring :ring nil
		      :current nil
		      :mark nil)
      )
  )

; add a new rule with same top in the rule ring which is modified

; op rule_ring$!add_rule: RuleRing Rule -> RuleRing .
(defun rule_ring$!add_rule (rr rule)
  (let ((ring (rule_ring-ring rr)))
    (if ring
	(rplacd ring (push rule (cdr ring)))
        (let ((new_ring (list rule)))
	  (setf (rule_ring-ring rr) (rplacd new_ring new_ring))
	  )
      )
    )
  )

; initialize a rule_ring, that is put the current and mark pointeurs
; at the "beginning of the list". Returns the rule under current.

; op rule_ring$initialize: rule_ring -> rule .
(defun rule_ring$initialize (rr)
  (setf (rule_ring-current rr) (rule_ring-ring rr))
  (setf (rule_ring-mark rr) nil)
  (car (rule_ring-current rr))
  )

; returns the next rule in the ring rule

; op rule_ring$next: rule_ring -> rule 
(defun rule_ring$next (rr)
  (unless (rule_ring-mark rr) (rule_ring$set_mark rr))
  (let ((rules (cdr (rule_ring-current rr))))
    (setf (rule_ring-current rr) rules)
    (car rules))
  )

; set the mark to the current rule

; op rule_ring$set_mark: rule_ring -> rule_ring .
(defun rule_ring$set_mark (rr)
  (setf (rule_ring-mark rr) (rule_ring-current rr))
  )

; returns true iff it is the end, i.e. iff no more rule of the ring
; can be apply, that is iff current and mark are the same.

; op rule_ring$end_test: rule_ring -> Bool .
(defun rule_ring$end_test (rr)
  (eq (rule_ring-current rr) (rule_ring-mark rr))
  )

; op rule_ring$!print: RuleRing -> Output .
;(defun rule_ring$!print (rr)
;  (do ((rule (rule_ring$initialize rr) (rule_ring$next rr))
;       )
;      ((rule_ring$end_test rr))
;      (rule$!print rule)
;      )
;  )

; op rule_ring$is_empty: RuleRing -> Bool .
(defun rule_ring$is_empty (rr)
  (null (rule_ring-ring rr))
  )

; op rule_ring$ring2list: RuleRing -> List[Rule] .
; *ck21may87*
(defun rule_ring$list (rr)
  (let ((list nil))
    (do ((rule (rule_ring$initialize rr) (rule_ring$next rr)))
	((rule_ring$end_test rr))
	(push rule list)
      )
    list
    )
  )

; op rule_ring$member : Rule RuleRing -> Bool
(defun rule_ring$member (r rr)
  (do ((rule (rule_ring$initialize rr) (rule_ring$next rr)))
      ((rule_ring$end_test rr) nil)
      (when (rule$similar r rule) (return t)))
  )

; add a new rule with same top in the rule ring which is modified
; op rule_ring$!add_rule: RuleRing Rule -> {RuleRing} Bool .
; uses rule$similar to see if the rule already appeats, if so keep higher
; return membership indication

(defun rule_ring$!adjoin_rule (rr rule)
  (do ((r (rule_ring$initialize rr) (rule_ring$next rr)))
      ((rule_ring$end_test rr))
      (when (rule$similar r rule)
	(let ((module obj$current_module))
	(unless module (break "rule_ring$!adjoin_rule: need current module"))
	(let ((so (module$sort_order module))
	      (newlhs (rule$lhs rule))
	      (oldlhs (rule$lhs r)))
	  (when (and
		 (not (term$is_var newlhs))
		 (not (term$is_var oldlhs))
		 (not (eq (term$head newlhs) (term$head oldlhs)))
		 (sort_order$is_included_in so
		   (term$sort oldlhs)
		   (term$sort newlhs)))
	    (rplaca (rule_ring-current rr) rule))
	  (return-from rule_ring$!adjoin_rule t) ; don't add
	  ))))
    (rule_ring$!add_rule rr rule) ; default is to add
    nil
  )

; op rule_ring$copy rule_ring -> rule_ring
(defun rule_ring$copy (rule_ring)
  (let ((ring (rule_ring-ring rule_ring)))
  (make-rule_ring
   :ring ring
   :current ring
   :mark nil
  )))
