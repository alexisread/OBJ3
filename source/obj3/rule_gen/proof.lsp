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

;; $Id: proof.lsp,v 205.2.1.1 2003/09/23 13:46:20 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                          Proof
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Prove-theorems just keeps trace that for non violating the 
; protecting or extending link between MOD and MOD1, the user
; has to inductively prove the assertions in RULE-SET-TO-PROPAGATE.
; They are added to the field "assertions" in the module MOD, with 
; a link to MOD1.

;   op prove-theorems : Module RuleSet Module HierarchyMode-> Module .

;   eq : prove-theorems(MOD, RULE-SET, MOD1, HIERARCHY-MODE)
;        =
;        add-assertions(MOD, RULE-SET, MOD1, HIERARCHY-MODE)

; op proof$prove_theorem: Module RuleSet Module HierarchyMode-> Module

#|
(defun proof$prove_theorem (MOD1 RS MOD2 HM)
   (when (and $$debug RS)
	(princ "----------------------------------------------") (terpri)
	(princ "I suppose that the set of rules:")(terpri)
	(dolist (r RS)
		(print$rule r)(terpri)
		)
	(princ "inherited from: ")
	(print$name MOD1)
	(princ " which is ")
	(princ HM)
	(princ " ")
	(print$name MOD2) (terpri)
	(princ "is inductively true ... are you sure?")(terpri)
	(princ "----------------------------------------------") (terpri)
	(module$!add_assertions MOD1 RS MOD2 HM)
	)
  )	
|#
