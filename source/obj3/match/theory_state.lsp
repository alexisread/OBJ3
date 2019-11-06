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

;; $Id: theory_state.lsp,v 205.2.1.1 2003/09/23 14:09:52 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Make the junction between all the states used by the different 
; theories.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; Claude Kirchner ;;;; Created: April 86.

;;;;;;;;;;;;;;;; Summary

; op theory_state$initialize: TheoryState System Environment -> TheoryState
; op theory_state$next_state: TheoryName TheoryState -> TheoryState

;;;;;;;;;;;;;;;; Representation

;theory_state = one_of[empty: null,
;                     A: A$state,
;                     C: C$state,
;                     I: I_state,
;                     Z: Z_state,
;                     AC: AC$state,
;                     AI: AI_state,
;                     AZ: AZ_state,
;                     CI: CI_state,
;                     CZ: CZ_state,
;                     IZ: IZ_state,
;                     ACI: ACI_state,
;                     ACZ: ACZ_state,
;                     AIZ: AIZ_state,
;                     CIZ: CIZ_state,
;                     ACIZ: ACIZ_state
;                                         ]

; returns a new theory_state
; initialize= proc(th_name: theory_name, sys) returns(cvt)


(defun theory_state$initialize (th_name sys env)
  (funcall (theory_info$init th_name) sys env))

; op theory_state$initialize: TheoryState System Environment -> TheoryState
;&&&& consider dropping env parameter from empty$state_initialize?
#|
(defun theory_state$initialize_direct (th_name sys env)
  (cond ((theory_name$is_empty th_name) (empty$state_initialize sys env))
	((theory_name$is_AC th_name) (AC$state_initialize sys env))
	((theory_name$is_C th_name) (C$state_initialize sys env))
	((theory_name$is_A th_name) (A$state_initialize sys env))
	((theory_name$is_Z th_name) (empty$state_initialize sys env))
	((theory_name$is_I th_name) (empty$state_initialize sys env))
	((theory_name$is_IZ th_name) (empty$state_initialize sys env))
	((theory_name$is_CI th_name) (C$state_initialize sys env))
	((theory_name$is_CZ th_name) (C$state_initialize sys env))
	((theory_name$is_CIZ th_name) (C$state_initialize sys env))
	((theory_name$is_AI th_name) (A$state_initialize sys env))
	((theory_name$is_AZ th_name) (A$state_initialize sys env))
	((theory_name$is_AIZ th_name) (A$state_initialize sys env))
	((theory_name$is_ACI th_name) (AC$state_initialize sys env))
	((theory_name$is_ACZ th_name) (AC$state_initialize sys env))
	((theory_name$is_ACIZ th_name) (AC$state_initialize sys env))
	(t (princ "theory_state$initialize: OOPS! watch me!") (terpri))
	)
  )
|#

; compute the next state

; op theory_state$next_state: TheoryName TheoryState -> System State
;                                                       signals(no_more)
(defun theory_state$next_state (th_name th_state)
  (funcall (theory_info$next th_name) th_state))

#|
(defun theory_state$next_state_direct (th_name th_state)
  (cond ((theory_name$is_empty th_name) (empty$next_state th_state)) 
	((theory_name$is_AC th_name) (AC$next_state th_state))
	((theory_name$is_A th_name) (A$next_state th_state))
	((theory_name$is_C th_name) (C$next_state th_state))
	((theory_name$is_Z th_name) (empty$next_state th_state))
	((theory_name$is_I th_name) (empty$next_state th_state))
	((theory_name$is_IZ th_name) (empty$next_state th_state))
	((theory_name$is_CI th_name) (C$next_state th_state))
	((theory_name$is_CZ th_name) (C$next_state th_state))
	((theory_name$is_CIZ th_name) (C$next_state th_state))
	((theory_name$is_AI th_name) (A$next_state th_state))
	((theory_name$is_AZ th_name) (A$next_state th_state))
	((theory_name$is_AIZ th_name) (A$next_state th_state))
	((theory_name$is_ACI th_name) (AC$next_state th_state))
	((theory_name$is_ACZ th_name) (AC$next_state th_state))
	((theory_name$is_ACIZ th_name) (AC$next_state th_state))
	)
  )
|#
