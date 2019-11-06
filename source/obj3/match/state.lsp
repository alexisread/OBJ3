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

;; $Id: state.lsp,v 205.2.1.1 2003/09/23 14:09:52 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                 State
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; Claude Kirchner ;;;; Created: April 86.

;;;;;;;;;;;;;;;; Summary



;;; Documented by state.clu

;;; state = cluster is get_match_system, initialize, create, next_state

; new in the rep is true iff the state has to be initialised
; i.e. it is the first call for this state

;rep = record{new: bool,
;             match_system : match_system,
;             sys_to_solve : system, % the system to solve in 
;                                    % which the solving
;                                    % is currently in the state "state".
;             th: theory, % the theory in which the system to be solved is
;             theory_state : one_of ...}

(defstruct (state
	    (:constructor make_state
	        (new match_system  sys_to_solve th_name theory_state)))
  new
  match_system
  sys_to_solve
  th_name
  theory_state)

; op get_match_system : State -> Match_System
(defun state$match_system (st)
 (state-match_system st)
)

(defun state$sys_to_solve (st)
  (state-sys_to_solve st)
  )

; returns a new state
; op state$create : Match_System System Theory_name Theory_State -> State
(defmacro state$create (m_sys sys_to_solve th_name theory_state)
  `(make_state t ,m_sys ,sys_to_solve ,th_name ,theory_state))

; returns a non empty decomposed and merged 
; Initialize a state in which a match system "m_sys" has been inserted.
; "m_s" is supposed to be merged and ready for mutation i.e. decomposed

; state$initialize : Term Term -> State NoMatch
; [ck: Mar  4 87]: never used!
(defun state$initialize (t1 t2) 
  (multiple-value-bind (m_sys no_match) 
		       (match_system$dec_merg (match_system$new t1 t2))
    (if no_match 
	(values nil t)
	(multiple-value-bind (sys th_name) 
			     (match_system$extract_one_system m_sys)
	  (values (state$create
		    m_sys sys th_name (theory_state$initialize th_name sys))
		  nil) 
	  )))
  )

; new state and true if a next state exists or
; an empty state and false otherwise
; --- Assume that the system to be solved is non empty.
; 1) looks for the next solution in the theory_state
; 2) it modifies the theory_state if there is a next one
; 3) it returns a completely new state.

; op state$next_state : State -> State No_More
(defun state$next_state (st)
  (let ((th_name (state-th_name st))  (th_state (state-theory_state st)))
    ; computes the next solution of th_state
    ; we quit this loop either if there is no more new th_state
    ; or a new match system has been computed
    (loop
	(multiple-value-bind (sys  new_th_state no_more)
	    (theory_state$next_state th_name th_state)
	  (if no_more 
	      (return (values nil t))
	      ; "match_system$add" performs the decomposition and merging
	      ; and must not destroy the current match system
	      (multiple-value-bind (new_m_sys no_match)
		  ; create a new merged match_system containing the old one 
		  ; and add sys
		  (match_system$add (state$match_system st) sys)
		; if there is no_match, continue (the loop)
		; else
		(unless no_match
		  ; try to return the  new state
		  (multiple-value-bind (sys_to_solve th_name) 
		      (system$extract_one_system 
			(match_system$system 
			new_m_sys))
		   (if (null sys_to_solve)
		       (return
			(values
			 (state$create
			  new_m_sys
			  nil the_empty_property
			  empty_empty_state) ;see empty.lsp
			 nil))
		    (multiple-value-bind (th_st no_match)
			(theory_state$initialize th_name sys_to_solve
						 (match_system$env new_m_sys))
		      ; if no match, try another theory_state
		      (unless no_match 
			  ; else modify the th_state of st
			  (setf (state-theory_state st) new_th_state)
			  ; and returns
			  (return 
			    (values
			      (state$create 
				(match_system$modif_m_sys new_m_sys 
							  sys_to_solve)
				sys_to_solve th_name th_st)
			      nil)))
		      )))))) ;;if
	  )
      ) ; loop
  ))
