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

;; $Id: match.lsp,v 205.2.1.1 2003/09/23 14:09:52 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;        The top level of the matching process
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; Claude Kirchner ;;;; Created: April 86.

;;;;;;;;;;;;;;;; Summary

; op match$first_match: Term Term -> GlobalState Substitution
;                                    signals(no_match, E_equal)
; op match$next_match: GlobalState -> GlobalState Substitution, 
;                                     signals(no_match) .
; op match$empty_match: Term Term -> Substitution, signals(no_match)

;;;;;;;;;;;;;;;; Notes

; The differences between OBJ2 and OBJ3, i.e. between Unsorted and 
; Order Sorted matching is that
;  1) OBJ2 test operators for equality (with "eq") and OBJ3 test only 
;     operators for equality of they names.
;     It appears in "$$decomposition :: equation.lsp"
;  2) OBJ2 do not test the sort of the variable:
;     It appears in "$$decomposition :: equation.lsp"

;;;;;;;;;;;;;;;;

; op match$first_match: Term Term -> GlobalState Substitution 
;                                    signals(no_match, E_equal)
(defun match$first_match (t1 t2)
  (multiple-value-bind (m_sys no_match) 
      (match_system$dec_merg (match_system$new t1 t2))
    ; note that is the two terms are similar then "m_sys" is empty,
    ; In the current code it is not signaled "E_equal", 
    ; it must be corrected (ck-04/11/86)
    (if no_match 
	(values nil nil t nil)
	(let ((gst (global_state$new)))
	  (cond ((system$is_empty (match_system$system m_sys))
		 (values gst 
			 (match_system$match_system2substitution m_sys) 
			 nil nil))
		((match_system$E_equal m_sys) 
		 (values nil nil nil t))
		(t (multiple-value-bind 
		     (sys th_name) (match_system$extract_one_system m_sys)
		     ;; the matching system is not modified,
		     ;; thus we create a new match_system
		     (multiple-value-bind (th_st no_match)
			 (theory_state$initialize th_name sys
						  ;; [ck: Nov 29 87]
						  (match_system$env m_sys))
		       (if no_match
			   (values nil nil t nil)
			   (multiple-value-bind (new_gst subst no_match)
			       (match$next_match 
				 (global_state$push 
				  gst 
				  (state$create 
				   (match_system$modif_m_sys m_sys sys)
				   sys
				   th_name
				   th_st)))
			     (values new_gst subst no_match nil))))
		     )))
	  ) ;; let
	) ;; if
    )
  ;;(print "out_match$first_match")
  )

; next_match suppose that the system on top of gst is fully 
; decomposed and merged.
; next_match = proc(gst: global_state) returns(global_state, substitution)
;                                      signals(no_match)

; op match$next_match: GlobalState -> GlobalState Substitution, 
;                                     signals(no_match) .
(defun match$next_match (gst)
  (block the_end 
    (let (st)
    (loop
      (when (global_state$is_empty gst) (return)) ;; exit loop
      (when (and $$debug (< 10 (length gst))) (break "TOO-BIG"))
      (setq st (global_state$top gst))
      (multiple-value-bind (new_st no_more) (state$next_state st)
 	(if no_more 
	    (setq gst (global_state$pop_top gst))
	    (progn (setq gst (global_state$push gst new_st))
		   (let* ((m_sys (state$match_system new_st))
			  (sys (match_system$system m_sys)))
		     (if (and (system$is_empty sys)
			      (system$is_empty (state$sys_to_solve new_st)))
	; popping: the reasoning is that a successful state also terminates
			 (progn
			   (setq gst (global_state$pop_top gst))
			 (return-from the_end
			      (values gst
				      (match_system$match_system2substitution
				       m_sys)
				      nil)
			      )
			 );; progn
		       ) ;; if
		     ) ;; let
		   );; progn
	    ) ;; if
	)
      )) ;; end let/loop
    (values nil nil t)
    );; block the_end
  )

;;; match without considering the properties of the operators

; empty_match = proc(t1, t2: term) returns(substitution) signals(no_match)

; op match$empty_match: Term Term -> Substitution, signals(no_match)
(defun match$empty_match (t1 t2)
  (multiple-value-bind (m_sys no_match) 
      (match_system$dec_merg (match_system$new t1 t2))
    (if no_match 
	(values nil t)
	(cond ((system$is_empty (match_system$system m_sys))
	       (values (match_system$match_system2substitution m_sys) nil))
	      (t (values nil t)))))
  )

(defun match$matches (t1 t2)
  (multiple-value-bind  (gs subst no eeq)
      (match$first_match t1 t2)
    (declare (ignore gs subst))
    (or eeq (not no))))
