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

;; $Id: match_system.lsp,v 205.2.1.1 2003/09/23 14:09:52 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                    The  match_systems
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; Claude Kirchner ;;;; Created: April 86.

;;;;;;;;;;;;;;;; Summary

; op match_system$system: MatchSystem -> System
; op match_system$env: MatchSystem -> Environment
; op match_system$new: Term Term -> MatchSystem
; op match_system$create: Environment System -> MatchSystem 
; op match_system$E_equal: MatchSystem -> Bool
; op match_system$add: MatchSystem System -> MatchSystem, signals(no_match)
; op match_system$dec_merg: MatchSystem -> MatchSystem, signals(no_match)
; op match_system$extract_one_system: MatchSystem -> System TheoryName
; op match_system$modif_m_sys: MatchSystem System -> MatchSystem 
; op match_system$match_system2system: MatchSystem -> System

;;;;;;;;;;;;;;;; Presentation

; a match system is a system of oriented equations which 
; first term may contains variables and the second one is considered as
; ground term.

; It is divided into two parts:
;   * the first one is a set of equations of type t1 == t2 where
;     t1 is a not a variable, we call it "sys_to_solve"
;   * the second one is a set of equations of type t1 == t2 where
;     t1 IS a variable, we call it "environment"

;;;;;;;;;;;;;;;; Representation

;   rep = record{sys : system, env: environment}

(defstruct (match_system
	    (:constructor match_system$create (env sys)))
	(sys (system$new))
	(env (environment$new))
	)

;;;;;;;;;;;;;;;;

; op match_system$system: MatchSystem -> System
(defun match_system$system (m_sys)
  (match_system-sys m_sys)
  )

; op match_system$env: MatchSystem -> Environment
(defun match_system$env (m_sys)
  (match_system-env m_sys)
  )

; From two terms "t1" and "t2" returns the m_system containing the
; equation t1 == t2.

; op match_system$new: Term Term -> MatchSystem
(defun match_system$new (t1 t2)
  (if (term$is_var t1)
      (match_system$create (environment$create t1 t2) (system$new))
      (match_system$create (environment$new) (system$create t1 t2))))

; create a new match_system

; op match_system$create: Environment System -> MatchSystem 
;(defun match_system$create (env sys)  (make-match_system :sys sys :env env))

; returns true iff all the equations in "m_sys" are composed of E_equal
; terms

; op match_system$E_equal: MatchSystem -> Bool
(defun match_system$E_equal (m_sys)
  (block no_E_equal
    (dolist (eq  (system$list_eq (match_system-sys m_sys)))
      (unless (term$equational_equal
	       (match_equation$t1 eq) (match_equation$t2 eq))
	(return-from no_E_equal nil)))
    (dolist (eq  (environment$list_eq (match_system-env m_sys)))
      (unless (term$equational_equal
	       (match_equation$t1 eq) (match_equation$t2 eq))
	(return-from no_E_equal nil)))
    (return-from no_E_equal t))
  )

; add try to returns a NEW match_system
; containing the (set) union of "sys" and "m_sys".
; For this purpose, it inserts in a new match_system the equation 
; of "sys" which are compatible with "m_sys". If they are not compatible
; then add signals no_match. 
; Z: The equations of "sys" are of any kind, i.e. var == term or term == term.
; U: Used by "state$next_state".

; Note that the arguments of add must not be destroyed or changed.

; op match_system$add: MatchSystem System -> MatchSystem, signals(no_match)
(defun match_system$add (m_sys sys)
  (block no_match
    (let* ((env (match_system-env m_sys))
	   (new_env (environment$new ) ) 
	   (new_sys (system$new))
	   )
	  ;; then we insert all the equations of "sys" in this new system if
	  ;; they are compatible with m_sys and copy the environment
      (when (environment$insert_if_coherent_with new_env 
					       env 
					       new_sys 
					       (system$list_eq sys))
	    (return-from no_match (values nil t)))
      ;; new_sys is modified but not m_sys
      ;; (environment$add new_env env)
      (setq new_sys (system$add new_sys (match_system$system m_sys)))
      (return-from no_match 
	(values (match_system$create 
		  new_env
		  new_sys)
		nil))
      )
    )
  )


; Returns the decomposition and merging of the given match_system

; op match_system$dec_merg: MatchSystem -> MatchSystem, signals(no_match)
(defun match_system$dec_merg (m_sys)
  (block no_match
    (let ((sys (match_system-sys m_sys))
	   (env (match_system-env m_sys))
	   (new_env (environment$new) )
	   (new_sys (system$new))
	   )
      (dolist (eq (system$list_eq sys))
	(multiple-value-bind (eq_list clash_of_symbol)
	    (match_equation$decomposition eq)
	  (if clash_of_symbol
	      (return-from no_match (values nil t))
	      (when (environment$insert_if_coherent_with 
		     new_env 
		     env 
		     new_sys 
		     eq_list)
		    (return-from no_match (values nil t))
		    ) ;; when
	      ) ;; if
	  ) ;; mult
	) ;; do
      (values (match_system$create 
		new_env
		new_sys)
	      nil)
      )
    )
  )

; returns the substitution associated to the m_system "m_s" 
; which is supposed to 
; be fully solved, that is such that "m_s.sys" is empty and merged.

; match_system$match_system2substitution: MatchSystem -> Substitution
(defun match_system$match_system2substitution (m_s)
  (match_system-env m_s))

; Extracts from the non fully decomposed part of "m_s"
; the biggest system to be solved into the theory "th".
; "th" and "sys" are returned.

; op match_system$extract_one_system: MatchSystem -> System TheoryName
(defun match_system$extract_one_system (m_s)
  (let ((sys (match_system$system m_s)))
    (if (system$is_empty sys) 
	(values nil the_empty_property)
	(system$extract_one_system sys)
	))
  )


; returns a new match_system with the same environment
; that "m_sys" but with a system equal to the system of m_sys
; except the elements in "sys". 
; Z: "sys" is supposed included into the system of "m_sys".

; op match_system$modif_m_sys: MatchSystem System -> MatchSystem 
(defun match_system$modif_m_sys (m_sys sys)
  (match_system$create
    (environment$copy1 (match_system$env m_sys)) 
    (match_system$set_difference_eq
     (system$list_eq (match_system$system m_sys)) sys)
    )
  )

(defun match_system$set_difference_eq (x y)
  (let ((res nil))
    (dolist (xe x)
      (unless (dolist (ye y nil) (when (eq xe ye) (return t)))
	(push xe res)))
    res))

; returns from a match_system a system (equivalent)

; op match_system$match_system2system: MatchSystem -> System
(defun match_system$match_system2system (m_sys)
  (system$make_system
    (system$list_eq
      (match_system$system m_sys))
    (environment$list_eq
      (match_system$env m_sys)))
)
