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

;; $Id: theory_name.lsp,v 205.2.1.1 2003/09/23 14:09:52 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; contains the macros defining the acces to the theory (or properties)
; of the operators. 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; Claude Kirchner ;;;; Created: April 86.

;;;;;;;;;;;;;;;; Summary

(defun theory_name$is_empty_for_matching (th_name)
  (theory_info$empty_for_matching th_name))

(defun theory_name$is_empty_for_matching_direct (th_name)
  (or (eq the_empty_property th_name)
      (eq the_I_property th_name)
      (eq the_Z_property th_name)
      (eq the_IZ_property th_name)
      )
  )  

(defmacro theory_name$is_empty (th_name)
  `(eq the_empty_property ,th_name))
       
(defmacro theory_name$is_A (th_name)
  `(eq the_A_property ,th_name))

(defmacro theory_name$is_C (th_name)
  `(eq the_C_property ,th_name))

(defmacro theory_name$is_I (th_name)
  `(eq the_I_property ,th_name))

(defmacro theory_name$is_Z (th_name)
  `(eq the_Z_property ,th_name))

(defmacro theory_name$is_AC (th_name)
  `(eq the_AC_property ,th_name))

(defmacro theory_name$is_AI (th_name)
  `(eq the_AI_property ,th_name))

(defmacro theory_name$is_AZ (th_name)
  `(eq the_AZ_property ,th_name))

(defmacro theory_name$is_CI (th_name)
  `(eq the_CI_property ,th_name))

(defmacro theory_name$is_CZ (th_name)
  `(eq the_CZ_property ,th_name))

(defmacro theory_name$is_IZ (th_name)
  `(eq the_IZ_property ,th_name))

(defmacro theory_name$is_ACI (th_name)
  `(eq the_ACI_property ,th_name))

(defmacro theory_name$is_ACZ (th_name)
  `(eq the_ACZ_property ,th_name))

(defmacro theory_name$is_AIZ (th_name)
  `(eq the_AIZ_property ,th_name))

(defmacro theory_name$is_CIZ (th_name)
  `(eq the_CIZ_property ,th_name))

(defmacro theory_name$is_ACIZ (th_name)
  `(eq the_ACIZ_property ,th_name))

; this really only needs to work for the symbols
(defun theory_name$code_direct (th_name)
  (cond
    ((or (eq 'empty_property th_name)
	 (theory_name$is_empty th_name))
     0)
    ((or (eq 'A_property th_name)
	 (theory_name$is_A th_name))
     8)
    ((or (eq 'AZ_property th_name)
	 (theory_name$is_AZ th_name))
     9)
    ((or (eq 'AC_property th_name)
	 (theory_name$is_AC th_name))
     12)
    ((or (eq 'ACZ_property th_name)
	 (theory_name$is_ACZ th_name))
     13)
    ((or (eq 'ACIZ_property th_name)
	 (theory_name$is_ACIZ th_name))
     15)
    ((or (eq 'C_property th_name)
	 (theory_name$is_C th_name))
     4)
    ((or (eq 'ACI_property th_name)
	 (theory_name$is_ACI th_name))
     14)
    ((or (eq 'CZ_property th_name)
	 (theory_name$is_CZ th_name))
     5)
    ((or (eq 'AIZ_property th_name)
	 (theory_name$is_AIZ th_name))
     11)
    ((or (eq 'Z_property th_name)
	 (theory_name$is_Z th_name))
     1)
    ((or (eq 'I_property th_name)
	 (theory_name$is_I th_name))
     2)
    ((or (eq 'AI_property th_name)
	 (theory_name$is_AI th_name))
     10)
    ((or (eq 'CI_property th_name)
	 (theory_name$is_CI th_name))
     6)
    ((or (eq 'IZ_property th_name)
	 (theory_name$is_IZ th_name))
     3)
    ((or (eq 'CIZ_property th_name)
	 (theory_name$is_CIZ th_name))
     7)
    (t 0) ; default
    ))

(defun theory_name$is_restriction_of (thn1 thn2)
  (= 0 (logandc2 (theory_info$code thn1) (theory_info$code thn2))))

(defun theory_name$is_restriction_of_ignoring_id (thn1 thn2)
  (= 0 (logandc2 (theory_info$code thn1)
		 (logior Z_flag (theory_info$code thn2)))))
