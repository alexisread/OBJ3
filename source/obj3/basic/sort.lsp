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

;; $Id: sort.lsp,v 206.2 2003/09/26 13:06:52 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;
;                    sort
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; note: sort equality is given by "eq"; sorts should always be
;;;   canonicalized.

;;;; Tim Winkler ;;;; Created: 7/7/86

;;;;;;;;;;;;;;;; Summary ;;;;;;;;;;;;;;;;
; op sort$name : Sort -> Name
; op sort$module : Sort -> Module
; op sort$info : Sort -> Info
; op sort$!replace_info : Sort Info -> {Sort}
; op sort$constructor : Sort -> Operator
; op sort$!replace_constructor : Sort Operator -> {Sort}
; op sort$is_built_in : Sort -> Bool

; for now it is represented by a structure

;; @todo Perhaps need to rename this structure to avoid clashing with
;; sort in CLOS. -- kiniry 25 Sept 2003

(defstruct (sort
	    (:conc-name sort$)
	    (:constructor sort$make)
	    (:print-function print$sort_name_3))
  name ;_internal ;&&&& 10 Sep 87 safer; see below too (take out eventually)
  module
  (constructor nil)
  (info nil))

; op sort$name : Sort -> Name
; op sort$module : Sort -> Module
; op sort$info : Sort -> Info
; op sort$constructor : Sort -> Operator
; from defstruct of sort

;&&&& eventually eliminate this
#|
(defun sort$name (sort)
  (if (typep sort 'sort)
      (sort$name_internal sort)
    (progn
      (princ "sort$name: illegal argument ") (print$name sort) (terpri)
      (break "sort$name: bad argument")
      ))
  )
|#

; op sort$create : Name Module -> Sort
(defun sort$create (name module)
  (sort$make
    :name name ;_internal
    :module module
  ))

;;; if a sort is from a protected module, cannot in fact destructively
;;;   change it as done by the above;  the above is probably OK for the
;;;   testing of the parser.

; op sort$!replace_info : Sort Info -> {Sort}
(defun sort$!replace_info (sort info)
  (setf (sort$info sort) info)
  )

; op sort$!replace_constructor : Sort Operator -> {Sort}
(defun sort$!replace_constructor (sort operator)
  (setf (sort$constructor sort) operator))

; op sort$is_built_in : Sort -> Bool
(defun sort$is_built_in (sort)
  (sort$info sort))
