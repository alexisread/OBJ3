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

;; $Id: topo_sort.lsp,v 205.2.1.1 2003/09/23 14:12:02 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Topological Sort
; a very simple version
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; want this to work for predicate as <= (not <)
; in fact, though, will only apply to sequences of distinct items
; very simple method based on selection sort
; -- select minimal element of those remaining, swap with next and continue
; this is specialized to lists
; op topo-sort : Lisp-List (Lisp Lisp -> Bool) -> ~Lisp-List
(defun topo-sort (lst pred)
  (let ((res lst)) ; save original list as final value
  ; run through the positions of lst successively filling them in
  (loop
    (when (null lst) (return))
    ; pos is location of val which is current minimal value
    (let ((pos lst) (val (car lst)) (rest (cdr lst)))
      ; scan through remainder of list rest updating pos and val
      (loop ; -- select minimal
	(when (null rest) (return))
	(let ((valr (car rest)))
	  (when (funcall pred valr val)
	    (setq pos rest val valr))) ; have found new minimal value
	(setq rest (cdr rest))) ; loop -- select minimal
      ; swap values at front of lst and at pos
      (rplaca pos (car lst))
      (rplaca lst val)
    ) ; let
    (setq lst (cdr lst))) ; loop
  res))
