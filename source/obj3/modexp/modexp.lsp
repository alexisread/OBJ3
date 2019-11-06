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

;; $Id: modexp.lsp,v 205.2.1.1 2003/09/23 14:15:42 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                    modexp -- abstraction layer for module expr
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; glossing over the distinction: premodule-expression vs module-expression
; module expressions are: A+B A*(renaming) A[B,C,...] or A[view...]
;   extensions: default view from sort name: A[sort]
; primitive operations on modules:
;     +, mapping/image (rename or view application), ; (dependent +)
; extension: TUPLE[A,B,...] -- variadic operator (already parsed ok)
;  further: RECORD[selA: A, selB: B,...]
; point to think about: default computations -- module with principle sort
; point to think about: qualified sort names
; note: may want to consider things such as: (A*(renaming))[B]

;;;;;;;;;;;;;;;; Summary ;;;;;;;;;;;;;;;;

; op modexp$make_plus : ModuleExpression ModuleExpression -> ModuleExpression
; op modexp$make_rename : ModuleExpression Rename -> ModuleExpression
; op modexp$make_instantiation : ModuleExpression list[ModuleExpression] ->
;                                  ModuleExpression
; op modexp$make_view : ModuleExpression ModuleExpression Mapping ->
;                         View
; op modexp$make_prim_view : ModuleExpression Mapping -> View
; op modexp$make_view_under : ModuleExpression View -> View
; op modexp$make_view_from : ModuleExpression ModuleExpression Mapping -> View
; op modexp$make_view_mapping :
;       ModuleExpression ModuleExpression Mapping Real-Mapping
;    -> View
; op modexp$!replace_mapping : View Real-Mapping -> View

; Mapping is "pre-mapping"; Real-Mapping is as defined by mapping.lsp
; op  modexp$is_view : ModuleExpression -> Bool

;;; preliminary: mapping's are list of form:
;;;   (... (op x y) .... (sort x y) ....) -- see more below

;;; define list-based structures with defrepr

;; (defun make_symb (x)
;;   (intern (string-upcase x)))

;; (defun concat_string (x y)
;;   (concatenate 'string (string x) (string y)))

;; the "is_" part is not needed -- Sula Ma

(defmacro defrepr (module_prefix name tag elts)
  `(defstruct (,tag
                (:conc-name ,module_prefix)
                (:constructor 
                 ,(intern (string-upcase 
                   (concatenate 'string (string module_prefix) (concatenate 'string "make_" (string name)))))
                 ,elts)
                (:type list)
                :named
                )
     ,@elts
     ))

;; #-GCL (defun modexp$make_plus (x y) `(+ ,x ,y)) ; A + B

(defrepr modexp$ plus + (plus1 plus2))

;; #-GCL (defun modexp$make_rename (x y) `(* ,x ,y)) ; A * (renaming)

(defrepr modexp$ rename * (ren_mod ren_map))
  ; premodule-expression level:
  ;   renaming = many-of (sort a b) or (op x y)
  ;   x is token or list of tokens (was parenthesized)
  ; evaluated rename:
  ;   see mapping; pair of a-lists

(defrepr modexp$ view_rename *view (vwren_view vwren_map))
  ; a delayed renaming of the image of a view

;;#-GCL (defun modexp$make_instantiation (x y) `(|:| ,x ,y)) ; A [ B ]
(defrepr modexp$ instantiation |:| (inst_mod inst_args))

;;#-GCL (defun modexp$make_view (x y z) `(view ,x ,y ,z)) ; view from TH to M MAPPING
  ; premodule-expression level:
  ;   mapping = many-of (sort a b) or (op (a x) (b y))
  ;   a is sort or 'universe; x is op_expr which is token sequence
(defrepr modexp$ view view (view_mod view_src view_tgt))

;; (defun modexp$make_prim_view (x y) `(prim_view ,x ,y)) ; not used (as yet)
;<> leave for now

;;#-GCL (defun modexp$make_view_under (x y) `(under ,x ,y)) ; A :: V
(defrepr modexp$ view_under view_under (under_mod under_view))

;;#-GCL (defun modexp$make_view_from (x y z) `(view_from ,x ,y ,z))
  ; similar to view but in canonical form -- that is:
  ;  re-ordered, terms parsed, sorts "found"
  ;  variables eliminated
(defrepr modexp$ view_from view_from
         (view_from_src view_from_tgt view_from_map))

;; (defun modexp$make_view_mapping (x y z map) `(view_mapping ,x ,y ,z ,map))
  ; similar to view-from but also has mapping at the end
;<> leave for now -- probably will be eliminated

;; (defun modexp$!replace_mapping (vw map)
;;   (if (or (eq 'view_from (car vw)) (eq 'view_mapping (car vw)))
;;       (progn
;;         (rplaca vw 'view_mapping)
;;        (rplacd (cdddr vw) (list map)))
;;     (break "Misuse of modexp$!replace_mapping")))

(defun modexp$is_view (me)
  (and (consp me)
       (member (car me) '(view view_from))))
                        ;prim_view? under?

