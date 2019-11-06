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

;; $Id: sort_order.lsp,v 206.1 2003/09/23 13:41:00 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;              Sort Order Cluster
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Helene and Claude Kirchner ;;;; Created: July 18, 86
;;;; Tim Winkler ;;;; Modified: July 24, 86
;;;;   change to: sort_order$!add_user_relation

;;;;;;;;;;;;;;;; Imported clusters


;;;;;;;;;;;;;;;; Summary

; op sort_order$new: -> SortOrder
; op sort_order$add_sort: SortOrder Sort -> SortOrder
; op sort_order$is_included_in : SortOrder Sort Sort  -> Bool .
; op sort_order$is_strictly_included_in : SortOrder Sort Sort  -> Bool .
; op sort_order$lower_sorts : SortOrder Sort  -> SortSet .
; op sort_order$lower_or_equal_sorts : SortOrder Sort  -> SortSet .
; op sort_order$greater_sorts : SortOrder Sort  -> SortSet .
; op sort_order$is_comparable_with :  Sort SortSet SortOrder -> Bool.
; op sort_order$max_minorants :  SortOrder SortSet -> SortSet .
; op sort_order$!add_user_relation: SortOrder Sort SortList -> SortOrder~
; op sort_order$imported_closure: List[SortOrder] -> SortOrder
; op sort_order$transitive_closure: SortOrder SortOrder -> SortOrder
; op sort_order$component_top : SortOrder Sort -> Sort
; --- local 
; op sort_order$$inter_lower_sorts: SortOrder SortSet -> SortSet
; op sort_order$$!union_ls: SortOrder Sort List[Sort] -> SortOrder~
; op sort_order$$!union_gs: SortOrder Sort List[Sort] -> SortOrder~

;;;;;;;;;;;;;;;; Representation

; A SortOrder is a list of SortRelations, where a SortRelation
; is composed of a sort, a list of lower sorts and a list of greater sorts.
; A SortOrder is implemented as an a-list 
; (sort . (lower_sorts . greater_sorts))

;;;;;;;;;;;;;;;; Typical use of this module

; an empty SortRelation must be associated to a
; module when it is created. This ordering is then incremented by the
; following operations:
;   a) module$add_sorts must call sort_relation$add_sort on this ordering,
;   b) then sort_relation$add_user_relation entered
;      all the declarations of subsorts in the module.
;   c) Compute the transitive closure of the gathered ordering on 
;      sub-modules (possibly renamed when using) by 
;      "sort_order$imported_closure" (Z: Note that its arguments must be 
;      copied before)
;   d) At last sort_order$transitive_closure is performed with the result of
;      the previous operation and the new incomplete ordering relative to 
;      this module.

; This is this ordering which is then accessible by the operation
;  module$sort_order

;;;;;;;;;;;;;;;;

; op sort_order$new: -> SortOrder
(defun sort_order$new ()
  ()
  )

; sort_order$add_sort adds a new sort with empty lists of lower 
; and greater sorts and returns the new order

; op sort_order$add_sort: SortOrder Sort -> SortOrder
(defun sort_order$add_sort (ORDER S)
  (acons S (cons nil nil) ORDER)
  )

; op sort_order$adjoin_sort: SortOrder Sort -> SortOrder
(defun sort_order$adjoin_sort (ORDER S)
  (let ((REL (assoc S ORDER)))
    (if REL ORDER
      (acons S (cons nil nil) ORDER))
  ))

; sort_order$add_user_relation adds a user declaration
; subsorts s < s1, ..., sn.

; op sort_order$!add_user_relation: SortOrder Sort SortList -> SortOrder~
(defun sort_order$!add_user_relation (ORDER S SL)
  (let ((REL (assoc S ORDER)))
    (unless REL (break "sort_order$!add_user_relation: sort not found"))
    (rplacd (cdr REL) (union (cddr REL) SL)))
  (dolist (S1 SL)
    (let ((REL (assoc S1 ORDER)))
      (unless REL (break "sort_order$!add_user_relation: sort not found"))
      (rplaca (cdr REL) (adjoin S (cadr REL)))
    ))
  )

; returns true iff the first sort is a subsort of the second
; in the reflexive transitive closure of the sort relation.

; op sort_order$is_included_in : SortOrder Sort Sort  -> Bool .

;  eq : is_included_in(ORDER,S1,S2)
;       =
;       (S1 == S2) or (S1 is_in get_lower_sorts(ORDER,S2))
(defun sort_order$is_included_in (ORDER S1 S2)
  (or (eq S1 S2)
      (eq S2 *obj$sort_Universal*)
      ;(member S1 (sort_order$lower_sorts ORDER S2)) ; optimized to
      (dolist (l (sort_order$lower_sorts ORDER S2) nil)
        (when (eq s1 l) (return t)))
  ))

; returns true iff the first sort is a subsort of the second
; in the (non-reflexive) transitive closure of the sort relation.

; op sort_order$is_strictly_included_in : SortOrder Sort Sort  -> Bool .

;  eq : is_strictly_included_in(ORDER,S1,S2)
;       =
;       S1 is_in get_lower_sorts(ORDER,S2)
(defun sort_order$is_strictly_included_in (ORDER S1 S2)
  (member S1 (sort_order$lower_sorts ORDER S2))
  )

; returns the set of sorts lower than the given sort by transitive
; closure

; op sort_order$lower_sorts : SortOrder Sort  -> SortSet .
(defun sort_order$lower_sorts (ORDER S)
   (cadr (assoc S ORDER))
   )

; returns the set of sorts lower or equal to than the given sort by transitive
; closure

; op sort_order$lower_or_equal_sorts : SortOrder Sort  -> SortSet .
(defun sort_order$lower_or_equal_sorts (ORDER S)
   (cons S (cadr (assoc S ORDER)))
   )

; returns the set of sorts greater than the given sort by transitive
; closure

; op sort_order$greater_sorts : SortOrder Sort  -> SortSet .
(defun sort_order$greater_sorts (ORDER S)
   (cddr (assoc S ORDER))
   )

; check is the first sort is greater or lower than one of the sort 
; in the SortSet using the relation contained into the SortOrder

; op sort_order$is_comparable_with :  Sort SortSet SortOrder -> Bool.

;  eq : S1 is_comparable_with (S2 union SORT_SET) in ORDER
;       =
;       if
;       (S1 is_in get_lower_sorts(ORDER, S2) 
;        or
;        S1 is_in get_greater_sorts(ORDER, S2))
;       then true
;       else S1 is_comparable_with SORT_SET in ORDER
;       fi .
(defun  sort_order$is_comparable_with (S  SORT_SET ORDER)
  (if SORT_SET
      (let ((S1 (car SORT_SET)))
	(if (or 
	     (eq S S1)
	     (member S (sort_order$lower_sorts ORDER S1))
	     (member S (sort_order$greater_sorts ORDER S1))
	     )
	    t
          (sort_order$is_comparable_with S (cdr SORT_SET) ORDER)
	  )
	);; let
    ()
    )
  )


; compute the set of maximal elements in the set of lower bounds
; of the given SortSet using the relation contained into the 
; SortOrder

; op sort_order$max_minorants :  SortOrder SortSet -> SortSet .

;  eq : max_minorants(ORDER, (S union SORT_SET))
;       =
;       maximal_elements(ORDER,
;                        inter_lower_sorts(ORDER,SORT_SET)) .

(defun  sort_order$max_minorants (ORDER SORT_SET)
  (let ((MAX_MIN nil) 
	(LOWER_BOUNDS (sort_order$$inter_lower_sorts ORDER SORT_SET))
	)
       (dolist (S LOWER_BOUNDS)
               (unless (intersection (sort_order$greater_sorts ORDER S)
				     LOWER_BOUNDS)
		       (setq MAX_MIN (adjoin S MAX_MIN))
		       ) ;; unless
	       )
       MAX_MIN
       ) ;; let
  )
	       
; sort_order$inter_lower_sorts compute the set of lower bounds of 
; a given set of sorts. If this set is empty returns nil.

; op sort_order$$inter_lower_sorts: SortOrder SortSet -> SortSet
(defun sort_order$$inter_lower_sorts (ORDER SORT_SET)
  (if (cdr SORT_SET)
      (intersection (sort_order$lower_or_equal_sorts ORDER (car SORT_SET))
		    (sort_order$$inter_lower_sorts ORDER (cdr SORT_SET))
		    )
      (if (car SORT_SET)
	  (sort_order$lower_or_equal_sorts ORDER (car SORT_SET))
	  ()
	  )
      )
)
      
                                                               
(defun sort_order$transitive_closure (PREVIOUS_ORDER NEW_ORDER)
  (let ((CLOSURE (sort_order$merge PREVIOUS_ORDER NEW_ORDER)))
    (dolist (RELATION CLOSURE)
      (let (;(S (car RELATION))
	    (LS (cadr RELATION))
	    (GS (cddr RELATION)))
	(dolist (S1 LS)
	  (dolist (S2 GS)
	    (sort_order$$!union_ls CLOSURE S2 (list S1))
	    (sort_order$$!union_gs CLOSURE S1 (list S2))
	    );; dolist
	  );; dolist
	);; let
      );; dolist
    CLOSURE
    );; let
  )

(defun sort_order$transitive_closure1 (sort_order)
  (sort_order$transitive_closure (sort_order$new) sort_order))

(defun sort_order$merge (so1 so2)
  (dolist (x so1)
    (let ((rel (assoc (car x) so2)))
      (if rel
	  (let ((cdr_x (cdr x)) (cdr_rel (cdr rel)))
	    (rplaca cdr_rel (union (car cdr_x) (car cdr_rel)))
	    (rplacd cdr_rel (union (cdr cdr_x) (cdr cdr_rel))))
	(push x so2))))
  so2
  )

; make the union of the the sorts lower than "S" with LS.

; op sort_order$$!union_ls: SortOrder Sort List[Sort] -> SortOrder~
(defun sort_order$$!union_ls (ORDER S LS)
  (let ((REL (assoc S ORDER)))
    (rplaca (cdr REL) (union (cadr REL) LS))
    )
  )


; op sort_order$$!union_gs: SortOrder Sort List[Sort] -> SortOrder~
(defun sort_order$$!union_gs (ORDER S LS)
  (let ((REL (assoc S ORDER)))
    (rplacd (cdr REL) (union (cddr REL) LS))
    )
  )

; Compute the transitive closure of the gathered ordering on 
; sub-modules (possibly renamed when using)

; op sort_order$imported_closure: List[SortOrder] -> SortOrder
(defun sort_order$imported_closure (L_SO)
  (let ((IMPORT_CLOSURE (pop L_SO))
	)
    (dolist (SO L_SO)
	    (setq IMPORT_CLOSURE
		  (sort_order$transitive_closure IMPORT_CLOSURE SO)
		  )
	    ) ; dolist
    IMPORT_CLOSURE
    )
  )

; op sort_order$is_in_same_connected_component: SortOrder Sort Sort -> Bool
(defun sort_order$is_in_same_connected_component (sort_order s1 s2)
  (or
   (eq *obj$sort_Universal* s1)
   (eq *obj$sort_Universal* s2)
   (sort_order$have_common_subsort sort_order s1 s2)
   (eq (sort_order$component_top sort_order s1)
       (sort_order$component_top sort_order s2)))
  )

; op sort_order$is_in_same_component_safe: SortOrder Sort Sort -> Bool
(defun sort_order$is_in_same_component_safe (sort_order s1 s2)
  (or
   (eq *obj$sort_Universal* s1)
   (eq *obj$sort_Universal* s2)
   (sort_order$is_included_in sort_order s1 s2)
   (sort_order$is_included_in sort_order s2 s1)
   (sort_order$have_common_subsort sort_order s1 s2)
   (eq (sort_order$component_top sort_order s1)
       (sort_order$component_top sort_order s2)))
  )

; op sort_order$component_top : SortOrder Sort -> Sort
(defun sort_order$component_top (sort_order sort)
  (let ((res sort)
	(cnt 5000))
  (declare (fixnum cnt))
  (loop
   (when (< cnt 0)
     (princ "sort_order$component_top: failing for ")
     (print$name res) (terpri)
     (return res))
   (setq cnt (- cnt 1))
   (let ((up
	  ;(sort_order$greater_sorts sort_order res) ; opt to
	  (cddr (dolist (i sort_order) (when (eq res (car i)) (return i))))
	  ))
   (if up
     (setq res (car up))
     (return res)))
  )))

; op sort_order$have_common_subsort : SortOrder Sort Sort -> Bool
(defun sort_order$have_common_subsort (sort_order s1 s2)
  (let ((ss1 (cadr (assoc s1 sort_order)))
	(ss2 (cadr (assoc s2 sort_order))))
    (dolist (s ss1 nil)
      (when (member s ss2) (return t)))
  ))

; op sort_order$copy : SortOrder -> SortOrder
(defun sort_order$copy (sort_order)
  (mapcar #'(lambda (i)
        (cons (car i) (let ((v (cdr i))) (cons (car v) (cdr v)))))
    sort_order)
  )
