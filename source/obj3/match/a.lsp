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

;; $Id: a.lsp,v 205.2.1.1 2003/09/23 14:09:24 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; All the procedures specific to the associative theory
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; Claude Kirchner ;;;; Created: April 86.

;;;;;;;;;;;;;;;; Summary

; op A$state_initialize: System Environment
;			 -> AssociativeState, signals(no_match)
; op A$next_state: AssociativeState -> System AssociativeState, 
; op A$equal: Term Term -> Bool
; op A$state_unparse: AssociativeState -> Output
;;;; Local
; op A$$associative_simplify: List[Term] List[Term] -> List[Term] List[Term]
; op A$$associativity_set_eq_state: List[Term] List[Term] -> EquationComp
; op A$$extract_in_from_to: Vector[Term] Int Int -> List[Term]
; op A$$increment_the_A_state: AssociativeState -> AssociativeState~
; op A$$try_increase_lexico: Vector[Int] Int -> Bool
; op A$$reset_equation_comp: Vector[EquationComp] Int -> Vector[EquationComp]
; op A$$reset_comp: EquationComp -> EquationComp~
; op A$$equation_comp_unparse: EquationComp -> Output

;;;;;;;;;;;;;;;; Representation

; A_state = associative_state

; associative_state = record{
; first the number of equations of the system
;                            size: int,
; then the top operator of each of the equations
; "f" for example if f is associative and all the equations are 
; of the form f(t1,...,tn) == f(...)
;                           operator: operator,
; then the system it self, presented as an array looking like:
;  t0 ,...,tn  == t'0 ,...,t'p   x0,..., xn-1
;  ...
;  ti0,...,tin == t'i0,...,t'ip  xi0,...,xin-1
;  ...
;  
;  where a potential solution is deduced from the xi in the 
;  following way:
;
;  xi0 = f(t'i0,...,t'ixi0)
;  xij = f(t'_{i_{x_{i_{j-1}}+1}},....,t'_{i_{x_{i_{j}}}})
;  xin = f(t'_{i_{x_{i_{n-1}}+1}},....,t'_{i_n})
;                            sys: array[eq_comp],
;  last a flag indicating if all the states of the system have been
;  explored
;                            no_more: bool}

; equation_comp = record{sz_left: integer,
;                        left: array[term], 
;                        right: array[term], 
;                        comp: array[integer]}

(defstruct (A_state
	    (:constructor make_A_state (size operator sys no_more)))
        (size 0 :type integer :read-only t)
	(operator nil :read-only t)
	sys ; array[eq_comp]
	(no_more nil) ; :bool
	)

(defstruct (equation_comp
	    (:constructor make_equation_comp (sz_left left right comp)))
        (sz_left 0 :type integer :read-only t)
        left
	right
	comp
	)

;;;;;;;;;;;;;;;;

; Initialize an associative state. Suppose that for any equation
; t1 == t2 of "sys", t1 and t2 are not E_equal.

; 0) take the associative normal form (at the first level) 
;    of each term of the system
; 1) it checks if the top symbols of each equation of the system 
;     have the same (associative) head function.
; 2) it checks if length(Left[i]) =< length(Right[i])
; 3) it simplifies all the similar term in Left[i] and Right[i]
;    at their left or right extremity
; 4) initialize left_state

; That is it!

; op A$state_initialize: System Environment
;			 -> AssociativeState, signals(no_match)
(defun A$state_initialize (sys env)
  (block no_match
    (let* ((dim (system$size sys))
	   (assoc_sys (make-array dim))
	   (op nil)
	   (i 0)
	   )
      (declare (fixnum i))
      (dolist (equation (system$list_eq sys))
	(let ((t1 (match_equation$t1 equation))
	      (t2 (match_equation$t2 equation)))
	  (unless (and
		   (not (term$is_var t2))
		   (operator$is_same_operator ;&&&&
		    (term$head t2)
		    (setq op (term$head t1))))
	    (return-from no_match (values nil t)))
	  (let ((sub1 (term$list_assoc_subterms t1 op))
		(sub1add nil)
		(sub2 (term$list_assoc_subterms t2 op))
		(fail nil))
	    ;; [ck: Dec  2 87] add the additional information contained
	    ;; in "env" into "sub1"
	    (dolist (val sub1)
	       (if (term$is_var val)
		     (let ((ima (environment$image env val))
			   (head nil) )
		       (if (null ima)
			   (push val sub1add)
		       (if (term$is_var ima)
			   (push ima sub1add)
			  (if (operator$is_same_operator
			       op (setq head (term$head ima)))
			      ;fairly tricky optimization
			      (setq sub1add
				(term$list_assoc_subterms_aux
				 ima head sub1add))
;			      (setq sub1add
;				    (nconc ;31 Mar 88 was append
;				     (nreverse ;21 Jun 89 was reverse
;				      (term$list_assoc_subterms ima head))
;				     sub1add
;				     ))
			      (push ima sub1add)
			     ))))
		 (push val sub1add)))
;	    (setq sub1 (nreverse sub1add)) ; a bit tricky
	    (when (> (length sub1) (length sub2))
	      (return-from no_match (values nil t)))
	    (multiple-value-setq (sub1 sub2 fail)
	      (A$$associative_simplify sub1 sub2))
	    (when (or fail
		      (and (null sub1) sub2)
		      (and (null sub2) sub1))
	      (return-from no_match (values nil t)))
	    ;31 Mar 88 sub1 may be nil but have modified a$$..set_eq_state
	    (setf (aref assoc_sys i)
	        (A$$associativity_set_eq_state sub1 sub2))
	    ))
	(setq i (1+ i))
	) ;end do
      (values 
	(make_A_state dim op assoc_sys nil)
	nil)
      )
    )
  )

; simplify the left symbols and the right symbols. Returns the modified
; list of terms
; example: (a,x,c,z,d,c) == (a,b,b,b,c,c,c,c) is simplified into
;            (x,c,z,d)   ==   (b,b,b,c,c,c)
; Z: notice that a variable  appearing in the right hand side
; should never be simplified. It  will be the case
; if the right hand side is always renamed before matching ...

; op A$$associative_simplify: List[Term] List[Term] -> List[Term] List[Term]

(defun A$$associative_simplify (sub1 sub2)
  (do ((t1 (car sub1) (car sub1))
       (t2 (car sub2) (car sub2)))
      ((or (not sub1) (not sub2) (not (term$equational_equal t1 t2))))
    (pop sub1)
    (pop sub2)
    )
  (setq sub1 (nreverse sub1) ;21 Jun 89 nrev.
	sub2 (nreverse sub2))
  (do ((t1 (car sub1) (car sub1))
       (t2 (car sub2) (car sub2)))
      ((or (not sub1) (not sub2) (not (term$equational_equal t1 t2))))
    (pop sub1)
    (pop sub2)
    )
  (let ((numvars 0))
    (declare (fixnum numvars))
  (dolist (x1 sub1) (when (term$is_var x1) (incf numvars)))
  (if (and (<= 2 numvars) (<= 3 (length sub1)) (<= 5 (length sub2)))
    (if (block test1 ;?
	 (dolist (x1 sub1 nil)
	   (unless (term$is_var x1)
	     (dolist (x2 sub2 (return-from test1 t))
	       (when (and (not (term$is_var x2))
			  (match$possibly_matches_nonvar x1 x2))
		 (return))))))
      (values nil nil t)
      (values (nreverse sub1) (nreverse sub2) nil))
    (values (nreverse sub1) (nreverse sub2) nil) ;11 Feb 88 made nreverse
  ))
  )

; initialize the first state associated with sub1 sub2

; op A$$associativity_set_eq_state: List[Term] List[Term] -> EquationComp
  
(defun A$$associativity_set_eq_state (sub1 sub2)
  (let* ((sz1 (length sub1))
	 (comp (make-array1 (if (= 0 sz1) 0 (- sz1 1))))) ;31 Mar 88 0 case
    (declare (type (vector fixnum) comp))
    (dotimes-fixnum (x (- sz1 1))
      ; x = 0,...,sz1-2
      ;built the array (1, 2, 3, 4) if sz1 = 5 
      (setf (aref comp x) (1+ x))
      )
    (make_equation_comp sz1 (list2array sub1) (list2array sub2) comp)
    )
  )

; op A$next_state: AssociativeState -> System AssociativeState, 
;                                      signals(no_more)

(defun A$next_state (A_st)
  ;;  (A_state$unparse A_st)
  (let* ((new_sys (system$new))
 	 (sz (A_state-size A_st))
	 (sys (A_state-sys A_st))
	 (op (A_state-operator A_st))
	 )
    (declare (fixnum sz)
	     (type (vector t) sys))
    (if (A_state-no_more A_st)	
	; there is no more A_state
	(values nil nil t)
        (progn
	  (dotimes-fixnum (k sz)	; k = 0,...,sz-1
		   ;; i.e. for each equation of the system
		   (let* ((eq_comp (aref sys k))
			  (sz_left (equation_comp-sz_left eq_comp))
			  (left (equation_comp-left eq_comp))
			  (right (equation_comp-right eq_comp))
			  (sz_right (1- (the fixnum (length right))))
			  (comp (equation_comp-comp eq_comp)))
		     (declare (fixnum sz_left sz_right)
			      (type (vector t) left right)
			      (type (vector fixnum) comp))
		     (dotimes-fixnum (l sz_left) ; l = 0,...,sz_left - 1
		       ;; i.e. for each term of the left hand 
		       ;; side of the equation 
		       (let ((deb (if (= l 0)
				      0 
				      (aref comp (the fixnum (- l 1)))))
			     (fin (if (= l (the fixnum (- sz_left 1)))
				      sz_right 
				      (the fixnum (- (aref comp l) 1)))))
			 (declare (fixnum deb fin))
			 (system$add_eq
			  new_sys
			  (match_equation$create 
			   (aref left l)
			   (a$$make_term op right deb fin)
			   ))
			 ) ;; let
		       ) ;; do
		     )
		   ) ;; do
	  (A$$increment_the_A_state A_st)		; A_st is modified
	  (values new_sys A_st nil)
	  )
	)
    )
  )

; op a$$make_term : Operator Array[Term] Int Int -> Term
; create a single term from a collection of terms
(defun a$$make_term (op vect first last)
  (declare (fixnum first last)
	   (type (vector t) vect))
  (if (= first last) (aref vect first)
    (let ((res (aref vect last)))
    (if (and
	 (< 1 (the fixnum (- last first)))
	 (let ((val (optable$operator_info
		     (module$operator_table
		      (if obj$current_module obj$current_module
			(operator$module op)))
		      op)))
           (or (null val) (null (cdr (operator_info$lowest val))))))
      (do ((i (1- last) (1- i)))
	  ((< i first) res)
	(declare (fixnum i))
	(setq res (term$make_term op (list (aref vect i) res)))
	)
      (do ((i (1- last) (1- i)))
	  ((< i first) res)
	(declare (fixnum i))
	(setq res (term$make_term_with_sort_check_binary op 
		    (list (aref vect i) res)))
	)))))

; returns the list of terms contained in the array of terms "t_arr"
; between the indices "from" and "to" both included.
; example: A$$extract_in_from_to((1,2,3,4,5,6), 1, 4) --> (2,3,4,5)
; note that the indices of the array are 0,1,2,3,4,5.

; op A$$extract_in_from_to: Vector[Term] Int Int -> List[Term]

(defun A$$extract_in_from_to (t_arr from to)
  (declare (fixnum from to))
  (let ((t_list nil))
    (do ((i to (- i 1)))
	((< i from)  t_list)
      (declare (fixnum i))
      (push (aref t_arr i) t_list)
      )
    )
  )

; modify the A_state "A_st" by  incrementing the state local to each
; equation of the system in a "variable basis numeration" way

; op A$$increment_the_A_state: AssociativeState -> AssociativeState~
(defun A$$increment_the_A_state (A_st)
  (block the_end
  (let ((sz (A_state-size A_st))
	(sys (A_state-sys A_st)))
    (declare (fixnum sz)
	     (type (vector t) sys))
  (let ((k 0) eq_comp)
    (declare (fixnum k))
  (loop
   (when (<= sz k) (return))
   (setq eq_comp (aref sys k))
   (when (A$$try_increase_lexico
	  (equation_comp-comp eq_comp)
	  (the fixnum (1- (length (equation_comp-right eq_comp)))))
     ; note that A$$try_increase_lexico modify in this case
     ; the  "comp"  of the current equation.
     ; After that the previous composant are reset like in
     ; 599 -> 600
     (A$$reset_equation_comp sys k)
     (return-from the_end (values nil)) ;&&&& was (values) -- undo?
	    ;otherwise, try to increase the next equation
     )
   (setq k (1+ k))
   ))
  ; this "normal" exit of the loop means that none of the
  ; state has been increased so there is no more state
  (setf (A_state-no_more A_st) t)
  ))
  )

; try to increase with respect with the lexicographical order
; on the arrays of integer the integer array "comp". These are
; the following constaints:
; 1) the elements of the array are all different and ordered in 
; increasing order: example (2 3 4 6 8)
; 2) the grastest element of the array in LESS OR EQUAL to "max"
; Returns true iff one have succeded to increment.

; op A$$try_increase_lexico: Vector[Int] Int -> Bool
(defun A$$try_increase_lexico (comp max)
  (declare (fixnum max)
	   (type (vector fixnum) comp))
  (let ((lim (1- (length comp))))
  (declare (fixnum lim))
  (do ((i lim (- i 1)))
      ((< i 0) nil)
    (declare (fixnum i))
    (let ((x (aref comp i)))
      (declare (fixnum x))
      (when (< x max)
	(setf (aref comp i) (1+ x))
	(do ((j (1+ i) (1+ j))
	     (v (+ x 2) (1+ v)))
	    ((< lim j))
	  (declare (fixnum j v))
	  (setf (aref comp j) v))
	(return t)
	))
    (setq max (1- max))
    )
  ))

; modifies the array "sys" of "equation_comp" in such a way that
; all the comp array are reset provide that they rank in "sys"
; is less (strictly) than K.

; op A$$reset_equation_comp: Vector[EquationComp] Int -> Vector[EquationComp]
(defun A$$reset_equation_comp (sys K)
  (dotimes-fixnum (i K)				; i = 0,...,K-1 
    (A$$reset_comp (aref sys i))
    )
  )

; reset the comp of "eq_comp" to his initial value i.e. (1,2,3,4,5)

; op A$$reset_comp: EquationComp -> EquationComp~
(defun A$$reset_comp (eq_comp)
  (let ((comp (equation_comp-comp eq_comp)))
    (declare (type (vector fixnum) comp))
    (dotimes-fixnum
        (x (1- (equation_comp-sz_left eq_comp))) ; x = 0,...,sz_left - 2
      (setf (aref comp x) (1+ x))
      )
    )
  )

; op A$equal: Term Term -> Bool
; know/require that t1 t2 have the same top operator
(defun A$equal (t1 t2)
;  (block the_end
;         (let ((result t)
;	       (l1 (term$subterms (term$right_associative_normal_form t1)))
;	       (l2 (term$subterms (term$right_associative_normal_form t2))))
;	   (unless (= (length l1)(length l2))
;		   (return-from the_end nil))
;	   (do
;	    ((x1 (car l1) (car l1));; (car nil) -> nil ...
;	     (x2 (car l2) (car l2)))
;	    ((or (not result) (null l1)))
;	    (setq result (term$equational_equal x1 x2))
;	    (pop l1)(pop l2)
;	    )
;	   result
;	   ))
  (let ((hd2 (term$head t2)))
  (if (theory$contains_associativity (operator$theory hd2))
    (let ((l1 (term$list_assoc_subterms t1 (term$head t1)))
	  (l2 (term$list_assoc_subterms t2 hd2)))
      (and
       (= (length l1) (length l2))
       (loop
	(when (null l1) (return (null l2)))
	(unless (term$equational_equal (car l1) (car l2)) (return nil))
	(setq l1 (cdr l1) l2 (cdr l2))
       ))
    )
    (and (term$equational_equal (term$arg_1 t1) (term$arg_1 t2))
	 (term$equational_equal (term$arg_2 t1) (term$arg_2 t2)))
   
    ))
  )
 
; op A$state_unparse: AssociativeState -> Output
;(defun A$state_unparse (A_st)
;  (print "--------------- unparse of: ")(prin1 A_st)(terpri)
;  (princ "size: ")(prin1 (A_state-size A_st))(terpri)
;  (princ "operator: ")(prin1 (A_state-operator A_st))(terpri)
;  (princ "sys: ")(dotimes (x (array-dimension (A_state-sys A_st) 0))
;		  (equation_comp$unparse (aref (A_state-sys A_st) x))
;		  )(terpri)
;  (princ "no_more: ")(prin1 (A_state-no_more A_st))(terpri)
;  )

; op A$$equation_comp_unparse: EquationComp -> Output
;(defun equation_comp$unparse (eq_comp)
;  (princ "---unparse of: ")(prin1 eq_comp)(terpri)(terpri)
;  (princ "---sz_left: ")(prin1 (equation_comp-sz_left eq_comp))(terpri)
;  (princ "---left: ") (dotimes (x (array-dimension 
;				   (equation_comp-left eq_comp) 0))
;		  (term$!print (aref (equation_comp-left eq_comp) x))
;		  )(terpri)
;  (princ "---right; ") (dotimes (x (array-dimension 
;				    (equation_comp-right eq_comp) 0))
;		  (term$!print (aref (equation_comp-right eq_comp) x))
;  (princ "---comp")(prin1 (equation_comp-comp eq_comp))(terpri)
;  )
