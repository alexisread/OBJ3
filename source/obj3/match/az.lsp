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

;; $Id: az.lsp,v 205.2.1.1 2003/09/23 14:09:24 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                  AZ matching
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; PDL modified TW's modification of CK's A code, 
;;; to handle AZ.  The bulk of the old A code is
;;; intact, and does 'the right thing', so PDL did
;;; not rewrite it.
;;;
;;; The substantive changes are:
;;; in AZ$state_initialize:
;;;    using term$list_associative_id_subterms in place of
;;;          term$list_associative_subterms.
;;; in AZ$state_initialize:
;;;    not failing if pattern is bigger than term, since vars
;;;          could be bound to zero, and shrink the term.
;;; in AZ$$increment_the_AZ_state:
;;;    searching through possibilities where LHS terms even 
;;;          have the possibility of matching nothing (they are zero)
;;;
;;; possible optimization: only let LHS vars match more than one
;;; or less than one thing.  LHS terms must match exactly one.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; all the procedures specific to the associative theory
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; op AZ$state_initialize: System Environment
;			 -> AssociativeState, signals(no_match)
; op AZ$next_state: AssociativeState -> System AssociativeState, 
; op AZ$equal: Term Term -> Bool
; op AZ$state_unparse: AssociativeState -> Output
;;;; Local
; op AZ$$associative_simplify: List[Term] List[Term] -> List[Term] List[Term]
; op AZ$$associativity_set_eq_state: List[Term] List[Term] -> EquationComp
; op AZ$$extract_in_from_to: Vector[Term] Int Int -> List[Term]
; op AZ$$increment_the_AZ_state: AssociativeState -> AssociativeState~
; op AZ$$try_increase_lexico: Vector[Int] Int -> Bool
; op AZ$$reset_equation_comp: Vector[EquationComp] Int -> Vector[EquationComp]
; op AZ$$reset_comp: EquationComp -> EquationComp~
; op AZ$$equation_comp_unparse: EquationComp -> Output

;;;;;;;;;;;;;;;; Representation

; AZ_state = associative_state

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

 (defstruct AZ_state
        (size 0 :type integer :read-only t)
	(operator nil :read-only t)
	sys ; array[eq_comp]
	(no_more nil ; :bool
		 )
	)

; already defined in assoc.lsp...
;(defstruct equation_comp
;        (sz_left 0 :type integer :read-only t)
;        left
;	right
;	comp
;	)

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

; op AZ$state_initialize: System Environment
;			 -> AssociativeState, signals(no_match)

(defun AZ$state_initialize (sys env)
  (block no_match
    (let* ((dim (system$size sys))
	   (assoc_sys (make-array dim))
	   (op nil)
	   (i 0)
	   )
      (dolist (equation (system$list_eq sys))
	(let ((t1 (match_equation$t1 equation))
	      (t2 (match_equation$t2 equation)))
	  (setq op (term$head t1))
	  (let ((sub1 (term$list_assoc_id_subterms t1 op))
		(sub1add nil)
		(sub2 (term$list_assoc_id_subterms t2 op)))
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
			      (setq sub1add
				    (nconc ;31 Mar 88 was append
				     (reverse
				      (term$list_assoc_id_subterms ima head))
				     sub1add
				     ))
			      (push ima sub1add)
			     ))))
		 (push val sub1add)))
 	    (setq sub1 (nreverse sub1add)) ; a bit tricky
; pdl - the following is invalid in AZ case, since a big term with ID's can shrink
;	    (when (> (length sub1) (length sub2))
;	      (return-from no_match (values nil t)))
	    (multiple-value-setq (sub1 sub2)
	      (AZ$$associative_id_simplify sub1 sub2))
	    ;31 Mar 88 sub1 may be nil but have modified AZ$$..set_eq_state
	    (setf (svref assoc_sys i)
		  (AZ$$associativity_id_set_eq_state sub1 sub2))
	    ))
	(setq i (1+ i))
	) ;end do
      (values 
	(make-AZ_state 
	  :size dim 
	  :operator op
	  :sys assoc_sys)
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

(defun AZ$$associative_id_simplify (sub1 sub2)
  (do ((t1 (car sub1) (car sub1))
       (t2 (car sub2) (car sub2)))
      ((or (not sub1) (not sub2) (not (term$equational_equal t1 t2))))
    (pop sub1)
    (pop sub2)
    )
  (setq sub1 (nreverse sub1) ; pdl changed this to nreverse
	sub2 (nreverse sub2)) ; 
  (do ((t1 (car sub1) (car sub1))
       (t2 (car sub2) (car sub2)))
      ((or (not sub1) (not sub2) (not (term$equational_equal t1 t2))))
    (pop sub1)
    (pop sub2)
    )
  (values (nreverse sub1) (nreverse sub2)) ;11 Feb 88 made nreverse
  )

; initialize the first state associated with sub1 sub2

; op AZ$$associativity_id_set_eq_state: List[Term] List[Term] -> EquationComp
  
(defun AZ$$associativity_id_set_eq_state (sub1 sub2)
  (let* ((sz1 (length sub1))
	 (comp (make-array (if (= 0 sz1) 0 (- sz1 1)) ;31 Mar 88 0 case
			   :initial-element 0))) ; pdl - az  ;@@@
    (values (make_equation_comp 
	     sz1 
	     (make-array (length sub1) :initial-contents sub1)
	     (make-array (length sub2) :initial-contents sub2)
	     comp))))

; op AZ$next_state: AssociativeState -> System AssociativeState, 
;                                      signals(no_more)

(defun AZ$next_state (AZ_st)
  (when *pdl-debug* 
	(AZ$state_unparse AZ_st))
  (let* ((new_sys (system$new))
 	 (sz (AZ_state-size AZ_st))
	 (sys (AZ_state-sys AZ_st))
	 (op (AZ_state-operator AZ_st))
	 )
    (if (AZ_state-no_more AZ_st)	
	; there is no more AZ_state
	(values nil nil t)
        (progn
	  (dotimes (k sz)		; k = 0,...,sz-1
		   ;; i.e. for each equation of the system
		   (let* ((eq_comp (svref sys k))
			  (sz_left (equation_comp-sz_left eq_comp))
			  (left (equation_comp-left eq_comp))
			  (right (equation_comp-right eq_comp))
			  (sz_right (array-dimension right 0))
			  (comp (equation_comp-comp eq_comp)))
		     (dotimes (l sz_left) ; l = 0,...,sz_left - 1
		       ;; i.e. for each term of the left hand 
		       ;; side of the equation 
		       (let ((deb (if (= l 0)
				      0 
				      (svref comp (1- l))))
			         ; was 1+ &&&&
			     (fin (if (= l (1- sz_left)) 
				      sz_right ;@@@
				      (svref comp l))))
			         ; 1- was omitted &&&&
			 (system$add_eq 
			  new_sys 
			  (match_equation$create 
			   (svref left l)
			   (AZ$$make_term op right deb fin)
			   ))
			 ) ;; let
		       ) ;; do
		     )
		   ) ;; do
	  (AZ$$increment_the_AZ_state AZ_st)		; AZ_st is modified
	  (when *pdl-debug* 
		(format t "~%az-winner is: ~A~%" new_sys))
	  (values new_sys AZ_st nil)
	  )
	)
    )
  )

; op AZ$$make_term : Operator Array[Term] Int Int -> Term
; create a single term from a collection of terms
(defun AZ$$make_term (op vect first last)
  (cond
   ((= first last) (term$make_zero op))
   ((= (1+ first) last) (svref vect first))
   (t
    (let ((res (svref vect (1- last))))
      (do ((i (- last 2) (1- i)))
	  ((< i first) res)
	(when (not  (term$is_zero_for_op (svref vect i) op))
	      (setq res (term$make_term_with_sort_check 
			 op (list (svref vect i) res)))))
      res))))

; modify the AZ_state "AZ_st" by  incrementing the state local to each
; equation of the system in a "variable basis numeration" way

; op AZ$$increment_the_AZ_state: AssociativeState -> AssociativeState~
(defun AZ$$increment_the_AZ_state (AZ_st)
  (block the_end
  (let ((sz (AZ_state-size AZ_st))
	(sys (AZ_state-sys AZ_st)))
  (let ((k 0) eq_comp)
  (loop
   (when (<= sz k) (return))
   (setq eq_comp (aref sys k))
   (when (AZ$$try_increase_lexico
	  (equation_comp-comp eq_comp)
	  (length (equation_comp-right eq_comp)) ;@@@
	  )
     ; note that AZ$$try_increase_lexico modify in this case
     ; the  "comp"  of the current equation.
     ; After that the previous composant are reset like in
     ; 599 -> 600
     (AZ$$reset_equation_comp sys k)
     (return-from the_end (values nil)) ;&&&& was (values) -- undo?
     ;otherwise, try to increase the next equation
     )
   (setq k (1+ k))
   ))
  ; this "normal" exit of the loop means that none of the
  ; state has been increased so there is no more state
  (setf (AZ_state-no_more AZ_st) t)
  ))
  )

; try to increase with respect with the lexicographical order
; on the arrays of integer the integer array "comp". These are
; the following constaints:
; 1) the elements of the array are all different and ordered in 
; pdl addition:  "weakly"
; increasing order: example (2 3 4 6 8)
; 2) the grastest element of the array in LESS OR EQUAL to "max"
; Returns true iff one have succeded to increment.

; op AZ$$try_increase_lexico: Vector[Int] Int -> Bool
(defun AZ$$try_increase_lexico (comp max)
  (let ((lim (1- (length comp))))
  (do ((i lim (- i 1)))
      ((< i 0) nil)
    (let ((x (svref comp i)))
      (when (< x max)
	(setf (svref comp i) (1+ x))
	  (do ((j (1+ i) (1+ j)))
	      ((< lim j))
	      (setf (svref comp j) (1+ x)))
	(return t)
	)))))

; modifies the array "sys" of "equation_comp" in such a way that
; all the comp array are reset provide that they rank in "sys"
; is less (strictly) than K.

; op AZ$$reset_equation_comp: Vector[EquationComp] Int -> Vector[EquationComp]
(defun AZ$$reset_equation_comp (sys K)
  (dotimes (i K)				; i = 0,...,K-1 
    (AZ$$reset_comp (svref sys i))
    )
  )

; reset the comp of "eq_comp" to his initial value i.e. (1,1,1,1,1)
; op AZ$$reset_comp: EquationComp -> EquationComp~
(defun AZ$$reset_comp (eq_comp)
  (let ((comp (equation_comp-comp eq_comp)))
    (dotimes (x (1- (equation_comp-sz_left eq_comp))) ; x = 0,...,sz_left - 2
      (setf (svref comp x) 0)
      )
    )
  )

; op AZ$equal: Term Term -> Bool
(defun AZ$equal (t1 t2)
  (let ((l1 (term$subterms (term$right_associative_id_normal_form t1)))
	(l2 (term$subterms (term$right_associative_id_normal_form t2))))
	;; note that l1 and l2 have zero's stripped out by normal-form
    (when (= (length l1) (length l2))
       (do* ((x1 l1 (cdr x1))
	     (x2 l2 (cdr x2))
	     (equal-p (term$equational_equal (car x1) (car x2))
		      (term$equational_equal (car x1) (car x2))))
	    ((or (not equal-p) (null x1))
	     equal-p)))))
 
; op AZ$state_unparse: AssociativeState -> Output
(defun AZ$state_unparse (AZ_st)
  (print "--------------- unparse of: ") 
;;(prin1 AZ_st)(terpri)
  (princ "size: ")(prin1 (AZ_state-size AZ_st))(terpri)
  (princ "operator: ")(prin1 (AZ_state-operator AZ_st))(terpri)
  (princ "sys: ")(dotimes (x (array-dimension (AZ_state-sys AZ_st) 0))
		  (equation_comp$unparse (svref (AZ_state-sys AZ_st) x))
		  )(terpri)
  (princ "no_more: ")(prin1 (AZ_state-no_more AZ_st))(terpri)
  )

; op AZ$$equation_comp_unparse: EquationComp -> Output
(defun equation_comp$unparse (eq_comp)
  (princ "---unparse of: ")(prin1 eq_comp)(terpri)(terpri)
  (princ "---sz_left: ")(prin1 (equation_comp-sz_left eq_comp))(terpri)
  (princ "---left: ") (dotimes (x (array-dimension 
				   (equation_comp-left eq_comp) 0))
		  (prin1 (svref (equation_comp-left eq_comp) x))
		  )(terpri)
  (princ "---right; ") (dotimes (x (array-dimension 
				    (equation_comp-right eq_comp) 0))
		  (prin1 (svref (equation_comp-right eq_comp) x))
		  )(terpri)
  (princ "---comp")(prin1 (equation_comp-comp eq_comp))(terpri)
  )
