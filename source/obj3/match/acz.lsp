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

;; $Id: acz.lsp,v 206.1 2003/09/29 12:46:23 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;
;        ACZ matching routines
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; OBJ3:ACZ$*    
;;;;
;;;; All the functions for ACZ matching in the OBJ3 system.
;;;;
;;;; This is modified from the version of 
;;;; April, 1990 incorporating TW's 2 bug fixes,
;;;; and various 'improvements' by PDL.
;;;;
;;;;
;;;; Patrick Lincoln, March 1989 - April 1990, 
;;;;  SRI Declarative Languages Project, CSL, SRI.
;;;
;;; The basic structure of the representation is taken from:
;;; ``Adventures In Associative-Commutative Unification''
;;; Patrick Lincoln and Jim Christian, from the 
;;; {\em Journal of Symbolic Computation}.
;;; The basic idea is to represent the terms in an ACZ system of equations
;;; as vectors of bit-vectors (integers), (a virtual boolean matrix) which 
;;; represents the state of the ACZ system.  
;;;
;;; For the moment, there will be no HZ symetry check.  (because 
;;; conditional rewrite rules might invalidate the check, and I
;;; can't figure out how to find out if a rule is conditional from
;;; inside the ACZ proc.)
;;;
;;; Modification History:
;;; June 1989: first working version.  Many small fixes, improvements.
;;; July 13, 1989: PDL adds a GCD failure check.
;;; July-August 1989: PDL uses TW's performance meter to find slow code.
;;;                   Lots of small performance mods.  
;;;                   Some of TW's C code used in place of dotimes, etc.
;;; January 2 1990: PDL wakes up, and realizes that to handle systems of
;;;                 multiple equations, he can use compatibility bitvectors,
;;;                 eliminating the old way of just calling ACZ recursively.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; the following interface-definitions have not changed from CK+TW's OBJ3.
;;; pdl will not flame at length here on the relative merits of this interface.

; op ACZ$state_initialize: System Environment -> ACState, signals(no_match)
; op ACZ$next_state: ACState -> System ACState, signals(no_more)
; op ACZ$equal: Term Term -> Bool
; op ACZ$unparse_ACZ_state: ACState -> Output

;;; the general plan is to 
;;; 1> initialize the system, by changing the representation of terms.
;;; 2> simplify the equation by tossing matching terms from both sides, 
;;;    and noting the repetitions and duplicates.
;;; 3> detect trivial cases, one variable on LHS, constant remaining on LHS
;;; 4> build matrix
;;; 5> solve matrix
;;; 6> construct solution from matrix
;;;
;;; The most time consuming will be 1, because the
;;; term-representation ensures tests for e-equality are expensive.
;;; 2 is definately worthwhile.  Perhaps more simplification could be done.
;;; 3 may or may not be worthwhile, but it is implemented.
;;; 4 is simply (make-array X :initial element 1)
;;; 5 is essentially search, but pdl turned it into counting (by rotation)
;;; 6 isn't pretty, but not too costly.
;;; 
;;; Note that the representation used here (bitvectors) have remarkably
;;; non differentiable performance curves, the step at 31 bits notable
;;; among its features.  Thus if lots of rules have more than 31 terms
;;; (under the top level ACZ function symbol, and after simplification)
;;; this will perform poorly.  However, there can be any number of terms
;;; in the RHS (actuals), and if there are less than 32 terms then
;;; this should be nice and fast.
;;;
;;; Representation:  c=constant, f=functional term, v=variable
;;;     After simplification, where duplicates (terms appearing on both
;;;     sides of equation) are removed, we build a matrix:
;;;
;;;
;;;               LHS
;;;         c      f     v
;;;       ___________________
;;;     |
;;;    c|  a       b      c
;;;     |
;;; R   |  
;;; H  f|  d       e      f
;;; S   |
;;;     | 
;;;    v|  g       h      i
;;;
;;;
;;; g,h,i are disallowed because we are doing matching, and the
;;;       RHS is assumed ground.
;;; a,d are disallowed because once we have simplified, any constant
;;;     left on the LHS has nothing to match.
;;; b is disallowed because funs never match constants.
;;; That leaves c,e,f.
;;; 
;;; Since i do not group equal terms together, the rows and columns of
;;; this matrix are filled with ones and zeros.  (instead of arbitrary ints)
;;; Then search is performed by moving bits instead of moving ints.
;;;
;;; c is represented by a one dimensional array (sv) of bitvectors.  
;;; e,f are represented by another one dimensional array of bitvectors.  
;;;     (e and f are concatinated together, essentially).
;;;
;;; Note is made of repeated terms and of compatibility between LHS and
;;; RHS function symbols (I know this is O(N^2), but returning flocks of
;;; unsolvable systems is worse than spending a few microseconds 
;;; computing the "match$possibly_matches" once and for all.)
;;; These notes are used to aviod redundant search, and are kept in
;;; several auxilliary vectors.
;;;
;;;            LHS
;;;       f f f | v v v v
;;;       1 2 3 | 1 2 3 4
;;;   ___________________
;;;   c1        | 0 0 0 1
;;;   c2        | 0 0 1 0
;;;   c3   NO   | 1 0 0 0
;;;R  c4        | 0 1 0 0
;;;H  c5        | 0 0 1 0
;;;S  -------------------
;;;   f1  0 0 1 | 0 0 0 0
;;;   f2  1 0 0 | 0 0 0 0
;;;   f3  0 1 0 | 0 0 0 0
;;;   f4  0 0 0 | 0 1 0 0
;;;
;;; Above is a sample solution matrix
;;;  From this the solution is read downward through the rows:
;;;  lv1=rc3, lv2=op(rc4,rf4), lv3=op(rc2,rc5), lv4=rc1
;;;  lf1=rf2, lf2=rf3, lf3=rf1, 
;;;
;;; NO means that no ones are allowed in this region.
;;;
;;; For small efficiency gains, the bounds on the matrix are stored 
;;; instead of recomputed.
;;; 
;;; Detail:
;;; This scheme needs to be modified for repeated variables.
;;; First, the repetition of every variable, function, and constant
;;; is noted in a vector.  Then only things with enough duplicates
;;; may have a 1 in a repeted column.  Finally, when returning the
;;; solution (system of equations), ignore all but one of the repeated
;;; variables or terms.
;;;
;;; Detail:
;;; For each RHS function a compatibility vector is built by testing
;;; match$possibly_matches against each LHS function.  This is stored
;;; in the compatibility vector, which also contains information from
;;; the repetitions.
;;;
;;; Implementation note:  For now, only ACZ matching is implemented.  
;;; ACZ and ACI equations are not handled, although PDL thinks they
;;; could be without too much pain.
;;;

;; @note kiniry 25 Sept 2003 - incfa defined in ac.lsp already.
;; (defmacro incfa (x) `(setf ,x (1+ ,x)))

;; @note kiniry 25 Sept 2003 - make-array1 and make-array2 defined in
;; ac.lsp already.
;; Generic versions
;; (defun make-array1 (m)
;;   (make-array m :element-type 'fixnum :initial-element 0))
;; (defun make-array2 (m n)
;;   (make-array (list m n) :element-type 'fixnum :initial-element 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct ACZ-state
  operators ;; array[op]; the top level operators of each eqn in the system
  LHS-f ;; array[term]  ; functional terms
  LHS-v ;; array[term]  ; variables
  RHS-c ;; array[term]  ; constants 
  RHS-f ;; array[term]  ; functional terms
  LHS-f-r ;; array[bool] ; notes repeated functional terms
  LHS-v-r ;; array[bool] ; notes repeated variables
  RHS-c-r ;; array[bool] ; notes repeated constants
  RHS-f-r ;; array[bool] ; notes repeated functional terms
  (LHS-mask 0) ;; long int ; variables and funs accounted for by RHS-c-sol
  (LHS-f-mask 0) ;; long int ; funs accounted for by RHS-c-sol
  (LHS-r-mask 0) ;; long int ; bitvector of all repeated (>0) terms on lhs
  RHS-c-sol ; array[int] ; solution matrix; constants
  RHS-c-max ; int ; max value of elements of RHS-c-sol
  RHS-f-sol ; array[int] ; solution matrix; functional terms
  RHS-f-max ; int ; max value of elements of RHS-f-sol
  RHS-full-bits ; int 11111...111		
  RHS-c-compat ; array[int] ; array of compatibility bitvectors
  RHS-f-compat ; array[int] ; array of compatibility bitvectors
  LHS-c-count ; int ; number of constants on LHS after simplification
  LHS-f-count ; int ; number of functions on LHS after simplification
  LHS-v-count ; int ; number of variables on LHS after simplification
  RHS-c-count ; int ; number of constants on RHS after simplification
  RHS-f-count ; int ; number of functions on RHS after simplification
  (no-more nil) ; when true implies that all solutions have been reported
  )

(defvar *pdl-debug* nil)
(defvar *use-one-var-opt* nil)

; the following is used only when we boil it down to a single var on left.
; it is only for performance enhancement...
(defstruct trivial-acz-state
  (sys nil)
  (no-more-p nil))

;; small utility.  Side effect.
(defmacro ACZ$$Rotate-Left (array m)
"; shifts the element one bit to the left"
  `(setf (aref ,array ,m)
	 (* 2 (aref ,array ,m))))

;; @note kiniry 25 Sept 2003 - delete-one-term defined in ac.lsp already.
;; (defun delete-one-term (x y)
;;   (if (term$equational_equal x (caar y)) (cdr y)
;;       (let ((last y) (rest (cdr y)))
;;         (loop
;;            (when (null rest) (return 'none))
;;            (when (term$equational_equal x (caar rest))
;;              (rplacd last (cdr rest))
;;              (return y))
;;            (setq last rest  rest (cdr rest))))))

(defmacro ACZ$$note-repeats (mset array max gcd)
"; puts all repeated terms together in the list, and bashes the array
 ; (into numbers) in locations corresponding to the duplicate terms. 
 ; returns the newly grouped permutation of list.
 ; e.g. for input (a b c c c d d e f f) and #(0 0 0 0 0 0 0 0 0),
 ; this should make the array into #(0 0 3 2 1 2 1 0 2 1)."
  `(let* ((list2 nil)
	 (counter (array-dimension ,array 0)) )
    (dolist (element ,mset)
	    (let ((n (cdr element)))
	      (declare (fixnum n))
	    (when (> n ,max)
		  (setq ,max n))
	    (setq ,gcd (gcd ,gcd n))
	    (if (> n 1) ; if it is repeated at all
		(dotimes-fixnum (x n)
			 (push (first element) list2)
			 (setq counter (1- counter))
			 (setf (aref ,array counter) (1+ x)))
	        (progn (push (first element) list2)
		       (setq counter (1- counter))
		       (setf (aref ,array counter) 0))))) ; this line optional
    list2))                                     ; (if 0'd array is guaranteed)

(defmacro ACZ$$eq-member (term list)
 "predicate. true if term is term$equational_equal some element of list"
  `(dolist (term2 ,list)
      (when (term$equational_equal ,term (car ,list))
	    (return t))))

(defun ACZ$state_initialize (sys env)
"; takes a system of equations and an environment, and 
 ; returns an ACZ-state, which is suitable for framing
 ; or passing to 'ACZ$next_state'"
  (block TOP
  (let ((eqn-number -1)
	(sys-operators (make-array (length sys)))
	(all-lhs-vars nil)
	(all-lhs-funs nil)
	(all-rhs-constants nil)
	(all-rhs-funs nil)
	(*print-circle* t))
  (when *pdl-debug* (format t "~&acz-init: length=~A~&" (length sys)))
  (dolist (equation sys)
	  (incf eqn-number)
     (let* (
	 (lhs-1 (match_equation$t1 equation))
	 (rhs-1 (match_equation$t2 equation))
	 (lhs-op (term$head lhs-1))
	 (rhs-op (term$head rhs-1))
	 (lhs-2 (term$list_ACZ_subterms lhs-1 lhs-op))
	 (rhs-2
	  (if (operator$is_AC_restriction_of rhs-op lhs-op)
	    (term$list_ACZ_subterms rhs-1 rhs-op)
	    (list rhs-1)))
	 (lhs-vars nil)
	 (lhs-constants nil)
	 (lhs-funs nil)
	 (rhs-constants nil)
	 (rhs-funs nil)
	 )
    (setf (aref sys-operators eqn-number) lhs-op)
    ;; quick failure cases of AC do Not apply to ACZ
    ;; build lhs- vars/funs/constants
    (dolist (term lhs-2)
	    (cond ((term$is_var term)
		   (let ((image (if env (environment$image env term) term)))
		     (cond ((null image) 
			    (push (cons term eqn-number) lhs-vars))
			   ((term$is_var image)
			    (push (cons image eqn-number) lhs-vars))
			   ((operator$is_AC_restriction_of
			     ;@@ was operator$is_ACZ_restriction_of
			     lhs-op (term$head image))
			    (dolist (term2 (term$list_ACZ_subterms
					    image (term$head image)))
				    (cond ((term$is_var term2)
					   (push (cons term2 eqn-number)
						 lhs-vars))
					  ((term$is_constant term2)
					   (push (cons term2 eqn-number)
						 lhs-constants))
					  (t (push (cons term2 eqn-number)
						   lhs-funs)))))
			   ((term$is_constant image)
			    (push (cons image eqn-number) lhs-constants))
			   (t (push (cons image eqn-number) lhs-funs)))))
		  ((term$is_constant term)
		   (when (not (term$is_zero_for_op term lhs-op))
			 (push (cons term eqn-number) lhs-constants)))
		  (t (push (cons term eqn-number) lhs-funs))))
    ;; now that the lhs is partitioned - lets play with the rhs
    (dolist (term rhs-2)
	    (cond ((term$is_var term)
		   (push (cons term eqn-number) rhs-constants))
		  ((term$is_constant term)
		   (when (not (term$is_zero_for_op term lhs-op))
		   (let ((new (delete-one-term term lhs-constants)))
		     (if (eq 'none new)
			 (push (cons term eqn-number) rhs-constants)
		       (setq lhs-constants new)))))
		  (t (let ((new (delete-one-term term lhs-funs)))
		     (if (eq 'none new)
			 (push (cons term eqn-number) rhs-funs)
		       (setq lhs-funs new))))))
    ;; now there are no duplicates (things appearing on both sides)
    (let ((lhs-c-count (length lhs-constants))
	  (lhs-f-count (length lhs-funs))
	  (lhs-v-count (length lhs-vars))
	  (rhs-c-count (length rhs-constants))
	  (rhs-f-count (length rhs-funs))
	  )
      ;; check trivial failure conditions
      (when (or (> lhs-c-count 0)  ; a const without anything to match it
		(and (< lhs-v-count 1)    ; no variables remain on lhs
		     (> rhs-c-count 0))   ; and constants remain on rhs
		(> lhs-f-count rhs-f-count)) ; too many funs to match
	        ;; this assumption may be dubius in ACZ --- can arbitrary 
	        ;; funs eventually reduce to identity?
	    (return-from TOP (values nil t)))    ; FAIL most miserably
      (setq all-lhs-funs (nconc lhs-funs all-lhs-funs))
      (setq all-lhs-vars (nconc lhs-vars all-lhs-vars))
      (setq all-rhs-constants (nconc rhs-constants all-rhs-constants))
      (setq all-rhs-funs (nconc rhs-funs all-rhs-funs)))))
  ;; we are now done with all equations.
  ;; NOTE that we have now gathered all equations into one giant morass
  (when *pdl-debug* (format t "~&acz-init: all-eqns processed~&"))
  (cond ((and (null all-lhs-funs) ; nothing left, all formulas removed
	      (null all-lhs-vars))
	 (when *pdl-debug* (format t "~%acz-rare case - all terms collapse~%"))
	 (if (and (null all-rhs-constants) ; this is rare
		  (null all-rhs-funs))
	     (return-from TOP (values (make-trivial-acz-state 
				       :sys (system$new)) nil))
	     (return-from TOP (values nil t))))
	 ;; maybe check for more simple cases, like one-var vs the world.
	((and *use-one-var-opt*
	      (null all-lhs-funs)       ; only one var left on lhs
	      (null (cdr all-lhs-vars)))
	 (let ((fresh-sys (system$new)))
	   (when *pdl-debug* 
		 (format t "acz-one-var-opt : ~A~%---~A~%" (caar all-lhs-vars)
		   (car all-lhs-vars)))
	   (system$add_eq 
	    fresh-sys (match_equation$create (caar all-lhs-vars)
		       (ACZ$$make_term (aref sys-operators (cdar all-lhs-vars))
				       (nconc all-rhs-constants all-rhs-funs))))
	   (return-from TOP (values (make-trivial-acz-state :sys fresh-sys) 
				    nil))))
	(t
  (let* ( (lhs-f-count (length all-lhs-funs))
	  (lhs-v-count (1+ (length all-lhs-vars))) ; note this is "wrong"
	  (rhs-c-count (length all-rhs-constants))
	  (rhs-f-count (length all-rhs-funs))
	  (lhs-f-r (make-array lhs-f-count :element-type 'fixnum))
	  (lhs-v-r (make-array lhs-v-count :element-type 'fixnum))
	  (rhs-c-r (make-array rhs-c-count :element-type 'fixnum))
	  (rhs-f-r (make-array rhs-f-count :element-type 'fixnum))
	  (LHS-f-ms (ACZ$$list2multi_set all-lhs-funs)) ; expensive.
	  (LHS-v-ms (ACZ$$list2multi_set all-lhs-vars)) ; expensive.
	  (RHS-c-ms (ACZ$$list2multi_set all-rhs-constants)) ; expensive.
	  (RHS-f-ms (ACZ$$list2multi_set all-rhs-funs)) ; expensive.
	  (l-f-m 0) ; TCW 14 Mar 91 mods associated with this var
	  (l-v-m 0) ; note this is not used
	  (r-m 0)
	  (l-gcd (or (cdar lhs-f-ms) (cdar lhs-v-ms) 1))
	  (r-gcd (or (cdar rhs-f-ms) (cdar rhs-c-ms) 1))
	  (LHS-f-list (ACZ$$note-repeats lhs-f-ms lhs-f-r l-f-m l-gcd))
	  (LHS-v-list  (cons (cons 'if-this-appears-youve-lost 999) 
			     (ACZ$$note-repeats lhs-v-ms lhs-v-r l-v-m l-gcd)))
	  (RHS-c-list (ACZ$$note-repeats rhs-c-ms rhs-c-r r-m r-gcd))
	  (RHS-f-list (ACZ$$note-repeats rhs-f-ms rhs-f-r r-m r-gcd))
	  (LHS-f (make-array lhs-f-count :initial-contents lhs-f-list))
	  (LHS-v (make-array lhs-v-count :initial-contents lhs-v-list))
	  (RHS-c (make-array rhs-c-count :initial-contents rhs-c-list))
	  (RHS-f (make-array rhs-f-count :initial-contents rhs-f-list))
	  (RHS-c-max (expt 2 (1- lhs-v-count)))
	  (RHS-f-max (expt 2 (+ -1 lhs-v-count lhs-f-count)))
	  (RHS-full-bits (- (expt 2 (+ lhs-v-count lhs-f-count)) 2))
	  (rhs-c-sol (make-array rhs-c-count :element-type 'fixnum))
	  (rhs-f-sol (make-array rhs-f-count :element-type 'fixnum))
	  (rhs-c-compat (make-array rhs-c-count :element-type 'fixnum))
	  (rhs-f-compat (make-array rhs-f-count :element-type 'fixnum))
	  (dummy-bit 1) ; to save a whole bunch of expt'ing
	  (lhs-r-mask 0)
	  (state (make-ACZ-state))
	  )
    (declare (type (vector fixnum)  rhs-c-compat rhs-f-compat)
	     (fixnum dummy-bit lhs-r-mask l-gcd r-gcd l-f-m r-m))
    ;(declare (ignore l-v-m)) not strictly true
    ;; one more easy failure check
    ; TCW 14 Mar 91 need to restrict this for ACZ
    (when (or (> l-f-m r-m) ; a lhs item is repeated more than any rhs
	      (not (integerp (/ r-gcd l-gcd))))
	  (return-from TOP (values nil t)))    ; FAIL most miserably
    ;; NOW, get down to the real work....
    ;; setup the repeat mask (first of v's)
    (dotimes-fixnum (j lhs-v-count)
	     (when (> (aref lhs-v-r j) 1)
		   (setq lhs-r-mask (logior lhs-r-mask dummy-bit))
		   (setq dummy-bit (* 2 dummy-bit))))
    ;; note dummy-bit might not be 1 here...
    (dotimes-fixnum (j lhs-f-count) ; (then of f's)
	     (when (> (aref lhs-f-r j) 1)
		   (setq lhs-r-mask (logior lhs-r-mask dummy-bit))
		   (setq dummy-bit (* 2 dummy-bit))))
    ;; now setup the compatibility bitvectors (for rhs-c)
  (when *pdl-debug* (format t "~&acz-init: setup compat (c)~&"))
    (dotimes-fixnum (i rhs-c-count)
	     (setq dummy-bit 1)
	     (let ((my-repeat-count (aref rhs-c-r i)))
	       (declare (fixnum my-repeat-count))
	       (dotimes-fixnum (j lhs-v-count)
		   (when (and (= (cdr (aref rhs-c i)) (cdr (aref lhs-v j)))
			      ;; both are from same equation, AND
			      (or (= (aref lhs-v-r j) my-repeat-count)
				  ;; the right repetition number OR 0
				  (= (aref lhs-v-r j) 0)))
			 (setf (aref rhs-c-compat i)
			       (logior (aref rhs-c-compat i) dummy-bit)))
		   (setq dummy-bit (* 2 dummy-bit)))))
    ;; now setup the compatibility bitvectors (for rhs-f)
  (when *pdl-debug* (format t "~&acz-init: setup compat (f) ~&"))
    (dotimes-fixnum (i rhs-f-count)
	     (setq dummy-bit 1)
	     (let ((my-repeat-count (aref rhs-f-r i)))
	       (declare (fixnum my-repeat-count))
	       (dotimes-fixnum (j lhs-v-count)
		   (when (and (= (cdr (aref rhs-f i)) (cdr (aref lhs-v j)))
			      ;; both are from same equation, AND
			      (or (= (aref lhs-v-r j) my-repeat-count)
				  (= (aref lhs-v-r j) 0)))
			 (setf (aref rhs-f-compat i)
			       (logior (aref rhs-f-compat i) dummy-bit)))
		   (setq dummy-bit (* 2 dummy-bit)))
	       ;; now lhs vars are taken care of, we need to deal with funs
	       (dotimes-fixnum (j lhs-f-count)
		   ;; for now, ignore repetition of funs (can be slower)
		   (when (and (= (cdr (aref rhs-f i)) (cdr (aref lhs-f j)))
			      ;; both are from same equation, AND
			      (match$possibly_matches (car (aref lhs-f j))
						      (car (aref rhs-f i))))
			 (setf (aref rhs-f-compat i)
			       (logior (aref rhs-f-compat i) dummy-bit)))
		   (setq dummy-bit (* 2 dummy-bit)))))
    ;; and now set up the initial state to a legal one (the smallest legal one)
    ;; by just rotating the bit until it logand's with the compatibility vector
    (dotimes-fixnum (i rhs-c-count)
	     (setq dummy-bit 1)
	     (if (and (= i 0) (= rhs-f-count 0))
		 (setf (aref rhs-c-sol 0) 1)
	     (let ((my-compat (aref rhs-c-compat i)))
	       (declare (fixnum my-compat))
	       (do ()
		   ((> dummy-bit rhs-c-max) 
		    (return-from TOP (values nil t)))
		   (when (not (zerop (logand dummy-bit my-compat)))
			 (setf (aref rhs-c-sol i) dummy-bit)
			 (return))
		   (setq dummy-bit (* 2 dummy-bit))))))
    (dotimes-fixnum (i rhs-f-count)
	     (setq dummy-bit 1)
	     (if (= i 0)
		 (setf (aref rhs-f-sol 0) 1)
	       (let ((my-compat (aref rhs-f-compat i)))
		 (declare (fixnum my-compat))
		 (do ()
		    ((> dummy-bit rhs-f-max) (return-from TOP
							  (values nil t)))
		    (when (not (zerop (logand dummy-bit my-compat)))
			  (setf (aref rhs-f-sol i) dummy-bit)
			  (return))
		    (setq dummy-bit (* 2 dummy-bit))))))
    ;; initialize the mask -
    (if (= rhs-f-count 0)
	(setf (ACZ-state-LHS-mask state) 0)
        (let ((temp 0))
	  (declare (fixnum temp))
	  (dotimes-fixnum (s rhs-c-count)
			  (setq temp (logior temp (aref rhs-c-sol s))))
	  (setf (ACZ-state-LHS-mask state) temp)))
    ;; and now stuff the state full of information, and return it.
	  (setf (ACZ-state-operators state) sys-operators)
	  (setf (ACZ-state-LHS-f state) lhs-f)
	  (setf (ACZ-state-LHS-v state) lhs-v)
	  (setf (ACZ-state-RHS-c state) rhs-c)
	  (setf (ACZ-state-RHS-f state) rhs-f)
	  (setf (ACZ-state-LHS-f-r state) lhs-f-r)
	  (setf (ACZ-state-LHS-v-r state) lhs-v-r)
	  (setf (ACZ-state-RHS-c-r state) rhs-c-r)
	  (setf (ACZ-state-RHS-f-r state) rhs-f-r)
	 ; (setf (ACZ-state-LHS-mask state) 0)
	  (setf (ACZ-state-LHS-f-mask state) 0)
	  (setf (ACZ-state-LHS-r-mask state) lhs-r-mask)
	  (setf (ACZ-state-RHS-c-sol state) rhs-c-sol)
	  (setf (ACZ-state-RHS-c-max state) rhs-c-max)
	  (setf (ACZ-state-RHS-f-sol state) rhs-f-sol)
	  (setf (ACZ-state-RHS-f-max state) rhs-f-max)
	  (setf (ACZ-state-RHS-full-bits state) rhs-full-bits)
	  (setf (ACZ-state-RHS-c-compat state) rhs-c-compat)
	  (setf (ACZ-state-RHS-f-compat state) rhs-f-compat)
	  (setf (ACZ-state-LHS-c-count state) 0)
	  (setf (ACZ-state-LHS-f-count state) lhs-f-count)
	  (setf (ACZ-state-LHS-v-count state) lhs-v-count) ; off 1+ intentionally
	  (setf (ACZ-state-RHS-c-count state) rhs-c-count)
	  (setf (ACZ-state-RHS-f-count state) rhs-f-count)
	  (setf (ACZ-state-no-more state) nil)
;	  (setf (ACZ-state-p state) 'ACZ-state)
;	  (when *pdl-debug* (format t "~&acz-init: state=~&")
;		(ACZ$unparse_ACZ-state state))
	  (values state nil)))))))

(defun ACZ$next_state (state)
  (when *pdl-debug* (format t "~&acz-next: ")
	(cond ((acz-state-p state)
	       (ACZ$unparse_ACZ-state state))
	      ((trivial-acz-state-p state)
	       (ACZ$trivial-unparse state))
	      (t 
	       (format t "~%~% ACZ$next_state called on non acz-state ~%~%"))))
  (if (not (ACZ-state-p state))
      (if (trivial-acz-state-p state)
	  (if (trivial-acz-state-no-more-p state)
	      (values nil nil t)
	      (progn 
		(setf (trivial-acz-state-no-more-p state) t)
		(values (trivial-acz-state-sys state) state nil)))
	  (progn (format t "~& ACZ$Next_State given non ACZ-state:~A~&" state)
		 (values nil nil t))) ; error behavior is failing
   (if (ACZ-state-no-more state)
      (values nil nil t)       ;; there are no more solutions - so fail
      (do* ((n 0) 
	    (rhs-f-sol (ACZ-state-rhs-f-sol state))
	    (rhs-f-max (ACZ-state-rhs-f-max state))
	    (rhs-f-compat (ACZ-state-rhs-f-compat state))
	    (rhs-f-count (ACZ-state-rhs-f-count state))
;	    (rhs-full-bits (ACZ-state-rhs-full-bits state)) ;@@
	    (lhs-r-mask (ACZ-state-lhs-r-mask state))
	    )
	   (nil) ; do forever
	   (declare (fixnum rhs-f-count rhs-f-max lhs-r-mask))
	   (cond ((>= n rhs-f-count) ; no next row
		  (ACZ$$next-state-sub state)
		  (if (ACZ-state-no-more state)
		      (if (and (= 0 (acz-state-LHS-f-count state))
			       (<= 1 (acz-state-LHS-v-count state))
			       (= 0 (acz-state-RHS-c-count state))
			       (= 0 (acz-state-RHS-f-count state))
			       ; TCW 14 Mar 91 vaguely plausible
			       (let ((lhs-v (ACZ-state-lhs-v state))
				     (ops (ACZ-state-operators state)))
			       (dotimes-fixnum (i (array-dimension lhs-v 0) t)
			         (if (< i 1) nil
				   (unless
				       (sort_order$is_included_in
					(module$sort_order obj$current_module)
					(term$sort
					 (car (theory$zero (operator$theory
				         (aref ops (cdr (aref lhs-v i)))))))
					(term$sort (car (aref lhs-v i))))
				     (return nil))))))
			(return (values (ACZ$$solution_from_state state)
					state nil))
			(return (values nil nil t))) ; failed at f-level
		      (setq n (1- n))))
		 ((< n 0)
		  (let ((temp (ACZ-state-LHS-mask state)))
;acz		    (logior (- (expt 2 (ACZ-stat-lhs-v-count state)) 1))
		    (dotimes-fixnum (s rhs-f-count)
			     (setq temp (logior temp (aref rhs-f-sol s))))
;		    (if (= rhs-full-bits temp) ;@@
			(return (values (ACZ$$solution_from_state state)
					state nil))
;		        (setq n 0))
		    ))
		 ((< (aref rhs-f-sol n) rhs-f-max)
		  (ACZ$$Rotate-Left rhs-f-sol n)
		  (when (and ;; this is a compatible position for this bit
			     (> (logand (aref rhs-f-sol n)
					(aref rhs-f-compat n)) 0)
			     ;; either this isnt a repeated term
			     (or (zerop (logand (aref rhs-f-sol n) lhs-r-mask))
				 ;; or it is, and its upper neighbor is home
				 (and (< (1+ n) rhs-f-count)
				      (= (* 2 (aref rhs-f-sol n))
					 (aref rhs-f-sol (1+ n))))))
			(setq n (1- n)))) ; then this row is ok, else redo 
		 (t ; this row (n) is already maxed
		  (setf (aref rhs-f-sol n) 1) ; reset this row to one
		  (setq n (1+ n)))))))) ; go to next row

(defun acz$args_nss (x) (ACZ$unparse_ACZ-state (car x)) (terpri))
(setf (get 'ACZ$$next-state-sub 'print_args) 'acz$args_nss)

(defun ACZ$$next-state-sub (state)
  (do* ((m 0)  ; only initialize these vars 
	(rhs-c-sol (ACZ-state-rhs-c-sol state))
	(rhs-c-max (ACZ-state-rhs-c-max state))
	(rhs-c-count (ACZ-state-rhs-c-count state))
	(rhs-c-compat (ACZ-state-rhs-c-compat state))
	(lhs-r-mask (ACZ-state-lhs-r-mask state)))
       (nil) ; forever
       (declare (type (vector fixnum) rhs-c-compat) 
		(fixnum lhs-r-mask rhs-c-count))
       (cond ((>= m rhs-c-count) ; no next row
	      (setf (ACZ-state-no-more state) T)
	      (return))
	     ((< m 0) ; no tests up here - could cut search here
	      (let ((temp 0)) ; the empty bitvector
		(dotimes-fixnum (s rhs-c-count)
			 (setq temp (logior temp (aref rhs-c-sol s))))
		(setf (ACZ-state-LHS-mask state) temp)
		(return)))
	     ((< (aref rhs-c-sol m) rhs-c-max)
	      (ACZ$$Rotate-Left rhs-c-sol m)
	      (when (and ;; this is a compatible position for this bit
		     (> (logand (aref rhs-c-sol m)
				(aref rhs-c-compat m)) 0)
		     ;; either this isnt a repeated term
		     (or (zerop (logand (aref rhs-c-sol m) lhs-r-mask))
			 ;; or it is, and its upper neighbor is home
			 (and (< (1+ m) rhs-c-count)
			      (= (* 2 (aref rhs-c-sol m))
				 (aref rhs-c-sol (1+ m))))))
		    (setq m (1- m)))) ; then this row is ok, else redo this row
	     (t ; this row (m) is already maxed
	      (setf (aref rhs-c-sol m) 1) ; reset this row
	      (setq m (1+ m)))))) ; go to next row

;; don't even think of using this again
(defmacro ACZ$$collapse-one-array-internal (rhs-sol rhs-array)
  `(dotimes-fixnum (j (array-dimension ,rhs-sol 0))
	    (when (> (logand (aref ,rhs-sol j) term-code) 0)
		  (push  (car (aref ,rhs-array j)) rhs-subterms))))

;; don't even think of using this again
(defmacro ACZ$$collapse-arrays-internal (lhs skip) 
  `(dotimes-fixnum (i (array-dimension ,lhs 0))
      (if (< i ,skip)
          nil
          (progn 
	(setq rhs-subterms nil)
	(setq term-code (* 2 term-code))
	(ACZ$$collapse-one-array-internal rhs-c-sol rhs-c)
	(ACZ$$collapse-one-array-internal rhs-f-sol rhs-f)
;	(print$brief (car (aref ,lhs i)))
;       (map nil #'print$brief rhs-subterms)
	(system$add_eq 
	 new-sys 
	 (match_equation$create (car (aref ,lhs i))
                  (if (null rhs-subterms)
		    (term$make_zero (aref ops (cdr (aref ,lhs i))))
		  (if (cdr rhs-subterms) ; implies length is greater than 1
		      (term$make_right_assoc_normal_form_with_sort_check
		          (aref ops (cdr (aref ,lhs i))) rhs-subterms)
		      (first rhs-subterms)))))))))

;; note that this relies on side effects of the macro above.
(defun ACZ$$solution_from_state (state)
"; given an ACZ-state, produce a solution (system of equations
 ; which, if true, imply the original ACZ equation true) from
 ; the matrix of 'state'"
  (let* ((ops (ACZ-state-operators state))
	 (lhs-f (ACZ-state-lhs-f state))
	 (lhs-v (ACZ-state-lhs-v state))
	 (rhs-c (ACZ-state-rhs-c state))
	 (rhs-f (ACZ-state-rhs-f state))
	 (rhs-c-sol (ACZ-state-rhs-c-sol state))
	 (rhs-f-sol (ACZ-state-rhs-f-sol state))
	 (new-sys (system$new))
	 (term-code 1)
	 (rhs-subterms nil))
    (ACZ$$collapse-arrays-internal lhs-v 1) 
    ;; note term-code is now the right thing. 
    (ACZ$$collapse-arrays-internal lhs-f 0) 
    (when *pdl-debug* 
	  (format t "~&acz-begin-winner: ")
	  (acz$unparse_ACZ-state state)
	  (format t "~&acz-end winner.~%")
	  )
    new-sys))

;; printout of important parts of ACZ state.
(defun ACZ$unparse_ACZ-state (ACZ_st)
  (format t "~&no more=~A~%" (ACZ-state-no-more ACZ_st))
  (format t "~&operators: ~&")
  (map nil #'print$brief (ACZ-state-operators ACZ_st))
  (format t "~&RHS-f: ~&")
  (map nil #'print$brief (ACZ-state-RHS-f ACZ_st))
  (format t "~&RHS-c: ~&")
  (map nil #'print$brief (ACZ-state-RHS-c ACZ_st))
  (format t "~&LHS-v: ~&")
  (map nil #'print$brief (ACZ-state-LHS-v ACZ_st)) 
  (format t "~&LHS-f: ~&")
  (map nil #'print$brief (ACZ-state-LHS-f ACZ_st))
  (format t "~& rhs-c-count=~A, rhs-f-count=~A~&"
	  (ACZ-state-RHS-c-count ACZ_st) 
	  (ACZ-state-RHS-f-count ACZ_st))
  (format t "~& lhs-c-count=~A, lhs-f-count=~A, lhs-v-count=~A~%"
	  (ACZ-state-LHS-c-count ACZ_st) 
	  (ACZ-state-LHS-f-count ACZ_st) 
	  (ACZ-state-LHS-v-count ACZ_st))
  (let ((*print-base* 2)) ; these be bitvectors, print them as such
    (format t "-------------------~%rhs-c-sol= ~A~&rhs-f-sol=~A~&"
	    (ACZ-state-RHS-c-sol ACZ_st) (ACZ-state-RHS-f-sol ACZ_st))
    (format t "~& rhs-c-max=~A, rhs-f-max=~A, rhs-full-bits=~A~&"
	    (ACZ-state-RHS-c-max ACZ_st) 
	    (ACZ-state-RHS-f-max ACZ_st)
	    (ACZ-state-RHS-full-bits ACZ_st))
    (format t "~& rhs-c-compat=~A, rhs-f-compat=~A~&"
	    (ACZ-state-RHS-c-compat ACZ_st) 
	    (ACZ-state-RHS-f-compat ACZ_st))
    (format t "~& rhs-c-r=~A, rhs-f-r=~A~&"
	    (ACZ-state-RHS-c-r ACZ_st) 
	    (ACZ-state-RHS-f-r ACZ_st))
    (format t "~& lhs-f-r=~A, lhs-v-r=~A~&"
	    (ACZ-state-LHS-f-r ACZ_st) 
	    (ACZ-state-LHS-v-r ACZ_st))
    (format t "~& lhs-mask=~A~%"
	    (ACZ-state-LHS-mask ACZ_st))
    (terpri)
    (format t "~& lhs-f-mask=~A~%"
	    (ACZ-state-LHS-f-mask ACZ_st))
    (format t "~& lhs-r-mask=~A~%"
	    (ACZ-state-LHS-r-mask ACZ_st))
    ))

;;
;; unchanged (this isn't good, but it works).  -pdl
;;
; Assume that t1 is NOT a variable
; op ACZ$equal: Term Term -> Bool
(defun ACZ$equal (t1 t2)
  (let ((op (term$head t1)))
    (ACZ$ms_equal
     (list2multi_set_list (term$list_ACZ_subterms t1 op))
     (list2multi_set_list (term$list_ACZ_subterms t2 op)))))

; check for multi-set equality
;uses term$equational_equal -- which can be pretty expensive
(defun ACZ$ms_equal (x y)
  (block the_end
  (let ((ydone 0))
    (dolist (xe x)
      (let ((xterm (car xe)) (xval (cdr xe)))
      (dolist (ye y
	       (return-from the_end nil)) ; didn't find xe in y
	(when (term$equational_equal xterm (car ye))
	  (unless (= xval (cdr ye)) (return-from the_end nil))
	  (setq ydone (1+ ydone))
	  (return))))) ; quit the inner do-list
    (unless (= ydone (length y)) (return-from the_end nil)))
    t))

;;; NOTE  this is a version for ACZ-internal use only.
;; it simply takes care of the "from which equation" info.
;; the input list is like
;; ((A . 1) (B . 2) (A . 1) (A . 3))
;; the result is like
;; (((A . 1) . 2) ((B . 2) . 1) ((A . 3) . 1))
;; where the A.1 means 'term a from equation 1'.
;; and (a.1).2  means 'term a from equation 1' appears twice.
(defun ACZ$$list2multi_set (list)
  (let ((ms_list nil))
    (dolist (x list)
	    (let ((ms_elt (assoc-if #'(lambda (y) 
			     (and (= (cdr x) (cdr y))
				  (term$equational_equal (car y) (car x))))
			      ms_list)))
	      (if ms_elt
		  (rplacd ms_elt (1+ (cdr ms_elt)))
		  (push (cons x 1) ms_list))))
    ms_list))

; op ACZ$$make_term : Operator List Of Term
; create a single term from a collection of terms
(defun ACZ$$make_term (op list)
  (if (null list)
      (term$make_zero op)
      (if (null (cdr list))
	  (car list)
	  (term$make_term_with_sort_check 
	    op (list (car list) (ACZ$$make_term op (cdr list)))))))

(defun acz$trivial-unparse (state)
  (let ((sys (trivial-acz-state-sys state))
	(no-more-p (trivial-acz-state-no-more-p state)))
    sys
    (format t "~% acz-unparse-trivial no-more-p = ~A~%" no-more-p)
    ))
