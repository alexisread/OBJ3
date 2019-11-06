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

;; $Id: ac.lsp,v 206.1 2003/09/29 12:46:23 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;        AC matching routines
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; OBJ3:AC$*    
;;;;
;;;; Version as of April, 1990 incorporating TW's 2 bug fixes,
;;;; and various 'improvements' by PDL.
;;;;
;;;; All the functions for AC matching in the OBJ3 system.
;;;;
;;;; Patrick Lincoln, March 1989 - April 1990, 
;;;  SRI Declarative Languages Project, CSL, SRI.
;;;
;;; The basic structure of the representation is taken from:
;;; ``Adventures In Associative-Commutative Unification''
;;; Patrick Lincoln and Jim Christian, from the 
;;; {\em Journal of Symbolic Computation}.
;;; The basic idea is to represent the terms in an AC system of equations
;;; as vectors of bit-vectors (integers), (a virtual boolean matrix) which 
;;; represents the state of the AC system.  
;;;
;;; For the moment, there will be no HZ symetry check.  (because 
;;; conditional rewrite rules might invalidate the check, and I
;;; can't figure out how to find out if a rule is conditional from
;;; inside the AC proc.)
;;;
;;; Modification History:
;;; June 1989: first working version.  Many small fixes, improvements.
;;; July 13, 1989: PDL adds a GCD failure check.
;;; July-August 1989: PDL uses TW's performance meter to find slow code.
;;;                   Lots of small performance mods.  
;;;                   Some of TW's C code used in place of dotimes, etc.
;;; January 2 1990: PDL wakes up, and realizes that to handle systems of
;;;                 multiple equations, he can use compatibility bitvectors,
;;;                 eliminating the old way of just calling AC recursively.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; the following interface-definitions have not changed from CK+TW's OBJ3.
;;; pdl will not flame at length here on the relative merits of this interface.

;; op AC$state_initialize: System Environment -> ACState, signals(no_match)
;; op AC$next_state: ACState -> System ACState, signals(no_more)
;; op AC$equal: Term Term -> Bool
;; op AC$unparse_AC_state: ACState -> Output

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
;;; (under the top level AC function symbol, and after simplification)
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
;;; Implementation note:  For now, only AC matching is implemented.  
;;; ACZ and ACI equations are not handled, although PDL thinks they
;;; could be without too much pain.
;;;

(defmacro dotimes-fixnum (&rest body)
  (let ((var (car (car body)))
        (lim (cadr (car body)))
        (res (cddr (car body)))
        (acts (cdr body))
        (limvar (gensym))
        (lab (gensym)))
    `(block ()
       (let* ((,limvar ,lim) (,var 0))
         (declare (fixnum ,var ,limvar))
         (tagbody
            ,lab
            (if (>= ,var ,limvar) (return (progn ,@res)))
            (tagbody ,@acts)
            (setq ,var (1+ ,var))
            (go ,lab))))))

(defmacro incfa (x) `(setf ,x (1+ ,x)))

;;----------------------------------------------------------------------
;; special purpose array-allocation routines

#+GCL
(Clines "
enum aelttype {  /* array element type */
aet_object,             /* t */
aet_ch,                 /* string-char */
aet_bit,                /* bit */
aet_fix,                /* fixnum */
aet_sf,                 /* short-float */
aet_lf,                 /* long-float */
aet_char,               /* signed char */
aet_uchar,              /* unsigned char */
aet_short,              /* signed short */
aet_ushort              /* unsigned short */
};
")

#+GCL
(Clines "
#define    ADIMLIM         16*1024*1024
#define    ATOTLIM         16*1024*1024

object makefixarr2(m,n)
 object m,n;
{
  int s, i, j;
  object x;
  int * alloc_relblock(int);
  int fixnnint(object);

  x = alloc_object(t_array);
  x->a.a_self = NULL;
  x->a.a_displaced = Cnil;
  x->a.a_rank = 2;
  x->a.a_dims = NULL;
  x->a.a_elttype = aet_fix;
  vs_base[0] = x;
  x->a.a_dims = (int *)alloc_relblock(sizeof(int)*2);
  if ((j = fixnnint(m)) > ADIMLIM) {
          vs_push(make_fixnum(1));
          FEerror(\"The ~:R array dimension, ~D, is too large.\",
                  2, vs_head, m);
  }
  s = (x->a.a_dims[0] = j);
  if ((j = fixnnint(n)) > ADIMLIM) {
       vs_push(make_fixnum(2));
       FEerror(\"The ~:R array dimension, ~D, is too large.\",
               2, vs_head, n);
  }
  s *= (x->a.a_dims[1] = j);
  if (s > ATOTLIM) {
      vs_push(make_fixnum(s));
      FEerror(\"The array total size, ~D, is too large.\",
              1, vs_head);
  }
  x->a.a_dim = s;
  x->a.a_adjustable = 0;
  x->fixa.fixa_self = (fixnum *)alloc_relblock(sizeof(fixnum)*s);
  for (i = 0;  i < s;  i++) x->fixa.fixa_self[i] = 0;
  return x;
}

object makefixarr1(n)
 object n;
{
  int d, i;
  object x;
  int * alloc_relblock(int);
  int fixnnint(object);

  if ((d = fixnnint(n)) > ADIMLIM)
       FEerror(\"The vector dimension, ~D, is too large.\",
               1, n);
  x = alloc_object(t_vector);
  x->v.v_self = NULL;
  x->v.v_displaced = Cnil;
  x->v.v_dim = d;
  x->v.v_adjustable = 0;
  x->v.v_elttype = (short)aet_fix;
  x->v.v_hasfillp = FALSE;
  x->v.v_fillp = x->v.v_dim;
  x->fixa.fixa_self = (fixnum *)alloc_relblock(sizeof(fixnum)*d);
  for (i = 0;  i < d;  i++) x->fixa.fixa_self[i] = 0;
  return x;
}
")

#+GCL (defentry make-array2 (object object) (object makefixarr2))
#+GCL (defentry make-array1 (object) (object makefixarr1))

;; More generic KCL/GCL versions:
;;(defun make-array1 (dimension)
;;  (si:make-vector 'fixnum dimension nil nil nil 0 nil))
;;(defun make-array2 (m n)
;;  (si:make-pure-array 'fixnum nil nil 0 nil m n))

;; Generic versions
#+(or LUCID CMU CLISP)
(defun make-array1 (m)
  (make-array m :element-type 'fixnum :initial-element 0))
#+(or LUCID CMU CLISP)
(defun make-array2 (m n)
  (make-array (list m n) :element-type 'fixnum :initial-element 0))

;; Implementation note: KCL/GCL structures are poorly implemented,
;; both in construction time (huge) and in space (2x reasonable).
;; Thus PDL implements AC-state on his own.
#|
(defstruct AC-state
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
  RHS-c-sol                  ; array[int] ; solution matrix; constants
  RHS-c-max                 ; int ; max value of elements of RHS-c-sol
  RHS-f-sol           ; array[int] ; solution matrix; functional terms
  RHS-f-max                 ; int ; max value of elements of RHS-f-sol
  RHS-full-bits                       ; int 11111...111               
  RHS-c-compat        ; array[int] ; array of compatibility bitvectors
  RHS-f-compat        ; array[int] ; array of compatibility bitvectors
  LHS-c-count  ; int ; number of constants on LHS after simplification
  LHS-f-count  ; int ; number of functions on LHS after simplification
  LHS-v-count  ; int ; number of variables on LHS after simplification
  RHS-c-count  ; int ; number of constants on RHS after simplification
  RHS-f-count  ; int ; number of functions on RHS after simplification
  RHS-f-match-top-ac ; array[bool] ; can that term match the top op? (nil)
  (no-more nil) ; when true implies that all solutions have been reported
  )
|#
(defmacro make-AC-state () 
  `(make-array 26))

(defmacro AC-state-operators (state)
  `(aref ,state 0))

(defmacro AC-state-lhs-f (state)
  `(aref ,state 1))

(defmacro AC-state-lhs-v (state)
  `(aref ,state 2))

(defmacro AC-state-rhs-c (state)
  `(aref ,state 3))

(defmacro AC-state-rhs-f (state)
  `(aref ,state 4))

(defmacro AC-state-lhs-f-r (state)
  `(aref ,state 5))

(defmacro AC-state-lhs-v-r (state)
  `(aref ,state 6))

(defmacro AC-state-rhs-c-r (state)
  `(aref ,state 7))

(defmacro AC-state-rhs-f-r (state)
  `(aref ,state 8))

(defmacro AC-state-lhs-mask (state)
  `(aref ,state 9))

(defmacro AC-state-lhs-f-mask (state)
  `(aref ,state 10))

(defmacro AC-state-lhs-r-mask (state)
  `(aref ,state 11))

(defmacro AC-state-rhs-c-sol (state)
  `(aref ,state 12 ))

(defmacro AC-state-rhs-c-max (state)
  `(aref ,state 13))

(defmacro AC-state-rhs-f-sol (state)
  `(aref ,state 14))

(defmacro AC-state-rhs-f-max (state)
  `(aref ,state 15))

(defmacro AC-state-rhs-full-bits (state)
  `(aref ,state 16))

(defmacro AC-state-rhs-c-compat (state)
  `(aref ,state 17))

(defmacro AC-state-rhs-f-compat (state)
  `(aref ,state 18))

(defmacro AC-state-lhs-c-count (state)
  `(aref ,state 19))

(defmacro AC-state-lhs-f-count (state)
  `(aref ,state 20))

(defmacro AC-state-lhs-v-count (state)
  `(aref ,state 21))

(defmacro AC-state-rhs-c-count (state)
  `(aref ,state 22))

(defmacro AC-state-rhs-f-count (state)
  `(aref ,state 23))

(defmacro AC-state-no-more (state)
  `(aref ,state 24))

(defmacro AC-state-ac-state-p (state)
  `(aref ,state 25))

(defun ac-state-p (state)
  (and (vectorp state) (equal (ac-state-ac-state-p state) 'ac-state)))

(defvar *pdl-debug* nil)

(defvar *half* (/ 1 2))

;; small utility.  Side effect.
(defmacro AC$$Rotate-Left (array m)
"; shifts the element one bit to the left"
  `(setf (aref ,array ,m)
      (* 2 (aref ,array ,m))))

(defun delete-one-term (x y)
  (if (term$equational_equal x (caar y)) (cdr y)
      (let ((last y) (rest (cdr y)))
        (loop
           (when (null rest) (return 'none))
           (when (term$equational_equal x (caar rest))
             (rplacd last (cdr rest))
             (return y))
           (setq last rest  rest (cdr rest))))))

(defmacro AC$$note-repeats (mset array max gcd)
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

(defmacro AC$$eq-member (term list)
; "predicate. true if term is term$equational_equal some element of list"
  `(dolist (term2 ,list)
      (when (term$equational_equal ,term (car ,list))
       (return t))))

(defun AC$state_initialize (sys env)
"; takes a system of equations and an environment, and 
 ; returns an AC-state, which is suitable for framing
 ; or passing to 'AC$next_state'"
  (block fail
  (let ((eqn-number -1)
  (sys-operators (make-array (length sys)))
       (all-lhs-vars nil)
      (all-lhs-funs nil)
      (all-rhs-constants nil)
 (all-rhs-funs nil)
      (*print-circle* t))
  (when *pdl-debug* (format t "~&init: length=~A~&" (length sys)))
  (dolist (equation sys)
     (incf eqn-number)
     (let* (
  (lhs-1 (match_equation$t1 equation))
    (rhs-1 (match_equation$t2 equation))
    (lhs-op (term$head lhs-1))
      (rhs-op (term$head rhs-1))
      (lhs-2 (term$list_AC_subterms lhs-1 lhs-op))
    (rhs-2 (term$list_AC_subterms rhs-1 rhs-op))
    (lhs-vars nil)
  (lhs-constants nil)
     (lhs-funs nil)
  (rhs-constants nil)
     (rhs-funs nil)
  )
    (setf (aref sys-operators eqn-number) lhs-op)
    ;; quick failure cases
    (cond ((not (operator$is_AC_restriction_of rhs-op lhs-op))
        (return-from FAIL (values nil t)))
     ((not (theory$contains_AC (operator$theory lhs-op)))
     (when *pdl-debug* (let ((*print-circle* t))
                         (format t "~&NOT AC: ~A:~&"
                                     (theory$name 
                                    (operator$theory lhs-op)))))
       (return-from FAIL (values nil t))) ; fail, ignore, or what?
    ((> (length lhs-2) (length rhs-2))
       (return-from FAIL (values nil t))) ; no possible match
         (t nil))
    ;; build lhs- vars/funs/constants
    (dolist (term lhs-2)
     (cond ((term$is_var term)
              (let ((image (if env (environment$image env term) term)))
                 (cond ((null image) 
                           (push (cons term eqn-number) lhs-vars))
                        ((term$is_var image)
                     (push (cons image eqn-number) lhs-vars))
                       ((operator$is_AC_restriction_of
                           lhs-op (term$head image))
                      (dolist (term2 (term$list_AC_subterms
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
                 (push (cons term eqn-number) lhs-constants))
           (t (push (cons term eqn-number) lhs-funs))))
    ;; now that the lhs is partitioned - lets play with the rhs
    (dolist (term rhs-2)
       (cond ((term$is_var term)
              (push (cons term eqn-number) rhs-constants))
           ((term$is_constant term)
                 (let ((new (delete-one-term term lhs-constants)))
                 (if (eq 'none new)
                  (push (cons term eqn-number) rhs-constants)
                   (setq lhs-constants new))))
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
      (when (or (> lhs-c-count 0) ; there ain't nothin to match it
          (and (< lhs-v-count 1)    ; no variables remain on lhs
               (> rhs-c-count 0))   ; and constants remain on rhs
         (> lhs-f-count rhs-f-count)) ; too many funs to match
       (return-from FAIL (values nil t)))    ; FAIL most miserably
      (setq all-lhs-funs (nconc lhs-funs all-lhs-funs))
      (setq all-lhs-vars (nconc lhs-vars all-lhs-vars))
      (setq all-rhs-constants (nconc rhs-constants all-rhs-constants))
      (setq all-rhs-funs (nconc rhs-funs all-rhs-funs)))))
  ;; we are now done with all equations.
  (when *pdl-debug* (format t "~&init: all-eqns processed~&"))
  (let* ( (lhs-f-count (length all-lhs-funs))
          (lhs-v-count (1+ (length all-lhs-vars))) ; note this is "wrong"
         (rhs-c-count (length all-rhs-constants))
        (rhs-f-count (length all-rhs-funs))
     (lhs-f-r (make-array lhs-f-count :element-type 'fixnum))
        (lhs-v-r (make-array lhs-v-count :element-type 'fixnum))
        (rhs-c-r (make-array rhs-c-count :element-type 'fixnum))
        (rhs-f-r (make-array rhs-f-count :element-type 'fixnum))
        (LHS-f-ms (AC$$list2multi_set all-lhs-funs))
    (LHS-v-ms (AC$$list2multi_set all-lhs-vars))
    (RHS-c-ms (AC$$list2multi_set all-rhs-constants))
       (RHS-f-ms (AC$$list2multi_set all-rhs-funs))
    (l-m 0)
         (r-m 0)
         (l-gcd (or (cdar lhs-f-ms) (cdar lhs-v-ms) 1))
          (r-gcd (or (cdar rhs-f-ms) (cdar rhs-c-ms) 1))
          (LHS-f-list (AC$$note-repeats lhs-f-ms lhs-f-r l-m l-gcd))
      (LHS-v-list (cons (cons 'dummy 13)
                        (AC$$note-repeats lhs-v-ms lhs-v-r l-m l-gcd)))
       (RHS-c-list (AC$$note-repeats rhs-c-ms rhs-c-r r-m r-gcd))
      (RHS-f-list (AC$$note-repeats rhs-f-ms rhs-f-r r-m r-gcd))
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
          (state (make-ac-state))
         )
    (declare (type (vector fixnum)  rhs-c-compat rhs-f-compat)
            (fixnum dummy-bit lhs-r-mask l-gcd r-gcd l-m r-m))
    ;; one more easy failure check
    (when (or (> l-m r-m) ; a lhs item is repeated more than any rhs
         (not (integerp (/ r-gcd l-gcd))))
   (return-from FAIL (values nil t)))    ; FAIL most miserably
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
  (when *pdl-debug* (format t "~&init: setup compat (c)~&"))
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
  (when *pdl-debug* (format t "~&init: setup compat (f) ~&"))
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
                (return-from FAIL (values nil t)))
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
             ((> dummy-bit rhs-f-max) (return-from FAIL
                                                    (values nil t)))
                  (when (not (zerop (logand dummy-bit my-compat)))
                      (setf (aref rhs-f-sol i) dummy-bit)
                     (return))
                 (setq dummy-bit (* 2 dummy-bit))))))
    ;; initialize the mask -
    (if (= rhs-f-count 0)
   (setf (AC-state-LHS-mask state) 0)
        (let ((temp 0))
         (declare (fixnum temp))
         (dotimes-fixnum (s rhs-c-count)
                         (setq temp (logior temp (aref rhs-c-sol s))))
   (setf (AC-state-LHS-mask state) temp)))
    ;; and now stuff the state full of information, and return it.
       (setf (ac-state-operators state) sys-operators)
         (setf (ac-state-LHS-f state) lhs-f)
     (setf (ac-state-LHS-v state) lhs-v)
     (setf (ac-state-RHS-c state) rhs-c)
     (setf (ac-state-RHS-f state) rhs-f)
     (setf (ac-state-LHS-f-r state) lhs-f-r)
         (setf (ac-state-LHS-v-r state) lhs-v-r)
         (setf (ac-state-RHS-c-r state) rhs-c-r)
         (setf (ac-state-RHS-f-r state) rhs-f-r)
        ; (setf (ac-state-LHS-mask state) 0)
     (setf (ac-state-LHS-f-mask state) 0)
    (setf (ac-state-LHS-r-mask state) lhs-r-mask)
   (setf (ac-state-RHS-c-sol state) rhs-c-sol)
     (setf (ac-state-RHS-c-max state) rhs-c-max)
     (setf (ac-state-RHS-f-sol state) rhs-f-sol)
     (setf (ac-state-RHS-f-max state) rhs-f-max)
     (setf (ac-state-RHS-full-bits state) rhs-full-bits)
     (setf (ac-state-RHS-c-compat state) rhs-c-compat)
       (setf (ac-state-RHS-f-compat state) rhs-f-compat)
       (setf (ac-state-LHS-c-count state) 0)
   (setf (ac-state-LHS-f-count state) lhs-f-count)
         (setf (ac-state-LHS-v-count state) lhs-v-count) ; off 1+ intentionally
          (setf (ac-state-RHS-c-count state) rhs-c-count)
         (setf (ac-state-RHS-f-count state) rhs-f-count)
         (setf (ac-state-no-more state) nil)
     (setf (ac-state-ac-state-p state) 'ac-state)
;   (when *pdl-debug* (format t "~&init: state=~&")
;              (AC$unparse_AC-state state))
      (values state nil)))))

(defun AC$next_state (state)
  (when *pdl-debug* (format t "~&next: ")
   (AC$unparse_ac-state state))
  (if (not (AC-state-p state))
      (progn (format t "~& AC$Next_State given non-ac-state:~A~&" state)
           (values nil nil t)) ; failing is default behavior...
   (if (AC-state-no-more state)
      (values nil nil t)       ;; there are no more solutions - fail
      (do* ((n 0) 
      (rhs-f-sol (AC-state-rhs-f-sol state))
          (rhs-f-max (AC-state-rhs-f-max state))
          (rhs-f-compat (AC-state-rhs-f-compat state))
            (rhs-f-count (AC-state-rhs-f-count state))
      (rhs-full-bits (AC-state-rhs-full-bits state))
          (lhs-r-mask (AC-state-lhs-r-mask state))
        )
      (nil) ; forever
         (declare (fixnum rhs-f-count rhs-f-max lhs-r-mask))
     (cond ((>= n rhs-f-count) ; no next row
                (AC$$next-state-sub state)
              (if (AC-state-no-more state)
                (if (and (= 0 (ac-state-LHS-f-count state))
                              (= 1 (ac-state-LHS-v-count state))
                              (= 0 (ac-state-RHS-c-count state))
                              (= 0 (ac-state-RHS-f-count state)))
                      (return (values (AC$$solution_from_state state)
                                 state nil))
                     (return (values nil nil t))) ; failed at f-level
                      (setq n (1- n))))
          ((< n 0)
                 (let ((temp (AC-state-LHS-mask state)))
                   (dotimes-fixnum (s rhs-f-count)
                          (setq temp (logior temp (aref rhs-f-sol s))))
                  (if (= rhs-full-bits temp)
                  (return (values (AC$$solution_from_state state)
                                 state nil))
                     (setq n 0))))
            ((< (aref rhs-f-sol n) rhs-f-max)
                (AC$$Rotate-Left rhs-f-sol n)
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

(defun ac$args_nss (x) (AC$unparse_AC-state (car x)) (terpri))
(setf (get 'AC$$next-state-sub 'print_args) 'ac$args_nss)

(defun AC$$next-state-sub (state)
  (do* ((m 0)  ; only initialize these vars 
    (rhs-c-sol (AC-state-rhs-c-sol state))
  (rhs-c-max (AC-state-rhs-c-max state))
  (rhs-c-count (AC-state-rhs-c-count state))
      (rhs-c-compat (AC-state-rhs-c-compat state))
    (lhs-r-mask (AC-state-lhs-r-mask state)))
       (nil) ; forever
       (declare (type (vector fixnum) rhs-c-compat) 
             (fixnum lhs-r-mask rhs-c-count))
       (cond ((>= m rhs-c-count) ; no next row
        (setf (AC-state-no-more state) T)
       (return))
      ((< m 0) ; no tests up here - could cut search here
              (let ((temp 0)) ; the empty bitvector
             (dotimes-fixnum (s rhs-c-count)
                  (setq temp (logior temp (aref rhs-c-sol s))))
          (setf (AC-state-LHS-mask state) temp)
           (return)))
           ((< (aref rhs-c-sol m) rhs-c-max)
        (AC$$Rotate-Left rhs-c-sol m)
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
(defmacro AC$$collapse-one-array-internal (rhs-sol rhs-array)
  `(dotimes-fixnum (j (array-dimension ,rhs-sol 0))
           (when (> (logand (aref ,rhs-sol j) term-code) 0)
              (push  (car (aref ,rhs-array j)) rhs-subterms))))

;; don't even think of using this again
(defmacro AC$$collapse-arrays-internal (lhs skip) 
  `(dotimes-fixnum (i (array-dimension ,lhs 0))
      (if (< i ,skip)
          nil
          (progn 
  (setq rhs-subterms nil)
 (setq term-code (* 2 term-code))
        (AC$$collapse-one-array-internal rhs-c-sol rhs-c)
       (AC$$collapse-one-array-internal rhs-f-sol rhs-f)
;      (print$brief (car (aref ,lhs i)))
;       (map nil #'print$brief rhs-subterms)
   (system$add_eq 
  new-sys 
        (match_equation$create (car (aref ,lhs i))
           (if (cdr rhs-subterms) ; implies length is greater than 1
           (term$make_right_assoc_normal_form_with_sort_check
                (aref ops (cdr (aref ,lhs i))) rhs-subterms)
                  (first rhs-subterms))))))))

;; note that this relies on side effects of the macro above.
(defun AC$$solution_from_state (state)
"; given an AC-state, produce a solution (system of equations
 ; which, if true, imply the original AC equation true) from
 ; the matrix of 'state'"
  (let* ((ops (AC-state-operators state))
       (lhs-f (AC-state-lhs-f state))
  (lhs-v (AC-state-lhs-v state))
  (rhs-c (AC-state-rhs-c state))
  (rhs-f (AC-state-rhs-f state))
  (rhs-c-sol (AC-state-rhs-c-sol state))
  (rhs-f-sol (AC-state-rhs-f-sol state))
  (new-sys (system$new))
  (term-code 1)
   (rhs-subterms nil))
    (AC$$collapse-arrays-internal lhs-v 1) 
    ;; note term-code is now the right thing. 
    (AC$$collapse-arrays-internal lhs-f 0) 
    (when *pdl-debug* (format t "~&winner: ")
;    (ac$unparse_ac-state state)
     )
    new-sys))

;; not all that useful printout of parts of AC state.
(defun AC$unparse_AC-state (AC_st)
  (format t "~&no more=~A~%" (AC-state-no-more AC_st))
  (format t "~&operators: ~&")
  (map nil #'print$brief (AC-state-operators AC_st))
  (format t "~&RHS-f: ~&")
  (map nil #'print$brief (AC-state-RHS-f AC_st))
  (format t "~&RHS-c: ~&")
  (map nil #'print$brief (AC-state-RHS-c AC_st))
  (format t "~&LHS-v: ~&")
  (map nil #'print$brief (AC-state-LHS-v AC_st)) 
  (format t "~&LHS-f: ~&")
  (map nil #'print$brief (AC-state-LHS-f AC_st))
  (format t "~& rhs-c-count=~A, rhs-f-count=~A~&"
         (AC-state-RHS-c-count AC_st) 
   (AC-state-RHS-f-count AC_st))
  (format t "~& lhs-c-count=~A, lhs-f-count=~A, lhs-v-count=~A~%"
          (AC-state-LHS-c-count AC_st) 
   (AC-state-LHS-f-count AC_st) 
   (AC-state-LHS-v-count AC_st))
  (let ((*print-base* 2)) ; these be bitvectors, print them as such
    (format t "-------------------~%rhs-c-sol= ~A~&rhs-f-sol=~A~&"
        (AC-state-RHS-c-sol AC_st) (AC-state-RHS-f-sol AC_st))
    (format t "~& rhs-c-max=~A, rhs-f-max=~A, rhs-full-bits=~A~&"
         (AC-state-RHS-c-max AC_st) 
     (AC-state-RHS-f-max AC_st)
      (AC-state-RHS-full-bits AC_st))
    (format t "~& rhs-c-compat=~A, rhs-f-compat=~A~&"
            (AC-state-RHS-c-compat AC_st) 
          (AC-state-RHS-f-compat AC_st))
    (format t "~& rhs-c-r=~A, rhs-f-r=~A~&"
       (AC-state-RHS-c-r AC_st) 
       (AC-state-RHS-f-r AC_st))
    (format t "~& lhs-f-r=~A, lhs-v-r=~A~&"
            (AC-state-LHS-f-r AC_st) 
       (AC-state-LHS-v-r AC_st))
    (format t "~& lhs-mask=~A~%"
       (AC-state-LHS-mask AC_st))
    (terpri)
    (format t "~& lhs-f-mask=~A~%"
        (AC-state-LHS-f-mask AC_st))
    (format t "~& lhs-r-mask=~A~%"
          (AC-state-LHS-r-mask AC_st))
    ))

;;
;; unchanged (this isn't good, but it works).  -pdl
;;
; Assume that t1 is NOT a variable
; op AC$equal: Term Term -> Bool
(defun AC$equal (t1 t2)
  (let ((op (term$head t1)))
    (AC$ms_equal
     (list2multi_set_list (term$list_AC_subterms t1 op))
     (list2multi_set_list (term$list_AC_subterms t2 op)))))

; check for multi-set equality
;uses term$equational_equal -- which can be pretty expensive
(defun AC$ms_equal (x y)
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

;;; NOTE  this is a version for AC-internal use only.
;; it simply takes care of the "from which equation" info.
;; the input list is like
;; ((A . 1) (B . 2) (A . 1) (A . 3))
;; the result is like
;; (((A . 1) . 2) ((B . 2) . 1) ((A . 3) . 1))
(defun AC$$list2multi_set (list)
;  (format t "~&AC$list2multi_set:  ")
;  (mapcar #'(lambda (x) (format t " ~A " (cdr x))) list)
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


