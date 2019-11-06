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

;; $Id: operator.lsp,v 206.1 2003/09/23 13:40:00 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                    Operators
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Claude Kirchner ;;;; Created: 4 June, 1986

;;;;;;;;;;;;;;;; Summary ;;;;;;;;;;;;;;;;

; op operator$create: Name List[Sort] Sort Module EquationalTheory List[Int]^2
;                     Bool ErrorStrategy Ring[Rule] List[Rule] -> Operator .
; op operator$name: Operator -> Name .
; op operator$arity: Operator -> List[Sort] .
; op operator$coarity: Operator -> Sort .
; op operator$rew_strategy : Operator -> List[Int] .
; op operator$module: Operator -> Module .
; op operator$theory: Operator -> EquationalTheory .
; op operator$theory_name: Operator -> EquationalTheoryName .
; op operator$rules_with_same_top: Operator -> Ring[Rule] .
; op operator$rules_with_different_top: Operator -> List[Rule] .
; op operator$!add_rule: Operator Rule -> Operator~ .
; op operator$is_memo : Operator -> Bool .
; op operator$precedence : Operator -> Nat .
; op operator$form : Operator -> Form-Description .
; op operator$syntactic_type : Operator ->
;                              {'latefix,'antefix,'juxatposition,'unspecified}
; op operator$!replace_syntactic_type : Operator Syntactic-Type -> Operator~
; op operator$!compute_rew_strategy : Module Operator -> {Module}
; op operator$error_strategy : Operator -> Bool .
; op operator$make_retract : Sort Sort -> Operator .
; op operator$is_a_retract : Operator -> Bool .
; op operator$is_associative : Operator -> Bool .
; op operator$is_commutative : Operator -> Bool .
; op operator$is_marked_strictly_overloaded : Operator -> Bool .
; op operator$!mark_strictly_overloaded : Operator -> Operator~
; op operator$polymorphic : Operator -> Function
; op operator$!make_polymorphic : Operator Function -> Operator~
; op operator$illdefined : Operator -> Bool
; op operator$!make_illdefined : Operator -> Operator~
; op operator$is_standard : Operator -> Bool
; op operator$canonicalize_name : Name -> Name
; op operator$is_same_operator : Operator Operator -> Bool
; op operator$is_same_overloaded_operator : Operator Operator -> Bool
; op operator$!replace_rew_strategy : Operator List[Int] -> ~Operator
; op operator$intrinsic: Operator -> Value .

;;;;;;;;;;;;;;;; Representation ;;;;;;;;;;;;;;;;

;;; An operator contains all the following interesting informations

; --- name                       ex: f              : Name
; --- arity(list of input sorts) ex: (Int Int)      : List[Sort]
; --- coarity                    ex: Real           : Sort
; --- module                     ex: REAL           : Module
; --- theory                     ex: [ACZ, (zero)]  : EquationalTheory
; --- rewrite strategy           ex: (2 0 1)        : List[Int]
; --- Memo                       ex : memo          : Bool
; --- error recovery strategy    ex: strict         : Bool ---true=strict
; --- list of rule with "name"    
;     as top                     ex: (f(x,o) -> f(o,x)) : Ring[Rule]
; --- list of rule with a top
;     different from "name"      ex: (f(x,x) -> x)  : List[Rule]
; --- precedence                 ex: 2333           : Integer
; --- form                       ex:                : FormDescription
; --- syntactic_type             ex:                : OneOf[latefix, antefix,
;                                                     juxtaposition, 
;                                                     unspecified]

; up to now it is represented by a structure

(defstruct operator
  name
  arity
  coarity
  module
  (illdefined nil)
  precedence
  form
  syntactic_type 
  (is_standard nil)
  (intrinsic nil)
  (intrinsic_module nil) ;@@ experimental
  (status nil)
)

; information is being shifted from the operator structure to the operator
; table

(defstruct (operator_info
	     (:conc-name operator_info$)
	     (:constructor operator_info$make
	         (&optional
		  (equational_theory theory$the_empty_theory)
		  rew_strategy
		  user_rew_strategy
		  memo
		  error_strategy
		  strictly_overloaded
		  polymorphic
		  (rules_with_same_top (rule_ring$create nil))
		  rules_with_different_top
		  ))
	     (:print-function print$operator_info_3)
	     )
	"collection of operator information"
  (equational_theory theory$the_empty_theory)
  (rew_strategy nil)
  (user_rew_strategy nil)
  (memo nil)
  (error_strategy nil) ;??? 8 Jul 87
  (strictly_overloaded nil)
  (polymorphic nil)
  (rules_with_same_top (rule_ring$create nil))
  (rules_with_different_top nil)
  (lowest nil)
  (highest nil)
  ;(rules_with_same_top nil) ; 19 May 1987 added rules here
  ;(rules_with_different_top (rule_ring$create nil))
  (sort_constraint nil)
  (associative_operators_above nil)
  )

; op operator$create: Name List[Sort] Sort Module EquationalTheory List[Int]^2
;                     Bool ErrorStrategy Ring[Rule] List[Rule] Int Form 
;                     SyntacticType -> Operator 

(defun operator$create (name arity coarity module eq_th
			     rew_strategy user_rew_strategy
			     memo error_strategy rw_same_t rw_different_t
			     precedence form
			     &optional (syntactic_type 'unspecified)
			       (is_standard nil)
			       (polymorphic nil)
			       (strictly_overloaded nil))

  (let ((operator
	 (make-operator
	  :name (operator$canonicalize_name name)
	  :arity arity
	  :coarity coarity
	  :module module
	  :precedence precedence
	  :form form
	  :syntactic_type syntactic_type
	  :is_standard is_standard
	  )))
  (let ((optab (module$operator_table module)))
    (when (null optab) (setq optab (operator_table$!create module)))
    (optable$update_operator_info optab operator
      (operator_info$make
       eq_th rew_strategy user_rew_strategy memo error_strategy
       strictly_overloaded polymorphic
       rw_same_t rw_different_t
      )))
  operator)
  )

; op operator$create_intrinsic : Name List[Sort] Sort Module EquationalTheory
;                     List[Int]^2 Bool ErrorStrategy Ring[Rule] List[Rule]
;                     Int Form  SyntacticType -> Operator 
(defun operator$create_intrinsic (name arity coarity module eq_th
			     rew_strategy user_rew_strategy
			     memo error_strategy rw_same_t rw_different_t
			     precedence form
			     &optional (syntactic_type 'unspecified)
			       (is_standard nil)
			       (polymorphic nil)
			       (strictly_overloaded nil))

	 (make-operator
	  :name (operator$canonicalize_name name)
	  :arity arity
	  :coarity coarity
	  :module module
	  :precedence precedence
	  :form form
	  :syntactic_type syntactic_type
	  :is_standard is_standard
	  :intrinsic
	    (operator_info$make
	     eq_th rew_strategy user_rew_strategy memo error_strategy
	     strictly_overloaded polymorphic
	     rw_same_t rw_different_t)))

; access to information
; op operator$name: Operator -> Name .
(defmacro operator$name (op)
 `(operator-name ,op)
  )

; op operator$arity: Operator -> List[Sort]
(defmacro operator$arity (op)
  `(operator-arity ,op)
  )

; op operator$coarity: Operator -> Sort .
(defmacro operator$coarity (op)
  `(operator-coarity ,op)
  )

; op operator$module: Operator -> Module .
(defmacro operator$module (op)
  `(operator-module ,op))

; op operator$intrinsic: Operator -> Value .
(defmacro operator$intrinsic (op)
  `(operator-intrinsic ,op))

; op operator$!update_intrinsic : Operator Operator-Info -> {Operator}
(defun operator$!update_intrinsic (op opinfo)
  (setf (operator$intrinsic op) opinfo)
  op
  )

; op operator$intrinsic_module: Operator -> Value .
(defmacro operator$intrinsic_module (op)
  `(operator-intrinsic_module ,op))

; op operator$status: Operator -> Value .
(defmacro operator$status (op)
  `(operator-status ,op))

(defvar obj$current_module)

; for debugging
; op operator$$chk : Module -> Module
;(defun operator$$chk (mod)
;  (if mod mod ;&&&& 6 Aug 87 was (typep mod 'module) but this is expensive
;    (progn
;      (break "Current module not defined")
;      )
;  ))
(defmacro operator$$chk (mod) mod) ;24 Feb 88

; op operator$theory: Operator -> EquationalTheory .
(defun operator$theory (op)
;  (module$operator_equational_theory (operator$$chk obj$current_module) op)
;  heavily used so short-cutting some of function calls
  (operator_info$equational_theory
   (optable$operator_info
    (module$operator_table (operator$$chk obj$current_module))
    op))
  )

; op operator$theory_name: Operator -> EquationalTheoryName .
(defun operator$theory_name (op)
;  (theory$name (module$operator_equational_theory
;		(operator$$chk obj$current_module) op))    
  (theory$name
   (operator_info$equational_theory
    (optable$operator_info
     (module$operator_table (operator$$chk obj$current_module))
     op))))

; op operator$theory_name_for_matching: Operator -> EquationalTheoryName .
(defun operator$theory_name_for_matching (op)
   (let ((eq_th 
	  (operator_info$equational_theory
	   (optable$operator_info
	    (module$operator_table (operator$$chk obj$current_module))
	    op))))
     (if (theory$zero_rule_only eq_th)
	 ; a bit ad hoc but worried about efficiency
	 (let ((inf (theory$name eq_th)))
	 (aref theory_info_array
	       (logandc2 (theory_info$code inf) Z_flag)))
     (theory$name eq_th))))

; op operator$is_memo : Operator -> Bool .
(defun operator$is_memo (op)
  (module$operator_memo (operator$$chk obj$current_module) op)
  )

; op operator$precedence : Operator -> Nat .
(defun operator$precedence (op)
  (operator-precedence op)
  )

; op operator$form : Operator -> Form-Description .
(defun operator$form (op)
  (operator-form op)
  )

; op operator$syntactic_type : Operator ->
;                              {'latefix,'antefix,'juxatposition,'unspecified}
(defun operator$syntactic_type (op)
  (operator-syntactic_type op)
  )

; returns "true" iff the error strategy is strict.
; op operator$error_strategy : Operator -> Bool .
(defun operator$error_strategy (op)
  (module$operator_error_strategy (operator$$chk obj$current_module) op)
  )


; The strategy of any operator is computed according to
; the following rules:
; 1/ "==" is a built-in operator with strategy (1 2 0).
; 2/ "if_then_fi" is a built-in operator with strategy (1 0 2).
; 3/ "if_then_else_fi" is a built-in operator with strategy (1 0 2 3).
; 4/ other built-in operators have strategy bottom-up, i.e. (1 ... n 0)
;    except if specified
; 5/ a. free constants (including built-in constants) have strategy nil.
;    b. non-free constants have strategy (0).
; 6/ "free constructors", i.e. operators that have no rule (but may have
;    built-in properties such as commutativity) have strategy (1 ... n).
; 7/ Other "ac" operators have strategy (1 ... n 0). @@ why? (ck)
; 8/ "operators that have no "rules with different top" are considered
;    to be "non free constructors" with bottom up strategy (1 ... n 0).
; 9/ All other operators have their strategy computed by the following
;    function that returns a permutation of 
;    [1 .. n] with some additional inserted Zeros:

; Z: We suppose that ALL the information relative to the operator are
;    entered when we compute the strategy. In particular all the rules
;    relative to this operator must be entered.
 
; patch 31/7/92
; op operator$!compute_rew_strategy : Module Operator -> {Module}
(defun operator$!compute_rew_strategy (mod op)
  ;; if the strategy is already specified, we don't modified it
  ;; this covers in particular cases 1, 2, 3, 4
  (if (not (eq 'none (module$operator_user_rew_strategy mod op)))
      (module$!operator_update_rewrite_strategy mod op
	(module$operator_user_rew_strategy mod op))
    (unless (and (module$operator_rew_strategy mod op)
		 (not (module$has_rules_for mod op))
		 (dolist (sm (module$sub_modules mod) t)
		   (when (and (eq 'including (cdr sm))
			      (member op (module$operators (car sm))))
		     (return nil))))
	    ;@@ above is somewhat approximate, want to improve
	(cond ((and (null (module$rules_with_different_top
			    obj$current_module op))
		    (rule_ring$is_empty
		     (module$rules_with_same_top
		      obj$current_module op)))
	       ;; cases 5.a and 6: the operator is free i.e. have no rule
	       (module$!operator_update_rewrite_strategy mod op
		     (util$make_list_1_n (length  (operator$arity op))))
	       )
	      ((or (not (theory_name$is_empty
			 (operator$theory_name op))) ;;case 7
		   (not (module$rules_with_different_top
			 obj$current_module op)) ;; case 8
		   (not (operator$arity op)) ;; case 5.b
		   )
	       ;; then the strategy is bottom up:
	       (module$!operator_update_rewrite_strategy mod op
		     (util$make_list_1_n_0 (length (operator$arity op))))
	       )
	      (t
	       ;; case 9: the real work begins!
	       (let ((strategy nil) (end_strategy nil)
;; patch	     (l_ar (length (operator$arity op))))
		     (l_ar (length (operator$arity op)))
		     (so (module$sort_order mod)))
		 (do ((occ 1 (1+ occ)))
		     ((< l_ar occ))
		    (block is_variable
			   (let ((rr  (module$rules_with_same_top
				       obj$current_module op)))
			     (do ((rule (rule_ring$initialize rr) 
					(rule_ring$next rr))
				  )
				 ((rule_ring$end_test rr))
				 (unless (term$is_var 
					  (term$arg_n (rule$lhs rule) occ))
					 (push occ strategy)
					 (return-from is_variable)
					 )
				 ) ;; do
			     ) ;; let   
		       (dolist (rule (module$rules_with_different_top
				      obj$current_module op))
			       (unless 
				 (let ((argn (term$arg_n
;; patch				       (rule$lhs rule) occ)))
					       (rule$lhs rule) occ))

					(opr (term$head (rule$lhs rule))))
				    (and
				      (term$is_var argn)
				      (not (rule$is_built_in rule))
				      (let ((inf (optable$operator_info
						  (module$operator_table mod)
						  opr)))
				      (and
				      ; highest is necessarily a superset
				      ; of lowest equal only if maximal
				      (equal
				       (length (operator_info$highest inf))
				       (length (operator_info$lowest inf)))
				      ;(variable$is_general_variable argn)
				      ;&&&& 10/14/86 want most general too
				      ;&&&& 7/31/92 no longer valid
				      (sort_order$is_included_in so
				        (nth (1- occ)
					     (operator$arity opr))
					(term$sort argn))))))
				 (push occ strategy)
				 (return-from is_variable)
				 )
			       )
		       (push occ end_strategy)
		       ) ;; block
		    ) ;; do
		 (module$!operator_update_rewrite_strategy mod op
		    (append (reverse strategy)
			    (if (member 0 strategy) nil '(0))
			    (reverse end_strategy)))
		 ) ;; let
	       )
	      ) ;; cond
	) ;; unless
    ) ;; if
  )

; op operator$!fix_strategy_and_rules : Module Operator -> {Module}
(defun operator$!fix_strategy_and_rules (module op)
  (let ((rr (module$rules_with_same_top module op))
	(inf (optable$operator_info
	      (module$operator_table module) op)))
    (if (not (eq 'none (module$operator_user_rew_strategy module op)))
	; user strat & not 0 at front -> elim wst
	(unless (eql 0 (car (module$operator_user_rew_strategy module op)))
	  (let ((rwst (rule_ring$list rr)))
	    (setf (operator_info$rules_with_different_top  inf)
	        (append rwst (operator_info$rules_with_different_top  inf)))
	    (setf (rule_ring-ring rr) nil)
	  ))
      ; no user; has wst -> add 0 to strat
;      (unless (or (rule_ring$is_empty rr)
;		  (= 0 (car (module$operator_rew_strategy module op)))
;		  (every #'(lambda (r)
;			     (and (rule$kind r)
;				  (not (eq 'id_completion (rule$kind r)))))
;		    (rule_ring$list rr))
;		  )
;	(push 0 (operator_info$rew_strategy inf))
;      );unless
      );if
  ))

; op operator$!replace_syntactic_type : Operator Syntactic-Type -> ~Operator
(defun operator$!replace_syntactic_type (op s_t)
  (setf (operator-syntactic_type op) s_t)
  )

; Retracts are "funny" operators since they are not attach to any module
; by the user but by the system.
; Z: The first sort must be greater than the second one.

; op operator$make_retract: Sort Sort -> Operator .
(defun operator$make_retract (sort1 sort2)
  (operator$make_named_retract
   (concatenate 'string
       "r:" (sort$name sort1) ">" (sort$name sort2))
   sort1 sort2)
  )

(defvar *operator$retract_table*
  (make-hash-table :test 'eq :size 500))

(defun operator$make_named_retract (name sort1 sort2)
  (multiple-value-bind (entry found) (gethash sort2 *operator$retract_table*)
  (let ((res (assoc sort1 entry)))
  (if res (cdr res)
  (let ((newop
  (make-operator
   :name (list 'retract name)
   :arity (list sort1)
   :coarity sort2
   :module (sort$module sort1)
   :precedence 0 ;; no importance
   :form (list (cons 'token name) '(token . "(")
	   (list* 'argument 127 sort1) '(token . ")"))
   :syntactic_type 'antefix
   :is_standard t ;15 Jul 88
   :intrinsic
     (operator_info$make
      theory$the_empty_theory
      '(1 0) ; was (list 1 0) 15 Jul 87
      '(1 0)
      nil
      t ;; i.e strict
      nil
      nil
      (rule_ring$create nil)
      nil
	  ;; in fact r(t) = t should be attached here.
	  ;; It is not since the rule associated to a 
	  ;; retract is evaluated in the "morphism rule"
	  ;; application part (in term$...)
      ))))
    (unless found (setf (gethash sort2 *operator$retract_table*) nil))
    (push (cons sort1 newop) (gethash sort2 *operator$retract_table*))
    newop
  )))))

; op operator$is_a_retract: Operator -> Bool .
(defun operator$is_a_retract (op)
  (eq (car (operator$name op)) 'retract)
  )

; op operator$is_associative : Operator -> Bool .
(defmacro operator$is_associative (op)
  `(theory$contains_associativity (operator$theory ,op))
  )

; op operator$is_identity : Operator -> Bool .
(defmacro operator$is_identity (op)
  `(theory$contains_identity (operator$theory ,op))
  )

; specially optimized version of last for use in rew$!reduce
(defmacro operator$is_associative_fast (op)
  `(test-flag A_flag
    (theory_info$code
     (theory$name
      (operator_info$equational_theory
       (optable$operator_info
	(module$operator_table obj$current_module) ,op))))))

; op operator$is_commutative : Operator -> Bool .
(defun operator$is_commutative (op)
  (theory$contains_commutativity (operator$theory op))
  )

; op operator$is_marked_strictly_overloaded : Operator -> Bool .
(defun operator$is_marked_strictly_overloaded (op)
  (module$operator_is_strictly_overloaded
   (operator$$chk obj$current_module) op))

; op operator$!mark_strictly_overloaded : Operator -> Operator~
(defun operator$!mark_strictly_overloaded (op)
  (module$!operator_mark_strictly_overloaded
   (operator$$chk obj$current_module) op)
  )

; op operator$polymorphic : Operator -> Function
(defun operator$polymorphic (op)
;  (module$operator_polymorphic
;   (operator$$chk obj$current_module) op)
  (operator_info$polymorphic
   (optable$operator_info
    (module$operator_table
     (operator$$chk obj$current_module))
    op))
  )

; op operator$!make_polymorphic : Operator Function -> Operator~
(defun operator$!make_polymorphic (op fn)
  (module$!operator_make_polymorphic
   (operator$$chk obj$current_module) op fn)
  )

; op operator$illdefined : Operator -> Bool
(defun operator$illdefined (op)
  (operator-illdefined op))

; op operator$!make_illdefined : Operator -> Operator~
(defun operator$!make_illdefined (op)
  (setf (operator-illdefined op) t)
  )

; op operator$is_standard : Operator -> Bool
(defun operator$is_standard (op)
  (operator-is_standard op)
  )

; op operator$canonicalize_name : Name -> Name
(defun operator$canonicalize_name (name)
  (let ((val (gethash name *operator$name_table* 'none)))
    (if (eq 'none val)
	(progn (setf (gethash name *operator$name_table*) name)
	       name)
      val)
  ))

(defvar *operator$name_table* 'void)

(defun operator$initialize_name_table ()
  (setq *operator$name_table* (make-hash-table :test 'equal :size 200)))

(eval-when (load eval) (operator$initialize_name_table))

; op operator$is_same_operator : Operator Operator -> Bool
(defun operator$is_same_operator (opx opy)
  (and (eq (operator$name opx) (operator$name opy))
       (let ((opxnm (operator$name opx)))
       (or (not (eq (car opxnm) 'retract))
	   (and
	    (eq (operator$coarity opx) (operator$coarity opy))
	    (eq (car (operator$arity opx)) (car (operator$arity opy)))
	    ))))
  )

(defmacro operator$is_same_operator_fast (opx opy)
  `(and (eq (operator$name ,opx) (operator$name ,opy))
       (let ((opxnm (operator$name ,opx)))
       (or (not (eq (car opxnm) 'retract))
	   (and
	    (eq (operator$coarity ,opx) (operator$coarity ,opy))
	    (eq (car (operator$arity ,opx)) (car (operator$arity ,opy)))
	    ))))
  )

; op operator$is_same_overloaded_operator : Operator Operator -> Bool
; similar to the above except (1) simplified; (2) does more checking
; This predicate is unclear in general; this is a strong partial case
; this is a bit messy for efficiencies sake
(defun operator$is_same_overloaded_operator (opx opy)
  (and
   (eq (operator$name opx) (operator$name opy))
   (let ((mod (if obj$current_module obj$current_module
		(operator$module opy))))
   (let ((arx (operator$arity opx))
	 (ary (operator$arity opy)))
  
   (and (= (length arx) (length ary))
     (let ((so (module$sort_order mod))
	   (coarx (operator$coarity opx))
	   (coary (operator$coarity opy)))
     (or
      (and
	(sort_order$is_included_in so coarx coary)
	(every2 #'(lambda (x y) (sort_order$is_included_in so x y))
	  arx ary))
      (and
	(sort_order$is_included_in so coary coarx)
	(every2 #'(lambda (x y) (sort_order$is_included_in so x y))
	  ary arx)))))))))

(defun operator$is_same_general_operator (opx opy)
  (and
   (operator$is_same_operator opx opy)
   (let ((mod (if obj$current_module obj$current_module
		(operator$module opy))))
   (let ((so (module$sort_order mod)))
   (sort_order$is_in_same_component_safe so
     (operator$coarity opx) (operator$coarity opy))
   (every2len #'(lambda (x y) (sort_order$is_in_same_component_safe so x y))
     (operator$arity opx) (operator$arity opy)))))
  )

; don't necessarily have equivalence classes; this may be wrong
(defun operator$is_same_qual_operator (opx opy) ;12 Jul 88
  (or (eq opx opy)
      (and (operator$is_same_operator opx opy)
	   (intersection-nonempty
	      (operator_info$highest
	       (optable$operator_info
		(module$operator_table (operator$module opy))
		opy))
	      (operator_info$highest
	       (optable$operator_info
		(module$operator_table (operator$module opx))
		opx))))))

(defun intersection-nonempty (x y)
  (dolist (ex x nil) ; default value
    (when (member ex y) (return t)))
  )

(defun operator$is_restriction_of (opx opy)
  (and
   (operator$is_same_operator_fast opx opy)
   (let ((mod (if obj$current_module obj$current_module
		(operator$module opy))))
   (let ((so (module$sort_order mod)))
   (and
   (sort_order$is_included_in so (operator$coarity opx) (operator$coarity opy))
   (every2len #'(lambda (x y) (sort_order$is_included_in so x y))
     (operator$arity opx) (operator$arity opy)))))))

; second operator is assumed to be just associative
(defun operator$is_associative_restriction_of (opx opy)
  (or (eq opx opy)
      (and (operator$is_restriction_of opx opy)
;	   (let ((thy_opx (operator$theory_name opx)))
;	     (or (eq 'empty_property thy_opx)
;		 (eq 'az_property thy_opx)
;		 (eq 'a_property thy_opx)))
	   (theory_name$is_restriction_of_ignoring_id
	    (operator$theory_name opy) (operator$theory_name opx))
      )))

; second operator is assumed to be associative-commutive
(defun operator$is_AC_restriction_of (opx opy)
  (or (eq opx opy)
      (and
;      (operator$is_restriction_of opx opy) expanded to
       (operator$is_same_operator_fast opx opy)
       (let ((mod (if obj$current_module obj$current_module
		    (operator$module opy))))
       (let ((so (module$sort_order mod)))
       (and
	(sort_order$is_included_in so
	    (operator$coarity opx) (operator$coarity opy))
	(every2len #'(lambda (x y) (sort_order$is_included_in so x y))
	    (operator$arity opx) (operator$arity opy)))))
;	   (member (operator$theory_name opx)
;		 '(empty_property a_property ac_property az_property
;		   acz_property c_property cz_property))
       (theory_name$is_restriction_of_ignoring_id
	(operator$theory_name opy) (operator$theory_name opx))
      )))

; second operator is assumed to be just commutive
(defun operator$is_commutative_restriction_of (opx opy)
  (or (eq opx opy)
      (and (operator$is_restriction_of opx opy)
;	   (let ((thy_opx (operator$theory_name opx)))
;	     (or (eq 'empty_property thy_opx)
;		 (eq 'c_property thy_opx)))
	   (theory_name$is_restriction_of_ignoring_id
	    (operator$theory_name opy) (operator$theory_name opx))
      )))

;op is A
;exists same name with AC then yes
; op operator$is_overloaded_with_AC_attribute : Operator -> Bool
(defun operator$is_overloaded_with_AC_attribute (op)
  (let ((mod (if obj$current_module obj$current_module (operator$module op))))
    ;&&&& eventually use special list of "same" operators?
    (dolist (op2 (module$operators mod) nil) ;nil is default value
      (when (and 
	      (not (eq op op2))
	      (operator$is_same_operator op2 op)
	      (theory$contains_AC (operator$theory op2))
	      )
	(return t))
    )
  ))

;op is A and satisfies the above condition
; op operator$greatest_AC_operator_less_than : Operator -> 
(defun operator$greatest_AC_operator_less_than (op)
  (let ((mod (if obj$current_module obj$current_module (operator$module op))))
    ;&&&& eventually use special list of "same" operators?
    (let ((res nil))
    (dolist (op2 (module$operators mod)) ;nil is default value
      (when (and
	      (not (eq op op2))
	      (operator$is_same_operator op2 op)
	      (theory$contains_AC (operator$theory op2))
	      (or (null res)
		  (operator$is_restriction_of op2 op)))
	(setq res op2))
    )
    res)
  ))

; op operator$list_associative_operator_above Operator Module .
(defun operator$list_associative_operator_above (operator module)
  (let ((res nil)
	(so (module$sort_order module))
	(coar (operator$coarity operator)))
    (dolist (o (module$operators module))
      (when (and (not (eq o operator))
		 (operator$is_same_operator o operator)
		 (sort_order$is_included_in so
		   coar (operator$coarity o))
		 (theory$contains_associativity
		  (operator$theory o)))
	(push o res)))
    res
    )
  )

; op operator$highest_operators_below : Module Operator Sort -> List[Operator]
(defun operator$highest_operators_below (module op s)
  (let ((ops 
	 (let ((val
		(optable$operator_info (module$operator_table module) op)))
	   (if (null val) nil (operator_info$highest val))))
	(so (module$sort_order module))
	(res nil)
	(obj$current_module module))
    (dolist (o ops)
      (when (sort_order$is_included_in so (operator$coarity o) s)
	(unless
	    (dolist (o2 ops nil)
	      (when (and
		     (not (eq o o2))
		     (sort_order$is_included_in so (operator$coarity o)
						(operator$coarity o2))
		     (sort_order$is_included_in so (operator$coarity o2) s)
		     (operator$is_included_in o o2))
		(return t)))
	  (push o res)))
      )
    res
  ))

; op operator$is_included_in : Operator Operator -> Bool
(defun operator$is_included_in (opx opy)
  (and
   (eq (operator$name opx) (operator$name opy))
   (let ((arx (operator$arity opx))
	 (ary (operator$arity opy)))
   (and (= (length arx) (length ary))
     (let ((so (module$sort_order
		(if obj$current_module obj$current_module
		  (operator$module opy)))))
     (and
      (sort_order$is_included_in so
          (operator$coarity opx) (operator$coarity opy))
      (every2 #'(lambda (x y) (sort_order$is_included_in so x y))
	      arx ary)))))))
