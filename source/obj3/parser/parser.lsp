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

;; $Id: parser.lsp,v 205.2.1.1 2003/09/23 13:50:15 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  OBJ3 Term Parser
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;  Author: Aristide Megrelis
;  Co-author: Tim Winkler
;  language: Common Lisp

;  -------------------------------------------------------------------------

;  the range of precedence (and precedence level) values is [ 0 .. 127 ] :

(defconstant parser$min_precedence  0)
(defconstant parser$max_precedence 127)

;  -------------------------------------------------------------------------

;  op parser$parser : 
;       TokenList  -- not empty !
;       Module
;       PrecedenceLevel  -- constraint
;       Sort + { Universal }  -- constraint
;       ->
;       LIST[ ( ( ObjTerm . PrecedenceLevel ) . TokenList ) ] .
;         -- possibly empty
;  
;  var token_list : TokenList .
;  var module : Module .
;  var level_constraint : PrecedenceLevel .
;  var sort_constraint : Sort + { Universal } .
;
;  eq : parser$parser(token_list, module, level_constraint, sort_constraint)
;       =
;       let terletox0_list =
;          parser$get_term(token_list, module, level_constraint)
;             -- possibly empty
;       if null(terletox0_list)
;         then empty-list
;         else parser$parser_continuing(terletox0_list, module,
;                             level_constraint, sort_constraint) .
;  
;  -- Notes on (input) constraints. They can be "turned off":
;  --   no precedence constraint: give highest precedence (within the range
;  --      of possible values);
;  --   no sort constraint: give sort Universal (a Lisp symbol) .
;  -- Notes on exceptional values:
;  --   the standard exceptional value is: empty-list; it can be returned by
;  --     parser$parser, parser$get_term, etc.
;  -- Note on variable naming:
;  --   terletox is coined to evoke "TERm-(precedence) LEvel-TOkenlist"
;  --     and is of type ( ( ObjTerm . PrecedenceLevel ) . TokenList )

(defun parser$parser
          ;  Note: parser$parser must not be called on an empty token_list:
          ;    responsibility of the caller !
  (token_list module level_constraint sort_constraint)
  (let
    ( (terletox0_list
       (parser$get_term token_list module level_constraint))
            ; -- token_list not empty !
    )
    (if (null terletox0_list)
    ;then:
      nil  ;return an empty solution
    ;else:
      (parser$parser_continuing terletox0_list module
                    level_constraint sort_constraint
      )
    )  ;end if
  )  ;end let
)

;  -------------------------------------------------------------------------

;  op parser$parser_continuing :
;       LIST[ ( ( ObjTerm . PrecedenceLevel ) . TokenList ) ]  -- not empty !
;       Module
;       PrecedenceLevel  -- constraint
;       Sort + { Universal }  -- constraint
;       ->
;       LIST[ ( ( ObjTerm . PrecedenceLevel ) . TokenList ) ]
;         -- possibly empty
;  
;  var module : Module .
;  var level_constraint : PrecedenceLevel .
;  var sort_constraint : Sort + { Universal } .
;  var E : ( ( ObjTerm . PrecedenceLevel ) . TokenList )  -- element
;  var S : LIST[ ( ( ObjTerm . PrecedenceLevel ) . TokenList ) ]  -- set
;  
;  eq : parser$parser_continuing(empty-list, module,
;                     level_constraint, sort_constraint)
;     = empty-set .
;  
;  eq : parser$parser_continuing(E adjoined-to S, module,
;                      level_constraint, sort_constraint)
;    =
;    parser$parser_continue_check(E, module, level_constraint, sort_constraint)
;     adjoined-to
;    parser$parser_continuing(S, module, level_constraint, sort_constraint).

(defun parser$parser_continuing
  (terletox0_list module level_constraint sort_constraint)
  (let
    ( (terletox_list_prime nil)  ;initialization--will serve as accumulator
                            ;  and be returned in the end
    )
    (dolist (terletox0 terletox0_list terletox_list_prime)
      (setq terletox_list_prime  ;accumulate
            (nconc (parser$parser_continue_check terletox0 module
                            level_constraint sort_constraint
                   )
                   terletox_list_prime
            )  ;end nconc
      )
    )  ;end dolist
  )  ;end let
)

;  -------------------------------------------------------------------------

;  op parser$parser_continue_check :
;       ( ( ObjTerm . PrecedenceLevel ) . TokenList )
;       Module
;       PrecedenceLevel  -- constraint
;       Sort + { Universal }  -- constraint
;       ->
;       LIST[ ( ( ObjTerm . PrecedenceLevel ) . TokenList ) ] .
;         -- possibly empty
;
;  var T0 : ObjTerm .
;  var L0 level_constraint : PrecedenceLevel .
;  var token_list : TokenList .
;  var module : Module .
;  var sort_constraint : Sort + { Universal } .
;
;  eq parser$parser_continue_check( ( ( T0 . L0 ) . token_list ),
;              module, level_constraint, sort_constraint)
;     =
;     let terletox_list =
;       parser$parser_continue( ( ( T0 . L0 ) . token_list ),
;                       module, level_constraint, sort_constraint)
;         -- possibly empty
;     if parser$in_same_connected_component(
;                            term$sort(T0),
;                            sort_constraint,
;                            module$sort_order(module))
;       then ( ( T0 . L0 ) . token_list ) adjoined-to terletox_list
;       else terletox_list .

(defun parser$parser_continue_check
  (terletox0 module level_constraint sort_constraint)
  (let*
    ( (obj_term0 (caar terletox0))
      (sort0 (term$sort obj_term0))
      (sort_order (module$sort_order module))
      (terletox_sublist_prime
        ;add obj_term0 in the set of solutions if its sort is correct:
        (if (parser$in_same_connected_component
	     sort0 sort_constraint sort_order)
        ;then:
          (list terletox0)
        ;else:
          nil
        )
      )
    )
    (nconc terletox_sublist_prime
	   (parser$parser_continue
	     terletox0 module level_constraint sort_constraint)
    )
  )  ;end let*
)

;  -------------------------------------------------------------------------

;  op parser$parser_continue :
;       ( ( ObjTerm . PrecedenceLevel ) . TokenList )
;       Module
;       PrecedenceLevel  -- constraint
;       Sort + { Universal }  -- constraint
;       ->
;       LIST[ ( ( ObjTerm . PrecedenceLevel ) . TokenList ) ] .
;         -- possibly empty
;  
;  var token_list : TokenList .
;  var T0 : ObjTerm .
;  var L0 level_constraint : PrecedenceLevel .
;  var module : Module .
;  var sort_constraint : Sort + { Universal }.
;
;  eq parser$parser_continue( ( ( T0 . L0 ) . token_list ),
;                     module, level_constraint, sort_constraint)
;     =
;     let current-token = inspect-next-token(token_list)
;                            -- inspect but do not swallow
;     let choice =
;       parser$choose_operators_from_token( ( T0 . L0 ), current-token,
;                        module, level_constraint)  -- possibly empty
;     if null(choice)
;       then empty-list
;       else parser$parser_continue_for_operators(token_list, T0, choice,
;                              module, level_constraint, sort_constraint) .

(defun parser$parser_continue
  (terletox0 module level_constraint sort_constraint)
  (let
    ( (token_list (cdr terletox0)) )  ;possibly emtpy 
    (if (null token_list)
    ;then:
      nil  ;return a void solution
    ;else:
        (let*
          ( (token1 (car token_list))
            (term_level0 (car terletox0))
            (choice 
              (parser$choose_operators_from_token
	        term_level0 token1 module level_constraint)
            )  ;choice possibly empty
          )
          (if (null choice)
          ;then:
            nil  ;return a void solution
          ;else:
              (parser$parser_continue_for_operators token_list
                                   (car term_level0)  ;obj_term0
                                   choice module
                                   level_constraint sort_constraint
              )
          )
        )  ;end let*
    )  ;end if
  )  ;end let
)

;  -------------------------------------------------------------------------

;  op parser$parser_continue_for_operators :
;       TokenList
;       ObjTerm  -- sub-term to build above; an acceptable first argument
;       LIST[ LatefixOperator + JuxtapositionOperator ]  -- not empty !
;       Module
;       PrecedenceLevel  -- constraint
;       Sort + { Universal }  -- constraint
;       ->
;       LIST[ ( ( ObjTerm . PrecedenceLevel ) . TokenList ) ]
;         -- possibly empty
;
;  var token_list : TokenList .
;  var T0 : ObjTerm .
;  var L0 level_constraint : PrecedenceLevel .
;  var module : Module .
;  var sort_constraint : Sort + { Universal } .
;  var E : LatefixOperator + JuxtapositionOperator .  -- element
;  var S : LIST[ LatefixOperator + JuxtapositionOperator ] .  -- set
;
;  eq parser$parser_continue_for_operators(token_list, T0, empty-set, module,
;                           level_constraint, sort_constraint)
;     = empty-set .
;  
;  eq parser$parser_continue_for_operators(token_list, T0, E adjoined-to S,
;                           module, level_constraint, sort_constraint)
;     =
;     parser$parser_continue_for_operator(token_list, T0, E, module,
;                     level_constraint, sort_constraint)
;       adjoined-to
;     parser$parser_continue_for_operators(token_list, T0, S, module,
;                             level_constraint, sort_constraint) .

(defun parser$parser_continue_for_operators
  (token_list obj_term0 late_juxt_operator_list
    module level_constraint sort_constraint
  )
  (let
    ( (terletox_list_prime nil)  ;initialization--to be returned in the end
    )
    (dolist (late_juxt_operator late_juxt_operator_list terletox_list_prime)
      ;accumulate:
      (setq terletox_list_prime
            (nconc (parser$parser_continue_for_operator
		    token_list obj_term0 late_juxt_operator
		    module level_constraint sort_constraint
                   )
                   terletox_list_prime
             )
      )
    )  ;end dolist
  )
)

;  -------------------------------------------------------------------------

;  op parser$parser_continue_for_operator :
;       TokenList
;       ObjTerm  -- sub-term to build above; an acceptable first argument
;       LatefixOperator + JuxtapositionOperator
;       Module
;       PrecedenceLevel  -- constraint
;       Sort + { Universal }  -- constraint
;       ->
;       LIST[ ( ( ObjTerm . PrecedenceLevel ) . TokenList ) ] .
;         -- possibly empty
;  
;  var token_list : TokenList .
;  var T0 : ObjTerm .
;  var level_constraint : PrecedenceLevel .
;  var operator : LatefixOperator + JuxtapositionOperator .
;  var module : Module .
;  var sort_constraint : Sort + { Universal } .
;  
;  eq parser$parser_continue_for_operator(token_list, T0, operator, module,
;                     level_constraint, sort_constraint)
;     =
;     let first_result =
;           parser$finish_term_for_operator(token_list, T0, operator, module)
;                 -- possibly an empty list
;     if null(first_result)
;       then nil
;       else
;         parser$parser_continuing(first_result, module,
;                        level_constraint, sort_constraint) .
;  
;  -- Note: parser$parser_continue_for_operator is not called unless operator
;  --    is a latefix operator or a juxtaposition operator,
;  --    and is so far an acceptable operator.

(defun parser$parser_continue_for_operator
  (token_list obj_term0 late_juxt_operator
    module level_constraint sort_constraint
  )
  (let
    ( (first_result_list
        (parser$finish_term_for_operator
	  token_list obj_term0 late_juxt_operator module)
      )
    )
    (if (null first_result_list)  ;
    ;then:
      nil  ;return an empty solution
    ;else:
        (parser$parser_continuing first_result_list module
                      level_constraint sort_constraint
        )
    )
  )  ;end let
)

;  -------------------------------------------------------------------------

;  op parser$finish_term_for_operator :
;       TokenList
;       ObjTerm  -- an acceptable first argument
;       LatefixOperator + JuxtapositionOperator
;         -- a so far acceptable latefix or juxtaposition operator to try
;       Module
;       -> LIST[ ( ( ObjTerm . PrecedenceLevel ) . TokenList ) ] .
;            -- possibly empty
;
;-- Notes:
;-- 1. This procedure is not called, unless:
;--	a. the next token to swallow is the first token part of the latefix
;--           operator given as input argument; or refers to a variable, a
;--           constant, a function, a prefix part of an operator or is "(";
;--	b. the latefix or juxtaposition operator given as input is
;--           acceptable so far, i.e. with regard to sort and precedence of
;--           the subterm obtained so far.
;
;-- 2.
;subsorts LatefixOperator < Operator .
;-- An operator is of sort LatefixOperator if its pattern starts with:
;-- "_" followed by a token and possibly other things, e.g. "_ + _", "_ !",
;-- "_ [ _ ]".
;
;-- 3.
;subsorts JuxtapositionOperator < Operator .
;-- An operator is of sort JuxtapositionOperator if its pattern starts with:
;-- "_ _", e.g. "_ _", "_ _ _ ", "_ _ _ foo _".

(defun parser$finish_term_for_operator
  (token_list obj_term0 late_juxt_operator module)
  (let*
    ( (form (operator$form late_juxt_operator))
      (rest_form (cdr form))  ;we already got the first argument
      (arg_acc_list (list (cons (list obj_term0) token_list))) ;initialization
      (arg_acc_list_prime  ;possibly nil
        (parser$collect_arguments arg_acc_list module rest_form)
      )
    )
    (if (null arg_acc_list_prime)
      nil  ;return a void answer
      ;else:
      (parser$make_terms late_juxt_operator arg_acc_list_prime module)
    )  ;end if
  )  ;end let*
)

;  -------------------------------------------------------------------------

;  op parser$get_term : 
;       TokenList  -- not empty !
;       Module
;       PrecedenceLevel  -- constraint
;       ->
;       LIST[ ( ( ObjTerm . PrecedenceLevel ) . TokenList ) ] .
;         -- possibly empty

(defun parser$get_term
  (token_list module level_constraint)
               ;Note: parser$get_term must not be called
               ;  with an empty token_list
               ;  --responsibility of the caller !
  ;let's examine the first token:
  (let
    ( (token1 (car token_list))  ;token_list non null
      (token_list_prime (cdr token_list))
    )
    ;what first token have we got ?
    (cond
      ;***A*** Reserved tokens
      ;***A1*** Parenthesized expression
      ( (equal token1 "(")
        (if (null token_list_prime)
        ;then:
          nil  ;return an empty set of solutions
        ;else:
          (parser$get_rest_of_parenthesized_expr token_list_prime module)
        )
      )
      ;***A2*** Unacceptable reserved tokens
      ( (member token1 '( ")" "," )) nil )  ;return empty set of solutions
      ;***B*** Regular tokens
      ;  They have to have been declared operators or variables:
      ( t (parser$get_term_for_regular_token token1 token_list_prime module
                                 level_constraint
          )
      )
    )  ;end cond
  )
)

;  ------------------------------------------------------------------------

;  op parser$get_rest_of_parenthesized_expr :
;       TokenList  -- not empty !
;       Module
;       ->
;       LIST[ ( ( ObjTerm . PrecedenceLevel ) . TokenList ) ] .
;         -- possibly empty

(defun parser$get_rest_of_parenthesized_expr (token_list module)
  (let
    ((terletox_list
      (parser$parser
       token_list module parser$max_precedence 'universal_sort))
     ;possibly empty
     (terletox_list_prime nil)  ;accumulator--to be returned in the end
     terletox)
    ; this is "over-general"
    ; group terms together with same remaining token list
    ; check for possible term qualification, if present treat
    ; group of terms as a unit
    (loop
      (when (null terletox_list) (return terletox_list_prime))
      (setq terletox (car terletox_list))
      (setq terletox_list (cdr terletox_list))
      (let
        ((token_list (cdr terletox))
	 (obj_terms (list (caar terletox)))
	 (rest_terletox_list nil))
	(dolist (tlt terletox_list)
	  (if (eq (cdr tlt) token_list)
	      (push (caar tlt) obj_terms) ;use rplacd for space ??
	    (push tlt rest_terletox_list)))
	(setq terletox_list rest_terletox_list)
	;for each solution, try to swallow ")";
        (if (equal (car token_list) ")")
              ;token_list is not empty and begins with ")":
        ;then:  ;swallow ")", set precedence level to 0, and accumulate:
	(if (and (cdr token_list)
		 (eq #\. (elt (cadr token_list) 0)))
	        ;&&&& might modify this last condition a bit
	    (multiple-value-bind (terms toks)
	        (parser$scan_qualification
		 obj_terms
		 (cdr token_list))
	      (dolist (tm terms)
		(setq terletox_list_prime
		     (cons (cons (cons tm parser$min_precedence)
				 toks)
			   terletox_list_prime))
		) ;dolist
	    )
	  ;else: there isn't a qualification; create continuations
	  (dolist (tm obj_terms)
            (setq terletox_list_prime
                 (cons (cons (cons tm parser$min_precedence)
                             (cdr token_list))
                       terletox_list_prime))
	    ) ;dolist
	  ) ;if for "."
        ;else:
          nil  ;reject
        )  ;end if
      )
    )  ;loop
  )
)

;  ------------------------------------------------------------------------

;  op parser$scan_qualification : TermList TokenList -> TermList TokenList
;  Token list starts with the qualification; for ((x + y) . A) is (. A)

(defun parser$scan_qualification (obj_terms token_list)
  (let ((tokens (if (equal "." (car token_list)) (cdr token_list)
		  (cons (subseq (car token_list) 1) (cdr token_list))))
	(res nil)
	qualifier rest)
  (if (equal "(" (car tokens))
      (let ((paren_pair (parser$scan_parenthesized_unit tokens)))
	(if (null paren_pair) (setq res 'unbalanced)
	  (setq qualifier (if (atom (car paren_pair))
			      (list (car paren_pair))
			    (car paren_pair))
		rest (cdr paren_pair))))
    (setq qualifier (list (car tokens))  rest (cdr tokens)))
  (if (eq 'unbalanced res) (values obj_terms token_list)
    (let ((modval (modexp_eval$eval (modexp_parse$parse qualifier))))
    (if (modexp_eval$is_error modval)
	(let ((srt (if (and obj$current_module
			    (or (stringp qualifier)
				(and (consp qualifier) (null (cdr qualifier))
				     (stringp (car qualifier)))))
		       (mod_eval$$find_qual_sort_in
			obj$current_module qualifier))))
	  (if srt
	      (let ((exact nil) (res nil) tm
		    (so (module$sort_order obj$current_module)))
		(loop
		  (when (null obj_terms) (return))
		  (setq tm (car obj_terms))
		  (setq obj_terms (cdr obj_terms))
		  (when (sort_order$is_included_in so (term$sort tm) srt)
		    (if res (progn (setq exact t) (push tm res) (return))
		      (push tm res))))
		(if exact
		    (let ((oldres res))
		      (setq res nil)
		      (dolist (tm oldres)
			(when (eq srt (term$sort tm)) (push tm res)))
		      (dolist (tm obj_terms)
			(when (eq srt (term$sort tm)) (push tm res)))))
		(values res rest))
	    (values obj_terms token_list)
	    )) ;let if
      (let ((exact nil) (res nil) tm)
	(loop
	 (when (null obj_terms) (return))
	 (setq tm (car obj_terms))
	 (setq obj_terms (cdr obj_terms))
	 (when (if (term$is_var tm)
		   (member (term$sort tm) (module$sorts modval))
		 (let ((hd (term$head tm)))
		 (if (and (null (term$subterms tm))
			  (eq 'constant (car (operator$name hd))))
		   (member (operator$coarity hd) (module$sorts modval))
		   (member (term$head tm) (module$operators modval)))))
	   (if res (progn (setq exact t) (push tm res) (return))
	     (push tm res))))
	(if exact
	    (let ((oldres res))
	      (setq res nil)
	      (dolist (tm oldres)
		(when (if (term$is_var tm)
			  (eq modval (sort$module (term$sort tm)))
			(eq modval (operator$module (term$head tm))))
		  (push tm res)))
	      (dolist (tm obj_terms)
		(when (if (term$is_var tm)
			  (eq modval(sort$module (term$sort tm)))
			(eq modval (operator$module (term$head tm))))
		  (push tm res)))))
	(values res rest))
    )) ;let if
  ))
  )

;  op parser$scan_parenthesized_unit : TokenList -> Pair[TokenList,TokenList]

(defun parser$scan_parenthesized_unit (tokens)
  (if (equal "(" (car tokens))
      (let ((count 1) (lst (cdr tokens)) (res nil))
	(loop
	 (when (null lst) (return 'unbalanced))
	 (let ((tok (car lst)))
	   (setq lst (cdr lst))
	   (when (and (= 1 count) (equal ")" tok))
	     (return (cons (if (and res (null (cdr res)))
			     (car res)
			     (nreverse res))
			   lst)))
	   (setq res (cons tok res))
	   (if (equal "(" tok) (incf count)
	   (if (equal ")" tok) (decf count)))
	 )
	)
      )
    ;(cons (car tokens) (cdr tokens)) equiv to
    tokens
    )
  )

;  ------------------------------------------------------------------------

;  op parser$get_term_for_regular_token :
;       Token
;       TokenList  -- possibly empty !
;       Module
;       PrecedenceLevel  -- constraint
;       ->
;       LIST[ ( ( ObjTerm . PrecedenceLevel ) . TokenList ) ] .
;         -- possibly empty

(defun parser$get_term_for_regular_token
  (token token_list module level_constraint)
  (let
    ( (op_var_list (dictionary$info_on_token (module$parse_dictionary module)
                                            token
                  )
      )
         ;list of Operators and Variables--token is the first token
         ;  to appear in the pattern !
         ;If choice between operators of
         ;  comparable sorts ?  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (terletox_list_prime nil)  ;accumulator--to be returned in the end
    )
    ;for each operator or variable, go ahead and collect solutions
    (dolist (op_var op_var_list terletox_list_prime)
                                ;terletox_list_prime to be returned
      (setq terletox_list_prime  ;accumulate
            (nconc (parser$get_term_for_op_var op_var token_list module
                                  level_constraint
                     )
                     terletox_list_prime
            )
      )
    )
  )
)

;  ------------------------------------------------------------------------

;  op parser$get_term_for_op_var :
;       Operator + Variable
;       TokenList  -- possibly empty
;       Module
;       PrecedenceLevel  -- constraint
;       ->
;       LIST[ ( ( ObjTerm . PrecedenceLevel ) . TokenList ) ] .
;         -- possibly empty

(defun parser$get_term_for_op_var (op_var token_list module level_constraint)
  (case (term_parser$syntactic_type op_var)

    ;***1*** Variable
    ;  Note: a variable is referenced by **ONE** token--always !
    (variable
      ;return a list of only one solution (precedence level is 0):
      (list (cons (cons op_var parser$min_precedence) token_list))
    )

    ;***2*** Antefix
    (antefix
      ;is precedence of antefix operator acceptable ?
      (if ( <= (operator$precedence op_var) level_constraint)
      ;then:
        (parser$get_term_from_antefix_operator op_var token_list module)
      ;else:
        nil  ;return a void solution
      )
    )
    ;***3*** Case token does not belong to sub-formula
    (otherwise nil)  ;return a void solution
  )
)

;  -------------------------------------------------------------------------

;  op parser$get_term_from_antefix_operator :
;       Operator  -- must be antefix !
;       TokenList  -- possibly empty
;       Module
;       -> LIST[ ( ( ObjTerm . PrecedenceLevel ) . TokenList ) ] .
;            -- possibly empty

(defun parser$get_term_from_antefix_operator
  (operator token_list module)
  (let*
    ( (form (operator$form operator))
      (rest_form (cdr form))  ;we already swallowed the first token
        ;rest_form possibly empty
      (arg_acc_list (list (cons nil token_list)))  ;initialization
      (arg_acc_list_prime
        (parser$collect_arguments arg_acc_list module rest_form)
        ;arg_acc_list_prime possibly empty
      )
    )
    (if (null arg_acc_list_prime)
      nil  ;return a void answer
      ;else
      (parser$make_terms operator arg_acc_list_prime module)
    )	
  )  ;end let*
)

;  ------------------------------------------------------------------------

;  op parser$choose_operators_from_token :
;       ( ObjTerm . PrecedenceLevel )
;       Token  -- the coming token
;       Module
;       PrecedenceLevel  -- constraint
;       ->
;       LIST[ LatefixOperator + JuxtapositionOperator ] .
;         -- possibly empty; any operator in the list is either latefix
;         --   (i.e. of pattern { _ <Token> <etc> } ) or juxtapositional
;         --   (i.e. of pattern { _ _ <etc> } ).

(defun parser$choose_operators_from_token
  (term_level0 token module level_constraint)
  (cond
    ;***A*** Reserved tokens (need not be declared)
    ( (equal token "(") 
      (parser$choose_juxtaposition_operators
        term_level0 module level_constraint)
    )
    ( (member token '( ")" "," ) ) nil )  ;return a void answer
    ;***B*** Regular tokens
    (t 
      (let
        ( (op_var_list (dictionary$info_on_token
                        (module$parse_dictionary module)
                        token
                      )
          )
        )
        (if (null op_var_list)
        ;then:
          nil  ;return a void answer
        ;else:
          (parser$choosing_operators
	    term_level0 op_var_list module level_constraint)
        )
      )  ;end let
    )
  )  ;end cond
)

;  -------------------------------------------------------------------------

;  op parser$choosing_operators :
;       ( ObjTerm . PrecedenceLevel )
;       LIST[ Operator + Variable ]  -- non empty !
;       Module
;       PrecedenceLevel  -- constraint
;       ->
;       LIST[ LatefixOperator + JuxtapositionOperator ] .

(defun parser$choosing_operators
  (term_level0 op_var_list module level_constraint)
  (let
    ( (late_juxt_op_list_prime nil)
        ;initialization--to be returned in the end
    )
    (dolist (op_var op_var_list late_juxt_op_list_prime)
      (setq late_juxt_op_list_prime  ;accumulate
        (union
	  (parser$choose_operators
	   term_level0 op_var module level_constraint)
	  late_juxt_op_list_prime
        )  ;union (rather than cons) necessary in this case, for there
           ;  is no garantee that no juxtaposition will show
           ;  up more than once
      )
    )  ;end dolist
  )
)

;  -------------------------------------------------------------------------

;  op parser$choose_operators :
;       ( ObjTerm . PrecedenceLevel )
;       Operator + Variable
;       Module
;       PrecedenceLevel  -- constraint
;       ->
;       LIST[ LatefixOperator + JuxtapositionOperator ] .

(defun parser$choose_operators
  (term_level0 op_var module level_constraint)
  (case (term_parser$syntactic_type op_var)
    ;***1***
    (variable
      (parser$choose_juxtaposition_operators
        term_level0 module level_constraint)
    )
    ;***2***
    (antefix
      ;is op_var acceptable with regard to level_constraint ?
      (if (<= (operator$precedence op_var) level_constraint)
      ;then:
        (parser$choose_juxtaposition_operators
	  term_level0 module level_constraint)
      ;else:
        nil  ;return a void answer
      )  ;end if
    )
    ;***3***
    (latefix
      (parser$choose_latefix_operators
        term_level0 op_var module level_constraint))
    ;***4***
    (otherwise nil)
  )  ;end case
)

;  ------------------------------------------------------------------------

;  op parser$choose_juxtaposition_operators :
;       ( ObjTerm . PrecedenceLevel )  -- a possible first argument
;       Module
;       PrecedenceLevel  -- constraint
;       ->
;       LIST[ JuxtapositionOperator ] .
;         -- possibly empty
;  Abstract: return juxtaposition operators
;    - of acceptable precedence with regard to level_constraint
;    - able to accept term_level0 as a first argument
;      (check sort and precedence)

(defun parser$choose_juxtaposition_operators
  (term_level0 module level_constraint)
  (let
    ( (juxt_op_list (module$juxtaposition module)) )
    (if (null juxt_op_list)
    ;then:
      nil  ;return a void answer
    ;else:
        (let
          ( (juxt_op_list_prime nil) )
              ;initialization--to be returned in the end
          (dolist (juxt_op juxt_op_list juxt_op_list_prime)
            (if
              (and 
                (<= (operator$precedence juxt_op) level_constraint)
                (parser$check_operator term_level0 juxt_op module)
              )
            ;then:
                (setq juxt_op_list_prime
                      (cons juxt_op juxt_op_list_prime)  ;accumulate
                )
            ;else:
                nil  ;don't accumulate
            )  ;end if
          )
        )
    )  ;end if
  )
)

;  -------------------------------------------------------------------------

;  op parser$choose_latefix_operators :
;       ( ObjTerm . PrecedenceLevel )
;       LatefixOperator
;       Module
;       PrecedenceLevel  -- constraint
;       ->
;       LIST[ LatefixOperator ] 
;         -- empty or singleton !
;  Abstract: return a singleton list containing latefix_operator if it is:
;    - of acceptable precedence with regard to level_constraint
;    - able to accept term_level0 as a first argument
;      (check sort and precedence)

(defun parser$choose_latefix_operators
  (term_level0 latefix_operator module level_constraint)
  (if
    (and
      (<= (operator$precedence latefix_operator) level_constraint)
      (parser$check_operator term_level0 latefix_operator module)      
    )
  ;then:
      (list latefix_operator)
  ;else:
      nil  ;return a void answer
  )  ;end if
)

;  -------------------------------------------------------------------------

;  op parser$check_operator :
;       ( ObjTerm . PrecedenceLevel )
;       JuxtapositionOperator + LatefixOperator
;       Module
;       ->
;       Bool .

;  Abstract: check that term_level0 is acceptable to
;    late_juxt_op as a first argument: check sorts and precedences.

(defun parser$check_operator (term_level0 late_juxt_op module)
  (let*
    ( (sort0 (term$sort (car term_level0)))
      (level0 (cdr term_level0))
      (form (operator$form late_juxt_op))
      (first_arg_constraints (car form))
      (first_arg_level_constraint (or (cadr first_arg_constraints) 0))
      (first_arg_sort_constraint (cddr first_arg_constraints))
      (sort_order (module$sort_order module))
    )
    (and
      (<= level0 first_arg_level_constraint)
      (parser$in_same_connected_component
        sort0 first_arg_sort_constraint sort_order
      )
    )  ;end and
  )  ;end let*
)

;  ------------------------------------------------------------------------

;  op parser$collect_arguments :
;       LIST[ ( ObjTermList . TokenList ) ]  -- not empty !
;       Module
;       Form  -- possibly empty
;       ->
;       LIST[ ( ObjTermList . TokenList ) ]
;
;  Form = LIST[ ( Flag . Value ) ]
;  Flag = { 'token, 'argument }
;  Value = if flag is 'token then Token  -- the token to swallow
;          if         'argument then ( PrecedenceLevel . Sort )
;                                      -- constraints on sub-formula
;  Examples
;  1. A Form value for Natural addition is:
;   '( (argument . ( <precedence level constraint> . <sort Natural> ) )
;      (token . "+")
;      (argument . ( <precedence level constraint> . <sort Natural> ) )
;    )
;  2. A Form value for some binary function is:
;   '( (token . "foo")
;      (token . "(")
;      (argument . ( 127 . <a Sort value> ) )
;      (token . ",")
;      (argument . ( 127 . <a Sort value> ) )
;      (token . ")")
;    )

(defun parser$collect_arguments
  (arg_acc_list module rest_form)
    ;rest_form possibly empty, i.e. nil
  (let
    ( (arg_acc_list_prime arg_acc_list)
         ;arg_acc_list_prime to be returned in the end
    )
    (dolist (form_item rest_form arg_acc_list_prime)
      (case (car form_item)
        ;***1***
        (token 
          (setq arg_acc_list_prime
                (parser$scan_token arg_acc_list_prime (cdr form_item))
          )
        )
        ;***2***
        (argument
          (setq arg_acc_list_prime
                (parser$collect_one_argument arg_acc_list_prime module
                                      (cadr form_item)(cddr form_item)
                )
          )
        )
      )  ;end case
      (if (null arg_acc_list_prime)
      ;then:
        (return nil)
        ;to avoid unnecessary additional loops, and to avoid calling
        ;  either parser$scan_token or
	;  parser$collect_one_argument with void arguments.
      )  ;end if
    )
  )
)

;  ------------------------------------------------------------------------

;  op parser$collect_one_argument :
;       LIST[ ( ObjTermList . TokenList ) ]  -- not empty !
;       Module
;       PrecedenceLevel  -- constraint
;       Sort + { Universal }  -- constraint
;       ->
;       LIST[ ( ObjTermList . TokenList ) ]

(defun parser$collect_one_argument
  (arg_acc_list module level_constraint sort_constraint)
  (let
    ( (arg_acc_list_prime nil)  ;to be returned in the end
    )
    (dolist (arg_acc arg_acc_list arg_acc_list_prime)
      (let
        ( (token_list (cdr arg_acc)) )
        (if (null token_list)
        ;then:
          nil  ;this iteration is finished
        ;else:
            (let*
              ( (arg_list (car arg_acc))
                (terletox_list (parser$parser token_list module
                                    level_constraint sort_constraint
                             )
                               ;notice that parser$parser is not called with
                               ;  token_list empty
                )
              )
              (dolist (terletox terletox_list)
                ;if terletox_list empty, no effect
                (let*
                  ( (arg_prime (caar terletox))
                    (token_list_prime (cdr terletox))
                    (arg_list_prime (cons arg_prime arg_list))
                      ;notice that we accumulate arguments in reverse order
                    (arg_acc_prime (cons arg_list_prime token_list_prime))
                  )
                  (setq arg_acc_list_prime
                        (cons arg_acc_prime arg_acc_list_prime)  ;accumulate
                  )
                )
              )  ;end dolist
            )
        )  ;end if
      )
    )  ;end dolist
  )
)

;  ------------------------------------------------------------------------

;  op parser$scan_token :
;       LIST[ ( ObjTermList . TokenList ) ]  -- not empty !
;       Token  -- ie Lisp character string
;       ->
;       LIST[ ( ObjTermList . TokenList ) ]

(defun parser$scan_token (arg_acc_list token)
  (let
    ( (arg_acc_list_prime nil)  ;to be returned in the end
    )
    (dolist (arg_acc arg_acc_list arg_acc_list_prime)
      (let
        ( (token_list (cdr arg_acc))
        )
        (if (equal token (car token_list))
              ;token_list is not empty and begins with expected token
        ;then:
            (let*
              ( (token_list_prime (cdr token_list))
                (arg_list (car arg_acc))
                (arg_acc_prime (cons arg_list token_list_prime))
              )
              (setq arg_acc_list_prime
                    (cons arg_acc_prime arg_acc_list_prime)
                ;accumulate
              )
            )
        ;else:
          nil
        )
      )
    )
  )
)

;  ------------------------------------------------------------------------

;  op parser$in_same_connected_component :
;       Sort
;       Sort + { Universal }  -- Universal is a Lisp symbol
;       SortOrder
;       ->
;       Bool .

;The following function should be a macro:
(defun parser$in_same_connected_component (sort1 sort2 sort_order)
  (cond
    ( (equal sort2 'universal_sort) t )
    ( (parser$is_included_in sort1 sort2 sort_order) t )
    ( (parser$is_included_in sort2 sort1 sort_order) t )
    ( t (sort$in_same_connected_component sort1 sort2 sort_order) )
  )
)

;  ------------------------------------------------------------------------

;  op parser$make_terms :
;       Operator
;       LIST[ ( LIST[ ObjTerm ] . TokenList ) ]  -- not empty
;       Module
;       -> LIST[ ( ( ObjTerm . PrecedenceLevel ) . TokenList ) ] .
;            -- possibly empty
;
;  Remark. This function is called by:
;    parser$finish_term_for_operator and parser$get_term_from_antefix_operator

(defun parser$make_terms
  (operator arg_acc_list module)
  (let
    ( (terletox_list nil) )  ;initialization--to be returned in the end
    (dolist (arg_acc arg_acc_list terletox_list)
      (block iteration
        (let*
          ;build result... :
          ( (arg_list (car arg_acc))
            (direct_arg_list (reverse arg_list))
              ;arguments were accumulated in reverse order !
            (arg_sort_list (mapcar #'term$sort direct_arg_list))
              ;arg_sort_list: list of argument sorts
            (operator_prime operator)  ;initialization
            (obj_term nil)  ;reservation
            (precedence_level (operator$precedence operator))
            (token_list (cdr arg_acc))
            (terletox nil) )  ;reservation
          (if (parser$are_argument_sorts_correct
	       operator arg_sort_list module)
            ;then (1):
            (progn
              (if (or (operator$is_marked_strictly_overloaded operator)
		      (operator$polymorphic operator)) ;&&&& change
                (progn
                  (setq operator_prime
                    (operator_table$lowest_operator operator arg_sort_list))
                  (if (equal operator_prime 'none)
                    ;then no result this iteration; do not accumulate:
                    (return-from iteration nil)))) ;if
              (setq obj_term
                (if (parser$are_well_defined_terms direct_arg_list)
                  ;then:
                  (term$make_term operator_prime direct_arg_list)
                  ;else:
                  (term$make_inheritedly_ill_term
                                 operator_prime direct_arg_list)))) ;progn
            ;else (2):
            (setq
              obj_term
                (term$make_directly_ill_term operator direct_arg_list))) ;if
          ;...and accumulate:
          (setq
            terletox (cons (cons obj_term precedence_level) token_list)
            terletox_list (cons terletox terletox_list)) ;setq
        ) ;let*
      ) ;block {iteration}
    ) ;dolist
  )
)

;  ------------------------------------------------------------------------

;  op parser$are_argument_sorts_correct :
;       Operator
;       LIST[ Sort ]  -- possibly empty (cf. constants)
;       Module
;       -> Bool .

;  Abstract: predicate returning 't {true} if each sort in the list
;    (meant to be a list of argument sorts) is included in the sort
;    expected by the operator for an argument at this position.

; !!! to optimize !!!
(defun parser$are_argument_sorts_correct
  (operator sort_list module)
  (if (null sort_list) t
    ;else:
    (let*
      ( (reference_sort_list (operator$arity operator))
        (sort_order (module$sort_order module))
        (result t) ;initialization
        (sort_list_prime sort_list)  ;initialization
        (sort nil)  ;reservation
      )
      (dolist (reference_sort reference_sort_list result)
        (setq sort (car sort_list_prime)
              sort_list_prime (cdr sort_list_prime)  ;for next iteration
        )
        (if (parser$is_included_in sort reference_sort sort_order)
          ;then do nothing; go to next iteration:
          nil
          ;else abort looping; return false:
          (return nil)
        )
      ) ;dolist
    )
  )
)

;  ------------------------------------------------------------------------

;  op parser$are_well_defined_terms :
;       LIST[ ObjTerm ]  -- possibly empty
;       -> Bool .

;  Abstract: predicate returning 't {true} if all terms are well-formed.

(defun parser$are_well_defined_terms
  (obj_term_list)
  (let
    ( (result t)  ;initialization
    )
    (dolist (obj_term obj_term_list result)
      (if (term$ill_defined obj_term)
        (return nil)  ;abort looping and return false
        ;else do nothing; go to next iteration
      )
    ) ;dolist
  )
)

;  ------------------------------------------------------------------------

;  op parser$is_included_in :
;       Sort
;       Sort + { Universal }  -- Universal is a Lisp symbol
;       SortOrder
;       ->
;       Bool .

;The following function should be a macro:
(defun parser$is_included_in (sort1 sort2 sort_order)
  (cond
    ( (equal sort2 'universal_sort) t )
    ( t (sort_order$is_included_in sort_order sort1 sort2) )
  )
)

;  ------------------------------------------------------------------------
