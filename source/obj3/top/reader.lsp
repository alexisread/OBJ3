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

;; $Id: reader.lsp,v 205.2.1.1 2003/09/23 14:04:45 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; OBJ lexical analyzer
; simple reader/readtable routines
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Tim Winkler ;;;; created: June 16, 86 ;;;;

;;;;;;;;;;;;;;;; Summary ;;;;;;;;;;;;;;;;
; op reader$read : {Input} -> Token [Input]
; op reader$read_sym : {Input} -> Token [Input]
; op reader$!set_syntax : {reader$read_table} Char Code -> {reader$read_table}
; op reader$get_syntax : {reader$read_table} Char -> Code
; op reader$!read_init : ->
; var *reader$eof_value* [ Constant ]
; op reader$is_at_eof : {Input:reader$$ch} -> Bool

;;;; READTABLE definition

(defconstant reader$$char_code_limit 192) ; Parameter

(defvar reader$$read_table (make-array (list reader$$char_code_limit)))
;;; nil entries mean treat as alphabetic/symbol component
;;; non-nil and symbol translate as symbol/single character token

(defmacro reader$!set_syntax (ch val)
  `(setf (aref reader$$read_table (char-code ,ch)) ,val))

(defmacro reader$get_syntax (ch)
  `(aref reader$$read_table (char-code ,ch)))

(defun reader$$!read_table_init ()
  (do ((i 0 (1+ i))) ((= i 192)) (setf (aref reader$$read_table i) nil))
  (reader$!set_syntax #\( '|(|) (reader$!set_syntax #\) '|)|)
  (reader$!set_syntax #\Space 'space)
  (reader$!set_syntax #\Tab 'space)
  (reader$!set_syntax #\Linefeed 'space)
  (reader$!set_syntax #\Page 'space)
  (reader$!set_syntax #\Return 'return)
  (reader$!set_syntax #\, '|,|)
  (reader$!set_syntax #\[ '|[|) (reader$!set_syntax #\] '|]|)
  (reader$!set_syntax #\{ '|{|) (reader$!set_syntax #\} '|}|)
  (reader$!set_syntax #\_ '|_|)
)

;;; note duplication of n
(defmacro reader$$valid_char_code (n)
  `(and (<= 0 ,n) (<= ,n reader$$char_code_limit)))

;;;; CHARACTER BUFFER

(defvar *reader$eof_value* '(*eof*))

(defvar reader$$ch 'space)

(defun reader$$get_char (file)
  #+MIPS (declare (optimize (safety 2)))
  (let ((inch (read-char file nil *reader$eof_value*)))
    (if (eq inch *reader$eof_value*) *reader$eof_value*
	(let ((val (reader$get_syntax inch)))
	  (if val val inch)))))

;;; READER ROUTINES

; op reader$is_at_eof : {Input:reader$$ch} -> Bool
(defun reader$is_at_eof ()
  ;; it is assumed that all uses of reader$$get_char are of the
  ;;   form: (setq reader$$ch (reader$$get_char ...))
  (eq *reader$eof_value* reader$$ch))

;;; read a symbol
(defun reader$read_sym (&optional (file *standard-input*))
  (loop (when (not (or (eq 'space reader$$ch) (eq 'return reader$$ch)))
	      (return))
	(setq reader$$ch (reader$$get_char file)))
  (if (eq *reader$eof_value* reader$$ch) *reader$eof_value*
    (reader$$read_symbol)))

;;; treat "(,)" specially; read a parenthesized unit
(defun reader$read (&optional (file *standard-input*))
  (loop (when (not (or (eq 'space reader$$ch) (eq 'return reader$$ch)))
	      (return))
	(setq reader$$ch (reader$$get_char file)))
  (if (eq *reader$eof_value* reader$$ch) *reader$eof_value*
  (case reader$$ch
    (|(| (reader$$read_list file)) ;...)
    (return nil)
    (otherwise
     (if (symbolp reader$$ch)
         (let ((str (string reader$$ch)))
             (setq reader$$ch 'space)
             (list (reader$$consider_token str)))
         (list (reader$$read_symbol file))))
  )))

;;; special top level read
;;; OBSOLETE
(defun reader$obj_read (&optional (file *standard-input*) (eofval nil))
  (let ((val (reader$read file)))
    (if (equal val '(return)) '(".")
    (if (eq *reader$eof_value* val) eofval
	val))))

;;; read up to matching close parenthesis
(defun reader$$read_list (&optional (file *standard-input*))
  (setq reader$$ch (reader$$get_char file))
  (reader$$read_rest_of_list file))

(defun reader$$read_rest_of_list (&optional (file *standard-input*))
  (loop (when (not (or (eq 'space reader$$ch) (eq 'return reader$$ch)))
	      (return))
	(setq reader$$ch (reader$$get_char file)))
  (if (eq *reader$eof_value* reader$$ch) *reader$eof_value*
  (if (eq '|)| reader$$ch)
    (progn
      (setq reader$$ch (reader$$get_char file))
      (list "(" ")"))
    (let ((res (list "(")) x)
      (loop
	(setq x (reader$read file))
	(when (eq *reader$eof_value* x) (return *reader$eof_value*))
	(setq res (append res x))
	(loop (when (not (or (eq 'space reader$$ch) (eq 'return reader$$ch)))
	        (return))
	  (setq reader$$ch (reader$$get_char file)))
	(when (eq '|)| reader$$ch)
	  (setq reader$$ch (reader$$get_char file))
	  (return (nconc res (list ")"))))
	(when (eq *reader$eof_value* reader$$ch) (return *reader$eof_value*))
      ))
  )))

;;; used only in reader$$read_symbol
(defvar reader$$buf (make-string 250))

(defvar obj_LISP$keyword "lisp:")
(defvar obj_BUILT-IN$keyword "built-in:")
(defvar obj_QUOTESYM$keyword "")

;;; read a symbol
(defun reader$$read_symbol (&optional (file *standard-input*))
  (let ((p -1) kind inch)
  (let ((res
    (loop
      (cond
       ;((eql #\Rubout reader$$ch) (decf p 1)) ;useful on Symbolics
	((characterp reader$$ch)
	 (incf p) (setf (aref reader$$buf p) reader$$ch))
	(t (let ((res (string reader$$ch)))
	     (setq reader$$ch 'space)
	     ;(setq reader$$ch (reader$$get_char file))
	     (return res))))
      (locally
	  #+MIPS (declare (optimize (safety 2)))
	  (setq inch (read-char file nil *reader$eof_value*)))
      (when (eq inch *reader$eof_value*)
	(if (<= 0 p)
	    (progn
	      (setq reader$$ch 'space)
	      (return (subseq reader$$buf 0 (1+ p))))
	  (return *reader$eof_value*)))
      (setq kind (reader$get_syntax inch))
      (setq reader$$ch (if kind kind inch))
      (when kind (return (subseq reader$$buf 0 (1+ p))))
    )))
    (reader$$consider_token res)
  )))

(defun reader$$suppress_ch (context)
  (declare (ignore context))
  (unless (eq *reader$eof_value* reader$$ch)
  (unless (or (eq 'space reader$$ch) (eq 'return reader$$ch))
    ;(princ "Warning: skipping input character '") (princ reader$$ch)
    ;(princ "'") (terpri)
    ;(princ "In the context of token: ") (princ context) (terpri)
    (unread-char
     (if (characterp reader$$ch) reader$$ch
       (char (string reader$$ch) 0)))
    (setq reader$$ch 'space)
    ))
  )

(defun reader$$consider_token (tok)
  (cond
   ((string= obj_LISP$keyword tok)
    (reader$$suppress_ch tok)
    (make-lisp :val (read))
    )
   ((string= obj_BUILT-IN$keyword tok)
    (reader$$suppress_ch tok)
    (make-bi-term :val (read))
    )
   ((string= obj_QUOTESYM$keyword tok)
    (reader$$suppress_ch tok)
    (read)
    )
   (t
    tok)
   )
  )

(defun reader$$read_error ()
  (format t "~&ERROR: OBJ read error~%")
  (obj3-to-top)
  )

;;;; INITIALIZATION

(defun reader$!read_init ()
  (reader$$!read_table_init)
  (setq reader$$ch 'space))
