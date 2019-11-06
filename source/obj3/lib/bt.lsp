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

;; $Id: bt.lsp,v 206.1 2003/09/26 13:02:36 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  GCL detailed backrace routines
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+GCL (progn

        (defun full-backtrace (&optional (start 0) (limit si::*ihs-top*))
          (when (< limit 0) (setq limit (- si::*ihs-top* limit)))
          (princ  "Backtrace: " *error-output*)
          (do* ((i (+ si::*ihs-base* start) (1+ i))
                (b nil))
               ((> i limit) (terpri *error-output*))
            (let ((fname (si::ihs-fname i)))
              (when b (princ " > " *error-output*))
              (if (= i si::*current-ihs*)
                  (write fname
                         :stream *error-output*
                         :escape t
                         :case :upcase)
                  (write fname
                         :stream *error-output*
                         :escape t
                         :case :downcase))
              (setq b t)))
          (values))

        (defun fb () (full-backtrace 0 -1))

        (defmacro ihs-vs-m (x y)
          (let ((chk (find-symbol "IHS-BASE" 'system)))
            (if chk `(,chk ,x ,y)
                `(,(find-symbol "VS" 'system) (+ (,(find-symbol "IHS-VS" 'system) ,x) ,y)))))

        (defun lcl (&optional (offset 0))
          (ihs-vs-m si::*current-ihs* offset))

        (defun system::*bak-prin* (x p)
          (let ((*standard-output* p)) (print$struct x)))
        (defvar system::*backtrace-printer* 'system::*bak-prin*)
        (defvar system::*backtrace-all* t)

        (defun bt ()
          (princ  "Backtrace: " *error-output*) (terpri *error-output*)
          (do* ((i system::*ihs-base* (1+ i)))
               ((> i system::*ihs-top*) (terpri *error-output*))
            (let ((fname (system::ihs-fname i)))
              (when (or system::*backtrace-all* (system::ihs-visible fname))
                (princ "-----------------------  ")
                (if (= i system::*current-ihs*)
                    (write fname
                           :stream *error-output*
                           :escape t
                           :case :upcase)
                    (write fname
                           :stream *error-output*
                           :escape t
                           :case :downcase))
                (terpri *error-output*)
                (funcall system::*backtrace-printer*
                         (ihs-vs-m i 0) *error-output*)
                (terpri *error-output*))))
          (values))

        (defun bak ()
          (princ  "Backtrace: " *error-output*) (terpri *error-output*)
          (do* ((i system::*ihs-top* (1- i)))
               ((< i system::*ihs-base*) (terpri *error-output*))
            (let ((fname (system::ihs-fname i)))
              (when (or system::*backtrace-all* (system::ihs-visible fname))
                (princ "-----------------------  ")
                (if (= i system::*current-ihs*)
                    (write fname
                           :stream *error-output*
                           :escape t
                           :case :upcase)
                    (write fname
                           :stream *error-output*
                           :escape t
                           :case :downcase))
                (terpri *error-output*)
                (funcall system::*backtrace-printer*
                         (ihs-vs-m i 0) *error-output*)
                (terpri *error-output*))))
          (values))
        )