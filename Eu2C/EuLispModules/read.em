;;; Copyright 1994-2010 Fraunhofer ISST
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'Eu2C'
;;;-----------------------------------------------------------------------------
;;
;;  Eu2C is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Eu2C is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: Read functions
;;;  Problems:
;;    Lacking a detailed error treatment.
;;;  Authors: Horst Friedrich, Rainer Rosenmuller
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule read
  (import ((only (<class>
                  <object>
                  <fpi>
                  <symbol>
                  <integer>
                  %signed-word-integer
                  %unsigned-word-integer
                  %signed-byte-integer
                  %string
                  make-fpi
                  make-swi
                  %extract
                  %cast
                  %mult
                  %plus
                  %eq
                  %gt
                  %lt
                  t)
                 tail)
           (only (<cons>
                  <null>
                  <list>
                  null?
                  cons?
                  cons
                  car
                  %pair-size
                  eq
                  %minus %lshiftr
                  %and %ge
                  %void %double-float)
                 apply)
           (only (get-dispatch-macro-character)
                 read-i)
           condition-ii
           (only (error)
                 condition-i)
           (only ($closed-bracket
                  ;$opend-bracket
                  $point
                  $char-string
                  $char-single-escape
                  $char-eof
                  $char-formfeed
                  $char-return
                  $char-newline
                  $char-open-bracket
                  $char-string-hex-l
                  $char-string-hex-u
                  $char-control-extension
                  *char-class-token*
                  char-class
                  *token-states*
                  half-vec-ref
                  half-vec-vec-ref
                  upper?
                  digit2figure10
                  digit2figure16)
                 char-tables)
           (only (stdin
                  %peek-unit
                  %unread-unit
                  %read-unit
                  ensure-open-character-input-stream
                  sscanf-3
                  sscanf-3-double
                  file-descriptor-pointer
                  <stream>
                  <end-of-stream>
                  stream-eos-action
                  setf-stream-eos-action
                  *not-eof-action*)
                 stream-i)
           (only (read-line
                  read-char
                  putback-char)
                 stream-generic)
           (only (<double-float>
                  dble
                  set-dble
                  make-dble)
                 double-float-i)
           (only (fpi?)
                 fpi-i)
           (only (strcmp)
                 c-string-interface)
           (only (make-symbol)
                 symbol)
           (only (make-string
                  <string>
                  duplicate-%string)
                 string-ii)
           (only (initialize-vector-from-list
                  make-uninitialized-vector
                  <vector>)
                 vector)
           (only (convert-int-char
                  convert-char-int
                  <character>)
                 character)
           ;; for <fpi>
           (only (<string-stack>
                  ?stack-string
                  ?cur-index
                  *buffer-1*
                  *buffer-2*
                  push-buffer
                  clear-buffer)
                 string-stack)
           (only (<double*>)
                 c-math))
   syntax (tail
           condition-ii)
   export (read-char
           putback-char
           read-line
           <read-error>
           read-based-int1
           read))

;;;----------------------------------------------------------------------------
;;; <read-error>
;;;----------------------------------------------------------------------------
(defcondition <read-error> ()
  ((stream type <object>
           keyword stream:
           default ()
           accessor stream)
   (error-number type <fpi>
                 keyword error-number:
                 default 77
                 accessor error-number)))

(%define-variable *sign* %signed-word-integer #%i1)

;;------------------------------------------------------------------------------
;;; Functions
;;------------------------------------------------------------------------------
(defmethod read-char ((stream <stream>))
  (%let ((unit %signed-word-integer
               (%cast %signed-word-integer
                      (if (ensure-open-character-input-stream stream)
                          (%read-unit stream)
                        (progn
                          (read-error stream #%i10)
                          (%read-unit stdin))))))
        (convert-int-char (make-fpi unit))))

(defmethod putback-char ((stream <stream>) (object <object>))
  (if (ensure-open-character-input-stream stream)
      (%unread-unit stream (make-swi (convert-char-int object)))
    (read-error stream #%i10))
  't)

(deflocal $end-of-stream-string
  (make-string (%literal %string () "END-OF-STREAM")))

(defmethod read-line ((stream <stream>))
  (if (ensure-open-character-input-stream stream)
      (let ((eos-action (stream-eos-action stream)))
        (setf-stream-eos-action stream read-line-eos-action)
        (let ((lvstring (read-line-1 stream)))
          (setf-stream-eos-action stream eos-action)
          (if (eq $end-of-stream-string lvstring)
              (progn (error <end-of-stream>
                            "End of stream during read-line"
                            stream: stream)
                     $end-of-stream-string)
            lvstring)))
    (read-error stream #%i10)))

(defun read-line-eos-action (stream)
  (make-fpi $char-eof))

(%define-function (read-line-1 <string>)
  ((stream <stream>))
  (%let ((buffer <string-stack> (%cast <string-stack> *buffer-2*))
         (ch %signed-word-integer (%read-unit stream)))
        (if (%eq ch $char-formfeed)
            (progn (push-buffer #%i0 buffer)
                   (scan-string buffer))
          (if (%eq ch $char-return)
              (progn (push-buffer #%i0 buffer)
                     (scan-string buffer))
            (if (%eq ch $char-newline)
                (progn (push-buffer #%i0 buffer)
                       (scan-string buffer))
              (if (%eq ch $char-eof)
                  (if (%eq #%i0 (?cur-index buffer))
                      $end-of-stream-string
                    (progn (push-buffer #%i0 buffer)
                           (scan-string buffer)))
                (progn (push-buffer ch  buffer)
                       (read-line-1 stream))))))))

(defun read optional-stream
  (if optional-stream
      (let ((stream (car optional-stream)))
        (if (ensure-open-character-input-stream stream)
            (read-expression stream)
          (progn ;(stream-condition-error 'read stream)
            (read-expression stdin))
          ))
    (read-expression stdin)
    ))

;; fur string streams nicht auf fd reduzieren, stream durchreichen!
;; for string streams not be reduced to fd, stream reach through!

;;;-----------------------------------------------------------------------------
;;; read-procedure self
;;;-----------------------------------------------------------------------------
(%define-function (read-expression <object>)
  ((stream <stream>))
  ;;         (print 'read-expression nl)
  ;;         (print (%cast <object> stream) nl)
  (let ((expr (read-expression1 stream)))
    (if (eq expr (%cast <object> $closed-bracket))
        (read-error stream #%i12)
      (if (eq expr (%cast <object> $point))
          (read-error stream #%i13)
        expr))))

(%define-function (read-expression1 <object>)
  ;; result: expression | $closed-bracket | $point
  ((stream <stream>))
  (setq *sign* #%i1)
  (%let ((ch %signed-word-integer
             (%and  (%peek-unit stream)
                    #%i127)))
        (setq *not-eof-action* t)
        (%let ((tok %signed-word-integer
                    (read-token #%i0 ch stream)))
              (setq *not-eof-action* ())
              (if (%eq tok #%i0)  ; #
                  (read-extension stream)
                (if (%eq tok #%i1)  ; (
                    (read-list stream)
                  (if (%eq tok #%i2)  ; )
                      (%cast <object> $closed-bracket) ; only for result type
                    (if (%eq tok #%i3)  ; <string-begin>
                        (read-string stream)
                      (read-expression1-1 tok stream))))))))

(%define-function (read-expression1-1 <object>)
  ((tok %signed-word-integer)
   (stream <stream>))
  (if (%eq tok #%i4)  ; ;
      (progn (read-comment stream) (read-expression1 stream))
    (if (%eq tok #%i5)  ; ,
        (cons 'unquotation (cons (read-expression stream) ()))
      (if (%eq tok #%i6)  ; '
          (cons 'quote (cons (read-expression stream) ()))
        (if (%eq tok #%i7) ; `
            (cons 'anti-quotation
                  (cons (read-expression stream) ()))
          (read-expression1-2 tok stream))))))

(%define-function (read-expression1-2 <object>)
  ((tok %signed-word-integer)
   (stream <stream>))
  (if (%eq tok #%i8)
      (progn
        (clear-buffer (%cast <string-stack> *buffer-1*))
        (clear-buffer (%cast <string-stack> *buffer-2*))
        (%cast <object> $point)) ;only for result type
    (if (%eq tok #%i9)
        (scan-symbol (%cast <string-stack> *buffer-1*))
      (if (%eq tok #%i10)

          (if (%eq #%i0 (?cur-index (%cast <string-stack> *buffer-2*)))
              (progn (%read-unit stream) (read-based-int stream))
            (scan-integer *sign* (%cast <string-stack> *buffer-2*)))

        (if (%eq tok #%i11)
            (scan-float *sign* (%cast <string-stack> *buffer-2*))
          (if (%eq tok #%i12)
              (scan-float *sign* (%cast <string-stack> *buffer-2*))
            (read-error stream (%minus tok #%i20)
                          ;; notwendig,
                          ;; da TI
                          ;; %pointer
                          ;; findet?!
                          )))))))

(%define-function (read-error <null>)
  ((stream <stream>)
   (err-nr %signed-word-integer))
  (error <read-error> "Read Error"
         stream: stream
         error-number: (make-fpi err-nr))
  (clear-buffer (%cast <string-stack> *buffer-1*))
  (clear-buffer (%cast <string-stack> *buffer-2*))
  ())

(%define-function (read-token %signed-word-integer)
  ((state %signed-word-integer)
   (char %signed-word-integer)
   (stream <stream>))
  ;;      (print 'read-token nl)
  ;;      (print (%cast <object> state) nl) (print (%cast <object> char) nl)
  ;;      (print (%cast <object> (char-class *char-class-token* char)) nl)
  (%let* ((state  %signed-word-integer
                  ;; noch fehler in %let mit typen darum cast notwendig:
                  (%cast %signed-word-integer
                         (half-vec-ref ;%extract   ;vector-ref
                          (half-vec-vec-ref ;%extract   ;vector-ref
                           *token-states*
                           (%cast %unsigned-word-integer state))
                          (%cast %unsigned-word-integer
                                 (char-class *char-class-token*
                                             (%cast %unsigned-word-integer char))))))
          (action %signed-word-integer (%lshiftr state #%I8))
          (next-state %signed-word-integer (%and state #%i255)))
         ;;                (print (%cast <object> state) nl)
         ;;                (print (%cast <object> action) nl)
         ;;                (print (%cast <object> next-state) nl)

         (read-token-action action char stream)
         (if (%ge next-state #%i20)
             (%minus next-state #%i20) ; result
           (read-token next-state
                       (%and (%peek-unit stream)
                             #%i127)
                       stream))))

(%define-function (read-token-action %void)
  ((action %signed-word-integer)
   (char %signed-word-integer)
   (stream <stream>))
  (if (%eq action #%i1)
      (progn (%read-unit stream)
             (push-buffer char (%cast <string-stack> *buffer-1*)))
    (if (%eq action #%i2)
        (progn (%read-unit stream)
               (push-buffer char  (%cast <string-stack> *buffer-1*))
               (setq *sign* #%i-1)) ; !!! syntax
      (if (%eq action #%i3)
          (%read-unit stream)
        (if (%eq action #%i4)
            ()
          (if (%eq action #%i5)
              (progn (%read-unit stream)
                     (push-buffer char (%cast <string-stack> *buffer-1*))
                     (push-buffer char (%cast <string-stack> *buffer-2*)))
            (if (%eq action #%i6)
                (progn (%read-unit stream)
                       (push-buffer char (%cast <string-stack> *buffer-1*))
                       (push-buffer #%i101 ; #\e
                                    (%cast <string-stack> *buffer-2*)))
              (if (%eq action #%i7)
                  (push-buffer char (%cast <string-stack> *buffer-1*))
                (char2string char)))))))) ;error or eof
  )

(%define-function (read-list <list>)
  ((stream <stream>))
  (let ((expr (read-expression1 stream)))
    (if (eq expr (%cast <object> $closed-bracket))
        ()
      (if (eq expr (%cast <object> $point))
          (read-error stream #%i13)
        (cons expr (read-list1 stream))))))

(%define-function (read-list1 <object>)
  ((stream <stream>))
  (let ((expr (read-expression1 stream)))
    (if (eq expr (%cast <object> $closed-bracket))
        ()
      (if (eq expr (%cast <object> $point))
          (let ((last-expr (read-expression1 stream)))
            (if (eq last-expr (%cast <object> $closed-bracket))
                (read-error stream #%i14)
              (if (eq last-expr (%cast <object> $point))
                  (read-error stream #%i15)
                (let ((bracket (read-expression1 stream)))
                  (if (eq bracket (%cast <object> $closed-bracket))
                      last-expr
                    (read-error stream #%i16))))))
        (cons expr (read-list1 stream))))))

(%define-function (read-comment <null>)
  ((stream <stream>))
  (%let* ((ch %signed-word-integer (%read-unit stream)))
         (if (%eq ch $char-formfeed) ()
           (if (%eq ch $char-return) ()
             (if (%eq ch $char-newline) ()
               (if (%eq ch $char-eof) ()
                 (read-comment stream))))))) ; fill into buffer?

(%define-function (read-extension <object>)
  ((stream <stream>))
  (%let ((ch %signed-word-integer (%peek-unit stream)))
        ;;              (print 'read-extension-char nl)
        ;;     (print ch nl)
        (if (%eq ch $char-open-bracket)
            (read-vector stream)
          (if (%eq ch $char-single-escape)
              (progn (%read-unit stream)
                     (read-character stream))
            (read-extended-extension stream #%i0)
            ;;           (read-based-int stream)
            ;;           (read-error stream #%i15)
            ))))

(%define-function (read-vector <object>)
  ((stream <stream>))
  (let ((lv1 (read-expression stream)))
    ;;          (print 'read-vector-result-list nl) (print lv1 nl)
    (if (cons? lv1)
        (initialize-vector-from-list
         (make-uninitialized-vector
          (%cast %unsigned-word-integer
                 ;;                      (make-swi
                 (%pair-size lv1)))   ;;)
         #%I0 lv1)
      (read-error stream #%i15))))

(%define-function (read-character <character>)
  ((stream <stream>))
  (%let ((ch %signed-word-integer (%peek-unit stream)))
        ;;(print 'read-character nl)
        ;;(print ch nl)
        (%let ((ch1 %signed-word-integer
                    (read-character-1 ch stream)))
              (convert-int-char (make-fpi ch1)))))

(%define-function (read-character-1 %signed-word-integer)
  ((ch %signed-word-integer)
   (stream <stream>))
  (if (%eq ch $char-string-hex-u)  ; X
      (progn (%read-unit stream)
             (read-char-escape-hex stream #%i0 #%i0))
    (if (%eq ch $char-string-hex-l) ; x
        (progn (%read-unit stream)
               (read-char-escape-hex stream #%i0 #%i0))
      (if (%eq ch $char-control-extension) ; ^
          (progn (%read-unit stream)
                 (%let ((ch2 %signed-word-integer
                             (%read-unit stream)))
                       ;;(print 'read-char-control nl) (print ch2 nl)
                       (if (upper? ch2)
                           (%minus ch2 #%i64) ; control-A == 1
                         (%minus ch2 #%i96)))) ; control-a == 1
        (read-character-named ch stream)))))

(%define-function (read-character-named %signed-word-integer)
  ((ch %signed-word-integer)
   (stream <stream>))
  (%let ((tok %signed-word-integer
              (read-token #%i1
                          (%and ch #%i127)
                          stream)))
        ;;(print 'read-char-name nl)
        ;;(print tok nl)
        ;;(print (%cast <string-stack> *buffer-1*) nl)
        (if (%lt tok #%i8)
            ch
          (if (%eq tok #%i8)
              (progn
                (clear-buffer (%cast <string-stack> *buffer-1*))
                (clear-buffer (%cast <string-stack> *buffer-2*))
                ch)
            (if (%eq tok #%i9)
                (convert-name-char-code stream (%cast <string-stack> *buffer-1*))
              (progn (read-error stream #%i17)
                     $char-control-extension))))))

(%define-function (convert-name-char-code %signed-word-integer)
  ((stream <stream>)
   (string-stack <string-stack>))
  (clear-buffer (%cast <string-stack> *buffer-2*))
  (%let ((string %string (?stack-string string-stack))
         (idx %signed-word-integer (?cur-index string-stack))
         ;;         (idx2 %signed-word-integer #%i0) ; removed by ak; never used
         )
        (push-buffer #%i0 string-stack)
        ;;       (print 'convert-name-char-code nl) ;(%write-string stream string) ;(print idx nl)
        (clear-buffer string-stack)
        (if (%eq idx #%i1)
            (progn ; stuss fur hf , falsch optimierung idx mehrmals verwenden,
              ;; damit nicht wegrationalisiert wird!
              ;;         (setq idx2 (%plus idx #%i3)) ; removed by ak; never used
              (%cast %signed-word-integer
                     (%extract string #%I0)))
          (progn
            (if (%eq #%i0 (strcmp string (%literal %string () "newline"))) $char-newline
              (if (%eq #%i0 (strcmp string (%literal %string () "alert"))) ; bell
                  #%i7
                (if (%eq #%i0 (strcmp string (%literal %string () "backspace"))) #%i8
                  (if (%eq #%i0 (strcmp string (%literal %string () "delete"))) #%i127
                    (if (%eq #%i0 (strcmp string (%literal %string () "formfeed"))) $char-formfeed
                      (convert-name-char-code-1 string stream))))))))))

(%define-function (convert-name-char-code-1 %signed-word-integer)
  ((string %string)
   (stream <stream>))
  (if (%eq #%i0 (strcmp string (%literal %string () "linefeed"))) $char-newline
    (if (%eq #%i0 (strcmp string (%literal %string () "return"))) $char-return
      (if (%eq #%i0 (strcmp string (%literal %string () "tab"))) #%i9
        (if (%eq #%i0 (strcmp string (%literal %string () "space"))) #%i32
          (if (%eq #%i0 (strcmp string (%literal %string () "vertical-tab"))) #%i11
            (progn (read-error stream #%i17) $char-control-extension)))))))

(%define-function (read-char-escape-hex %signed-word-integer)
  ((stream <stream>)
   (ch-code-res %signed-word-integer)
   (counter %signed-word-integer))
  (if (%eq counter #%i4)
      ch-code-res
    (%let* ((ch %signed-word-integer (%peek-unit stream)))
           (if (hex-digit? ch)
               (progn (%read-unit stream)
                      (read-char-escape-hex stream
                                            (%plus (digit2figure16 ch)
                                                   (%mult #%i16 ch-code-res))
                                            (%plus counter #%i1)))
             ch-code-res))))

(%define-function (read-string <string>)
  ((stream <stream>))
  (%let* ((ch %signed-word-integer (%read-unit stream)))
         (if (%eq ch $char-string)
             (progn (push-buffer #%i0 (%cast <string-stack> *buffer-2*))
                    (scan-string (%cast <string-stack> *buffer-2*)))
           (if (%eq ch $char-single-escape)
               (read-string-escape stream)
             (if (%eq ch $char-eof)
                 (progn (read-error stream #%i13)
                        (push-buffer #%i0 (%cast <string-stack> *buffer-2*))
                        (scan-string (%cast <string-stack> *buffer-2*)))
               (progn (push-buffer ch (%cast <string-stack> *buffer-2*))
                      (read-string stream))
               )))))

(%define-function (read-string-escape <string>)
  ((stream <stream>))
  (%let ((ch %signed-word-integer (%read-unit stream)))
        (if (%eq ch $char-string)
            (progn (push-buffer ch (%cast <string-stack> *buffer-2*))
                   (read-string stream))
          (if (%eq ch $char-single-escape)
              (progn (push-buffer ch (%cast <string-stack> *buffer-2*))
                     (read-string stream))
            (if ;(or (%eq ch #%i78)   ; N ;    (%eq ch #%I110)) ; n
                ;; %eq and all other %-comparisons are not valid; as a function expression
                (%eq ch #%i78)  ; N
                (progn (push-buffer $char-newline (%cast <string-stack> *buffer-2*))
                       (read-string stream))
              (if (%eq ch #%i110)  ; n
                  (progn (push-buffer $char-newline (%cast <string-stack> *buffer-2*))
                         (read-string stream))
                (read-string-escape-1 ch stream)))))))

(%define-function (read-string-escape-1 <string>)
  ((ch %signed-word-integer)
   (stream <stream>))
  (if (%eq ch #%i97)  ; a alert
      (progn (push-buffer #%i7 (%cast <string-stack> *buffer-2*))
             (read-string stream))
    (if (%eq ch #%i98)  ; b backspace
        (progn (push-buffer #%i8 (%cast <string-stack> *buffer-2*))
               (read-string stream))
      (if (%eq ch #%i100)  ; d delete
          (progn (push-buffer #%i127 (%cast <string-stack> *buffer-2*))
                 (read-string stream))
        (if (%eq ch #%i102)  ; f formfeed
            (progn (push-buffer $char-formfeed (%cast <string-stack> *buffer-2*))
                   (read-string stream))
          (read-string-escape-2 ch stream))))))

(%define-function (read-string-escape-2 <string>)
  ((ch %signed-word-integer)
   (stream <stream>))
  (if (%eq ch #%i108)  ; l linefeed
      (progn (push-buffer $char-newline (%cast <string-stack> *buffer-2*))
             (read-string stream))
    (if (%eq ch #%i114)  ; r return
        (progn (push-buffer $char-return (%cast <string-stack> *buffer-2*))
               (read-string stream))
      (if (%eq ch #%i116)  ; t tab
          (progn (push-buffer #%i9 (%cast <string-stack> *buffer-2*))
                 (read-string stream))
        (if (%eq ch #%i118)  ; v vertical-tab
            (progn (push-buffer #%i11 (%cast <string-stack> *buffer-2*))
                   (read-string stream))
          (if (%eq ch $char-string-hex-u)  ; X
              (read-string-escape-hex stream #%i0 #%i0)
            (if (%eq ch $char-string-hex-l) ; x
                (read-string-escape-hex stream #%i0 #%i0)
              ;; (if (%eq ch $char-eof) or other chars
              (progn (read-error stream #%i13)
                     (push-buffer $char-newline (%cast <string-stack> *buffer-2*))
                     (read-string stream)) )))))))

(%define-function (read-string-escape-hex <string>)
  ((stream <stream>)
   (ch-code-res %signed-word-integer)
   (counter %signed-word-integer))
  ;;(print 'read-string-escape-hex nl)
  ;;(print ch-code-res nl)
  ;;(print counter nl)
  (if (%eq counter #%i4)
      (progn (push-buffer ch-code-res (%cast <string-stack> *buffer-2*))
             (read-string stream))
    (%let* ((ch %signed-word-integer (%peek-unit stream)))
           (if (hex-digit? ch)
               (progn (%read-unit stream)
                      (read-string-escape-hex stream
                                              (%plus (digit2figure16 ch)
                                                     (%mult #%i16 ch-code-res))
                                              (%plus counter #%i1)))
             (progn (push-buffer ch-code-res (%cast <string-stack> *buffer-2*))
                    (read-string stream))))))

(%define-function (hex-digit? <object>)
  ((ch %signed-word-integer))
  (if (%lt #%i47 ch)
      (if (%gt #%i58 ch)   ; 0-9
          'true
        (if (%lt #%i64 ch)
            (if (%gt #%i71 ch)   ; A-F
                'true
              (if (%lt #%i96 ch)
                  (if (%gt #%i103 ch) ; a-f
                      'true ()) ())) ())) ()))

(%define-function (scan-symbol <symbol>)
  ((string-stack <string-stack>))
  (push-buffer #%i0 string-stack)
  (let ((lvsym (make-symbol
                (make-string
                 (?stack-string string-stack)
                 ))))
    ;; strdup in make-symbol
    (clear-buffer (%cast <string-stack> *buffer-1*))
    (clear-buffer (%cast <string-stack> *buffer-2*))
    lvsym)
  )

(%define-function (scan-string <string>)
  ((string-stack <string-stack>))
  (let ((lvsym (make-string
                (duplicate-%string (?stack-string string-stack)))))
    (clear-buffer (%cast <string-stack> *buffer-1*))
    (clear-buffer (%cast <string-stack> *buffer-2*))
    lvsym))

(%define-function (scan-integer <integer>)
  ((sign %signed-word-integer)
   (string-stack <string-stack>))
  (make-fpi (%mult sign
                     (scan-integer1 string-stack
                                    #%i0 #%i0
                                    (?cur-index string-stack)))))

(%define-function (scan-integer1 %signed-word-integer)
  ((string-stack <string-stack>)
   (res %signed-word-integer)
   (ci %signed-word-integer)
   (li %signed-word-integer))
  (%let ((char %signed-word-integer
               ;; siehe oben %let und typen
               (%cast %signed-word-integer
                      (%extract (?stack-string string-stack) ci))))
        (if (%eq ci li)
            (progn (clear-buffer string-stack)
                   (clear-buffer (%cast <string-stack> *buffer-1*))
                   res)
          (scan-integer1 string-stack
                         (%plus (digit2figure10 char)
                                (%mult #%i10 res))
                         (%plus #%i1 ci)
                         li))))

(%define-function (read-extended-extension <object>)
  ((stream <stream>)
   (ifix %signed-word-integer))
  (%let ((ch %signed-word-integer (%peek-unit stream)))
        (if (%eq (char-class *char-class-token*
                             (%cast %unsigned-word-integer ch))
                 #%b2)   ; dec digit
            (progn (%read-unit stream)
                   (read-extended-extension stream
                                            (%plus (digit2figure10 ch)
                                                   (%mult ifix #%i10))))
          (%let ((ch2 %signed-word-integer (%read-unit stream)))
                (if (%eq ch2 $char-string-hex-l)
                    (progn (read-based-int1 stream #%i16 #%i0))
                  (if (%eq ch2 $char-string-hex-u)
                      (progn (read-based-int1 stream #%i16 #%i0))
                    (if (%eq ch2 #%i111)   ; o
                        (progn (read-based-int1 stream #%i8 #%i0))
                      (if (%eq ch2 #%i79)   ; O
                          (progn (read-based-int1 stream #%i8 #%i0))
                        (if (%eq ch2 #%i98)   ; b
                            (progn (read-based-int1 stream #%i2 #%i0))
                          (if (%eq ch2 #%i66)   ; B
                              (progn (read-based-int1 stream #%i2 #%i0))
                            (if (%eq ch2 #%i114)   ; r
                                (progn (read-based-int2 stream ifix))
                              (if (%eq ch2 #%i82)  ; R
                                  (progn (read-based-int2 stream ifix))
                                (let* ((subchar (convert-int-char
                                                 (make-fpi ch2)))
                                       (function (get-dispatch-macro-character
                                                  #\# subchar)))
                                  (if (null? function)
                                      (read-error stream #%i20)
                                    (function stream subchar)))))))))))))))

(%define-function (read-based-int <integer>)
  ((stream <stream>))
  (let ((obj (read-extended-extension stream #%i0)))
    (if (fpi? obj)
        obj
      (progn
        (read-error stream #%i20)
        0))))

(%define-function (read-based-int2 <integer>)
  ((stream <stream>)
   (base %signed-word-integer))
  (if (%lt base #%i37)
      (read-based-int1 stream base #%i0)
    (progn (read-error stream #%i20) 0)))

(%define-function (read-based-int1 <integer>)
  ((stream <stream>)
   (base %signed-word-integer)
   (res %signed-word-integer))
  (%let ((ch %signed-word-integer (%peek-unit stream)))
        (if (based-digit? ch base)
            (progn (%read-unit stream)
                   (read-based-int1 stream base (%plus (%mult base res)
                                                       (digit2figure16 ch))))
          (make-fpi (%mult *sign* res)))))

(%define-function (based-digit? <object>)
  ((ch %signed-word-integer)
   (base %signed-word-integer))
  (if (%lt ch #%i48) ; 0
      ()
    (if (%lt ch #%i57) ; 9
        (if (%lt (%minus ch #%i48) base)
            'true
          ())
      (if (%lt ch #%i65) ;A
          ()
        (if (%lt ch #%i91) ;Z
            (if (%lt (%minus ch #%i55) base)
                'true
              ())
          (if (%lt ch #%i97) ;a
              ()
            (if (%lt ch #%i123) ;z
                (if (%lt (%minus ch #%i88) base)
                    'true
                  ())
              ())))))))

;;    (deflocal float-pointer (%literal <vector> #%I2 (#%I1 #%I2)))
;;    (deflocal float-pointer (%literal <vector> 2 (#%I1 #%I2)))
;;    (deflocal float-pointer (make-double 1))

(%define-function (scan-float <double-float>)
  ((sign %signed-word-integer)
   (string-stack <string-stack>))
  (push-buffer #%i0 string-stack)
  (%let ((float-pointer <double-float>
                        (%cast <double-float>
                               ;; ? ? ? z.Z. fehler beim ausfassen alignment
                               (make-dble (%cast %double-float
                                                 0)))
                        ))
        (sscanf-3-double (?stack-string string-stack)
                  (%literal %string () "%le") (%cast <double*> float-pointer))
        (clear-buffer (%cast <string-stack> *buffer-1*))
        (clear-buffer (%cast <string-stack> *buffer-2*))
        (set-dble float-pointer (%mult (%cast %double-float sign)
                                       (dble float-pointer)))
        float-pointer))

(%define-function (char2string %void) ;eof
  ((char %signed-word-integer))
  (push-buffer  char (%cast <string-stack> *buffer-1*))
  ())

;;   (defun peek-unit o-stream
;;     (let ((unit
;;            (if o-stream
;;              (if (ensure-open-character-input-stream (car o-stream))
;;                (%peek-unit (car o-stream))
;;               (read-error stream #%i10))
;;            (%peek-unit stdin))))
;;       (convert-int-char (make-fpi unit))))

;;   (defun read-unit o-stream
;;     (let ((unit
;;            (if o-stream
;;              (if (ensure-open-character-input-stream (car o-stream))
;;                (%read-unit (car o-stream))
;;               read-error stream #%i10))
;;               (%read-unit stdin))))
;;       (convert-int-char (make-fpi unit))))

;;;-----------------------------------------------------------------------------
;;; Type schemes for type inference
;;;-----------------------------------------------------------------------------

(%annotate-function read-char new-signature
  (((var0 var1)
    ((var var0) (atom? <object>))
    ((var var1) (atom? <stream>)))))

(%annotate-function read new-signature
  (((var0 var1)
    ((var var0) (atom? <object>))
    ((var var1) (atom? <list>)))))

;;;-----------------------------------------------------------------------------
)  ;; End of module read
;;;-----------------------------------------------------------------------------
