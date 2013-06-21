;;; -*- Mode: Common-Lisp; Syntax: Common-Lisp; Package: LINJ; Base: 10 -*-

;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; Functions to implement FORMAT and FORMATTER for CMU Common Lisp.
;;;
;;; Written by William Lott, with lots of stuff stolen from the previous
;;; version by David Adam and later rewritten by Bill Maddox.
;;;

(in-package "LINJ")

;;This file requires the Linj readtable
(eval-when (:compile-toplevel :load-toplevel)
  (setq *readtable* *linj-readtable*))


;;This is the stream "variable" to be used in code generation

(defvar *stream*)

(defvar *result-type*)

(defun emit-format-code (type fmt stream full-args)
  (let ((*stream* stream)
        (*result-type* type))
    (%formatter fmt full-args)))

(defun gen-write-char (form)
  (ecase *result-type*
    (:expression (if (characterp form) form `(real-the char ,form)))
    (:statement `(write-char ,form ,*stream*))
    (:string-buffer `(send ,*stream* append ,form))))

(defun gen-write-string (form)
  (ecase *result-type*
    (:expression (if (stringp form) form `(real-the java.lang.string ,form)))
    (:statement `(write-string ,form ,*stream*))
    (:string-buffer `(send ,*stream* append ,form))))

(defun gen-prin1 (form)
  (ecase *result-type*
    (:expression form)
    (:statement `(prin1 ,form ,*stream*))
    (:string-buffer `(send ,*stream* append ,form))))

(defun gen-princ (form)
  (ecase *result-type*
    (:expression form)
    (:statement `(princ ,form ,*stream*))
    (:string-buffer `(send ,*stream* append ,form))))

(defun gen-terpri ()
  (ecase *result-type*
    (:expression '#\newline)
    (:statement `(terpri ,*stream*))
    (:string-buffer `(send ,*stream* append #\newline))))

(defun gen-fresh-line ()
  (ecase *result-type*
    (:expression '#\newline)
    (:statement `(fresh-line ,*stream*))
    (:string-buffer `(send ,*stream* append #\newline))))

(defun gen-progn (forms)
  (ecase *result-type*
    (:expression
     `(concat ,@forms))
    ((:statement :string-buffer)
     (if (and (not (endp forms)) (endp (rest forms)))
         (first forms)
         `(progn ,@forms)))))

(defun gen-when (test forms)
  (ecase *result-type*
    (:expression
     `(if ,test ,(gen-progn forms) ""))
    ((:statement :string-buffer)
     `(when ,test
        ,@forms))))


(defstruct (format-directive
            (:print-function %print-format-directive))
  (string (required-argument) :type simple-string)
  (start (required-argument) :type (and unsigned-byte fixnum))
  (end (required-argument) :type (and unsigned-byte fixnum))
  (character (required-argument) :type base-char)
  (colonp nil :type (member t nil))
  (atsignp nil :type (member t nil))
  (params nil :type list))

(defun required-argument ()
  (error "Missing argument"))

(defun %print-format-directive (struct stream depth)
  (declare (ignore depth))
  (print-unreadable-object (struct stream)
    (write-string (format-directive-string struct) stream
                  :start (format-directive-start struct)
                  :end (format-directive-end struct))))

(defvar *format-directive-expanders*
  (make-array char-code-limit :initial-element nil))
(defvar *format-directive-interpreters*
  (make-array char-code-limit :initial-element nil))

(defun %print-format-error (condition stream)
  (cl:format stream
             "~:[~;Error in format: ~]~
              ~?~@[~%  ~A~%  ~V@T^~]"
             (format-error-print-banner condition)
             (format-error-complaint condition)
             (format-error-arguments condition)
             (format-error-control-string condition)
             (format-error-offset condition)))

(defvar *default-format-error-control-string* nil)
(defvar *default-format-error-offset* nil)

(define-condition format-error (error)
  ((complaint :reader format-error-complaint :initarg :complaint)
   (arguments :reader format-error-arguments :initarg :arguments :initform nil)
   (control-string :reader format-error-control-string
                   :initarg :control-string
                   :initform *default-format-error-control-string*)
   (offset :reader format-error-offset :initarg :offset
           :initform *default-format-error-offset*)
   (print-banner :reader format-error-print-banner :initarg :print-banner
                 :initform t))
  (:report %print-format-error))

;;;; TOKENIZE-CONTROL-STRING

(defun tokenize-control-string (string)
  (declare (simple-string string))
  (let ((index 0)
        (end (length string))
        (result nil))
    (loop
      (let ((next-directive (or (position #\~ string :start index) end)))
        (when (> next-directive index)
          (push (subseq string index next-directive) result))
        (when (= next-directive end)
          (return))
        (let ((directive (parse-directive string next-directive)))
          (push directive result)
          (setf index (format-directive-end directive)))))
    (nreverse result)))

(defun parse-directive (string start)
  (let ((posn (1+ start)) (params nil) (colonp nil) (atsignp nil)
        (end (length string)))
    (flet ((get-char ()
             (if (= posn end)
                 (error 'format-error
                        :complaint "String ended before directive was found."
                        :control-string string
                        :offset start)
                 (schar string posn))))
      (loop
        (let ((char (get-char)))
          (cond ((or (char<= #\0 char #\9) (char= char #\+) (char= char #\-))
                 (multiple-value-bind
                       (param new-posn)
                     (parse-integer string :start posn :junk-allowed t)
                   (push (cons posn param) params)
                   (setf posn new-posn)
                   (case (get-char)
                     (#\,)
                     ((#\: #\@)
                      (decf posn))
                     (t
                      (return)))))
                ((or (char= char #\v) (char= char #\V))
                 (push (cons posn :arg) params)
                 (incf posn)
                 (case (get-char)
                   (#\,)
                   ((#\: #\@)
                    (decf posn))
                   (t
                    (return))))
                ((char= char #\#)
                 (push (cons posn :remaining) params)
                 (incf posn)
                 (case (get-char)
                   (#\,)
                   ((#\: #\@)
                    (decf posn))
                   (t
                    (return))))
                ((char= char #\')
                 (incf posn)
                 (push (cons posn (get-char)) params)
                 (incf posn)
                 (unless (char= (get-char) #\,)
                   (decf posn)))
                ((char= char #\,)
                 (push (cons posn nil) params))
                ((char= char #\:)
                 (if colonp
                     (error 'format-error
                            :complaint "Too many colons supplied."
                            :control-string string
                            :offset posn)
                     (setf colonp t)))
                ((char= char #\@)
                 (if atsignp
                     (error 'format-error
                            :complaint "Too many at-signs supplied."
                            :control-string string
                            :offset posn)
                     (setf atsignp t)))
                (t
                 (when (char= (schar string (1- posn)) #\,)
                   (push (cons (1- posn) nil) params))
                 (return))))
        (incf posn))
      (let ((char (get-char)))
        (when (char= char #\/)
          (let ((closing-slash (position #\/ string :start (1+ posn))))
            (if closing-slash
                (setf posn closing-slash)
                (error 'format-error
                       :complaint "No matching closing slash."
                       :control-string string
                       :offset posn))))
        (make-format-directive
         :string string :start start :end (1+ posn)
         :character (char-upcase char)
         :colonp colonp :atsignp atsignp
         :params (nreverse params))))))


;;;; Specials used to communicate information.

;;; *UP-UP-AND-OUT-ALLOWED* -- internal.
;;;
;;; Used both by the expansion stuff and the interpreter stuff.  When it is
;;; non-NIL, up-up-and-out (~:^) is allowed.  Otherwise, ~:^ isn't allowed.
;;;
(defvar *up-up-and-out-allowed* nil)

;;; *LOGICAL-BLOCK-POPPER* -- internal.
;;;
;;; Used by the interpreter stuff.  When it non-NIL, its a function that will
;;; invoke PPRINT-POP in the right lexical environemnt.
;;;
(defvar *logical-block-popper* nil)

;;; *ONLY-SIMPLE-ARGS* -- internal.
;;;
;;; Used by the expander stuff.  Initially starts as T, and gets set to NIL
;;; if someone needs to do something strange with the arg list (like use
;;; the rest, or something).
;;;
(defvar *only-simple-args*)

;;; *ORIG-ARGS-AVAILABLE* -- internal.
;;;
;;; Used by the expander stuff.  We do an initial pass with this as NIL.
;;; If someone doesn't like this, they (throw 'need-orig-args nil) and we try
;;; again with it bound to T.  If this is T, we don't try to do anything
;;; fancy with args.
;;;
(defvar *orig-args-available* nil)

;;; *SIMPLE-ARGS* -- internal.
;;;
;;; Used by the expander stuff.  List of (symbol . offset) for simple args.
;;;
(defvar *simple-args*)

(defvar *orig-args*)
(defvar *args*)
(defvar *no-value* (list 'progn))


;;;; FORMATTER

(defun %formatter (control-string args)
  (let* ((*simple-args* nil)
         (*only-simple-args* t)
         (*orig-args* args)
         (*args* args))
    (expand-control-string control-string)))


(defun expand-control-string (string)
  (let* ((string (etypecase string
                   (simple-string
                    string)
                   (string
                    (coerce string 'simple-string))))
         (*default-format-error-control-string* string)
         (directives (tokenize-control-string string)))
    (ecase *result-type*
      ((:statement :string-buffer)
       `(block nil
          ,@(expand-directive-list directives)))
      (:expression
       `(concat
         ,@(remove '(concat) (expand-directive-list directives) :test #'equal)))))) ;;small optimization

(defun expand-directive-list (directives)
  (let ((results nil)
        (remaining-directives directives))
    (loop
      (unless remaining-directives
        (return))
      (multiple-value-bind (form new-directives)
          (expand-directive (car remaining-directives)
                            (cdr remaining-directives))
        (unless (equal form '(progn))
          (setq results (combine-format-form form results)))
        (setf remaining-directives new-directives)))
    (reverse results)))

(defun combine-format-form (form forms)
  (if (let-pattern (((write-string (?is ?obj (lambda (obj) (and (stringp obj) (string= obj "")))) ?ignore) form)) t)
      forms
      (or
       (let-pattern (((write-string (?is ?string1 stringp) ?stream) form)
                     (((write-string (?is ?string2 stringp) ?ignore) . ?ignore) forms))
         `((write-string ,(concatenate 'string ?string2 ?string1) ,?stream) ,@(rest forms)))
       (let-pattern (((terpri ?stream) form)
                     (((write-string ?string2 ?ignore) . ?ignore) forms))
         `((write-line ,?string2 ,?stream) ,@(rest forms)))
       (let-pattern (((terpri ?stream) form)
                     (((princ ?obj ?ignore) . ?ignore) forms))
         `((println ,?obj ,?stream) ,@(rest forms)))
       (cons form forms))))

(defun expand-directive (directive more-directives)
  (etypecase directive
    (format-directive
     (let ((expander
             (aref *format-directive-expanders*
                   (char-code (format-directive-character directive))))
           (*default-format-error-offset*
             (1- (format-directive-end directive))))
       (if expander
           (funcall expander directive more-directives)
           (error 'format-error
                  :complaint "Unknown directive."))))
    (simple-string
     (values (gen-write-string directive)
             more-directives))))

(defun protect (form)
  (if (or *orig-args-available* (not *only-simple-args*))
      `(if (endp args)
           (error "No more arguments.")
           (let ((arg (first args)))
             (setq args (rest args))
             ,form))
      form))

(defun expand-next-arg (&optional offset)
  (if (or *orig-args-available* (not *only-simple-args*))
      'arg
      (if (endp *args*)
          (error 'format-error
                 :complaint "No more arguments."
                 :control-string *default-format-error-control-string*
                 :offset (or offset *default-format-error-offset*))
          (pop *args*))))


;;;; Format directive definition macros and runtime support.

(defmacro expander-next-arg (string offset)
  `(if args
       (pop args)
       (error 'format-error
              :complaint "No more arguments."
              :control-string ,string
              :offset ,offset)))

(defmacro expander-pprint-next-arg (string offset)
  `(progn
     (when (null args)
       (error 'format-error
              :complaint "No more arguments."
              :control-string ,string
              :offset ,offset))
     (pprint-pop)
     (pop args)))

(eval-when (:compile-toplevel :execute)

  (defmacro def-complex-format-directive (char lambda-list &body body)
    (let ((defun-name (intern (cl:format nil
                                         "~:@(~:C~)-FORMAT-DIRECTIVE-EXPANDER"
                                         char)))
          (directive (gensym))
          (directives (if lambda-list (car (last lambda-list)) (gensym))))
      `(progn
         (defun ,defun-name (,directive ,directives)
           ,@(if lambda-list
                 `((let ,(mapcar #'(lambda (var)
                                     `(,var
                                       (,(intern (concatenate
                                                  'string
                                                  "FORMAT-DIRECTIVE-"
                                                  (symbol-name var))
                                                 (symbol-package 'foo))
                                        ,directive)))
                                 (butlast lambda-list))
                     ,@body))
                 `((declare (ignore ,directive ,directives))
                   ,@body)))
         (%set-format-directive-expander ,char #',defun-name))))

  (defmacro def-format-directive (char lambda-list &body body)
    (let ((directives (gensym))
          (declarations nil)
          (body-without-decls body))
      (loop
        (let ((form (car body-without-decls)))
          (unless (and (consp form) (eq (car form) 'declare))
            (return))
          (push (pop body-without-decls) declarations)))
      (setf declarations (reverse declarations))
      `(def-complex-format-directive ,char (,@lambda-list ,directives)
         ,@declarations
         (values (progn ,@body-without-decls)
                 ,directives))))

  (defmacro once-only (specs &body body)
    (labels ((frob (specs body)
               (if (null specs)
                   `(progn ,@body)
                   (let ((spec (first specs)))
                     (when (/= (length spec) 2)
                       (error "Malformed Once-Only binding spec: ~S." spec))
                     (let ((name (first spec))
                           (exp-temp (gensym)))
                       `(let ((,exp-temp ,(second spec))
                              (,name (gensym "OO-")))
                          `(let ((,,name ,,exp-temp))
                             ,,(frob (rest specs) body))))))))
      (frob specs body)))

  ;; (defmacro expand-bind-defaults (specs params &body body)
  ;;   (once-only ((params params))
  ;;     (if specs
  ;;    (collect ((expander-bindings) (runtime-bindings))
  ;;             (dolist (spec specs)
  ;;               (destructuring-bind (var default) spec
  ;;                 (let ((symbol (gensym)))
  ;;                   (expander-bindings
  ;;                    `(,var ',symbol))
  ;;                   (runtime-bindings
  ;;                    `(list ',symbol
  ;;                           (let* ((param-and-offset (pop ,params))
  ;;                                  (offset (car param-and-offset))
  ;;                                  (param (cdr param-and-offset)))
  ;;                             (case param
  ;;                               (:arg `(or ,(expand-next-arg offset)
  ;;                                          ,,default))
  ;;                               (:remaining
  ;;                                (progn (setf *only-simple-args* nil) (error "BUM"))
  ;;                                '(length args))
  ;;                               ((nil) ,default)
  ;;                               (t param))))))))
  ;;             `(let ,(expander-bindings)
  ;;                `(let ,(list ,@(runtime-bindings))
  ;;                   ,@(if ,params
  ;;                         (error 'format-error
  ;;                                :complaint
  ;;                        "Too many parameters, expected no more than ~D"
  ;;                                :arguments (list ,(length specs))
  ;;                                :offset (caar ,params)))
  ;;                   ,,@body)))
  ;;    `(progn
  ;;       (when ,params
  ;;         (error 'format-error
  ;;                :complaint "Too many parameters, expected no more than 0"
  ;;                :offset (caar ,params)))
  ;;       ,@body))))

  (defmacro expand-bind-defaults (specs params &body body)
    (once-only ((params params))
      (if specs
          `(let ,(mapcar #'(lambda (spec)
                             (destructuring-bind (var default) spec
                               `(,var
                                 (let* ((param-and-offset (pop ,params))
                                        (offset (car param-and-offset))
                                        (param (cdr param-and-offset)))
                                   (case param
                                     (:arg (let ((arg (expand-next-arg offset)))
                                             (if (false-literal-p arg) ;;nil arg
                                                 ,default
                                                 arg)))
                                     (:remaining
                                      (if *only-simple-args*
                                          (length *args*)
                                          '(length args)))
                                     ;;                                  (:remaining
                                     ;;                                   (length *args*))
                                     ((nil) ,default)
                                     (t param))))))
                  specs)
             (unless (endp ,params)
               (error 'format-error
                      :complaint "Too many parameters, expected no more than ~D"
                      :arguments (list ,(length specs))
                      :offset (caar ,params)))
             ,@body)
          `(progn
             (when ,params
               (error 'format-error
                      :complaint "Too many parameters, expected no more than 0"
                      :offset (caar ,params)))
             ,@body))))

  ); eval-when

(defun %set-format-directive-expander (char fn)
  (setf (aref *format-directive-expanders* (char-code (char-upcase char))) fn)
  char)

(defun find-directive (directives kind stop-at-semi)
  (if directives
      (let ((next (car directives)))
        (if (format-directive-p next)
            (let ((char (format-directive-character next)))
              (if (or (char= kind char)
                      (and stop-at-semi (char= char #\;)))
                  (car directives)
                  (find-directive
                   (cdr (flet ((after (char)
                                 (member (find-directive (cdr directives)
                                                         char
                                                         nil)
                                         directives)))
                          (case char
                            (#\( (after #\)))
                            (#\< (after #\>))
                            (#\[ (after #\]))
                            (#\{ (after #\}))
                            (t directives))))
                   kind stop-at-semi)))
            (find-directive (cdr directives) kind stop-at-semi)))))

(defun format-princ (stream arg colonp atsignp mincol colinc minpad padchar)
  (format-write-field stream
                      (if (or arg (not colonp))
                          (princ-to-string arg)
                          "()")
                      mincol colinc minpad padchar atsignp))

(def-format-directive #\A (colonp atsignp params)
  (protect
   (if colonp
       (error "Linj separates NIL and ().  A consequence is that it doesn't make sense to use ~~:A as ~~A will never print NIL for an empty list")
       (if params
           (expand-bind-defaults
               ((mincol 0) (colinc 1) (minpad 0) (padchar #\space)) params
             (gen-princ `(format-write-field-to-string
                          ,(expand-next-arg)
                          ,mincol ,colinc ,minpad ,padchar ,atsignp)))
           (gen-princ (expand-next-arg))))))


(defun format-prin1 (stream arg colonp atsignp mincol colinc minpad padchar)
  (format-write-field stream
                      (if (or arg (not colonp))
                          (prin1-to-string arg)
                          "()")
                      mincol colinc minpad padchar atsignp))

(def-format-directive #\S (colonp atsignp params)
  (protect
   (cond (params
          (expand-bind-defaults
              ((mincol 0) (colinc 1) (minpad 0) (padchar #\space)) params
            `(format-prin1 ,*stream* ,(expand-next-arg) ,colonp ,atsignp
                           ,mincol ,colinc ,minpad ,padchar)))
         (colonp
          `(let ((arg ,(expand-next-arg)))
             (if arg
                 (prin1 arg)
                 (princ "()"))))
         (t
          (gen-prin1 (expand-next-arg))))))


(def-format-directive #\C (colonp atsignp params)
  (protect
   (expand-bind-defaults
       () params
     (if colonp
         `(format-print-named-character ,(expand-next-arg) ,*stream*)
         (if atsignp
             (gen-prin1 (expand-next-arg))
             (gen-write-char (expand-next-arg)))))))


(def-format-directive #\W (colonp atsignp params)
  (protect
   (expand-bind-defaults
       () params
     (if (or colonp atsignp)
         `(let (,@(when colonp
                    '((*print-pretty* t)))
                ,@(when atsignp
                    '((*print-level* nil)
                      (*print-length* nil))))
            (output-object ,(expand-next-arg) ,*stream*))
         `(output-object ,(expand-next-arg) ,*stream*)))))


;;;; Integer outputting.

;;; FORMAT-PRINT-NUMBER does most of the work for the numeric printing
;;; directives.  The parameters are interpreted as defined for ~D.
;;;
;; (defun format-print-integer (stream number print-commas-p print-sign-p
;;                           radix mincol padchar commachar commainterval)
;;   (let ((*print-base* radix)
;;      (*print-radix* nil))
;;     (if (integerp number)
;;      (let* ((text (princ-to-string (abs number)))
;;             (commaed (if print-commas-p
;;                          (format-add-commas text commachar commainterval)
;;                          text))
;;             (signed (cond ((minusp number)
;;                            (concatenate 'string "-" commaed))
;;                           (print-sign-p
;;                            (concatenate 'string "+" commaed))
;;                           (t commaed))))
;;        ;; colinc = 1, minpad = 0, padleft = t
;;        (format-write-field stream signed mincol 1 0 padchar t))
;;      (princ number stream))))

;; (defun format-add-commas (string commachar commainterval)
;;   (let ((length (length string)))
;;     (multiple-value-bind (commas extra)
;;                       (truncate (1- length) commainterval)
;;       (let ((new-string (make-string (+ length commas)))
;;          (first-comma (1+ extra)))
;;      (replace new-string string :end1 first-comma :end2 first-comma)
;;      (do ((src first-comma (+ src commainterval))
;;           (dst first-comma (+ dst commainterval 1)))
;;          ((= src length))
;;        (setf (schar new-string dst) commachar)
;;        (replace new-string string :start1 (1+ dst)
;;                 :start2 src :end2 (+ src commainterval)))
;;      new-string))))

(defun expand-format-integer (base colonp atsignp params)
  (protect
   (if (or colonp atsignp params)
       (expand-bind-defaults
           ((mincol 0) (padchar #\space) (commachar #\,) (commainterval 3))
           params
         (gen-princ `(format-print-integer-to-string
                      ,(expand-next-arg)
                      ,colonp ,atsignp ,base ,mincol ,padchar ,commachar ,commainterval)))
       (gen-princ `(to-base ,(expand-next-arg) ,base)))))

(def-format-directive #\D (colonp atsignp params)
  (expand-format-integer 10 colonp atsignp params))

(def-format-directive #\B (colonp atsignp params)
  (expand-format-integer 2 colonp atsignp params))

(def-format-directive #\O (colonp atsignp params)
  (expand-format-integer 8 colonp atsignp params))

(def-format-directive #\X (colonp atsignp params)
  (expand-format-integer 16 colonp atsignp params))

(def-format-directive #\R (colonp atsignp params)
  (protect
   (if params
       (expand-bind-defaults
           ((base 10) (mincol 0) (padchar #\space) (commachar #\,)
            (commainterval 3))
           params
         (gen-princ `(format-print-integer-to-string
                      ,(expand-next-arg)
                      ,colonp ,atsignp ,base ,mincol ,padchar ,commachar ,commainterval)))
       (if atsignp
           (if colonp
               (gen-princ `(format-print-old-roman-to-string ,(expand-next-arg)))
               (gen-princ `(format-print-roman-to-string ,(expand-next-arg))))
           (if colonp
               (gen-princ `(format-print-ordinal-to-string ,(expand-next-arg)))
               (gen-princ `(format-print-cardinal-to-string ,(expand-next-arg))))))))


;;;; Plural.

(def-format-directive #\P (colonp atsignp params end)
  (expand-bind-defaults () params
    (let ((arg (cond
                 ((not colonp)
                  (expand-next-arg))
                 (*orig-args-available*
                  `(if (eq orig-args args)
                       (error 'format-error
                              :complaint "No previous argument."
                              :offset ,(1- end))
                       (do ((arg-ptr orig-args (cdr arg-ptr)))
                           ((eq (cdr arg-ptr) args)
                            (car arg-ptr)))))
                 (t
                  (if (eq *orig-args* *args*)
                      (error 'format-error
                             :complaint "No previous argument."
                             :offset (1- end))
                      (do ((arg-ptr *orig-args* (cdr arg-ptr)))
                          ((eq (cdr arg-ptr) *args*)
                           (car arg-ptr))))))))
      (let ((form (if atsignp
                      (gen-write-string `(if (eql ,arg 1) "y" "ies"))
                      (ecase *result-type*
                        (:expression `(if (eql ,arg 1) "s" ""))
                        (:statement `(unless (eql ,arg 1) (write-char #\s ,*stream*)))
                        (:string-buffer `(unless (eql ,arg 1) (send ,*stream* append #\s)))))))
        (if (not colonp)
            (protect form)
            form)))))


;;;; Floating point noise.

(defun decimal-string (n)
  (write-to-string n :base 10 :radix nil :escape nil))

;; (def-format-directive #\F (colonp atsignp params)
;;   (when colonp
;;     (error 'format-error
;;         :complaint
;;         "Cannot specify the colon modifier with this directive."))
;;   (protect
;;    (expand-bind-defaults
;;     ((w nil) (d nil) (k nil) (ovf nil) (pad #\space)) params
;;     `(format-fixed ,*stream* ,(expand-next-arg) ,w ,d ,k ,ovf ,pad ,atsignp))))

;; (defun format-fixed (stream number w d k ovf pad atsign)
;;   (if (numberp number)
;;       (if (floatp number)
;;        (format-fixed-aux stream number w d k ovf pad atsign)
;;        (if (rationalp number)
;;            (format-fixed-aux stream
;;                              (coerce number 'single-float)
;;                              w d k ovf pad atsign)
;;            (format-write-field stream
;;                                (decimal-string number)
;;                                w 1 0 #\space t)))
;;       (format-princ stream number nil nil w 1 0 pad)))


;;; We return true if we overflowed, so that ~G can output the overflow char
;;; instead of spaces.
;;;
;; (defun format-fixed-aux (stream number w d k ovf pad atsign)
;;   (cond
;;    ((or (not (or w d))
;;      (and (floatp number)
;;           (or (float-infinity-p number)
;;               (float-nan-p number))))
;;     (prin1 number stream)
;;     nil)
;;    (t
;;     (let ((spaceleft w))
;;       (when (and w (or atsign (minusp number))) (decf spaceleft))
;;       (multiple-value-bind
;;        (str len lpoint tpoint)
;;        (lisp::flonum-to-string (abs number) spaceleft d k)
;;      ;;if caller specifically requested no fraction digits, suppress the
;;      ;;optional trailing zero
;;      (when (and d (zerop d)) (setq tpoint nil))
;;      (when w
;;        (decf spaceleft len)
;;        ;;optional leading zero
;;        (when lpoint
;;          (if (or (> spaceleft 0) tpoint) ;force at least one digit
;;              (decf spaceleft)
;;              (setq lpoint nil)))
;;        ;;optional trailing zero
;;        (when tpoint
;;          (if (> spaceleft 0)
;;              (decf spaceleft)
;;              (setq tpoint nil))))
;;      (cond ((and w (< spaceleft 0) ovf)
;;             ;;field width overflow
;;             (dotimes (i w) (write-char ovf stream))
;;             t)
;;            (t
;;             (when w (dotimes (i spaceleft) (write-char pad stream)))
;;             (if (minusp number)
;;                 (write-char #\- stream)
;;                 (if atsign (write-char #\+ stream)))
;;             (when lpoint (write-char #\0 stream))
;;             (write-string str stream)
;;             (when tpoint (write-char #\0 stream))
;;             nil)))))))

;; (def-format-directive #\E (colonp atsignp params)
;;   (when colonp
;;     (error 'format-error
;;         :complaint
;;         "Cannot specify the colon modifier with this directive."))
;;   (protect
;;    (expand-bind-defaults
;;     ((w nil) (d nil) (e nil) (k 1) (ovf nil) (pad #\space) (mark nil))
;;     params
;;     `(format-exponential ,*stream* ,(expand-next-arg) ,w ,d ,e ,k ,ovf ,pad ,mark
;;                       ,atsignp))))

;; (defun format-exponential (stream number w d e k ovf pad marker atsign)
;;   (if (numberp number)
;;       (if (floatp number)
;;        (format-exp-aux stream number w d e k ovf pad marker atsign)
;;        (if (rationalp number)
;;            (format-exp-aux stream
;;                            (coerce number 'single-float)
;;                            w d e k ovf pad marker atsign)
;;            (format-write-field stream
;;                                (decimal-string number)
;;                                w 1 0 #\space t)))
;;       (format-princ stream number nil nil w 1 0 pad)))


;; (defun format-exponent-marker (number)
;;   (if (typep number *read-default-float-format*)
;;       #\e
;;       (typecase number
;;      (single-float #\f)
;;      (double-float #\d)
;;      (short-float #\s)
;;      (long-float #\l))))

;;;Here we prevent the scale factor from shifting all significance out of
;;;a number to the right.  We allow insignificant zeroes to be shifted in
;;;to the left right, athough it is an error to specify k and d such that this
;;;occurs.  Perhaps we should detect both these condtions and flag them as
;;;errors.  As for now, we let the user get away with it, and merely guarantee
;;;that at least one significant digit will appear.

;;; toy@rtp.ericsson.se:  The Hyperspec seems to say that the exponent
;;; marker is always printed.  Make it so.  Also, the original version
;;; causes errors when printing infinities or NaN's.  The Hyperspec is
;;; silent here, so let's just print out infinities and NaN's instead
;;; of causing an error.
;; (defun format-exp-aux (stream number w d e k ovf pad marker atsign)
;;   (if (and (floatp number)
;;         (or (float-infinity-p number)
;;             (float-nan-p number)))
;;       (prin1 number stream)
;;       (multiple-value-bind (num expt)
;;                         (lisp::scale-exponent (abs number))
;;      (let* ((expt (- expt k))
;;             (estr (decimal-string (abs expt)))
;;             (elen (if e (max (length estr) e) (length estr)))
;;             (fdig (if d (if (plusp k) (1+ (- d k)) d) nil))
;;             (fmin (if (minusp k) (- 1 k) nil))
;;             (spaceleft (if w
;;                            (- w 2 elen
;;                               (if (or atsign (minusp number))
;;                                   1 0))
;;                            nil)))
;;        (if (and w ovf e (> elen e)) ;exponent overflow
;;            (dotimes (i w) (write-char ovf stream))
;;            (multiple-value-bind
;;                (fstr flen lpoint)
;;                (lisp::flonum-to-string num spaceleft fdig k fmin)
;;              (when w
;;                (decf spaceleft flen)
;;                (when lpoint
;;                  (if (> spaceleft 0)
;;                      (decf spaceleft)
;;                      (setq lpoint nil))))
;;              (cond ((and w (< spaceleft 0) ovf)
;;                     ;;significand overflow
;;                     (dotimes (i w) (write-char ovf stream)))
;;                    (t (when w
;;                         (dotimes (i spaceleft) (write-char pad stream)))
;;                       (if (minusp number)
;;                           (write-char #\- stream)
;;                           (if atsign (write-char #\+ stream)))
;;                       (when lpoint (write-char #\0 stream))
;;                       (write-string fstr stream)
;;                       (write-char (if marker
;;                                       marker
;;                                       (format-exponent-marker number))
;;                                   stream)
;;                       (write-char (if (minusp expt) #\- #\+) stream)
;;                       (when e
;;                         ;;zero-fill before exponent if necessary
;;                         (dotimes (i (- e (length estr)))
;;                           (write-char #\0 stream)))
;;                       (write-string estr stream)))))))))

;; (def-format-directive #\G (colonp atsignp params)
;;   (when colonp
;;     (error 'format-error
;;         :complaint
;;         "Cannot specify the colon modifier with this directive."))
;;   (protect
;;    (expand-bind-defaults
;;     ((w nil) (d nil) (e nil) (k nil) (ovf nil) (pad #\space) (mark nil))
;;     params
;;     `(format-general ,*stream* ,(expand-next-arg) ,w ,d ,e ,k ,ovf ,pad ,mark ,atsignp))))

;; (defun format-general (stream number w d e k ovf pad marker atsign)
;;   (if (numberp number)
;;       (if (floatp number)
;;        (format-general-aux stream number w d e k ovf pad marker atsign)
;;        (if (rationalp number)
;;            (format-general-aux stream
;;                                (coerce number 'single-float)
;;                                w d e k ovf pad marker atsign)
;;            (format-write-field stream
;;                                (decimal-string number)
;;                                w 1 0 #\space t)))
;;       (format-princ stream number nil nil w 1 0 pad)))


;;; toy@rtp.ericsson.se:  Same change as for format-exp-aux.
;; (defun format-general-aux (stream number w d e k ovf pad marker atsign)
;;   (if (and (floatp number)
;;         (or (float-infinity-p number)
;;             (float-nan-p number)))
;;       (prin1 number stream)
;;       (multiple-value-bind (ignore n)
;;        (lisp::scale-exponent (abs number))
;;      (declare (ignore ignore))
;;      ;;Default d if omitted.  The procedure is taken directly
;;      ;;from the definition given in the manual, and is not
;;      ;;very efficient, since we generate the digits twice.
;;      ;;Future maintainers are encouraged to improve on this.
;;      (unless d
;;        (multiple-value-bind (str len)
;;            (lisp::flonum-to-string (abs number))
;;          (declare (ignore str))
;;          (let ((q (if (= len 1) 1 (1- len))))
;;            (setq d (max q (min n 7))))))
;;      (let* ((ee (if e (+ e 2) 4))
;;             (ww (if w (- w ee) nil))
;;             (dd (- d n)))
;;        (cond ((<= 0 dd d)
;;               (let ((char (if (format-fixed-aux stream number ww dd nil
;;                                                 ovf pad atsign)
;;                               ovf
;;                               #\space)))
;;                 (dotimes (i ee) (write-char char stream))))
;;              (t
;;               (format-exp-aux stream number w d e (or k 1)
;;                               ovf pad marker atsign)))))))

(def-format-directive #\$ (colonp atsignp params)
  (protect
   (expand-bind-defaults
       ((d 2) (n 1) (w 0) (pad #\space)) params
     `(format-dollars ,*stream* ,(expand-next-arg) ,d ,n ,w ,pad ,colonp
                      ,atsignp))))

;; (defun format-dollars (stream number d n w pad colon atsign)
;;   (if (rationalp number) (setq number (coerce number 'single-float)))
;;   (if (floatp number)
;;       (let* ((signstr (if (minusp number) "-" (if atsign "+" "")))
;;           (signlen (length signstr)))
;;      (multiple-value-bind (str strlen ig2 ig3 pointplace)
;;                           (lisp::flonum-to-string number nil d nil)
;;        (declare (ignore ig2 ig3))
;;        (when colon (write-string signstr stream))
;;        (dotimes (i (- w signlen (max 0 (- n pointplace)) strlen))
;;          (write-char pad stream))
;;        (unless colon (write-string signstr stream))
;;        (dotimes (i (- n pointplace)) (write-char #\0 stream))
;;        (write-string str stream)))
;;       (format-write-field stream
;;                        (decimal-string number)
;;                        w 1 0 #\space t)))


;;;; line/page breaks and other stuff like that.

(def-format-directive #\% (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
           :complaint
           "Cannot specify either colon or atsign for this directive."))
  (if params
      (expand-bind-defaults
          ((count 1)) params
        (ecase *result-type*
          (:expression
           (make-string count :initial-element #\newline))
          (:statement
           `(dotimes (i ,count)
              (terpri ,*stream*)))
          (:string-buffer
           `(dotimes (i ,count)
              (send ,*stream* append #\newline)))))
      (gen-terpri)))


(def-format-directive #\& (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
           :complaint
           "Cannot specify either colon or atsign for this directive."))
  (if params
      (expand-bind-defaults
          ((count 1)) params
        (ecase *result-type*
          (:expression
           (make-string count :initial-element #\newline))
          (:statement
           (gen-progn
            `((fresh-line ,*stream*)
              (dotimes (i ,(1- count))
                (terpri ,*stream*)))))
          (:string-buffer
           (gen-progn
            `((send ,*stream* append #\newline)
              (dotimes (i ,(1- count))
                (send ,*stream* append #\newline)))))))
      (gen-fresh-line)))


(def-format-directive #\| (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
           :complaint
           "Cannot specify either colon or atsign for this directive."))
  (if params
      (expand-bind-defaults
          ((count 1)) params
        (ecase *result-type*
          (:expression
           (make-string count :initial-element #\page))
          (:statement
           `(dotimes (i ,count)
              (write-char #\page ,*stream*)))
          (:string-buffer
           `(dotimes (i ,count)
              (send ,*stream* append #\page)))))
      (gen-write-char '#\page)))


(def-format-directive #\~ (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
           :complaint
           "Cannot specify either colon or atsign for this directive."))
  (if params
      (expand-bind-defaults
          ((count 1)) params
        (ecase *result-type*
          (:expression
           (make-string count :initial-element #\~))
          (:statement
           `(dotimes (i ,count)
              (write-char #\~ ,*stream*)))
          (:string-buffer
           `(dotimes (i ,count)
              (send ,*stream* append #\~)))))
      (gen-write-char '#\~)))


(def-complex-format-directive #\newline (colonp atsignp params directives)
  (when (and colonp atsignp)
    (error 'format-error
           :complaint
           "Cannot specify both colon and atsign for this directive."))
  (values (expand-bind-defaults () params
            (if atsignp
                (gen-write-char '#\newline)
                (gen-progn '())))
          (if (and (not colonp)
                   directives
                   (simple-string-p (car directives)))
              (cons (string-left-trim '(#\space #\newline #\tab)
                                      (car directives))
                    (cdr directives))
              directives)))




;;;; Tab and simple pretty-printing noise.

(def-format-directive #\T (colonp atsignp params)
  (declare (ignore colonp atsignp));;HACK this shouldn't be ignored
  (if params
      (expand-bind-defaults
          ((count 1)) params
        (ecase *result-type*
          (:expression
           (make-string count :initial-element #\tab))
          (:statement
           `(dotimes (i ,count)
              (write-char #\tab ,*stream*)))
          (:string-buffer
           `(dotimes (i ,count)
              (send ,*stream* append #\tab)))))
      (gen-write-char '#\tab)))


(defun output-spaces (stream n)
  (let ((spaces #.(make-string 100 :initial-element #\space)))
    (loop
      (when (< n (length spaces))
        (return))
      (write-string spaces stream)
      (decf n (length spaces)))
    (write-string spaces stream :end n)))

;; (defun format-relative-tab (stream colrel colinc)
;;   (if (pp:pretty-stream-p stream)
;;       (pprint-tab :line-relative colrel colinc stream)
;;       (let* ((cur (lisp::charpos stream))
;;           (spaces (if (and cur (plusp colinc))
;;                       (- (* (ceiling (+ cur colrel) colinc) colinc) cur)
;;                       colrel)))
;;      (output-spaces stream spaces))))

;; (defun format-absolute-tab (stream colnum colinc)
;;   (if (pp:pretty-stream-p stream)
;;       (pprint-tab :line colnum colinc stream)
;;       (let ((cur (lisp::charpos stream)))
;;      (cond ((null cur)
;;             (write-string "  " stream))
;;            ((< cur colnum)
;;             (output-spaces stream (- colnum cur)))
;;            (t
;;             (unless (zerop colinc)
;;               (output-spaces stream
;;                              (- colinc (rem (- cur colnum) colinc)))))))))

(def-format-directive #\_ (colonp atsignp params)
  (expand-bind-defaults () params
    `(pprint-newline ,(if colonp
                          (if atsignp
                              :mandatory
                              :fill)
                          (if atsignp
                              :miser
                              :linear))
                     ,*stream*)))



(def-format-directive #\I (colonp atsignp params)
  (when atsignp
    (error 'format-error
           :complaint "Cannot specify the at-sign modifier."))
  (expand-bind-defaults ((n 0)) params
    `(pprint-indent ,(if colonp :current :block) ,n ,*stream*)))




;;;; *

(def-format-directive #\* (colonp atsignp params end)
  (if atsignp
      (if colonp
          (error 'format-error
                 :complaint "Cannot specify both colon and at-sign.")
          (expand-bind-defaults ((posn 0)) params
            (if *orig-args-available*
                `(if (<= 0 ,posn (length orig-args))
                     (setf args (nthcdr ,posn orig-args))
                     (error 'format-error
                            :complaint "Index ~D out of bounds.  Should have been ~
                                    between 0 and ~D."
                            :arguments (list ,posn (length orig-args))
                            :offset ,(1- end)))
                (progn
                  (if (<= 0 posn (length *orig-args*))
                      (setf *args* (nthcdr posn *orig-args*))
                      (error 'format-error
                             :complaint "Index ~D out of bounds.  Should have been ~
                                    between 0 and ~D."
                             :arguments (list posn (length *orig-args*))
                             :offset (1- end)))
                  (gen-progn '())))))
      (if colonp
          (expand-bind-defaults ((n 1)) params
            (if *orig-args-available*
                `(do ((cur-posn 0 (1+ cur-posn))
                      (arg-ptr orig-args (cdr arg-ptr)))
                     ((eq arg-ptr args)
                      (let ((new-posn (- cur-posn ,n)))
                        (if (<= 0 new-posn (length orig-args))
                            (setf args (nthcdr new-posn orig-args))
                            (error 'format-error
                                   :complaint
                                   "Index ~D out of bounds.  Should have been ~
                                between 0 and ~D."
                                   :arguments
                                   (list new-posn (length orig-args))
                                   :offset ,(1- end))))))
                (progn
                  (do ((cur-posn 0 (1+ cur-posn))
                       (arg-ptr *orig-args* (cdr arg-ptr)))
                      ((eq arg-ptr *args*)
                       (let ((new-posn (- cur-posn n)))
                         (if (<= 0 new-posn (length *orig-args*))
                             (setf *args* (nthcdr new-posn *orig-args*))
                             (error 'format-error
                                    :complaint
                                    "Index ~D out of bounds.  Should have been ~
                                between 0 and ~D."
                                    :arguments
                                    (list new-posn (length *orig-args*))
                                    :offset (1- end))))))
                  (gen-progn '()))))
          (if params
              (expand-bind-defaults ((n 1)) params
                (if *orig-args-available*
                    `(dotimes (i ,n)
                       ,(protect (expand-next-arg)))
                    (progn
                      (dotimes (i n)
                        (expand-next-arg))
                      (gen-progn '()))))
              (progn
                (expand-next-arg)
                (gen-progn '()))))))



;;;; Indirection.

(def-format-directive #\? (colonp atsignp params string end)
  (declare (ignore colonp atsignp params end))
  (when string
    (error "Can't deal with indirect format strings ~A" string)))


;;;; Capitalization.

(def-complex-format-directive #\( (colonp atsignp params directives)
  (let ((close (find-directive directives #\) nil)))
    (unless close
      (error 'format-error
             :complaint "No corresponding close paren."))
    (let* ((posn (position close directives))
           (before (subseq directives 0 posn))
           (after (nthcdr (1+ posn) directives)))
      (values
       (expand-bind-defaults () params
         (gen-write-string
          `(,(if colonp
                 (if atsignp
                     'string-upcase
                     'string-capitalize)
                 (if atsignp
                     (error "Can't do :capitalize-first in Linj")
                     'string-downcase))
            ,(let ((*result-type* :expression))
               (gen-progn `(,@(expand-directive-list before)))))))
       after))))



(def-complex-format-directive #\) ()
  (error 'format-error
         :complaint "No corresponding open paren."))




;;;; Conditionals

(defun parse-conditional-directive (directives)
  (let ((sublists nil)
        (last-semi-with-colon-p nil)
        (remaining directives))
    (loop
      (let ((close-or-semi (find-directive remaining #\] t)))
        (unless close-or-semi
          (error 'format-error
                 :complaint "No corresponding close bracket."))
        (let ((posn (position close-or-semi remaining)))
          (push (subseq remaining 0 posn) sublists)
          (setf remaining (nthcdr (1+ posn) remaining))
          (when (char= (format-directive-character close-or-semi) #\])
            (return))
          (setf last-semi-with-colon-p
                (format-directive-colonp close-or-semi)))))
    (values sublists last-semi-with-colon-p remaining)))

(def-complex-format-directive #\[ (colonp atsignp params directives)
  (multiple-value-bind (sublists last-semi-with-colon-p remaining)
      (parse-conditional-directive directives)
    (if atsignp
        (if colonp
            (error 'format-error
                   :complaint
                   "Cannot specify both the colon and at-sign modifiers.")
            (if (cdr sublists)
                (error 'format-error
                       :complaint
                       "Can only specify one section")
                (expand-bind-defaults () params
                  (expand-maybe-conditional (car sublists) remaining))))
        (if colonp
            (if (= (length sublists) 2)
                (expand-bind-defaults () params
                  (expand-true-false-conditional (car sublists)
                                                 (cadr sublists)
                                                 remaining))
                (error 'format-error
                       :complaint
                       "Must specify exactly two sections."))
            (expand-bind-defaults ((index (expand-next-arg))) params
              (let ((clauses nil))
                (when last-semi-with-colon-p
                  (push `(t ,@(expand-directive-list (pop sublists)))
                        clauses))
                (let ((count (length sublists)))
                  (dolist (sublist sublists)
                    (push `(,(decf count)
                            ,@(expand-directive-list sublist))
                          clauses)))
                (values `(case ,index ,@clauses) remaining)))))))

(defun expand-maybe-conditional (sublist directives)
  (flet ((hairy ()
           (error "Can't treat this case")))
    (if *only-simple-args*
        (let ((test (car *args*))) ;;HACK this causes repeated evaluation
          (let ((guts (expand-directive-list sublist)))
            (gen-progn (cons (gen-when test guts) (expand-directive-list directives)))))
        (hairy))))

(defun expand-true-false-conditional (true false directives)
  (flet ((hairy ()
           (values
            (let ((arg (expand-next-arg)))
              (let ((then-forms (let ((*args* *args*))
                                  (expand-directive-list (append true directives))))
                    (else-forms (let ((*args* *args*))
                                  (expand-directive-list (append false directives)))))
                (destructuring-bind (prefix l1 l2 suffix)
                    (prefix-list1-list2-suffix then-forms else-forms)
                  (let ((core (cond ((null l1)
                                     `(unless ,arg ,@l2))
                                    ((null l2)
                                     `(when ,arg ,@l1))
                                    (t
                                     `(if ,arg
                                          ,(gen-progn `(,@l1))
                                          ,(gen-progn `(,@l2)))))))
                    (if (or prefix suffix)
                        (gen-progn `(,@prefix ,core ,@suffix))
                        core)))))
            (list))))
    (hairy)))
;;       (if *only-simple-args*
;;        (multiple-value-bind
;;            (true-guts true-args true-simple)
;;            (let ((*simple-args* *simple-args*)
;;                  (*only-simple-args* t))
;;              (values (expand-directive-list (append true directives))
;;                      *simple-args*
;;                      *only-simple-args*))
;;          (multiple-value-bind
;;              (false-guts false-args false-simple)
;;              (let ((*simple-args* *simple-args*)
;;                    (*only-simple-args* t))
;;                (values (expand-directive-list (append false directives))
;;                        *simple-args*
;;                        *only-simple-args*))
;;            (if (= (length true-args) (length false-args))
;;                `(if ,arg
;;                     (progn
;;                       ,@true-guts)
;;                     ,(do ((false false-args (cdr false))
;;                           (true true-args (cdr true))
;;                           (bindings nil (cons `(,(caar false) ,(caar true))
;;                                               bindings)))
;;                          ((eq true *simple-args*)
;;                           (setf *simple-args* true-args)
;;                           (setf *only-simple-args*
;;                                 (and true-simple false-simple))
;;                           (if bindings
;;                               `(let ,bindings
;;                                  ,@false-guts)
;;                               `(progn
;;                                  ,@false-guts)))))
;;                (progn
;;                  (progn (setf *only-simple-args* nil) (error "BUM"))
;;                  (hairy)))))
;;        (hairy))


(defun prefix-list1-list2-suffix (list1 list2)
  (labels ((prefix (l1 l2 prefix in-prefix)
             (cond ((or (null l1) (null l2))
                    (list (reverse prefix) l1 l2 (list)))
                   ((equal (first l1) (first l2))
                    (prefix (rest l1) (rest l2) (cons (first l1) prefix) in-prefix))
                   ((not in-prefix)
                    (list (reverse prefix) l1 l2 (list)))
                   (t
                    (destructuring-bind (suffix l1 l2 ignore)
                        (prefix (reverse l1) (reverse l2) (list) (not in-prefix))
                      (declare (ignore ignore))
                      (list (reverse prefix) (reverse l1) (reverse l2) (reverse suffix)))))))
    (prefix list1 list2 (list) t)))

(def-complex-format-directive #\; ()
  (error 'format-error
         :complaint
         "~~; not contained within either ~~[...~~] or ~~<...~~>."))





(def-complex-format-directive #\] ()
  (error 'format-error
         :complaint
         "No corresponding open bracket."))


;;;; Up-and-out.

(defvar *outside-args*)

(def-format-directive #\^ (colonp atsignp params)
  (when atsignp
    (error 'format-error
           :complaint "Cannot specify the at-sign modifier."))
  (when (and colonp (not *up-up-and-out-allowed*))
    (error 'format-error
           :complaint "Attempt to use ~~:^ outside a ~~:{...~~} construct."))
  `(when ,(case (length params)
            (0 (if colonp
                   '(null outside-args)
                   (progn
                     (setf *only-simple-args* nil)
                     '(null args))))
            (1 (expand-bind-defaults ((count 0)) params
                 `(zerop ,count)))
            (2 (expand-bind-defaults ((arg1 0) (arg2 0)) params
                 `(= ,arg1 ,arg2)))
            (t (expand-bind-defaults ((arg1 0) (arg2 0) (arg3 0)) params
                 `(<= ,arg1 ,arg2 ,arg3))))
     ,(if colonp
          '(return-from outside-loop nil)
          '(return))))



;;;; Iteration.
(def-complex-format-directive #\{ (colonp atsignp params string end directives)
  (declare (ignore end))
  (let ((close (find-directive directives #\} nil)))
    (unless close
      (error 'format-error
             :complaint
             "No corresponding close brace."))
    (let* ((closed-with-colon (format-directive-colonp close))
           (posn (position close directives)))
      (labels
          ((compute-insides ()
             (if (zerop posn)
                 (if *orig-args-available*
                     (error "Can't deal with indirect format strings ~A" string)
                     (throw 'need-orig-args nil))
                 (let ((*up-up-and-out-allowed* colonp))
                   (expand-directive-list (subseq directives 0 posn)))))
           (compute-loop-aux (count)
             (when atsignp
               (progn (setf *only-simple-args* nil)))
             `(do ()
                  (,@(unless closed-with-colon
                       '((null args))))
                ,@(when count
                    `((when (minusp (decf count))
                        (return))))
                ,@(if colonp
                      (let ((*only-simple-args* nil)
                            (*orig-args-available* t))
                        `((let* ((orig-args ,(expand-next-arg))
                                 (outside-args args)
                                 (args orig-args))
                            (block nil
                              ,@(compute-insides)))))
                      (compute-insides))
                ,@(when closed-with-colon
                    '((when (null args)
                        (return))))))
           (compute-loop ()
             (if params
                 (expand-bind-defaults ((count nil)) params
                   (if count
                       `(let ((count ,count))
                          ,(compute-loop-aux count))
                       (compute-loop-aux nil)))
                 (compute-loop-aux nil)))
           (compute-block ()
             (if colonp
                 `(block outside-loop
                    ,(compute-loop))
                 (compute-loop)))
           (compute-bindings ()
             (if atsignp
                 `(let* ((args (list ,@*args*)))
                    ,(compute-block))
                 (if *only-simple-args*
                     `(let* ((orig-args ,(expand-next-arg))
                             (args orig-args))
                        ,(let ((*only-simple-args* nil)
                               (*orig-args-available* t))
                           (compute-block)))
                     (protect
                      `(let ((args (the cons arg)))
                         ,(compute-block)))))))
        (values (if (zerop posn)
                    `(let ((inside-string ,(expand-next-arg)))
                       ,(compute-bindings))
                    (compute-bindings))
                (nthcdr (1+ posn) directives))))))



(def-complex-format-directive #\} ()
  (error 'format-error
         :complaint "No corresponding open brace."))





;;;; Justification.

;; (defparameter *illegal-inside-justification*
;;   (mapcar (lambda (x) (parse-directive x 0))
;;        '("~W" "~:W" "~@W" "~:@W"
;;          "~_" "~:_" "~@_" "~:@_"
;;          "~:>" "~:@>"
;;          "~I" "~:I" "~@I" "~:@I"
;;          "~:T" "~:@T")))

;; (defun illegal-inside-justification-p (directive)
;;   (member directive *illegal-inside-justification*
;;        :test (lambda (x y)
;;                (and (format-directive-p x)
;;                     (format-directive-p y)
;;                     (eql (format-directive-character x) (format-directive-character y))
;;                     (eql (format-directive-colonp x) (format-directive-colonp y))
;;                     (eql (format-directive-atsignp x) (format-directive-atsignp y))))))

;; (def-complex-format-directive #\< (colonp atsignp params string end directives)
;;   (multiple-value-bind
;;       (segments first-semi close remaining)
;;       (parse-format-justification directives)
;;     (values
;;      (if (format-directive-colonp close)
;;       (multiple-value-bind
;;           (prefix per-line-p insides suffix)
;;           (parse-format-logical-block segments colonp first-semi
;;                                       close params string end)
;;         (expand-format-logical-block prefix per-line-p insides
;;                                      suffix atsignp))
;;       (let ((count (reduce #'+ (mapcar (lambda (x)
;;                                          (count-if #'illegal-inside-justification-p x))
;;                                        segments))))
;;         (when (> count 0)
;;           ;; ANSI specifies that "an error is signalled" in this
;;           ;; situation.
;;           (error 'format-error
;;                  :complaint "~D illegal directive~:P found inside justification block"
;;                  :arguments (list count)))
;;         (expand-format-justification segments colonp atsignp
;;                                    first-semi params)))
;;      remaining)))



;; (defun parse-format-justification (directives)
;;   (let ((first-semi nil)
;;      (close nil)
;;      (remaining directives))
;;     (collect ((segments))
;;       (loop
;;      (let ((close-or-semi (find-directive remaining #\> t)))
;;        (unless close-or-semi
;;          (error 'format-error
;;                 :complaint "No corresponding close bracket."))
;;        (let ((posn (position close-or-semi remaining)))
;;          (segments (subseq remaining 0 posn))
;;          (setf remaining (nthcdr (1+ posn) remaining)))
;;        (when (char= (format-directive-character close-or-semi)
;;                     #\>)
;;          (setf close close-or-semi)
;;          (return))
;;        (unless first-semi
;;          (setf first-semi close-or-semi))))
;;       (values (segments) first-semi close remaining))))

;; (defun expand-format-justification (segments colonp atsignp first-semi params)
;;   (let ((newline-segment-p
;;       (and first-semi
;;            (format-directive-colonp first-semi))))
;;     (expand-bind-defaults
;;      ((mincol 0) (colinc 1) (minpad 0) (padchar #\space))
;;      params
;;       `(let ((segments nil)
;;           ,@(when newline-segment-p
;;               '((newline-segment nil)
;;                 (extra-space 0)
;;                 (line-len 72))))
;;       (block nil
;;         ,@(when newline-segment-p
;;             `((setf newline-segment
;;                     (with-output-to-string (stream)
;;                       ,@(expand-directive-list (pop segments))))
;;               ,(expand-bind-defaults
;;                    ((extra 0)
;;                     (line-len '(or (lisp::line-length stream) 72)))
;;                    (format-directive-params first-semi)
;;                  `(setf extra-space ,extra line-len ,line-len))))
;;         ,@(mapcar #'(lambda (segment)
;;                       `(push (with-output-to-string (stream)
;;                                ,@(expand-directive-list segment))
;;                              segments))
;;                   segments))
;;       (format-justification stream
;;                             ,@(if newline-segment-p
;;                                   '(newline-segment extra-space line-len)
;;                                   '(nil 0 0))
;;                             segments ,colonp ,atsignp
;;                             ,mincol ,colinc ,minpad ,padchar)))))



;; (defun format-justification (stream newline-prefix extra-space line-len strings
;;                           pad-left pad-right mincol colinc minpad padchar)
;;   (setf strings (reverse strings))
;;   (when (and (not pad-left) (not pad-right) (null (cdr strings)))
;;     (setf pad-left t))
;;   (let* ((num-gaps (+ (1- (length strings))
;;                    (if pad-left 1 0)
;;                    (if pad-right 1 0)))
;;       (chars (+ (* num-gaps minpad)
;;                 (loop
;;                   for string in strings
;;                   summing (length string))))
;;       (length (if (> chars mincol)
;;                   (+ mincol (* (ceiling (- chars mincol) colinc) colinc))
;;                   mincol))
;;       (padding (- length chars)))
;;     (when (and newline-prefix
;;             (> (+ (or (lisp::charpos stream) 0)
;;                   length extra-space)
;;                line-len))
;;       (write-string newline-prefix stream))
;;     (flet ((do-padding ()
;;           (let ((pad-len (truncate padding num-gaps)))
;;             (decf padding pad-len)
;;             (decf num-gaps)
;;             (dotimes (i pad-len) (write-char padchar stream)))))
;;       (when pad-left
;;      (do-padding))
;;       (when strings
;;      (write-string (car strings) stream)
;;      (dolist (string (cdr strings))
;;        (do-padding)
;;        (write-string string stream)))
;;       (when pad-right
;;      (do-padding)))))

;; (defun parse-format-logical-block
;;        (segments colonp first-semi close params string end)
;;   (when params
;;     (error 'format-error
;;         :complaint "No parameters can be supplied with ~~<...~~:>."
;;         :offset (caar params)))
;;   (multiple-value-bind
;;       (prefix insides suffix)
;;       (multiple-value-bind (prefix-default suffix-default)
;;                         (if colonp (values "(" ")") (values nil nil))
;;      (flet ((extract-string (list prefix-p)
;;               (let ((directive (find-if #'format-directive-p list)))
;;                 (if directive
;;                     (error 'format-error
;;                            :complaint
;;                            "Cannot include format directives inside the ~
;;                             ~:[suffix~;prefix~] segment of ~~<...~~:>"
;;                            :arguments (list prefix-p)
;;                            :offset (1- (format-directive-end directive)))
;;                     (apply #'concatenate 'string list)))))
;;      (case (length segments)
;;        (0 (values prefix-default nil suffix-default))
;;        (1 (values prefix-default (car segments) suffix-default))
;;        (2 (values (extract-string (car segments) t)
;;                   (cadr segments) suffix-default))
;;        (3 (values (extract-string (car segments) t)
;;                   (cadr segments)
;;                   (extract-string (caddr segments) nil)))
;;        (t
;;         (error 'format-error
;;                :complaint "Too many segments for ~~<...~~:>.")))))
;;     (when (format-directive-atsignp close)
;;       (setf insides
;;          (add-fill-style-newlines insides
;;                                   string
;;                                   (if first-semi
;;                                       (format-directive-end first-semi)
;;                                       end))))
;;     (values prefix
;;          (and first-semi (format-directive-atsignp first-semi))
;;          insides
;;          suffix)))

;; (defun add-fill-style-newlines (list string offset)
;;   (if list
;;       (let ((directive (car list)))
;;      (if (simple-string-p directive)
;;          (nconc (add-fill-style-newlines-aux directive string offset)
;;                 (add-fill-style-newlines (cdr list)
;;                                          string
;;                                          (+ offset (length directive))))
;;          (cons directive
;;                (add-fill-style-newlines (cdr list)
;;                                         string
;;                                         (format-directive-end directive)))))
;;       nil))

;; (defun add-fill-style-newlines-aux (literal string offset)
;;   (let ((end (length literal))
;;      (posn 0))
;;     (collect ((results))
;;       (loop
;;      (let ((blank (position #\space literal :start posn)))
;;        (when (null blank)
;;          (results (subseq literal posn))
;;          (return))
;;        (let ((non-blank (or (position #\space literal :start blank
;;                                       :test #'char/=)
;;                             end)))
;;          (results (subseq literal posn non-blank))
;;          (results (make-format-directive
;;                    :string string :character #\_
;;                    :start (+ offset non-blank) :end (+ offset non-blank)
;;                    :colonp t :atsignp nil :params nil))
;;          (setf posn non-blank))
;;        (when (= posn end)
;;          (return))))
;;       (results))))


;;;; User-defined method.

;; (def-format-directive #\/ (string start end colonp atsignp params)
;;   (let ((symbol (extract-user-function-name string start end)))
;;     (collect ((param-names) (bindings))
;;       (dolist (param-and-offset params)
;;      (let ((param (cdr param-and-offset)))
;;        (let ((param-name (gensym)))
;;          (param-names param-name)
;;          (bindings `(,param-name
;;                      ,(case param
;;                         (:arg (expand-next-arg))
;;                         (:remaining '(length args))
;;                         (t param)))))))
;;       `(let ,(bindings)
;;       (,symbol ,*stream* ,(expand-next-arg) ,colonp ,atsignp
;;                ,@(param-names))))))


;; (defun extract-user-function-name (string start end)
;;   (let ((slash (position #\/ string :start start :end (1- end)
;;                       :from-end t)))
;;     (unless slash
;;       (error 'format-error
;;           :complaint "Malformed ~~/ directive."))
;;     (let* ((name (string-upcase (let ((foo string))
;;                                ;; Hack alert: This is to keep the compiler
;;                                ;; quit about deleting code inside the subseq
;;                                ;; expansion.
;;                                (subseq foo (1+ slash) (1- end)))))
;;         (first-colon (position #\: name))
;;         (second-colon (if first-colon (position #\: name :start (1+ first-colon))))
;;         (package-name (if first-colon
;;                           (subseq name 0 first-colon)
;;                           "COMMON-LISP-USER"))
;;         (package (find-package package-name)))
;;       (unless package
;;      (error 'format-error
;;             :complaint "No package named ~S"
;;             :arguments (list package-name)))
;;       (intern (cond
;;                 ((and second-colon (= second-colon (1+ first-colon)))
;;                  (subseq name (1+ second-colon)))
;;                 (first-colon
;;                  (subseq name (1+ first-colon)))
;;                 (t name))
;;               package))))
