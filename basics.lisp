;;; -*- Mode: Common-Lisp; Syntax: Common-Lisp; Package: LINJ; Base: 10 -*-

;;; Copyright (C) Antonio Menezes Leitao Created on Thu May  2 20:01:15 2002
;;; Copyright (C) eValuator, Lda

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package "LINJ")

;;This file requires the Linj readtable
(eval-when (:compile-toplevel :load-toplevel)
  (setq *readtable* *linj-readtable*))

;;Let's use meaningful error messages:

(defun linj-error (msg &rest args)
  (let ((methods (list))
        (types (list)))
    (let ((args (mapcar #'(lambda (arg)
                            (if (typep arg 'linj-node)
                                (let ((type  (ignore-errors (type-declaration-name (containing-type-declaration arg))))
                                      (method (ignore-errors (ast-node-form (containing-method-declaration arg)))))
                                  (when method (pushnew method methods))
                                  (when type (pushnew type types))
                                  ;;				(list (type-of arg) arg))
                                  arg)
                                ;;                              (do ((arg arg (ignore-errors (ast-node-parent arg))))
                                ;;                                  ((or (null arg) (ignore-errors (ast-node-form arg))) (ignore-errors (ast-node-form arg)))))
                                arg))
                        args)))
      (with-linj-syntax ()
        (apply #'format *error-output* msg args)
        (when methods
          (format *error-output* " in method~P ~{~%~S~^~%    and   ~}~%" (length methods) methods))
        (when types
          (format *error-output* " in type~P ~{~A~^ ~}~%" (length types) types))
        (apply #'error msg args)))))

;;Constants

(def-basic-constant true t)
(def-basic-constant false nil)
(def-basic-constant null)
(def-basic-constant this)
(def-basic-constant super)
(def-basic-constant class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Options

(defclass declaration-options ()
  ((options :accessor declaration-options :initform (list) :initarg :options)))

(defun get-option (name e &optional default)
  (getf (declaration-options e) name default))

(defun (setf get-option) (value name e)
  (setf (getf (declaration-options e) name) value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Now, we enter in the parse phase.
;;Each syntactical element in linj code is translated into an abstract
;;syntax tree node.

(def-category linj-node (ast-node)
  ())

(defmethod containing-method-declaration ((e linj-node))
  (and (ast-node-parent e)
       (containing-method-declaration (ast-node-parent e))))

(defmethod containing-type-declaration ((e linj-node))
  (and (not (null (ast-node-parent e)))
       (self-or-containing-type-declaration (ast-node-parent e))))

(defmethod self-or-containing-type-declaration ((e linj-node))
  (containing-type-declaration e))

;;Base classes
(def-category expression (linj-node)
  ())

(def-category statement (linj-node)
  ())

(def-category top-level-form (linj-node)
  ())

;;Lists:

(def-category linj-list-node (linj-node)
  ((elements :initform (list) :accessor linj-list-node-elements)))

;;Statements
(def-list-syntax statement-list (linj-list-node) statement)

(def-unparse statement-list (e)
  (let ((statements (statement-list-elements e)))
    (if (not (endp statements)) ;;more than zero?
        (if *print-out-braces*
            (format t "{~4I~{~:@_~/ppblk/~}~:@_~:/ppblk/~I~:@_}"
                    (butlast statements)
                    (first (last statements)))
            ;;This doesn't look OK...
            (format t "~{~/ppblk/~^~:@_~}~:@_~:/ppblk/"
                    (butlast statements)
                    (first (last statements))))
        (when *print-out-braces*
          (format t "{~:@_}")))))

;;Simplest concrete syntax

;;every atom that is not a symbol is a literal. true (t) and false (nil)
;;are also literals.

(defun literal-symbols (obj)
  (or (null-value-p obj)
      (true-value-p obj)
      (false-value-p obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;For other kinds of numeric literals

;;We also accept 0x123 for numbers in hexadecimal notation.

(defun number-with-base (expr)
  (and (symbolp expr)
       (let ((str (symbol-name expr))
             (prefix "0x"))
         (let ((prefix-length (length prefix))
               (str-length (length str)))
           (and (< prefix-length str-length)
                (string-equal str prefix :end1 prefix-length) ;;hexadecimal?
                (multiple-value-bind (val end)
                    (let ((*read-base* 16))
                      (read-from-string str nil nil :start prefix-length :preserve-whitespace t))
                  (and val
                       (= end str-length)
                       (numberp val)
                       val)))))))

(defun number-with-type-p (expr type)
  (and (symbolp expr)
       (let ((str (symbol-name expr)))
         (let ((start1 (- (length str) (length type))))
           (and (> start1 0)
                (string-equal str type :start1 start1)
                (multiple-value-bind (val end)
                    (read-from-string str nil nil :end start1 :preserve-whitespace t)
                  (and val
                       (= end start1)
                       (or (numberp val)
                           (number-with-base val)))))))))

(defun number-with-type-string (expr type)
  (let ((str (symbol-name expr)))
    (let ((start1 (- (length str) (length type))))
      (subseq str 0 start1))))

(defun number-with-type-value (expr type)
  (let ((str (symbol-name expr)))
    (let ((start1 (- (length str) (length type))))
      (read-from-string str nil nil :end start1))))

(defconst +byte+ "byte")
(defconst +short+ "short")
(defconst +long+ "l")
(defconst +bignum+ "big")
(defconst +big-integer+ "bigint")
(defconst +big-decimal+ "bigdec")

(defconst +number-extensions+ (list +byte+ +short+ +long+ +bignum+ +big-integer+ +big-decimal+))

(defun literal-expression-p (expression)
  (and (atom expression)
       (or (literal-symbols expression)
           (stringp expression)
           (and (not (symbolp expression))
                (not (vectorp expression)))
           (some (curry #'number-with-type-p expression) +number-extensions+)
           (number-with-base expression))));;for the non-type-extended case

(def-syntax literal (expression)
  (?is ?value literal-expression-p))

(defun make-literal (arg)
  (make-instance 'literal :value arg))

(defmacro def-literal-predicate (name predicate)
  `(defun ,name (object)
     (and (literal-p object)
          (,predicate (literal-value object)))))

(def-literal-predicate null-literal-p null-value-p)
(def-literal-predicate true-literal-p true-value-p)
(def-literal-predicate false-literal-p false-value-p)
(def-literal-predicate string-literal-p stringp)

;; # For byte, from -128 to 127, inclusive
;; # For short, from -32768 to 32767, inclusive
;; # For int, from -2147483648 to 2147483647, inclusive
;; # For long, from -9223372036854775808 to 9223372036854775807, inclusive
;; # For char, from '\u0000' to '\uffff' inclusive, that is, from 0 to 65535
;;unused for now...
(defun limits-for-type (type)
  (ecase type
    ((byte) (values -128 127))
    ((short) (values -32768 32767))
    ((int) (values -2147483648 2147483647))
    ((long) (values -9223372036854775808 9223372036854775807))
    ((char) (values 0 65535))))

(defun byte-value-p (value)
  (<= -128 value 127))

(defun short-value-p (value)
  (<= -32768 value 32767))

(defun char-value-p (value)
  (or (characterp value)
      (<= 0 value 65535)))

(defun int-value-p (value)
  (<= -2147483648 value 2147483647))

(defun long-value-p (value)
  (<= -9223372036854775808 value 9223372036854775807))

(defmethod get-type ((e literal))
  (with-parent ((ast-node-parent e))
    (cond ((or (true-literal-p e) (false-literal-p e))
           (boolean-type))
          ((null-literal-p e)
           (null-type)) ;;should I use (object-type)?
          (t
           (let ((value (literal-value e)))
             (etypecase value
               ;;We represent longs as symbols of the form 0l, 1L, etc.
               (symbol
                (or (some #'(lambda (ext)
                              (and (number-with-type-p value ext)
                                   (cond ((eq ext +byte+) (byte-type))
                                         ((eq ext +short+) (short-type))
                                         ((eq ext +long+) (long-type))
                                         ((eq ext +bignum+) (bignum-type))
                                         ((eq ext +big-integer+) (big-integer-type))
                                         ((eq ext +big-decimal+) (big-decimal-type))
                                         (t (error "Unknown type ~A" ext)))))
                          +number-extensions+)
                    (let ((val (number-with-base value)))
                      (and val (integer-type-for-value val)))
                    (error "Unknown type of literal ~S" e)))
               (integer (integer-type-for-value value))
               (ratio (bignum-type))
               (short-float (float-type))
               (float (double-type))
               (character (char-type))
               (string (string-type))
               ;; (simple-vector (tuple-type)) ;;unsupported in Allegro 7.0
               ))))))

;;;4.2.1 Integral Types and Values
;;; The values of the integral types are integers in the following ranges:
;;; For int, from -2147483648 to 2147483647, inclusive
;;; For long, from -9223372036854775808 to 9223372036854775807, inclusive
(defun integer-type-for-value (value)
  (cond ((int-value-p value)
         (int-type))
        ((long-value-p value)
         (long-type))
        (t
         (bignum-type))))

(def-unparse literal (e)
  (unparse-literal (literal-value e)))

(defun lisp-char-to-java-char (char)
  (case char
    ((#\backspace) "\\b")
    ((#\tab) "\\t")
    ((#\newline) "\\n")
    ((#\page) "\\f")
    ((#\return) "\\r")
    ((#\') "\\'")
    ((#\\) "\\\\")
    (t (princ-to-string char))))

(defmethod unparse-literal ((value character))
  (format t "'~A'" (lisp-char-to-java-char value)))

(defmethod unparse-literal ((value float))
  (let ((*read-default-float-format* (type-of value)))
    (princ value))
  (typecase value
    (short-float (princ "f"))))

(defmethod unparse-literal ((value string))
  ;;We need to replace all newlines by '\n'
  (write-char #\")
  (dotimes (i (length value))
    (cond ((char= (char value i) #\")
           (princ "\\\""))
          ((char= (char value i) #\')
           (princ "'"))  ;;We could use \' but I prefer '
          (t
           (princ (lisp-char-to-java-char (char value i))))))
  (write-char #\"))

(defmethod unparse-literal ((value ratio))
  (format t "Bignum.valueOf(~A, ~A)" (numerator value) (denominator value)))

(defmethod unparse-literal ((value integer))
  (cond ((int-value-p value)
         (format t "~D" value))
        ((long-value-p value)
         (format t "~DL" value))
        (t
         (format t "Bignum.valueOf(\"~A\")" value))))

;; (defmethod unparse-literal ((value simple-vector))
;;   (format t "new Object[] { }"))

(defmethod unparse-literal ((value t))
  (cond ((number-with-type-p value +byte+)
         (format t "((byte)~A)" (number-with-type-value value +byte+)))
        ((number-with-type-p value +short+)
         (format t "((short)~A)" (number-with-type-value value +short+)))
        ((number-with-type-p value +long+)
         (princ value))
        ((number-with-type-p value +bignum+)
         (let ((val (number-with-type-value value +bignum+)))
           (if (long-value-p val)
               (format t "Bignum.valueOf(~A)" val)
               (format t "Bignum.valueOf(\"~A\")" val))))
        ((number-with-type-p value +big-integer+)
         (let ((val (number-with-type-value value +big-integer+)))
           ;;We will optimize construction using the (supposedly) faster method
           (cond ((not (integerp val))
                  (error "The big-integer ~A must have integer representation." value))
                 ((int-value-p val)
                  ;;due to user's demand, we will use also an int (although
                  ;;it will be automatically coerced to long)
                  (format t "BigInteger.valueOf(~A)" val))
                 ((long-value-p val)
                  (format t "BigInteger.valueOf(~AL)" val))
                 (t
                  (format t "new BigInteger(\"~A\")" (number-with-type-string value +big-integer+))))))
        ((number-with-type-p value +big-decimal+)
         ;;Note that, due to 2.3.1.1, the Common Lisp reader might not
         ;;be able to create the number.  I don't like ignore-errors
         ;;but it seems the correct thing to do in this case
         (let ((val (ignore-errors (number-with-type-value value +big-decimal+)))
               (val-str (number-with-type-string value +big-decimal+)))
           (cond ((and (integerp val) (long-value-p val))
                  (format t "BigDecimal.valueOf(~A)" val))
                 ((string= (princ-to-string val) val-str)
                  ;;no loss in precision
                  (format t "new BigDecimal(~A)" (number-with-type-string value +big-decimal+)))
                 (t
                  (format t "new BigDecimal(\"~A\")" (number-with-type-string value +big-decimal+))))))
        (t
         (princ (java-constant-name value)))))

(defun get-default-initializer (type)
  (cond ((boolean-type-p type) (false-value))
        ((int-type-p type) 0)
        ((long-type-p type) '0l)
        ((char-type-p type) #\a)
        ((double-type-p type) 0.0l0)
        ((float-type-p type) 0.0s0)
        (t (null-value))))

;;Every symbol except null, true and false are names
(defun name-symbol-p (obj)
  (and (symbolp obj)
       (not (literal-expression-p obj))
       (not (name&type-p obj)))) ;;to exclude types
;;This restriction was added to allow shadowing of parameters and let-bindings
;;See unparse of references and variable-declarations

;;I removed it again
;;       (check-underscore-usage obj)))

(defun check-underscore-usage (sym)
  (assert (or (not (find #\_ (symbol-name sym)))
              ;;upper-case-p bc due to read-table-case :invert, the
              ;;upper-case symbols such as SPECIAL_CHAR are read as special_char
              (notany #'upper-case-p (symbol-name sym)))
          (sym)
          "Don't use underscores in Linj programs (~S)" sym)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;We must convert linj-symbols to java symbols

;;We allow for mixed syntax. [This must be tested. I don't like name
;;mangling, but...]

(defun java-reserved-name-p (name)
  (member name '(abstract    default    if            private      #|this|#
                 boolean     do         implements    protected    throw
                 break       double     import        public       throws
                 byte        else       instanceof    return       transient
                 case        extends    int           short        try
                 catch       final      interface     static       void
                 char        finally    long          strictfp     volatile
                 class       float      native        #|super|#    while
                 const       for        new           switch
                 continue    goto       package       synchronized)))

;;SEVERE HACK!!!!!
;;There's a problem with using symbols and their property list to keep
;;information regarding the original form and the java translated form.
;;The problem is that if Linj parses, e.g., <component> and then
;;<Component>, both symbols will be translated to Component and both will
;;try to associate the original form on the property list of Component. A
;;better solution would be to use the original form and associate with it
;;the java translation, but this would force me to select the java
;;translation whenever I needed to ask the java server or to find standard
;;classes, etc.
;;Since Linj is already in the 'Legacy' status, I'll use a simplified
;;solution.  The idea is that if the java translation of a symbol is the
;;same as the symbol, we don't associate them as this indicates that the
;;symbol does not designate a linj type. Otherwise we associate it.

(defun build-java-symbol (category &key name &allow-other-keys)
  (let ((symbol
          (cond ((or (keywordp name)
                     (null (symbol-package name)) ;;gensyms
                     (member name '(+ - * / = < <= > >= 1-))) ;;some special names that pervert linj conventions.
                 name)
                ((member category '(linj-name unreserved-linj-name))
                 (read-from-string
                  (linj-name-to-java-name
                   (if (java-reserved-name-p name)
                       (concatenate 'string "_" (princ-to-string name))
                       (princ-to-string name)))))
                ((eq category 'linj-type-name)
                 (read-from-string (linj-name-to-java-type-name (princ-to-string name))))
                (t
                 (error "Which category is this ~A?" category)))))
    (unless (eq symbol name)
      (unless (or (null (get symbol :original-name))
                  (eq (get symbol :original-name) name))
        (cerror "If continued, forget old linj name"
                "Same java symbol ~A designating different linj names ~A and ~A"
                symbol (get symbol :original-name) name))
      (setf (get symbol :original-name) name))
    symbol))

(defun linj-original-name (java-symbol)
  (or (get java-symbol :original-name)
      java-symbol))

;;The ast-node-form for objects of unknown type:

(defmethod ast-node-form ((e t))
  e)

(defmethod ast-node-form ((e symbol))
  (linj-original-name e))

(defun unlinj-name (stream identifier colonp atp)
  (declare (ignore colonp atp))
  (format stream
          (if (keywordp identifier)
              (linj-name-to-java-name (princ-to-string identifier))
              (princ-to-string identifier))))

(defun unlinj-type (stream name colonp atp)
  (declare (ignore colonp atp))
  (format stream "~{~A~^.~}" (split-at-char (princ-to-string name) #\/)))

(def-syntax linj-name ()
  (?is ?name name-symbol-p)
  :constructor build-java-symbol)

(defun name-symbol-or-null-p (obj)
  (and (symbolp obj)
       (or (eq obj 'null)
           (not (literal-expression-p obj)))))

(def-syntax linj-type-name ()
  (?is ?name name-symbol-or-null-p)
  :constructor build-java-symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unreserved-name-symbol-p (obj)
  (and (name-symbol-p obj)
       (unreserved-word-p obj)))

(def-syntax unreserved-linj-name ()
  (?is ?name unreserved-name-symbol-p)
  :constructor build-java-symbol)

(defmethod (setf ast-node-parent) (val (obj symbol)) val)


;;Let's introduce some caching

(defclass cached-declaration ()
  ((decl :accessor cached-declaration :initform nil)))

(defmethod find-declaration :around ((e cached-declaration))
  (when (null (cached-declaration e))
    (setf (cached-declaration e) (call-next-method)))
  (cached-declaration e))

;;Regular references include everything
(def-syntax reference (expression cached-declaration)
  ?name/linj-name)

;;Sometimes, specially in partial parses, it is useful to declare the type of a reference
(defparameter *variable-type-separator* #\/)

(defun name&type-p (name)
  (and (symbolp name)
       (let ((str (princ-to-string name)))
         (let ((pos (position *variable-type-separator* str)))
           (and pos
                (> pos 0)
                (< pos (1- (length str))))))))

(defun extract-name&type (name)
  (let ((name&type-str (princ-to-string name)))
    (let ((position (position *variable-type-separator* name&type-str)))
      (values (read-from-string (subseq name&type-str 0 position))
              ;;;HACK the "<" and ">"  are here just for legacy reasons
              ;;;(the defclass form needs it to distinguish between old linj forms and the proper CLOS forms)
              (read-from-string (concatenate 'string "<" (if position (subseq name&type-str (1+ position)) "object") ">"))))))

(def-syntax typed-reference (reference)
  (?is ?name name&type-p)
  :constructor make-typed-reference)

(defun make-typed-reference (category &key original-form name)
  (declare (ignore category))
  (multiple-value-bind (name type)
      (extract-name&type name)
    (let ((ref (make-instance 'reference
                              :original-form original-form
                              :name (parse name 'linj-name))))
      (setf (cached-declaration ref)
            (make-instance 'uninitialized-variable-declaration
                           :original-form original-form
                           :name (parse name 'linj-name)
                           :type (parse type 'type-reference)))
      ref)))

;;Some simple recognizers
(defmacro def-reference-predicate (name predicate)
  `(defun ,name (object)
     (and (reference-p object)
          (,predicate (reference-name object)))))

(def-reference-predicate this-reference-p this-value-p)
(def-reference-predicate super-reference-p super-value-p)
(def-reference-predicate keyword-reference-p keywordp)

(defmethod printable-representation ((e t))
  e)

(defmethod printable-representation ((e reference))
  (reference-name e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Here is the protocol necessary to infer the type information and
;;manipulate the code:

;;; find-declaration is applicable to
;;;  - reference
;;;      returns the variable declaration searching the containing
;;;      environments for a lexical declaration
;;;  - type-reference
;;;      returns the type declaration (using get-type-declaration)
;;;  - method-call-expression
;;;      returns the method declaration by first locating the type
;;;      declaration of the method call receiver (using
;;;      get-type-declaration) and then searching in the declaration for
;;;      the intended method
;;;  - allocation-expression
;;;      returns the constructor declaration by first locating the type
;;;      declaration of the expression (using get-type-declaration) and
;;;      then searching in the declaration for the intended constructor
;;;  - slot-value-reference
;;;      returns the slot declaration by first locating the type
;;;      declaration (using get-type-declaration) and then searching in
;;;      the declaration for the intended slot


;;; get-type-declaration is applicable to
;;;  - expression
;;;      returns get-type-declaration of get-type of expression
;;;  - reference
;;;      returns deals with this and super and reverts to the expression case otherwise
;;;  - class-or-interface-type-reference
;;;      returns from cache or reads from file, generates error on
;;;      primitive-type-reference and array-type-reference


;;; get-type is applicable to
;;;  - literal
;;;      returns the type of the literal (boolean, string, etc, a type-reference)
;;;  - reference
;;;      returns the application of get-type to the find-declaration of the reference
;;;  - type-reference
;;;      returns the type-reference
;;;  - conditional-expression
;;;      checks arms type consistency using get-type and returns one of them
;;;  - explicit-type-node
;;;      returns the explicitely given type
;;;  - inferred-type-node (such as variable-declaration, vector-initialization, method-declaration)
;;;      uses infer-type to compute the type and caches the result
;;;  - operator-expression
;;;      either returns explicit type or computes a type based on type promotion
;;;  - return-statement
;;;      returns the type of the return statement expression
;;;  - array-expression
;;;      returns a composed array-type-reference built based on the array
;;;      subtype and number of dimensions
;;;  - array-reference
;;;      returns a type-reference obtained through the decomposition of
;;;      the array-reference expression and the number of indexes
;;;  - expression-statement
;;;      returns the type of the expression
;;;  - slot-declaration
;;;      returns the type of the variable declaration
;;;  - method-call-expression
;;;      finds the declaration and returns the get-type of the method-declaration
;;;  - next-method-call-expression
;;;      returns the type of the containing method
;;;  - slot-value-reference
;;;      finds the declaration and returns the get-type of the slot-declaration
;;;  - type-declaration
;;;      returns the type declaration name
;;;  - tuple-expression
;;;      returns an object[]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun strict-find-declaration (e)
  (let ((decl (find-declaration e)))
    (or decl
        (linj-error "Couldn't find declaration for ~S" e))))

(defmethod find-declaration ((e reference))
  (if (this-reference-p e)
      ;;should check if within non-static method
      (containing-type-declaration e)
      (find-declaration-in e (ast-node-parent e))))

;;Escaped from ast-tree
(defmethod find-declaration-in ((what t) (e null))
  nil)

(defmethod find-declaration-in ((what t) (e linj-node))
  (find-declaration-in what (ast-node-parent e)))

;;type

(defmethod get-type ((e reference))
  (cond ((super-reference-p e);;this is an optimization that avoids loading the superclass
         (or (class-declaration-superclass (containing-type-declaration e))
             (error "No superclass usable for class ~W"
                    (type-declaration-name (containing-type-declaration e)))))
        (t
         (get-type (strict-find-declaration e)))))

;;Generic expressions have a type and, consequently, a type declaration
(defmethod get-type-declaration ((e expression))
  (get-type-declaration (get-type e)))

(defmethod get-type-declaration ((e reference))
  (cond ((this-reference-p e)
         (containing-type-declaration e))
        ((super-reference-p e)
         (let ((super (class-declaration-superclass (containing-type-declaration e))))
           (or (and super
                    (get-type-declaration super))
               (error "No superclass usable for class ~W"
                      (type-declaration-name (containing-type-declaration e))))))
        (t
         (call-next-method))))

;;unparse

(defparameter *trust-references-p* nil)

(def-unparse reference (e)
  (cond ((or *trust-references-p* (this-reference-p e))
         (format t "~/unlinj-name/" (reference-name e)))
        ((alias-this-reference-p e)
         (format t "this"))
        (t
         ;;references to different types (than the one where the symbol occurs) must be qualified
         (let ((decl (find-declaration e)))
           (if (slot-declaration-p decl)
               (let ((current-type-declaration (containing-type-declaration e))
                     (real-type-declaration (containing-type-declaration decl)))
                 (if (eq current-type-declaration real-type-declaration) ;;normal slot reference?
                     (format t "~/unlinj-name/" (reference-name e))
                     (let ((outer (containing-type-declaration current-type-declaration)))
                       (if outer ;;it's an inner-class, check for shadowing
                           (let ((other-decl (find-declaration-in e outer)))
                             (if (and other-decl (not (eq other-decl decl))) ;;ambiguous,  must add "this."
                                 (format t "this.~/unlinj-name/" (reference-name e))
                                 (if (contained-type-declaration-p current-type-declaration real-type-declaration)
                                     (format t "~/unlinj-name/" (reference-name e))
                                     (format t "~/ppexp/.~/unlinj-name/"
                                             (with-parent ((ast-node-parent e))	;;So that the type reference belongs to the correct tree
                                               (copy-type (get-type real-type-declaration)))
                                             (reference-name e)))))
                           ;;is it an inherited slot?
                           (if (find-declaration-in e current-type-declaration)
                               (format t "~/unlinj-name/" (reference-name e))
                               ;;it must be a foreign slot
                               (format t "~/ppexp/.~/unlinj-name/"
                                       (with-parent ((ast-node-parent e))	;;So that the type reference belongs to the correct tree
                                         (copy-type (get-type real-type-declaration)))
                                       (reference-name e)))))))
               (format t "~/unlinj-name/" (reference-name e)))))))

(defun contained-type-declaration-p (decl-in decl-out)
  (let ((cont-decl-in (containing-type-declaration decl-in)))
    (and cont-decl-in
         (or (eq cont-decl-in decl-out)
             (contained-type-declaration-p cont-decl-in decl-out)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;We need some reference type descriptions provided in the standard package ("java" "lang")
(define-type-expansion object <java.lang.Object>)
(define-type-expansion string <java.lang.String>)
(define-type-expansion string-buffer <java.lang.StringBuffer>)

(define-type-expansion int-wrapper <java.lang.Integer>)
(define-type-expansion long-wrapper <java.lang.Long>)
(define-type-expansion boolean-wrapper <java.lang.Boolean>)
(define-type-expansion float-wrapper <java.lang.Float>)
(define-type-expansion double-wrapper <java.lang.Double>)
(define-type-expansion char-wrapper <java.lang.Character>)

(define-type-expansion number-wrapper <java.lang.Number>)

;;Some array types

(define-type-expansion tuple <java.lang.Object[]>)
(define-type-expansion string-array <java.lang.String[]>)

;;This isn't exactly correct but...
(define-type-expansion null <java.lang.null>)

;;For generic arthmetic (including fractions)
(define-type-expansion bignum <linj.bignum>)

;;and for not so generic...
(define-type-expansion big-integer <java.math.BigInteger>)
(define-type-expansion big-decimal <java.math.BigDecimal>)

;;and even less generic...
(define-type-expansion math <java.lang.Math>)

;;Conses
(define-type-expansion cons <linj.cons>)

;;Symbols
(define-type-expansion symbol <linj.symbol>)

;;imports
(define-type-expansion java.lang.* java.lang.*)

;;enumerations/iterations
(define-type-expansion enumeration <java.util.Enumeration>)
(define-type-expansion iterator <java.util.Iterator>)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;A very special type:

(define-type-expansion cyclic <java.lang.CyclicType>)

(defun unknown-type ()
  (parse :infer-it 'type-reference))
