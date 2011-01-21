;; -*- Mode: Common-Lisp; Syntax: Common-Lisp; Package: LINJ; Base: 10 -*-

;;; Copyright (C) Antonio Menezes Leitao Created on Fri Oct 13 09:30:05 2000
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

;;14.8 Expression Statements
;; Certain kinds of expressions may be used as statements by following them with semicolons:

;; ExpressionStatement:
;; 	StatementExpression ;

;; StatementExpression:
;; 	Assignment
;; 	PreIncrementExpression
;; 	PreDecrementExpression
;; 	PostIncrementExpression
;; 	PostDecrementExpression
;; 	MethodInvocation
;; 	ClassInstanceCreationExpression

;;Let's define the statement-expression category as a subcategory of expression:
(def-category statement-expression (expression)
  ())

;;Now, the expression-statement as a subcategory of a statement:
(def-syntax expression-statement (statement)
  ;;  ?expression/statement-expression)
  ;;We don't use the above definition because we want to infer return
  ;;statements.  This forces us to accept any expression as
  ;;expression-statement and only latter we might check whether all
  ;;expression-statements-expressions are, in fact, statement-expressions.
  ?expression/expression)

(defmethod get-type ((e expression-statement))
  (get-type (expression-statement-expression e)))

(def-unparse expression-statement (e)
  (format t "~:/ppexp/;" (expression-statement-expression e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Assignments and such:

(def-syntax setf-expression (statement-expression)
  (setf ?l-value/expression ?r-value/expression))

;;The type of an assignment is the type of the r-value in Common Lisp
;;and the type of the l-value in Java.  In the past, I decided to
;;treat it as void but now I prefer to approach the Common Lisp way.
;;Note that this requires the type inferencer to treat void as an
;;absorvent element in type-merges.

(defmethod get-type ((e setf-expression))
  (get-type (setf-expression-l-value e)))

(defparameter *check-assignments-to-closure-variables*  t)

(defun check-not-final-variable (var)
  (when (and *check-assignments-to-closure-variables* (reference-p var))
    (let ((decl (find-declaration var)))
      (when (and (variable-declaration-p decl)
		 (get-option :final decl))
	(linj-error "Can't assign a value to the variable ~A that is accessed in a closure" var)))))

(defmethod visit-current ((e setf-expression) (visitor cast-and-wrap))
  (check-not-final-variable (setf-expression-l-value e))
  (setf (setf-expression-r-value e)
	(convert-type-to-lvalue (setf-expression-l-value e) (setf-expression-r-value e))))

;; 5.2 Assignment Conversion
;; Assignment conversion occurs when the value of an expression is
;; assigned (§15.26) to a variable: the type of the expression must be
;; converted to the type of the variable. Assignment contexts allow
;; the use of an identity conversion (§5.1.1), a widening primitive
;; conversion (§5.1.2), or a widening reference conversion
;; (§5.1.4). In addition, a narrowing primitive conversion may be used
;; if all of the following conditions are satisfied:
;;     * The expression is a constant expression of type byte, short,
;;       char or int.
;;     * The type of the variable is byte, short, or char.
;;     * The value of the expression (which is known at compile time,
;;       because it is a constant expression) is representable in the
;;       type of the variable.

(defun assignment-convertion (value type error-fn)
  (let ((val-type (get-type value)))
    (if (literal-p value)
      (if (or (byte-type-p val-type)
	      (short-type-p val-type)
	      (char-type-p val-type)
	      (int-type-p val-type))
	(let ((rval (literal-value value)))
	  (flet ((bogus ()
		   (error "Trying to convert the literal value ~A to the type ~A that doesn't contain it" rval type)))
	    (cond ((byte-type-p type) (if (byte-value-p rval) value (bogus)))
		  ((short-type-p type) (if (short-value-p rval) value (bogus)))
		  ((char-type-p type) (if (char-value-p rval) value (bogus)))
		  (t (cast-and-wrap-if-needed type val-type value error-fn)))))
	(cast-and-wrap-if-needed type val-type value error-fn))
      (cast-and-wrap-if-needed type val-type value error-fn))))

(defun convert-type-to-lvalue (lvalue rvalue)
  (assignment-convertion
   rvalue
   (get-type lvalue)
   #'(lambda (expected current expr)
       (linj-error "Can't assign variable ~S of type ~S~%with value ~S of type ~S"
		   lvalue
		   expected
		   expr
		   current))))

(def-unparse setf-expression (e)
  (when *print-out-parenthesis*
    (format t "("))
  (format t "~/pp/ = ~:/ppexp/" 
	  (setf-expression-l-value e)
	  (setf-expression-r-value e))
  (when *print-out-parenthesis*
    (format t ")")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-category in-form ()
  ((object :reader in-form-object)))

(defmethod visit :before ((e in-form) (visitor adjust-parents))
  ;;Correct context for the initializer to jump over the in-expression
  (setf (ast-node-parent (in-form-object e)) (ast-node-parent e)))

(defmethod find-declaration-in ((what t) (e in-form))
  (or (find-declaration-in what (get-type-declaration (get-type (in-form-object e))))
      (find-declaration-in what (ast-node-parent e))))

(def-syntax in-expression (in-form expression)
  (in ?object/expression ?expression/expression))

(def-syntax in-type-expression (in-expression)
  (in (the ?object/type-reference) ?expression/expression))

(defmethod get-type ((e in-expression))
  (get-type (in-expression-expression e)))

(def-unparse in-expression (e)
  (unparse-object (in-expression-expression e) *standard-output*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-syntax in-statement (in-form statement)
  (in ?object/expression . ?body/statement-list))

(def-syntax in-type-statement (in-statement)
  (in (the ?object/type-reference) . ?body/statement-list))

(def-unparse in-statement (e)
  (format t "~{~/ppblk/~^~:@_~}"
	  (statement-list-elements (in-statement-body e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun collect-upward-in-forms (node)
  (cond ((or (static-block-p node)
	     (method-declaration-p node)
	     (slot-declaration-p node))
	 (list))
	((in-expression-p node)
	 (cons (in-expression-object node)
	       (collect-upward-in-forms (ast-node-parent node))))
	((in-statement-p node)
	 (cons (in-statement-object node)
	       (collect-upward-in-forms (ast-node-parent node))))
	(t
	 (collect-upward-in-forms (ast-node-parent node)))))

;;if
(def-syntax if-statement (statement)
  (if ?test/expression ?then/statement ?else/statement)
  :strict-p t)

;;In fact, this will become a little more complicated because we want to
;;combine multiple ifs. The rule is that when the else statement is another
;;if-statement, then we print this as "else if ..." and not as "else { if ... }"

(defparameter *translate-reference-type-to-boolean* t)

(defun from-reference-type-to-boolean (expr)
  (if *translate-reference-type-to-boolean*
    (let ((type (get-type expr)))
      (cond ((boolean-type-p type)
	     expr)
	    ((cons-type-p type)
	     (with-parent ((ast-node-parent expr))
	       (parse `(not (endp ,expr)) 'expression)))
	    ((class-or-interface-type-reference-p type)
	     (with-parent ((ast-node-parent expr))
	       (parse `(not (eq ,expr null)) 'expression)))
	    (t
	     (error "Can't use expression ~A in a context where a boolean is expected" expr))))
    expr))

(def-unparse if-statement (e)
  (format t
	  (cond ((and (if-statement-p (ast-node-parent e))
		      (or (if-statement-p (if-statement-else e))
			  (cond-statement-p (if-statement-else e))))
		 "if (~:/ppexp/) ~/ppstm/ else ~/pp/")
		((and (if-statement-p (ast-node-parent e))
		      (eq (if-statement-else (ast-node-parent e)) e))
		 "if (~:/ppexp/) ~/ppstm/ else ~/ppstm/")
		((or (if-statement-p (if-statement-else e))
		     (cond-statement-p (if-statement-else e)))
		 "~@<if (~:/ppexp/) ~/ppstm/ else ~/pp/~:>")
		(t
		 "~@<if (~:/ppexp/) ~/ppstm/ else ~/ppstm/~:>"))
	  (from-reference-type-to-boolean (if-statement-test e))
	  (if-statement-then e)
	  (if-statement-else e)))

;;cond
(def-syntax clause (linj-node)
  (?test/expression . ?body/statement-list)
  :strict-p t)

(def-unparse clause (e)
  (cond ((true-literal-p (clause-test e))
 	 (format t "~/ppblk/" (clause-body e)))
 	(t
	 (format t "if (~:/ppexp/) ~/ppblk/"
		 (from-reference-type-to-boolean (clause-test e))
		 (clause-body e)))))

(def-list-syntax clause-list (linj-list-node) clause)

(def-unparse clause-list (e)
  (let ((up (ast-node-parent (ast-node-parent e))))
    (format t (if (or (and (if-statement-p up)
			   (eq (if-statement-else up) (ast-node-parent e)))
		      (cond-statement-p up))
		"~/pp/~{ else ~/pp/~}"
		"~@<~/pp/~{ else ~/pp/~}~:>")
	    (first (clause-list-elements e))
	    (rest (clause-list-elements e)))))

(def-syntax cond-statement (statement)
  (cond . ?clauses/clause-list)
  :strict-p t)

(def-unparse cond-statement (e)
  (unparse-object (cond-statement-clauses e) *standard-output*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;conditional expression
(def-syntax conditional-expression (expression)
  (if ?test/expression ?then/expression ?else/expression))

;;; The type of a conditional expression is determined as follows:

;;;  If the second and third operands have the same type (which may be the
;;; null type), then that is the type of the conditional expression.

;;;  Otherwise, if the second and third operands have numeric type, then
;;; there are several cases:

;;;    If one of the operands is of type byte and the other is of type
;;; short, then the type of the conditional expression is short.

;;;    If one of the operands is of type T where T is byte, short, or char,
;;; and the other operand is a constant expression of type int whose value
;;; is representable in type T, then the type of the conditional expression
;;; is T.

;;;    Otherwise, binary numeric promotion (§5.6.2) is applied to the
;;; operand types, and the type of the conditional expression is the
;;; promoted type of the second and third operands. Note that binary
;;; numeric promotion performs value set conversion (§5.1.8).

;;;    If one of the second and third operands is of the null type and the
;;; type of the other is a reference type, then the type of the conditional
;;; expression is that reference type.

;;;    If the second and third operands are of different reference types,
;;; then it must be possible to convert one of the types to the other type
;;; (call this latter type T) by assignment conversion (§5.2); the type of
;;; the conditional expression is T. It is a compile-time error if neither
;;; type is assignment compatible with the other type.

(defmethod get-type ((e conditional-expression))
  (let ((then (conditional-expression-then e))
	(else (conditional-expression-else e)))
    (assert (not (and (null-literal-p then)
		      (null-literal-p else))))
    (let ((type-then (get-type then))
	  (type-else (get-type else)))
      (merge-types type-then type-else))))

;;I guess there's a problem here due to type promotions.  When both then
;;and else are char, the return type is char? It should, but...
(def-unparse conditional-expression (e)
  (format t (if *print-out-parenthesis*
	      "(~@<(~:/ppexp/) ? ~_~/pp/ : ~_~/pp/~:>)"
	      "~@<(~:/ppexp/) ? ~_~/pp/ : ~_~/pp/~:>")
	  (from-reference-type-to-boolean (conditional-expression-test e))
	  (conditional-expression-then e)
	  (conditional-expression-else e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;We also have a 'cond' as an expression (this is beautifull) as long as we
;;convert it to an 'if'. 
;;Note that we cannot translate clauses with multiple consequents because
;;Java does not have the C/C++ comma operator.

(def-transform expression (cond (t ?expression))
  ?expression)

(def-transform expression (cond (?test ?expression) . ?rest-clauses)
  (if ?test
    ?expression
    (cond . ?rest-clauses)))

(def-transform expression (cond (t ?expression1 ?expression2 . ?more-expressions))
  (progn ?expression1 ?expression2 . ?more-expressions))

(def-transform expression (cond (?test ?expression1 ?expression2 . ?more-expressions) . ?rest-clauses)
  (if ?test
    (progn
      ?expression1 ?expression2 . ?more-expressions)
    (cond . ?rest-clauses)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Let's continue.
;;when
(def-syntax when-statement (statement) 
  (when ?test/expression . ?body/statement-list)
  :strict-p t)

(def-unparse when-statement (e)
  (format t "~@<if (~:/ppexp/) ~/ppblk/~:>"
	  (from-reference-type-to-boolean (when-statement-test e))
	  (when-statement-body e)))

;;unless
(def-transform statement (unless ?test . ?body)
  (when (not ?test) . ?body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass type-node ()
  ())

;;Some nodes have an explicit (or easily determinable type)
;;Let's create a class just for those nodes

(defclass explicit-type-node (type-node)
  ((type :accessor explicit-type-node-type :initarg :type)))

(defmethod get-type ((e explicit-type-node))
  (explicit-type-node-type e))


;;In other cases, the type might be inferred

(defclass inferred-type-node (type-node)
  ((type :accessor inferred-type-node-type :initform (unknown-type) :initarg :type)))

(defmethod get-type ((e inferred-type-node))
  (assert (not (null (inferred-type-node-type e)))) ;;I suspect this can't happen now
  (when (unknown-type-reference-p (inferred-type-node-type e))
    (setf (inferred-type-node-type e) (with-parent (e) (copy-type (infer-type e)))))
  (inferred-type-node-type e))

;;;;;Let's do this now so that the errors are presented before
;;;unparsing HACK!!!!!!  This __MUST__ be done now, otherwise terrible
;;;bugs show up.  This must be studied in detail to understand what's
;;;happening.  I'm really afraid there are some nasty bugs waiting to
;;;show up.
(defmethod visit :after ((e inferred-type-node) (v parse-tree-finish))
  (get-type e))
;;HACK: shouldn't be doing this on around methods belonging to interfaces because it might need type-inference over call-next-method and this isn't defined!

;;let-bindings and such:
(def-category variable-declaration (declaration-options inferred-type-node linj-node)
  ((name :accessor variable-declaration-name :initarg :name)
   (initializer :accessor variable-declaration-initializer :initform nil :initarg :initializer)))

(defun supplied-p (var)
  (get-option :supplied-p var))

(defun (setf supplied-p) (val var)
  (setf (get-option :supplied-p var) val))

(defmethod infer-type ((e variable-declaration))
  (get-type (variable-declaration-initializer e)))

(defun initialized-variable-declaration-p (param)
  (not (null (variable-declaration-initializer param))))

(defmethod visit-current ((e variable-declaration) (visitor cast-and-wrap))
  (when (variable-declaration-initializer e)
    (setf (variable-declaration-initializer e)
	  (with-parent (e) ;;initializer doesn't have auto-parent
	    (convert-to-type (variable-declaration-initializer e) (get-type e) :force-cast nil)))))

(def-unparse variable-declaration (e)
  (format t "~@<~:[~;final ~]~/pp/ ~/unlinj-name/~:[~; = ~4I~_~:/ppexp/~I~]~:>"
	  (get-option :final e)
	  (get-type e)
	  (variable-declaration-name e)
	  (and *print-out-initializer* 
	       (not (null (variable-declaration-initializer e))))
	  (variable-declaration-initializer e)))


;;The syntaxes for variable declarations (mainly parameters) are:
;; (defun xpto (a b/int (c 1) (d/long 1))
;;   ...)

;; The possibilities are:
;; linj-name                             - untyped, uninitialized variable
;; linj-name/type-reference              - typed, uninitialized variable
;; (linj-name expression)                - untyped, initialized variable
;; (linj-name/type-reference expression) - typed, initialized variable

;;I'm not sure about the last form.  It can be trivially implemented as
;; (linj-name (the type-reference expression))
;;thus reducing the number of cases to three.

;;However, there's a different decomposition which is more ortogonal, namely
;; var-or-var/type
;; (var-or-var/type expression)
;;where var-or-var/type is
;; linj-name
;; linj-name/type-reference

;;This suggests a more structured variable declaration.  Unfortunately, I'm afraid
;;it would break a lot of code so I'll stick with the first four possibilities

;;complete with type name and initializer
(def-syntax full-variable-declaration (variable-declaration)
  (?name/linj-name (the ?type/type-reference ?initializer/expression)))

;;lacking initializer
(def-syntax uninitialized-variable-declaration (variable-declaration)
  (?name/linj-name (the ?type/type-reference)))

;;lacking initializer but with special syntax for type declaration

(def-syntax short-uninitialized-variable-declaration (variable-declaration)
  (?is ?name name&type-p)
  :constructor make-short-uninitialized-variable-declaration)

(defun make-short-uninitialized-variable-declaration (category &key original-form name)
  (declare (ignore category))
  (multiple-value-bind (name type)
      (extract-name&type name)
    (make-instance 'uninitialized-variable-declaration
		   :original-form original-form
		   :name (parse name 'linj-name)
		   :type (parse type 'type-reference))))

;;lacking type
;;Types might be computed by our "type inferencer"
(def-syntax untyped-variable-declaration (variable-declaration)
  (?name/linj-name ?initializer/expression)
  :components (?type/type-reference))

(def-syntax short-untyped-variable-declaration (untyped-variable-declaration)
  ((?is ?name name&type-p) ?initializer/expression)
  :constructor make-short-untyped-variable-declaration
  :components (?type/type-reference))

(defun make-short-untyped-variable-declaration (category &key original-form name initializer)
  (declare (ignore category))
  (multiple-value-bind (name type)
      (extract-name&type name)
    (make-instance 'untyped-variable-declaration
		   :original-form original-form
		   :name (parse name 'linj-name)
		   :type (parse type 'type-reference)
		   :initializer (parse initializer 'expression))))


;;lacking type and initializer
(def-syntax untyped-uninitialized-variable-declaration (variable-declaration)
  ?name/linj-name
  :components (?type/type-reference))

(defmethod infer-type ((e untyped-uninitialized-variable-declaration))
  ;;There are several ways to infer the type of a function or method parameter:

  ;; - infer its type from the operations done with the parameter.  This
  ;;   might be problematic in the presence of polimorfic operators such as
  ;;   the + or = operators that can deal with int our double

  ;; - look to a method call and identify what type of objects are being
  ;;   passed as arguments to that parameter.
  ;; 
  ;;We can try both venues.  The idea is that a variable might have a set
  ;;of potential types that, at code generation time, must be resolved to a
  ;;singleton.
  ;;For the moment, I'll use just the first approach
;;  (object-type))
  (setf (inferred-type-node-type e) (object-type))
  (let ((meth (containing-method-declaration e)))
    (when meth
      (let ((meth-inferred-type (inferred-type-node-type meth)))
	(let ((type (get-type meth)))
	  (unless (or (cyclic-type-p type) (void-type-p type))
	    (force-type meth type))
	  ;;Now, check if all my uses have a lower common type in casts expressions
	  (setf (inferred-type-node-type e)
		(let ((casts (collect-all-type-info-for-references-to meth e)))
		  (cond ((endp casts) ;;No casts, (object-type) is OK
			 (object-type))
			((endp (rest casts)) ;;Just one. Use it
			 (first casts))
			(t
			 (block common-sub-type
			   (reduce #'(lambda (type1 type2)
				       (cond ((super-type-p type1 type2)
					      type2)
					     ((super-type-p type2 type1)
					      type1)
					     (t	;;No good common subtype
					      (return-from common-sub-type (object-type)))))
				   casts)))))))
	(setf (inferred-type-node-type meth) meth-inferred-type))))
  (inferred-type-node-type e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;We will restrict bindings so that they only accept variable declarations
;;with initialization forms.  If you want a non-initialized reference,
;;write (the <???> null)

(def-list-syntax bindings (linj-list-node) untyped-variable-declaration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;I use a nice trick to overcome the stupid java limitation regarding shadowing:
;;names have a trailing number that expresses its shadowing level.

(defun shadowing-level (name node &optional (level 0))
  (let ((ref (make-instance 'reference
			    :name name
			    :parent (ast-node-parent (ast-node-parent node)))))
    (labels ((shadow-level (e)
	       (let ((decl (find-declaration-in ref e)))
		 (cond ((null decl)
			0)
		       ((slot-declaration-p decl)
			level) ;;slots can be shadowed
		       ((and (parameter-list-p (ast-node-parent decl))
			     (method-declaration-p (ast-node-parent (ast-node-parent decl))))
			1) ;;parameters can't be shadowed but there's no need to look further up
		       (t ;;everything else (let, for, etc)
			(1+ (shadow-level (ast-node-parent (ast-node-parent (ast-node-parent decl))))))))))
      (shadow-level (ast-node-parent (ast-node-parent (ast-node-parent node)))))))

(defclass collect-references-to ()
  ((declaration :initarg :declaration :reader get-declaration)
   (references :initform (list) :accessor get-references)))

(defmethod visit :before ((ref reference) (visitor collect-references-to))
  (when (eq (find-declaration ref) (get-declaration visitor))
    (push ref (get-references visitor))))

(defun all-references-to (var-decl &optional (within (ast-node-parent (ast-node-parent var-decl))))
  (let ((visitor (make-instance 'collect-references-to :declaration var-decl)))
    (visit within visitor)
    (get-references visitor)))

;;HACK: this is operating too soon, preventing semantic macros that rebind variables to work properly.  Consider the example:
;;(for-each (e l)
;;  (for-each (e e)
;;    ..))
(defmethod visit :before ((e bindings) (visitor parse-tree-finish))
  (dolist (var (bindings-elements e))
    (let ((level (shadowing-level (variable-declaration-name var) var)))
      (when (> level 0)
	(let ((new-name (unshadowed-name (variable-declaration-name var) var)))
	  (dolist (ref (all-references-to var))
	    (setf (reference-name ref) new-name))
	  (setf (variable-declaration-name var) new-name))))))

(defun unshadowed-name (name node)
  (labels ((try (suf default-level)
	     (if (= (shadowing-level (conc-symbol name suf) node default-level) 0)
	       suf
	       (try (1+ suf) default-level))))
    (do* ((first-try (try 0 0) second-try)
	  (second-try (try first-try 1) (try first-try 1)))
	((eq first-try second-try) (conc-symbol name first-try)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-syntax let-statement (statement)
  (p-let ?bindings/bindings . ?body/statement-list)
  :strict-p t)

(def-macro-transform let-statement (let ?bindings . ?body)
  (multiple-value-bind (new-binds reordered-binds)
      (serialize-parallel ?bindings)
    (if new-binds
	`(p-let ,new-binds
	  (p-let ,reordered-binds 
	   (unlet-macro-transform ,(mapcar #'(lambda (bind)
					       `(expression ,(first bind)))
					   (append new-binds reordered-binds))
	    (p-let () ,@?body))))
	(if reordered-binds
	    `(p-let ,reordered-binds
	      (unlet-macro-transform ,(mapcar #'(lambda (bind)
						  `(expression ,(first bind)))
					      reordered-binds)
	       (p-let () ,@?body)))
	    `(p-let () ,@?body)))))

(defmethod visit :before ((e let-statement) (visitor adjust-parents))
  ;;Correct context for the initializers to jump over the let
  (dolist (bind (bindings-elements (let-statement-bindings e)))
    (setf (ast-node-parent (variable-declaration-initializer bind)) (ast-node-parent e))))

;;In Common Lisp, references inside bindings can't refer to the bindings
;;themselves and this suggests that the following method should be active:

; (defmethod find-declaration-in ((ref reference) (e bindings))
; (find-declaration-in ref (ast-node-parent (ast-node-parent e))))

;;This is necessary to deal with code like (let ((x (- x))) ...) without
;;infinite regression.

;;However, in Java, this rule isn't valid.  In fact, the specification says:

;; 14.4.2 Scope of Local Variable Declarations

;; The scope of a local variable declaration in a block (§14.4.2) is the
;; rest of the block in which the declaration appears, starting with its
;; own initializer (§14.4) and including any further declarators to the
;; right in the local variable declaration statement.

;; The name of a local variable v may not be redeclared as a local
;; variable of the directly enclosing method, constructor or initializer
;; block within the scope of v, or a compile-time error occurs. The name
;; of a local variable v may not be redeclared as an exception parameter
;; of a catch clause in a try statement of the directly enclosing method,
;; constructor or initializer block within the scope of v, or a
;; compile-time error occurs. However, a local variable of a method or
;; initializer block may be shadowed (§6.3.1) anywhere inside a class
;; declaration nested within the scope of the local variable.

;;All this implies that in order to translate the shadowing of let-bindings
;;or parameters, we need to rename the new bindings, that is, we should
;;translate (let ((x 1)) (let ((x (- x))) ...x...)) into something like 
;;(let ((x 1)) (let ((x1 (- x))) ...x1...)).  I'm not in the mood to
;;do this know.

(defmethod find-declaration-in ((ref reference) (e let-statement))
  (or (find (reference-name ref)
	    (bindings-elements (let-statement-bindings e))
	    :key #'variable-declaration-name)
      (call-next-method)))

;;Due to the fact that a statement-list is represented in Java using a
;;block and that blocks can introduce local variables (as long as they
;;don't shadow other variables - stupid language), we will define a special
;;(simplified) unparse for statement-lists composed by a unique let
;;statement.

;; (defclass references-p ()
;;   ((name :initarg :name :reader get-name)
;;    (exit :initarg :exit :reader get-exit)))

;; (defun unneded-braces-p (e)
;;   (let ((all-bindings (collect-all-bindings e)))
;;     (or (endp all-bindings)
;; 	(or (not (statement-list-p (ast-node-parent e)))
;; 	    (let ((next-statements (rest (member e (statement-list-elements (ast-node-parent e))))))
;; 	      (every #'(lambda (decl)
;; 			 (notany #'(lambda (statement)
;; 				     (references-p (variable-declaration-name decl) statement))
;; 				 next-statements))
;; 		     all-bindings))))))


(defun no-redeclarations-p (e out-bindings)
  (or (not (statement-list-p (ast-node-parent e)))
      (and (no-redeclarations-in-statement-list 
	    (rest (member e (statement-list-elements (ast-node-parent e)))) out-bindings)
	   (no-redeclarations-p (ast-node-parent (ast-node-parent e)) out-bindings))))

(defclass operate-bindings-visitor ()
  ((function :initarg :function :reader operate-binding-function)))

(defmethod visit :before ((bindings bindings) (visitor operate-bindings-visitor))
  (funcall (operate-binding-function visitor) (bindings-elements bindings)))

(defun no-redeclarations-in-statement-list (statements out-bindings)
  (visit statements
	 (make-instance
	  'operate-bindings-visitor
	  :function #'(lambda (bindings)
			(when (some #'(lambda (bind)
					(some #'(lambda (out-bind)
						  (eq (variable-declaration-name bind) (variable-declaration-name out-bind)))
					      out-bindings))
				    bindings)
			  (return-from no-redeclarations-in-statement-list nil)))))
  t)

(defun unneded-braces-p (e)
  (let ((out-bindings (bindings-elements (let-statement-bindings e))))
    (or (endp out-bindings)
	(no-redeclarations-p e out-bindings))))
	


;; (def-unparse let-statement (e)
;;   (let ((statements (statement-list-elements (let-statement-body e))))
;;     (if (not (endp statements)) ;;more than zero?
;;       (if (and *print-out-braces* (not (unneded-braces-p e)))
;; 	(format t "~@<{~4I~{~:@_~/pp/;~}~{~:@_~/ppblk/~}~:@_~:/ppblk/~I~:@_}~:>"
;; 		(bindings-elements (let-statement-bindings e))
;; 		(butlast statements)
;; 		(first (last statements)))
;; 	(format t "~@<~{~/pp/;~:@_~}~{~/ppblk/~:@_~}~:/ppblk/~:>"
;; 		(bindings-elements (let-statement-bindings e))
;; 		(butlast statements)
;; 		(first (last statements))))
;;       (if (and *print-out-braces* (not (unneded-braces-p e)))
;; 	(format t "~@<{~4I~{~:@_~/pp/;~}~I~:@_}~:>"
;; 		(bindings-elements (let-statement-bindings e)))
;; 	(format t "~@<~{~/pp/;~:@_~}~:>"
;; 		(bindings-elements (let-statement-bindings e)))))))

(def-unparse let-statement (e)
  (let ((statements (statement-list-elements (let-statement-body e))))
    (if (not (endp statements)) ;;more than zero?
      (if (and *print-out-braces* (not (unneded-braces-p e)))
	(format t "~@<{~4I~{~:@_~/pp/;~}~{~:@_~/ppblk/~}~:@_~/ppblk/~I~:@_}~:>"
		(bindings-elements (let-statement-bindings e))
		(butlast statements)
		(first (last statements)))
	(format t "~@<~{~/pp/;~:@_~}~{~/ppblk/~:@_~}~/ppblk/~:>"
		(bindings-elements (let-statement-bindings e))
		(butlast statements)
		(first (last statements))))
      (if (and *print-out-braces* (not (unneded-braces-p e)))
	(format t "~@<{~4I~{~:@_~/pp/;~}~I~:@_}~:>"
		(bindings-elements (let-statement-bindings e)))
	(format t "~@<~{~/pp/;~:@_~}~:>"
		(bindings-elements (let-statement-bindings e)))))))

;; (defmethod visit :before ((ref reference) (visitor references-p))
;;   (when (eq (reference-name ref) (get-name visitor))
;;     (funcall (get-exit visitor))))

;; ;;This is not strictly a reference but it is also a situation where the let-statement can't be inlined.
;; (defmethod visit :before ((var variable-declaration) (visitor references-p))
;;   (when (eq (variable-declaration-name var) (get-name visitor))
;;     (funcall (get-exit visitor))))

;; (defun references-p (name node)
;;   (let ((visitor (make-instance 'references-p
;; 				:name name
;; 				:exit #'(lambda () (return-from references-p t)))))
;;     (visit node visitor)
;;     nil))

;;;;;;;;;;;;;;;;;;;;;;

(defclass collect-bindings-visitor ()
  ((bindings :initform (list) :accessor get-bindings)))

(defmethod visit :before ((var variable-declaration) (visitor collect-bindings-visitor))
  (push var (get-bindings visitor)))

(defun collect-all-bindings (node)
  (let ((visitor (make-instance 'collect-bindings-visitor)))
    (visit node visitor)
    (get-bindings visitor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-macro-transform let-statement (let* ?bindings . ?body)
  (if (endp ?bindings)
    `(let ,?bindings
       ,@?body)
    `(let (,(first ?bindings))
       (let* ,(rest ?bindings)
	 ,@?body))))

;;;;;;;;;;;;;;;;;;;;;

;;A very simple let-statement:
(def-transform let-statement (progn . ?body)
  (let () . ?body))

;;A very special case:
(def-transform expression (prog1 ?form (incf ?form))
  (post-incf ?form))


;;A not so simple let-statement:
(def-macro-transform let-statement (prog1 ?form . ?body)
  (with-new-names (results)
   `(let ((,results ,?form))
      ,@?body
      ,results)))

;;The equivalent form...
(def-transform let-statement (multiple-value-prog1 ?form . ?body)
  (prog1 ?form . ?body))

;;;prog2
(def-transform let-statement (prog2 ?form1 ?form2 . ?body)
  (progn ?form1 (prog1 ?form2 . ?body)))

;;;;;;;;;;;;;;;;;;;;;;;;

(def-list-syntax argument-list (linj-list-node) expression)

(defparameter *infer-principal-value* t)

(defun principal-value (arg)
  (if *infer-principal-value*
    (with-parent ((ast-node-parent arg))
      (cast-and-wrap-if-needed
       (get-principal-type arg)
       (get-type arg)
       arg
       #'(lambda (expected current expr)
	   (declare (ignore expected current expr))
	   (linj-error "Something weird happen with ~A" arg))))
    arg))

(def-unparse argument-list (e)
  (format t "(~@<~{~:/ppexp/~^, ~_~}~:>)"
	  (mapcar #'principal-value (argument-list-elements e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Another needed feature is generic iteration
;;We'll consider first a do-type form on top of which one can define
;;several other macros (including, presumably, the loop macro)

(def-syntax for-statement (statement)
  (for (?bindings/bindings ?test/expression ?updates/argument-list)
       . ?body/statement-list)
  :strict-p t)

(defmethod visit :before ((e for-statement) (visitor adjust-parents))
  ;;Correct context for the initializers to jump over the for
  (dolist (bind (bindings-elements (for-statement-bindings e)))
    (setf (ast-node-parent (variable-declaration-initializer bind)) (ast-node-parent e))))

(defmethod find-declaration-in ((ref reference) (e for-statement))
  (or (find (reference-name ref)
	    (bindings-elements (for-statement-bindings e))
	    :key #'variable-declaration-name)
      (call-next-method)))

(def-unparse for-statement (e)
  (if (and (null (bindings-elements (for-statement-bindings e)))
	   (null (argument-list-elements (for-statement-updates e))))
    (format t "~@<while (~:/ppexp/) ~/ppblk/~:>"
	    (from-reference-type-to-boolean (for-statement-test e))
	    (for-statement-body e))
    (format t "~@<for ~<(~{~/pp/~^, ~}; ~1I~_~:/ppexp/; ~_~{~:/ppexp/~^, ~_~})~:> ~/ppblk/~:>"
	    (list (bindings-elements (for-statement-bindings e))
		  (from-reference-type-to-boolean (for-statement-test e))
		  (argument-list-elements (for-statement-updates e)))
	    (for-statement-body e))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;An implementation for (block ?label . ?body)
;;An implementation for (return) (return ?expr) (return-from ?label) (return-from ?label ?expr)

;;;In Common Lisp
;;;do, do*, dolist, dotimes, simple loop, and complex loop
;;;establish implicit blocks (labeled nil)

;;;defun, defmethod, and the complex loop with the :named keyword
;;;establish implicit blocks (labelled the name of the definition)

;;;(block ?name . ?body) establishes an explicit block named ?name
;;;Moreover
;;;(return) = (return-from nil)
;;;(return ?expr) = (return-from nil ?expr)

(def-transform statement (return) (return-from nil))

(def-transform statement (return ?expr) (return-from nil ?expr))

;;;In Java
;;;A break statement with no label attempts to transfer
;;;control to the innermost enclosing switch, while, do, or for
;;;statement of the immediately enclosing method or initializer block;

;;;A break statement with label Identifier attempts to transfer
;;;control to the enclosing labeled statement that has the same
;;;Identifier as its label

;;There are special forms to break or 'continue' a block.  Common Lisp
;;doesn't contain them, but Linj does.  However, they can only be used
;;with a surrounding block.  Obviously, in the 'continue' case, just
;;like in Java, for it to take any effect, the block must contain just
;;one loop.

;;Note also that I don't expect that programmers will use the break
;;statement in Linj because the "equivalent" return-from statement is
;;available and it spares the return/break Java duality.

(def-transform statement (break) (break nil))

(def-syntax break-statement (statement)
  (break ?label/linj-name))

(def-unparse break-statement (e)
  (format t "break ~/unlinj-name/;" (break-statement-label e)))

(def-syntax empty-break-statement (break-statement)
  (break nil)
  :slots ((label :initform nil)))

(def-unparse empty-break-statement (e)
  (format t "break;"))

;;;;;;;;;;;;;;;;;;;;;;;;

(def-transform statement (continue) (continue nil))

(def-syntax continue-statement (statement)
  (continue ?label/linj-name))

(def-unparse continue-statement (e)
  (format t "continue ~/unlinj-name/;" (continue-statement-label e)))

(def-syntax empty-continue-statement (continue-statement)
  (continue nil)
  :slots ((label :initform nil)))

(def-unparse empty-continue-statement (e)
  (format t "continue;"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Now, let's deal with processing all this return/break/continue mess.

;;First, we, create a special statement that will be used to implement
;;the proper Java return statement.  Note that this statemtent (and
;;its subclasses) cannot be present in any Linj file and it is only
;;generated as the result of the compilation process
(def-category return-statement (statement)
  ((expression :accessor return-statement-expression :initform nil)))

(defmethod get-type ((e return-statement))
  (if (null (return-statement-expression e))
    (void-type)
    (get-type (return-statement-expression e))))

(def-unparse return-statement (e)
  (format t "return~@[ ~:/ppexp/~];"
	  (return-statement-expression e)))

;;Now, we create two varieties (the void and non-void return statement)
;;;HACK:: This must be changed
(def-syntax non-void-return-statement (return-statement)
  (cannot-parse-this-return ?expression/expression)
  :strict-p t)

(def-syntax void-return-statement (return-statement)
  (cannot-parse-this-return)
  :strict-p t)

;;Now, let's specify the Linj statements.  We can write (return),
;;(return ...), (return-from ...), and (return-from ... ...) and they
;;will all be parsed as just one type of return, namely the
;;return-from-statement.  Some of these 'returns' have the containing
;;method as target and these will be transformed into the above
;;return-statements.  The remaining ones should be transformed into
;;break statements.

(def-syntax return-from-statement (statement)
  (return-from ?label/linj-name &optional ?expression/expression))

;;The empty-label return-from statement
(def-syntax empty-return-from-statement (return-from-statement)
  (return-from nil &optional ?expression/expression)
  :slots ((label :initform nil)))

(defmethod return-from-method-p ((e return-from-statement))
  (let ((block (containing-block-named e (return-from-statement-label e))))
    (eq block (containing-method-declaration e))))

;;We will implement another pass to transform all return statements.

(defclass process-return-from-statements ()
  ())

(defmethod visit :before ((stat return-from-statement) (visitor process-return-from-statements))
  (if (return-from-method-p stat)
    (error "Something wrong happened during inference of method exit points for ~A" stat)
    (if (null (return-from-statement-expression stat))
      (change-class stat 'break-statement)
      (error "Can't return a value to an inner block from return ~A" stat))))

;;After this pass and after the exit point inference, there should not
;;be any more return-from statements.
(def-unparse return-from-statement (e)
  (error "The ~A return-from-statement must have been translated into
a return-statement or into a break statement" e))

(defmethod get-type ((e return-from-statement))
  (if (null (return-from-statement-expression e))
    (void-type)
    (get-type (return-from-statement-expression e))))

;;Linj contains something very similar to a Common Lisp block:

(def-syntax block-statement (statement)
  (block ?name/linj-name . ?body/statement-list))

;;And nil blocks (we need extra syntax as nil is never parsable as itself (and linj-names are symbols))
(def-syntax nil-block-statement (block-statement)
  (block nil . ?body/statement-list)
  :slots ((name :initform nil)))

;;We will remove the block when we are sure it isn't needed.  This is
;;necessary for further simplifications (such as let's)

;;First pass:

;;In several cases, we might have a block immediately followed by another block.  This might be the result of code like

;; (block outer (loop inner (return) (return-from outer)))

;; where the inner loop is expanded into something like

;; (block outer (block nil (while t inner (return-from nil) (return-from outer))))

;; In this case, it is preferable to "compress"  the blocks:

;; (block outer (while t inner (return-from outer) (return-from outer)))

;; the result is semantically equivalente but has other advantages. Consider:

;; (block outer (loop inner (return) (continue outer)))

;; Here, the intent is to continue a labeled loop but it can only work if we "compress" the blocks.

(defclass compress-blocks ()
  ())

(defmethod visit :before ((e block-statement) (visitor compress-blocks))
  (unless (not (block-statement-name e))
    (compress-blocks e)))

(defun compress-blocks (e)
  (let ((statements (statement-list-elements (block-statement-body e))))
    (when (endp (rest statements))
      (let ((statement (first statements)))
	(when (nil-block-statement-p statement)
	  (let ((label (ast-node-form (block-statement-name e))))
	    (multiple-value-bind (breaks continues returns)
		(collect-exits-for-block statement)
	      (loop for (e) in breaks
		    do (change-class e 'break-statement :label (parse label 'linj-name)))
	      (loop for (e) in continues
		    do (change-class e 'continue-statement :label (parse label 'linj-name)))
	      (loop for (e) in returns
		    do (change-class e 'return-from-statement :label (parse label 'linj-name)))))
	  (setf (block-statement-body e)
		(block-statement-body statement))
	  e)))))


(defun collect-exits-for-block (statement)
  (let ((collector (make-instance 'collect-return-from :block statement)))
    (visit statement collector)
    (values (breaks collector) (continues collector) (returns collector))))
  
(defclass remove-unneded-blocks ()
  ())

(defmethod visit :before ((e block-statement) (visitor remove-unneded-blocks))
  (remove-unneded-blocks e))

(defun remove-unneded-blocks (e)
  (multiple-value-bind (breaks continues returns)
      (collect-exits-for-block e)
    (when (every #'(lambda (elems)
		     (or ;;If there are no references to the block, we can remove it
		      (endp elems)
		      (and (not (block-statement-name e))
			   (endp (second (first elems))))))
		 (list breaks continues returns))
      (loop for (e) in breaks
	    do (change-class e 'empty-break-statement :label nil))
      (loop for (e) in continues
	    do (change-class e 'empty-continue-statement :label nil))
      (loop for (e) in returns
	    do (change-class e 'empty-return-from-statement :label nil))
      ;;HACK: this might be simpler:
      (change-class e 'let-statement :bindings (parse '() 'bindings))
;;       ;;But I'll use the older version for now.
;;       (cond ((statement-list-p (ast-node-parent e))
;; 	     (setf (statement-list-elements (ast-node-parent e))
;; 		   (mapcan #'(lambda (statement)
;; 			       (if (eq statement e)
;; 				 (statement-list-elements (block-statement-body e))
;; 				 (list statement)))
;; 			   (statement-list-elements (ast-node-parent e)))))
;; 	    ((endp (rest (statement-list-elements (block-statement-body e)))) ;;just one
;; 	     (with-parent ((ast-node-parent e))
;; 	      (become-instance e (first (statement-list-elements (block-statement-body e))))))
;; 	    (t
;; 	     (let ((node (with-parent ((ast-node-parent e))
;; 			   (parse `(progn ,@(statement-list-elements (block-statement-body e)))
;; 				  'statement))))
;; 	       (apply-visitors 'after-parse node)
;; 	       (become-instance e node))))
      e)))

(def-unparse block-statement (e)
  (if (avoid-labelled-statement-brackets-p (block-statement-body e))
    (format t "~/unlinj-name/: ~{~/ppblk/~^~:@_~}"
	    (block-statement-name e)
	    (statement-list-elements (block-statement-body e)))
    (format t "~@<~/unlinj-name/: ~/ppblk/~:>"
	    (block-statement-name e)
	    (block-statement-body e))))

(defun avoid-labelled-statement-brackets-p (statement-list)
  (and (endp (rest (statement-list-elements statement-list)))
       (let ((unique-statement (first (statement-list-elements statement-list))))
	 (or (not (let-statement-p unique-statement))
	     (and (endp (bindings-elements (let-statement-bindings unique-statement))) 
		  (avoid-labelled-statement-brackets-p (let-statement-body unique-statement)))))))


;;HACK: This should be done using lambdas so that it returns as soon as possible
(defclass collect-return-from ()
  ((block :reader collect-block :initarg :block)
   (returns :accessor returns :initform (list))
   (breaks :accessor breaks :initform (list))
   (continues :accessor continues :initform (list))))

(defmethod visit ((e return-from-statement) (visitor collect-return-from))
  (let ((block (containing-block-named e (return-from-statement-label e))))
    (when (eq block (collect-block visitor))
      (push (list e (jumped-over-blocks e (return-from-statement-label e)))
	    (returns visitor)))))

(defmethod visit ((e continue-statement) (visitor collect-return-from))
  (let ((block (containing-block-named e (continue-statement-label e))))
    (when (eq block (collect-block visitor))
      (push (list e (jumped-over-blocks e (continue-statement-label e)))
	    (continues visitor)))))

(defmethod visit ((e break-statement) (visitor collect-return-from))
  (let ((block (containing-block-named e (break-statement-label e))))
    (when (eq block (collect-block visitor))
      (push (list e (jumped-over-blocks e (break-statement-label e)))
	    (breaks visitor)))))

;;Let's define some iteration forms on top of this

;; (def-transform for-statement (loop . ?body) SEE FILE loop.lisp
;;   (block nil (for (() t ()) . ?body)))

(def-transform for-statement (while ?test . ?body)
  (block nil (for (() ?test ()) . ?body)))

(def-transform for-statement (do . ?rest)
  (do-iter nil . ?rest))

(def-transform for-statement (do* . ?rest)
  (do-iter t . ?rest))

;;;From a set of parallel bindings (or assignments) in the form
;;;((var1 . exprs1) (val2 . exprs2) ... (varn . exprsn))
;;;returns a reordering and extension of the bindings so that they can be
;;;done sequencially.  The bindings have that strange form because
;;;they can be used both for let and for do bindings. To avoid parsing the
;;;code, this is done conservatively by mixin all namespaces.

;;;The idea is that if one of the exprs references one of the other vars,
;;;the corresponding binding must be done before the binding of those vars
;;;((x 1) (y x)) -> ((y x) (x 1))
;;;((x 1) (y (1+ x)) (z (+ x y))) -> ((z (+ x y)) (y (1+ x)) (x 1))
;;;((x y) (y x)) -> ((x-old x) (x y) (y x-old))

(defun serialize-parallel (all-binds)
    ;;First, mutual dependencies
  (let ((mutual-depends
	 (mapcon #'(lambda (binds)
		     (let ((bind1 (first binds)))
		       (mapcan #'(lambda (bind2)
				   (if (and (depends-p bind1 bind2 all-binds)
					    (depends-p bind2 bind1 all-binds))
				     (list bind1)
				     (list)))
			       (rest binds))))
		 all-binds)))
    (if mutual-depends
	;;;add extra vars
      (let ((rebinds (mapcar #'(lambda (mutual)
				 (let ((var (first mutual)))
				   (cons var (gen-new-name var all-binds))))
			     (remove-duplicates mutual-depends :key #'second))))
	(let ((extras (mapcar #'(lambda (mutual rebind)
				  (let ((var (car rebind))
					(new-var (cdr rebind)))
				    (if (rest (rest mutual)) ;;do binds
				      `(,new-var ,(second mutual) ,var)
				      `(,new-var ,var))))
			      mutual-depends
			      rebinds)))
	  (values extras
		  (stable-sort (mapcar #'(lambda (bind)
					   (if (rest (rest bind)) ;;do binds
					     (let ((extra (assoc (first bind) rebinds)))
					       (if extra
						 `(,(first bind) ,(cdr extra) ,@(sublis rebinds (rest (rest bind))))
						 `(,(first bind) ,@(sublis rebinds (rest bind)))))
					     `(,(first bind) ,@(sublis rebinds (rest bind)))))
				       all-binds)
			       #'(lambda (b1 b2) (depends-p b1 b2 all-binds))))))
      (values (list)
	      (nreverse
	       (stable-sort (copy-list all-binds)
			    #'(lambda (b1 b2) (not (depends-p b1 b2 all-binds)))))))))

(defun depends-p (b1 b2 binds)
  (and (consp b1)
       (consp b2)
       (some #'(lambda (expr1)
		 (dependency-path-p expr1 (first b2) binds))
	     (rest b1))))

(defun dependency-path-p (expr var binds &optional (closed-binds (list)))
  (labels ((iterate (form)
	     (if (atom form)
	       (and (symbolp form)
		    (or (eq form var)
			(let ((bind (assoc form binds)))
			  (and bind
			       (not (member bind closed-binds)) 
			       (some #'(lambda (expr1)
					 (dependency-path-p expr1 var binds (cons bind closed-binds)))
				     (rest bind))))))
	       (or (iterate (car form))
		   (iterate (cdr form))))))
    (iterate expr)))

;;For the do and let binds
(defun serialize-parallel-bindings (binds)
  (multiple-value-bind (new-binds reordered-binds)
      (serialize-parallel binds)
    (append new-binds reordered-binds)))

(def-macro-transform for-statement (do-iter ?sequencial
				     ?bind-and-update-list 
				     (?end-test . ?result-forms)
				     . ?body)
  (multiple-value-bind (new-binds reordered-binds)
      (if ?sequencial
	(values (list) ?bind-and-update-list)
	(serialize-parallel ?bind-and-update-list))
      (setq ?bind-and-update-list (append new-binds reordered-binds))
      (flet ((build-for (pre)
			`(for (,pre
			       (not ,?end-test)
			       ,(mapcan #'(lambda (bind-and-update)
					    (when (rest (rest bind-and-update))
					      (list `(setf ,(first (last bind-and-update 3))
							   ,(first (last bind-and-update))))))
					?bind-and-update-list))
			      ,@?body))
	     (first-two (l) (list (first l) (second l))))
	(let ((?result-forms
	       (if (and ?result-forms
			(or (expression-references-p ?result-forms 'error)
			    (expression-references-p ?result-forms 'throw)
			    ;;(expression-references-p ?body 'return) ;;This is not safe but, until I have a better idea
			    (expression-references-p ?body 'return-from)))
		 `((when ,?end-test ,@?result-forms))
		 ?result-forms)))
	  `(block nil
	     ,(if (or (rest ?bind-and-update-list) ;;more than one => 'for' isn't enough
		      ?result-forms ;;more than zero => 'for' isn't enough
		      (and (not ?sequencial) ;;paralel => 'for' is sequential but
			   ;;this only makes sense if
			   (rest (rest ?bind-and-update-list)))) ;;more than one
		(let ((for-bind
		       (if ?sequencial
			 (let ((bind (first (last ?bind-and-update-list))))
			   (and (not (expression-references-p ?result-forms (first bind)))
				bind))
			 (find-if-not #'(lambda (var) (expression-references-p ?result-forms var))
				      reordered-binds
				      :key #'first
				      :from-end t))))
		  (if for-bind
		    `(let* ;;the binds are serialized ,(if ?sequencial 'let* 'let)
			 ,(mapcar #'first-two (remove for-bind ?bind-and-update-list :count 1))
		       ,(build-for (list (first-two for-bind)))
		       ,@?result-forms)
		    `(let* ;;the binds are serialized ,(if ?sequencial 'let* 'let)
			 ,(mapcar #'first-two ?bind-and-update-list)
		       ,(build-for '())	;;could put the initializations here but 'let'
		       ;;doesn't allow uninitialized bindings
		       ,@?result-forms)))
		(build-for (mapcar #'(lambda (bind-and-update)
				       (first-two bind-and-update))
				   ?bind-and-update-list))))))))

(defun expression-references-p (e var)
  "Poor's man approach."
  (if (atom e)
    (eq e var)
    (or (expression-references-p (car e) var)
	(expression-references-p (cdr e) var))))

;;Iteration
;;simple dotimes

(def-macro dotimes ((var end . result-forms) . body)
  (if (or (numberp end)
	  (and (atom end) (not (expression-references-p body end))))
    `(do ((,var 0 (1+ ,var)))
	 ((>= ,var ,end) . ,result-forms) . ,body)
    (with-new-names (limit)
      `(let ((,limit ,end))
	 (do ((,var 0 (1+ ,var)))
	     ((>= ,var ,limit) . ,result-forms) . ,body)))))

;;Now, for a dolist, which must use the (supposedly) defined first, rest
;;and endp

(def-macro dolist ((var expr . result-forms) . body)
  (with-new-names (list)
    `(do ((,list (real-the linj.cons ,expr) (rest ,list)))
	 ((endp ,list) . ,result-forms)
       (let ((,var (first ,list))) . ,body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Parallel assignment can be implemented with the help of serialize-parallel

(def-macro-transform setf-expression (psetq ?var ?val)
  (assert (symbolp ?var))
  `(setq ,?var ,?val))

(def-macro-transform statement (psetq ?var ?val)
  (assert (symbolp ?var))
  `(setq ,?var ,?val))

(def-macro-transform statement (psetq . ?vars-vals)
  (dolist-2 ((var val) ?vars-vals)
    (declare (ignore val))
    (assert (symbolp var)))
  `(psetf . ,?vars-vals))

(defparameter *parallel-assignment-can-reorder* t)

(def-macro-transform statement (psetf ?var1 ?val1 . (?is ?others consp))
  (expand-psetq-psetf `(,?var1 ,?val1 ,@?others)))

(defun expand-psetq-psetf (var-vals)
  (let ((binds (list)))
    (dolist-2 ((var val) var-vals)
      (push (list var val) binds))
    (expand-assignments (nreverse binds))))

(defun expand-assignments (binds)
  (if *parallel-assignment-can-reorder*
    (multiple-value-bind (new-binds reordered-binds)
	(serialize-parallel binds)
      (if new-binds
	`(let ,new-binds
	   (setf ,@(mapcan #'identity reordered-binds)))
	`(setf ,@(mapcan #'identity reordered-binds))))
    (let ((new-binds
	   (mapcar #'(lambda (bind)
		       `(,(gen-new-name (first bind) binds) ,(second bind)))
		   binds)))
      `(let ,new-binds
	 (setf ,@(mapcan #'(lambda (bind new-bind)
			     `(,(first bind) ,(first new-bind)))
			 binds new-binds))))))


(def-macro-transform statement (rotatef ?var1 ?var2)
  `(psetf ,?var1 ,?var2 ,?var2 ,?var1))


;;;We need to simplify some expressions before they can be unparsed.  One
;;;simple way is to partially evaluate them at macro expansion time.
;;Example:

(def-transform expression (not (not ?expr)) ?expr)
(def-transform expression (not t) nil)
(def-transform expression (not nil) t)
(def-transform expression (not (> ?x ?y)) (<= ?x ?y))
(def-transform expression (not (>= ?x ?y)) (< ?x ?y))
(def-transform expression (not (< ?x ?y)) (>= ?x ?y))
(def-transform expression (not (<= ?x ?y)) (> ?x ?y))
(def-transform expression (not (eq ?x ?y)) (neq ?x ?y))
(def-transform expression (not (= ?x ?y)) (/= ?x ?y))
(def-transform expression (not (/= ?x ?y)) (= ?x ?y))

(def-transform expression (or) nil)
(def-transform expression (and) t)
(def-transform expression (or ?expr) ?expr)
(def-transform expression (and ?expr) ?expr)

;;Maybe we are pushing this too far but they are useful for extended loop macro expansions
(def-macro-transform expression (not (or . ?exprs)) `(and ,@(mapcar #'(lambda (expr) `(not ,expr)) ?exprs)))
(def-macro-transform expression (not (and . ?exprs)) `(or ,@(mapcar #'(lambda (expr) `(not ,expr)) ?exprs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;The case statement isn't simply a macro transformation on top of cond.
;;For <int>, <long> and <char> types, the translation to Java can expand to
;;a switch or to a cond.

;;First, transform atomic keys into a list with the key:

(def-macro-transform nil (case ?expr . ?clauses)
    (when (some #'(lambda (clause)
		    (and (atom (first clause))
			 (not (eq (first clause) 't))
			 (not (eq (first clause) 'otherwise))))
		?clauses)
      `(case ,?expr
	 ,@(mapcar #'(lambda (clause)
		       (if (and (atom (first clause))
				(not (eq (first clause) 't))
				(not (eq (first clause) 'otherwise)))
			 `((,(first clause)) ,@(rest clause))
			 clause))
		   ?clauses))))

;;Second, case expressions are a special case that needs special parsing

(def-macro-transform expression (case ?expr . ?clauses)
  (and (or (reference-p ?expr)
	   (symbolp ?expr))
       (from-case-to-cond ?expr ?clauses)))

;;Third, case statements must also save the cond version because the
;;unparse will depends on the type of the case expression

(def-macro-transform statement (case ?expr . ?clauses)
  `(yet-unknow-case-form
    ,?expr
    ,?clauses
    ,(if (or (reference-p ?expr)
	   (symbolp ?expr))
	 (from-case-to-cond ?expr ?clauses)
	 (with-new-names (case)
	   `(let ((,case ,?expr))
	     ,(from-case-to-cond case ?clauses))))))

(defun from-case-to-cond (?expr ?clauses)
  `(cond ,@(mapcar #'(lambda (clause)
		       `(,(cond ((listp (first clause))
				 `(or ,@(mapcar #'(lambda (elem)
						    `(eql ,?expr ',elem))
						(first clause))))
				((or (eq (first clause) 't) (eq (first clause) 'otherwise))
				 't)
				(t ;;this should never happen bc of the
				 ;;previous macro, but...
				 (error "Can't process case clause ~S" (first clause))))
			 ,@(rest clause)))
		   ?clauses)))

;;Now, the syntax.
;;HACK: This doesn't look pretty.
(def-syntax default-case-clause (linj-node)
  ((?is ?test (lambda (test) (or (eq test 't) (eq test 'otherwise)))) . ?body/statement-list))

(defun last-statement-terminates-p (statement-list)
  ;;empty statement lists do not last-statement-terminates-p
  (if (null (statement-list-elements statement-list))
    nil
    (progn
      (apply-to-exit-point statement-list
			   #'(lambda (statement)
			       (unless (or (return-statement-p statement)
					   (return-from-statement-p statement)
					   (throw-statement-p statement))
				 (return-from last-statement-terminates-p nil)))
			   (list 'normal-flow))
      t)))

(def-unparse default-case-clause (e)
  (format t "~@<default:~4I~{~:@_~/ppblk/~}~:[~:@_break;~;~]~I~:>"
	  (statement-list-elements (default-case-clause-body e))
	  (last-statement-terminates-p (default-case-clause-body e))))

(def-syntax case-clause (default-case-clause)
  (?keys/argument-list . ?body/statement-list))

(def-unparse case-clause (e)
  (if (some #'let-statement-p (statement-list-elements (case-clause-body e))) ;;This must go deeper.
    (format t "~@<~{case ~/pp/:~^~:@_~}~4I {~{~:@_~/ppblk/~}~:[~:@_break;~;~]~I~:@_}~:>"
	    (argument-list-elements (case-clause-keys e))
	    (statement-list-elements (case-clause-body e))
	    (last-statement-terminates-p (case-clause-body e)))
    (format t "~@<~{case ~/pp/:~^~:@_~}~4I~{~:@_~/ppblk/~}~:[~:@_break;~;~]~I~:>"
	    (argument-list-elements (case-clause-keys e))
	    (statement-list-elements (case-clause-body e))
	    (last-statement-terminates-p (case-clause-body e)))))

(def-list-syntax case-clause-list (linj-list-node) default-case-clause)

(def-syntax case-statement (statement)
  (yet-unknow-case-form ?expression/expression ?clauses/case-clause-list ?let-cond/statement))

;;The type of the Expression must be char, byte, short, or int, or a compile-time error occurs.

(defun switch-type-reference-p (type)
  (or (char-type-p type)
      (byte-type-p type)
      (short-type-p type)
      (int-type-p type)))

;; (defmethod visit :before ((e case-statement) (visitor parse-tree-finish))
;;   (unless (switch-type-reference-p (get-type (case-statement-expression e)))
;;     (let ((expr (ast-node-form (case-statement-expression e)))
;; 	  (clauses (ast-node-form (case-statement-clauses e))))
;;       (let ((statement
;; 	     (with-parent ((ast-node-parent e))
;; 	       (parse (if (reference-p (case-statement-expression e))
;; 			(from-case-to-cond expr clauses)
;; 			(with-new-names (case)
;; 			  `(let ((,case ,expr))
;; 			     ,(from-case-to-cond case clauses))))
;; 		      'statement))))
;; 	(apply-previous-visitors statement)
;; 	;;It is important to replace the case with the cond for several
;; 	;;reasons, including properly dealing with the let unparsing and
;; 	;;also to avoid repeated transformation between case and cond.
;; 	(setf (statement-list-elements (ast-node-parent e))
;; 	  (substitute statement e (statement-list-elements (ast-node-parent e))))))))

(def-unparse case-statement (e)
  (if (switch-type-reference-p (get-type (case-statement-expression e)))
    (format t "~@<switch (~:/ppexp/) {~{~:@_~/pp/~}~:@_}~:>"
	    (case-statement-expression e)
	    (case-clause-list-elements (case-statement-clauses e)))
    (unparse-object (case-statement-let-cond e) *standard-output*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Operator expressions

;;; 5.6.1 Unary Numeric Promotion

;;; Some operators apply unary numeric promotion to a single operand, which
;;; must produce a value of a numeric type:

;;; If the operand is of compile-time type byte, short, or char, unary
;;; numeric promotion promotes it to a value of type int by a widening
;;; conversion (§5.1.2).  Otherwise, a unary numeric operand remains as is
;;; and is not converted.

;;; Unary numeric promotion is performed on expressions in the following
;;; situations:

;;; Each dimension expression in an array creation expression (§15.10) 
;;; The index expression in an array access expression (§15.13) 
;;; The operand of a unary plus operator + (§15.15.3) 
;;; The operand of a unary minus operator - (§15.15.4) 
;;; The operand of a bitwise complement operator ~ (§15.15.5) 
;;; Each operand, separately, of a shift operator >>, >>>, or << (§15.19);
;;; therefore a long shift distance (right operand) does not promote the
;;; value being shifted (left operand) to long

(defparameter *unary-numeric-promotion-types*
    (list (byte-type) (short-type) (char-type)))

(defparameter *unary-numeric-acceptable-types*
    (list (int-type) (long-type) (float-type) (double-type)));; (big-integer-type) (big-decimal-type)))

(defun unary-numeric-promotion (type)
  (cond ((member type *unary-numeric-promotion-types* :test #'equal-type-p)
	 (int-type)) ;;promotion
	((member type *unary-numeric-acceptable-types* :test #'equal-type-p)
	 type)
	(t
	 (linj-error "Unnaceptable type ~S for unary numeric promotion" type))))

;;; 5.6.2 Binary Numeric Promotion

;;; When an operator applies binary numeric promotion to a pair of
;;; operands, each of which must denote a value of a numeric type, the
;;; following rules apply, in order, using widening conversion (§5.1.2) to
;;; convert operands as necessary:

;;; If either operand is of type double, the other is converted to double. 
;;; Otherwise, if either operand is of type float, the other is converted to float. 
;;; Otherwise, if either operand is of type long, the other is converted to long. 
;;; Otherwise, both operands are converted to type int. 

;;; Binary numeric promotion is performed on the operands of certain operators:

;;; The multiplicative operators *, / and % (§15.17) 
;;; The addition and subtraction operators for numeric types + and - (§15.18.2) 
;;; The numerical comparison operators <, <=, >, and >= (§15.20.1) 
;;; The numerical equality operators == and != (§15.21.1) 
;;; The integer bitwise operators &, ^, and | (§15.22.1) 
;;; In certain cases, the conditional operator ? : (§15.25)

(defmethod merge-types :around ((t1 type-reference) (t2 type-reference))
  (cond ((cyclic-type-p t1) t2)
	((cyclic-type-p t2) t1)
	(t (call-next-method))))

(defmethod merge-types ((t1 type-reference) (t2 type-reference))
  (cond ((or (void-type-p t1)
	     (void-type-p t2))
	 (void-type))
	((or (and (bignum-type-p t1)
		  (or (int-type-p t2) (long-type-p t2) (big-integer-type-p t2)))
	     (and (bignum-type-p t2)
		  (or (int-type-p t1) (long-type-p t1) (big-integer-type-p t1))))
	 (bignum-type))
	((or (and (big-integer-type-p t1)
		  (or (int-type-p t2) (long-type-p t2)))
	     (and (big-integer-type-p t2)
		  (or (int-type-p t1) (long-type-p t1))))
	 (big-integer-type))
	((or (and (long-wrapper-type-p t1)
		  (or (int-type-p t2) (long-type-p t2)))
	     (and (long-wrapper-type-p t2)
		  (or (int-type-p t1) (long-type-p t1))))
	 (long-wrapper-type))
	((or (and (double-wrapper-type-p t1)
		  (or (float-type-p t2) (double-type-p t2)))
	     (and (double-wrapper-type-p t2)
		  (or (float-type-p t1) (double-type-p t1))))
	 (double-wrapper-type))
	((and (boolean-type-p t1)
	      (class-or-interface-type-reference-p t2))
	 t1)
	((and (boolean-type-p t2)
	      (class-or-interface-type-reference-p t1))
	 t2)
	((or (and (null-type-p t1) (not (class-or-interface-type-reference-p t2)))
	     (and (null-type-p t2) (not (class-or-interface-type-reference-p t1)))
	     (and (class-or-interface-type-reference-p t1) (not (class-or-interface-type-reference-p t2)))
	     (and (class-or-interface-type-reference-p t2) (not (class-or-interface-type-reference-p t1))))
	 (linj-error "Can't promote types ~S and ~S" t1 t2))
	((and (boolean-type-p t1)
	      (boolean-type-p t2))
	 (boolean-type))
	((or (boolean-type-p t1)
	     (boolean-type-p t2))
	 (linj-error "Can't promote types ~S and ~S" t1 t2))
	((or (byte-type-p t1) (short-type-p t1) (char-type-p t1))
	 (merge-types (int-type) t2))
	((or (byte-type-p t2) (short-type-p t2) (char-type-p t2))
	 (merge-types t1 (int-type)))
	(t 
	 (find-if #'(lambda (type)
		      (or (equal-type-p t1 type)
			  (equal-type-p t2 type)))
		  *primitive-types-order*))))

(defun most-generic-type (types)
  (if (endp types)
    (object-type)
    (reduce #'merge-types types)))

(defun always-boolean (&rest types)
  (declare (ignore types))
  (boolean-type))

(defun always-string (&rest types)
  (declare (ignore types))
  (string-type))

;;Base class

(def-category operator-expression (expression)
  ((name :accessor operator-expression-name)))

;;;To keep all operators
(defparameter *operators* (make-hash-table))

(defstruct operator
  name
  associative-p
  unparse
  return-type)

(defmacro def-operator (type name return-type &key (associative-p nil) (unparse (princ-to-string name)))
  `(push (make-operator :name ',name 
			:associative-p ,associative-p
			:unparse ,unparse
			:return-type #',return-type)
	 (gethash ',type *operators* (list))))

(defun operator-name-p (type name)
  (member name (gethash type *operators*) :key #'operator-name))

(defun get-operator (type oper)
  (find oper (gethash type *operators*) :key #'operator-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Unary operators

(defun unary-operator-name-p (oper)
  (operator-name-p :unary oper))

(def-syntax unary-operator-expression (operator-expression)
  ((?is ?name unary-operator-name-p) ?arg/expression))

(defmethod get-type ((e unary-operator-expression))
  (let ((function 
	 (operator-return-type
	  (get-operator :unary
			(operator-expression-name e)))))
    (funcall function (get-principal-type (unary-operator-expression-arg e)))))

(def-unparse unary-operator-expression (e)
  (let ((unparse (operator-unparse (get-operator :unary (unary-operator-expression-name e))))
	(arg (principal-value (unary-operator-expression-arg e))))
    (if (functionp unparse)
      (funcall unparse arg)
      (format t (if *print-out-parenthesis* "(~A ~/ppexp/)" "~A ~/ppexp/")
	      unparse
	      arg))))

;;The unary operators

(def-operator :unary lognot unary-numeric-promotion :unparse "~")
(def-operator :unary not    always-boolean
	      :unparse #'(lambda (arg)
			   (if *translate-reference-type-to-boolean*
			     (let ((type (get-principal-type arg)))
			       (if (boolean-type-p type)
				 (format t (if *print-out-parenthesis* "(! ~/ppexp/)" "! ~/ppexp/")
					 arg)
				 (format t (if *print-out-parenthesis* "(~:/ppexp/)" "~:/ppexp/")
					 (cond ((cons-type-p type)
						(with-parent ((ast-node-parent arg))
						  (parse `(endp ,arg) 'expression)))
					       ((class-or-interface-type-reference-p type)
						(with-parent ((ast-node-parent arg))
						  (parse `(= ,arg null) 'expression)))
					       (t
						(error "Can't use expression ~A in a context where a boolean is expected" arg))))))
			     (format t 
				     (if *print-out-parenthesis* "(! ~/ppexp/)" "! ~/ppexp/")
				     arg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Binary operators

(defun binary-operator-name-p (oper)
  (operator-name-p :binary oper))

(def-syntax binary-operator-expression (operator-expression)
  ((?is ?name binary-operator-name-p) ?arg1/expression ?arg2/expression))

(defmethod get-type ((e binary-operator-expression))
  (let ((function 
	 (operator-return-type
	  (get-operator :binary
			(operator-expression-name e)))))
    (funcall function 
	     (get-principal-type (binary-operator-expression-arg1 e))
	     (get-principal-type (binary-operator-expression-arg2 e)))))


(def-unparse binary-operator-expression (e)
  (let ((arg1 (principal-value (binary-operator-expression-arg1 e)))
	(arg2 (principal-value (binary-operator-expression-arg2 e)))
	(operator (binary-operator-expression-name e)))
    (let ((unparse (operator-unparse (get-operator :binary operator))))
      (if (functionp unparse)
	(funcall unparse arg1 arg2)
	(format t (if *print-out-parenthesis*
		    "(~@<~/ppexp/ ~A ~_~/ppexp/)~:>"
		    "~@<~/ppexp/ ~A ~_~/ppexp/~:>")
		;;	    (if *print-out-parenthesis* "(~/ppexp/ ~A ~/ppexp/)" "~/ppexp/ ~A ~/ppexp/")
		arg1
		unparse
		arg2)))))

;;A few binary
(def-operator :binary eq always-boolean :unparse "==")
(def-operator :binary neq always-boolean :unparse "!=")

(defparameter *check-overflow-shift* t)

(def-operator :binary ash
  (lambda (t1 t2)
    (declare (ignore t2))
    (unary-numeric-promotion t1))
  :unparse 
  #'(lambda (arg1 arg2)
      (flet ((out (oper arg2)
	       (format t (if *print-out-parenthesis*
			     "(~@<~/ppexp/ ~A ~_~/ppexp/)~:>"
			     "~@<~/ppexp/ ~A ~_~/ppexp/~:>")
		       arg1
		       oper
		       arg2)))
	(if (literal-p arg2) 
	    (let ((val (literal-value arg2))
		  (type1 (unary-numeric-promotion (get-type arg1))))
	      (cond ((int-type-p type1)
		     (unless (or (< -32 val 32) (not *check-overflow-shift*))
		       (linj-error "Shifting the int ~A more than +/- 31 positions seems incorrect (current ~A)." arg1 arg2)))
		    ((long-type-p type1)
		     (unless (or (< -64 val 64) (not *check-overflow-shift*))
		       (linj-error "Shifting the long ~A more than +/- 63 positions seems incorrect (current ~A)" arg1 arg2))))
	      (if (< val 0)
		  (out ">>" (make-literal (- val)))
		  (out "<<" arg2)))
	    (out "<<" arg2)))))

;;(def-operator :binary 'compute	     ash    :unparse ">>")
;;We also need these:

(def-operator :binary << ;;left-ash
  (lambda (t1 t2)
    (declare (ignore t2))
    (unary-numeric-promotion t1)))

(def-operator :binary >> ;;right-ash
  (lambda (t1 t2)
    (declare (ignore t2))
    (unary-numeric-promotion t1)))

(def-operator :binary >>> ;;right-right-ash
  (lambda (t1 t2)
    (declare (ignore t2))
    (unary-numeric-promotion t1)))

;;;These are n-ary
;;(def-operator :binary logior merge-types :unparse "|")
;;(def-operator :binary logand merge-types :unparse "&")
;;(def-operator :binary logxor merge-types :unparse "^")

(def-operator :binary =  always-boolean :unparse "==")
(def-operator :binary <  always-boolean	:unparse "<")
(def-operator :binary >  always-boolean	:unparse ">")
(def-operator :binary <= always-boolean	:unparse "<=")
(def-operator :binary >= always-boolean	:unparse ">=")
(def-operator :binary /= always-boolean :unparse "!=")

;;Some common uses:
(defmacro def-ordered-predicate (oper)
  `(def-macro-transform expression (,oper ?x ?y ?z . ?w)
     (let ((wrongs (remove-if #'atom (list* ?x ?y ?z ?w))))
       (if wrongs
	 (error "Can't expand (~S ...) with arguments ~S that are not literals or variables" ',oper wrongs)
	 (labels ((repeat (l)
			  (cons `(,',oper ,(first l) ,(second l))
				(if (endp (rest (rest l)))
				  (list)
				  (repeat (rest l))))))
	   `(and ,@(repeat (list* ?x ?y ?z ?w))))))))

(defmacro def-unordered-predicate (oper)
  `(def-macro-transform expression (,oper ?x ?y ?z . ?w)
     (let ((wrongs (remove-if #'atom (list* ?x ?y ?z ?w))))
       (if wrongs
	 (error "Can't expand (~S ...) with arguments ~S that are not literals or variables" ',oper wrongs)
	 (labels ((repeat (l)
		   (if (endp (rest (rest l)))
		     (list `(,',oper ,(first l) ,(second l)))
		     (nconc (mapcar #'(lambda (elem)
					`(,',oper ,(first l) ,elem))
				    (rest l))
			    (repeat (rest l))))))
	   `(and ,@(repeat (list* ?x ?y ?z ?w))))))))

(def-ordered-predicate <)
(def-ordered-predicate >)
(def-ordered-predicate <=)
(def-ordered-predicate >=)
(def-ordered-predicate =)
(def-unordered-predicate /=)

;;For the single argument case:
(def-transform expression (< ?ignore) t)
(def-transform expression (> ?ignore) t)
(def-transform expression (<= ?ignore) t)
(def-transform expression (>= ?ignore) t)
(def-transform expression (= ?ignore) t)
(def-transform expression (/= ?ignore) t)

;;Between mod and rem, rem is the most similar to %
(def-operator :binary rem    merge-types    :unparse "%")

;;N-Ary operators

(defun n-ary-operator-name-p (oper)
  (operator-name-p :n-ary oper))

(def-syntax n-ary-operator-expression (operator-expression)
  ((?is ?name n-ary-operator-name-p) . ?arguments/argument-list))

(defmethod get-type ((e n-ary-operator-expression))
  (let ((function
	 (operator-return-type
	  (get-operator :n-ary
			(operator-expression-name e)))))
    (funcall function
	     (mapcar #'get-principal-type (argument-list-elements (n-ary-operator-expression-arguments e))))))

(defun associative-operator-format-string (operator)
  (format nil
	  (if *print-out-parenthesis*
	    "(~~@<~~{~~/ppexp/~~^ ~A ~~_~~})~~:>"
	    "~~@<~~{~~/ppexp/~~^ ~A ~~_~~}~~:>")
	  operator))

(def-unparse n-ary-operator-expression (e)
  (let ((operator (get-operator :n-ary (n-ary-operator-expression-name e))))
    (let ((args (argument-list-elements (n-ary-operator-expression-arguments e)))
	  (unparse (operator-unparse operator)))
;;      (assert (and (not (null args)) (not (null (rest args)))))
      ;;To deal with multiple-returned-values
      (setf args (mapcar #'principal-value args))
      (when (and (eq (n-ary-operator-expression-name e) '/)
		 (endp (rest args)))
	(push (parse 1 'literal) args))
      ;;We need to freeze typecasts so that they don't disappear due to super-type-p relationships
;;       (dolist (arg args)
;; 	(when (cast-expression-p arg)
;; 	  (setf (force-cast-p arg) t)))
      (if (functionp unparse)
	(funcall unparse args)
	(cond ((and (not (endp args)) (endp (rest args))) ;;unary version
	       (format t (if *print-out-parenthesis* "(~A ~/ppexp/)" "~A ~/ppexp/")
		       unparse
		       (first args)))
	      ((operator-associative-p operator)
	       (format t (associative-operator-format-string unparse) args))
	      (t
	       (format t (if *print-out-parenthesis*
			   "(~:/ppmultiargs/)"
			   "~:/ppmultiargs/")
		       (cons unparse args))))))))

(defun unary-or-n-ary-operator-type (types)
  (if (and (not (endp types)) (endp (rest types))) ;unary version
    (unary-numeric-promotion (first types))
    (most-generic-type types)))

(def-operator :n-ary +      unary-or-n-ary-operator-type :associative-p t) ;;Note: in Linj + doesn't concatenate strings
(def-operator :n-ary -      unary-or-n-ary-operator-type)
(def-operator :n-ary *      most-generic-type :associative-p t)
(def-operator :n-ary /      most-generic-type)

(def-operator :n-ary logior most-generic-type :unparse "|" :associative-p t)
(def-operator :n-ary logand most-generic-type :unparse "&" :associative-p t)
(def-operator :n-ary logxor most-generic-type :unparse "^" :associative-p t)

(def-transform expression (logeqv ?n1 ?n2) (lognot (logxor ?n1 ?n2)))
(def-transform expression (lognand ?n1 ?n2) (lognot (logand ?n1 ?n2))) 
(def-transform expression (lognor ?n1 ?n2) (lognot (logior ?n1 ?n2)))
(def-transform expression (logandc1 ?n1 ?n2) (logand (lognot ?n1) ?n2))
(def-transform expression (logandc2 ?n1 ?n2) (logand ?n1 (lognot ?n2)))
(def-transform expression (logorc1 ?n1 ?n2) (logior (lognot ?n1) ?n2))
(def-transform expression (logorc2 ?n1 ?n2) (logior ?n1 (lognot ?n2)))

(def-transform expression (logtest ?n1 ?n2) (not (zerop (logand ?n1 ?n2))))

(def-transform expression (boole boole-clr ?ignore ?ignore) 0)
(def-transform expression (boole boole-set ?ignore ?ignore) 1)
(def-transform expression (boole boole-1 ?n1 ?ignore) ?n1)
(def-transform expression (boole boole-2 ?ignore ?n2) ?n2)
(def-transform expression (boole boole-c1 ?n1 ?ignore) (lognot ?n1))
(def-transform expression (boole boole-c2 ?ignore ?n2) (lognot ?n2))
(def-transform expression (boole boole-and ?n1 ?n2) (logand ?n1 ?n2))
(def-transform expression (boole boole-ior ?n1 ?n2) (logior ?n1 ?n2))
(def-transform expression (boole boole-xor ?n1 ?n2) (logxor ?n1 ?n2))
(def-transform expression (boole boole-eqv ?n1 ?n2) (logeqv ?n1 ?n2))
(def-transform expression (boole boole-nand ?n1 ?n2) (lognand ?n1 ?n2))
(def-transform expression (boole boole-nor ?n1 ?n2) (lognor ?n1 ?n2))
(def-transform expression (boole boole-andc1 ?n1 ?n2) (logandc1 ?n1 ?n2))
(def-transform expression (boole boole-andc2 ?n1 ?n2) (logandc2 ?n1 ?n2))
(def-transform expression (boole boole-orc1 ?n1 ?n2) (logorc1 ?n1 ?n2))
(def-transform expression (boole boole-orc2 ?n1 ?n2) (logorc2 ?n1 ?n2))


;;And & or need to adapt to non-boolean args
(def-operator :n-ary or  always-boolean
  :unparse #'(lambda (args)
	       (let ((args (mapcar #'from-reference-type-to-boolean args)))
		 (format t (associative-operator-format-string "||") args))))

(def-operator :n-ary and always-boolean
  :unparse #'(lambda (args)
	       (let ((args (mapcar #'from-reference-type-to-boolean args)))
		 (format t (associative-operator-format-string "&&") args))))


;;To concatenate strings (and other types), use the (old)Emacs-like concat
(def-operator :n-ary concat always-string
  :unparse #'(lambda (args)
	       (let ((args (if (string-type-p (get-type (first args)))
			     args
			     (cons (make-instance 'literal :value "") args))))
		 (if (endp (rest args))
		   (unparse-object (first args) *standard-output*)
		   (format t (associative-operator-format-string "+") args)))))

;;Given the fact that concat is always of type string, we can simplify concat combinations

(def-transform n-ary-operator-expression (concat)
  "")

(def-macro-transform n-ary-operator-expression (concat . ?args)
  (when (some #'(lambda (arg)
		  (let-pattern (((concat ?ignore . ?ignore) arg)) t))
	      ?args)
    `(concat ,@(mapcan #'(lambda (arg)
			   (or (let-pattern (((concat . ?args) arg)) (copy-list ?args))
			       (list arg)))
		       ?args))))

;;;;;;;;;;;;;;;;;

;;typep is applicable only to class-or-interface-type-reference

(def-syntax typep-expression (expression)
  (typep ?expression/expression '?type/type-reference))

(defmethod get-type ((e typep-expression))
  (boolean-type))

(def-unparse typep-expression (e)
  (unless (class-or-interface-type-reference-p (typep-expression-type e))
    (linj-error "In ~S, typep can only test non-primitive types" e))
  (unless (or
	   ;;subclass relationship
	   (super-type-p (get-principal-type (typep-expression-expression e))
			 (typep-expression-type e))
	   ;;submixin relationship
	   (and (mixin-declaration-p (get-type-declaration (typep-expression-type e)))
		(super-type-p (typep-expression-type e)
			      (get-principal-type (typep-expression-expression e)))))
    (linj-error "In ~S, typep can only test for a subtype of the type of the expression ~S (which is ~S)"
		e (typep-expression-expression e) (get-principal-type (typep-expression-expression e))))
  (format t (if *print-out-parenthesis* "(~/ppexp/ ~A ~/ppexp/)" "~/ppexp/ ~A ~/ppexp/")
	  (typep-expression-expression e)
	  'instanceof
	  (typep-expression-type e)))

;;;;;;;;;;;;;;;;;
;;Arrays

(def-syntax array-expression (inferred-type-node expression)
  (array ?element-type/type-reference ?dimensions/argument-list ?contents/argument-list))

(defmethod infer-type ((e array-expression))
  (with-parent (e)
    (if (unknown-type-reference-p (array-expression-element-type e))
      (let ((subtype
	     (common-type-or-object-type-for-args
	      (argument-list-elements
	       (array-expression-contents e)))))
	(setf (array-expression-element-type e) (with-parent (e) subtype))
	(make-instance 'array-type-reference
		       :subtype subtype
		       :original-form `(array-type ,(ast-node-form subtype))))
      (reduce #'(lambda (dim r)
		  (declare (ignore dim))
		  (make-instance 'array-type-reference
				 :subtype r
				 :original-form `(array-type ,(ast-node-form r))))
	      (argument-list-elements (array-expression-dimensions e))
	      :initial-value (array-expression-element-type e)
	      :from-end t))))

(defun common-type-or-object-type-for-args (args)
  (copy-type (common-type-or-object-type (mapcar #'get-type args))))

(defun common-type-or-object-type (types)
  (if (or (every #'primitive-type-reference-p types)
	  (every #'class-or-interface-type-reference-p types))
    (most-generic-type-for-array types)
    (object-type)))

(defun most-generic-type-for-array (types)
  (if (endp types)
    (object-type)
    (reduce #'merge-types-for-array types)))

(defmethod merge-types-for-array ((t1 type-reference) (t2 type-reference))
  (if (equal-type-p t1 t2)
    t1
    (merge-types t1 t2)))

;;initial-contents is composed of a nested structure of sequences. The
;;numbers of levels in the structure must equal the rank of
;;array. Each leaf of the nested structure must be of the type given
;;by element-type. If array is zero-dimensional, then initial-contents
;;specifies the single element. Otherwise, initial-contents must be a
;;sequence whose length is equal to the first dimension; each element
;;must be a nested structure for an array whose dimensions are the
;;remaining dimensions, and so on.

;;This was done after Markus Ziegler bug report.

(defun process-initial-contents (dims contents)
  (if (endp dims)
    (if (and (consp contents) (eq (first contents) 'quote))
      (if (numberp (second contents)) (second contents) contents)
      contents)
    (let ((args
	   (cond ((eq (first contents) 'quote)
		  (mapcar #'(lambda (e) `',e) (second contents)))
		 ((eq (first contents) 'list)
		  (rest contents))
		 (t
		  (error "Can't deal with ~S" contents)))))
      (assert (listp args) ()
	      "Missing initialization elements for make-array")
      (assert (= (first dims) (length args)) ()
	      "Array with :initial-contents ~A that doesn't match the length ~A of the array" contents dims)
      `(vector ,@(mapcar #'(lambda (e) (process-initial-contents (rest dims) e)) args)))))

(defun process-initial-element (dims element)
  (if (endp dims)
    element
    `(vector ,@(loop for i below (first dims)
		     collect (process-initial-element (rest dims) element)))))

(def-macro-transform expression (make-array ?dimensions . ?options)
    ;;We are looking for two types of dimensions '(...) or (list ...)
  (flet ((get-elems (expr)
	   (cond ((not (consp expr))
		  ;; (error "Array dimensions must be expressions of the form '(...) or (list ...), not ~A" expr)
		  (list expr))
		 ((eq (first expr) 'quote)
		  (if (listp (first (rest expr)))
		    (mapcar #'(lambda (e) (if (numberp e) e `',e))
			    (first (rest expr)))
		    (first (rest expr))))
		 ((eq (first expr) 'list) 
		  (rest expr))
		 (t (error "Can't deal with ~S" expr)))))
    (let ((type (getf ?options :element-type))
	  (dims (if (integerp ?dimensions)
		  (list ?dimensions)
		  (get-elems ?dimensions)))
	  (initial-contents (getf ?options :initial-contents))
	  (initial-element (getf ?options :initial-element)))
      (let ((elems (cond ((and initial-contents initial-element)
			  (error "Can't use :initial-contents and :initial-element simultaneously"))
			 (initial-contents
			  (rest (process-initial-contents dims initial-contents)))
			 (initial-element
			  (assert (atom initial-element) ()
				  "The :initial-element ~A must be an atomic expression" initial-element)
			  (assert (every #'integerp dims) ()
				  "When an :initial-element is provided, the dimensions ~A must be compile-time constants" dims)
			  (rest (process-initial-element dims initial-element)))
			 (t (list)))))
	;;this only deals with initialization of the first dimension.
	`(array ,(cond (type (second type)) ;;type must be quoted
		       (elems :infer-it)
		       (t (object-type-description)))
		,dims
		,elems)))))

(defmethod visit-current ((e array-expression) (visitor cast-and-wrap))
  (let ((type (array-type-reference-subtype (get-type e))))
    (setf (argument-list-elements (array-expression-contents e))
	  (mapcar #'(lambda (arg)
		      (convert-to-type arg type :force-cast nil))
		  (argument-list-elements (array-expression-contents e))))))

(def-unparse array-expression (e)
  (let ((dimensions (argument-list-elements (array-expression-dimensions e)))
	(contents (argument-list-elements (array-expression-contents e))))
    (multiple-value-bind (primitive-type array-dims)
	(labels ((get-dims (type &optional (count 0))
		   (if (array-type-reference-p type)
		     (get-dims (array-type-reference-subtype type) (1+ count))
		     (values type count))))
	  (get-dims (get-type e)))
      (let ((rest-dimensions (make-list (- array-dims (length dimensions)))))
	(if (endp contents)
	  (format t "new ~/pp/~{[~:/ppexp/]~}~{[~*]~}"
		  primitive-type
		  dimensions
		  rest-dimensions)
	  (format t "~@<new ~/pp/~{[~*]~}~{[~*]~} { ~4I~_~@<~{~:@/ppexp/~^, ~_~}~:> }~I~:>"
		  primitive-type
		  (argument-list-elements (array-expression-dimensions e))
		  rest-dimensions
		  contents))))))

(def-syntax array-reference (expression)
  (aref ?expression/expression . ?indexes/argument-list))

(def-unparse array-reference (e)
  (format t "~/pp/~{[~:/ppexp/]~}" 
	  (array-reference-expression e)
	  (mapcar #'(lambda (arg)
		      (convert-to-type arg (int-type)))
		  (argument-list-elements (array-reference-indexes e)))))

(defmethod array-or-values-type-reference-subtype ((e array-type-reference) dim)
  (declare (ignore dim))
  (array-type-reference-subtype e))

(defmethod get-type ((e array-reference))
  (reduce #'(lambda (dim r)
	      (array-or-values-type-reference-subtype r dim))
	  (argument-list-elements (array-reference-indexes e))
	  :initial-value (get-type (array-reference-expression e))
	  :from-end t))

;;An important syntactic element is the vector initialization.  Although
;;I'm still undecided regarding the use of vectors, its initialization can
;;be frozen right now:

(def-syntax infer-vector-initialization (inferred-type-node expression)
  (vector . ?arguments/argument-list))

(def-syntax literal-vector (infer-vector-initialization)
  (?is ?literal vectorp)
  :constructor make-literal-vector)

(defun make-literal-vector (category &key original-form literal)
  (declare (ignore category))
  (make-instance 'infer-vector-initialization
		 :original-form original-form
		 :arguments (parse (mapcar #'(lambda (elem) `(quote ,elem))
					   (coerce literal 'list))
				   'argument-list)))

(defmethod infer-type ((e infer-vector-initialization))
  (let ((subtype
	 (common-type-or-object-type-for-args
	  (argument-list-elements
	   (infer-vector-initialization-arguments e)))))
    (make-instance 'array-type-reference
		   :subtype subtype
		   :original-form `(array-type ,(ast-node-form subtype)))))

(defmethod visit-current ((e infer-vector-initialization) (visitor cast-and-wrap))
  (let ((type (array-type-reference-subtype (get-type e))))
    (setf (argument-list-elements (infer-vector-initialization-arguments e))
	  (mapcar #'(lambda (arg)
		      (convert-to-type arg type :force-cast nil))
		  (argument-list-elements (infer-vector-initialization-arguments e))))))

(def-unparse infer-vector-initialization (e)
  (let ((type (array-type-reference-subtype (get-type e))))
    (let ((args (argument-list-elements (infer-vector-initialization-arguments e))))
      (if (not *print-out-init-braces*)
	(format t "~@<{ ~@<~{~:/ppexp/~^, ~_~}~:> }~:>" args)
	(unparse-vector-inits type args)))))

(defun unparse-vector-inits (type args)
  (format t "~@<new ~/pp/[] { ~4I~_~@<~{~:@/ppexp/~^, ~_~}~:> }~I~:>" type args))

;;;;;;;;;;;;;;;;;
;;Casts

(def-syntax cast-expression (explicit-type-node expression)
  (the ?type/type-reference ?expression/expression)
  :slots ((force-cast-p :initarg :force-cast-p :initform nil :accessor force-cast-p)))

(defmethod visit-current ((e cast-expression) (visitor cast-and-wrap))
  (setf (cast-expression-expression e)
	(convert-to-type (cast-expression-expression e) (cast-expression-type e) :force-cast nil)))

(def-unparse cast-expression (e)
  (if (equal-type-p (cast-expression-type e)
		    (get-type (cast-expression-expression e)))
    (format t "~:[~:/ppexp/~;~/ppexp/~]"
	    *print-out-parenthesis*
	    (cast-expression-expression e))
    (format t "~:[(~/pp/)~/ppexp/~;((~/pp/)~/ppexp/)~]"
	      *print-out-parenthesis*
	      (cast-expression-type e)
	      (cast-expression-expression e))))

(def-syntax forced-cast-expression (cast-expression)
  (force-the ?type/type-reference ?expression/expression)
  :slots ((force-cast-p :initform t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;And for type promises:
;;HACK. This must be merged with the above!!!!

(def-syntax assert-type-expression (explicit-type-node expression)
  (real-the ?type/type-reference ?expression/expression))

(def-unparse assert-type-expression (e)
  (if (equal-type-p (assert-type-expression-type e)
		    (get-type (assert-type-expression-expression e)))
    (format t "~:[~:/ppexp/~;~/ppexp/~]"
	    *print-out-parenthesis*
	    (assert-type-expression-expression e))
    (if (super-type-p (get-type (assert-type-expression-expression e))
		      (assert-type-expression-type e))
      (format t "~:[(~/pp/)~/ppexp/~;((~/pp/)~/ppexp/)~]"
	      *print-out-parenthesis*
	      (assert-type-expression-type e)
	      (assert-type-expression-expression e))
      (unparse-object
       (with-parent ((ast-node-parent e))
	(convert-to-type (assert-type-expression-expression e) (assert-type-expression-type e) :force-cast nil))
       *standard-output*))))

;; (def-unparse assert-type-expression (e)
;;   (format t "~:[~:/ppexp/~;~/ppexp/~]"
;; 	  *print-out-parenthesis*
;; 	  (assert-type-expression-expression e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;outer references

(def-syntax outer-expression (explicit-type-node expression)
  (this ?type/type-reference))

(def-unparse outer-expression (e)
  (format t "~/pp/.this" (outer-expression-type e)))

;;super outer references

(def-syntax super-outer-expression (explicit-type-node expression)
  (super ?type/type-reference))

(def-unparse super-outer-expression (e)
  (format t "~/pp/.super" (super-outer-expression-type e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass collect-type-info ()
  ((declaration :initarg :declaration :reader collect-type-info-declaration)
   (found :initform (list) :accessor collect-type-info-found)
   (escape :initarg :escape :reader collect-type-info-escape)))

(defun collect-all-type-info-for-references-to (meth variable-declaration)
  (block escape
    (let ((collect (make-instance 'collect-type-info
				  :declaration variable-declaration
				  :escape #'(lambda () (return-from escape (list))))))
      (visit meth collect)
      (collect-type-info-found collect))))

;;If there are typep-expressions in the body, abort the computation
;;because the type information collected depends on the predicate.
(defmethod visit :before ((typep typep-expression) (collect collect-type-info))
  (funcall (collect-type-info-escape collect)))

(defmethod visit :before ((assert-type assert-type-expression) (collect collect-type-info))
  (when (and (reference-p (assert-type-expression-expression assert-type))
	     (eq (find-declaration (assert-type-expression-expression assert-type))
		 (collect-type-info-declaration collect)))
    (push (assert-type-expression-type assert-type)
	  (collect-type-info-found collect))))

;;Setf expressions can also help type inference
(defmethod visit :before ((assign setf-expression) (collect collect-type-info))
  (when (and (reference-p (setf-expression-r-value assign))
	     (eq (find-declaration (setf-expression-r-value assign))
		 (collect-type-info-declaration collect)))
    (push (get-type (setf-expression-l-value assign))
	  (collect-type-info-found collect))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Parameters

(def-syntax parameter (linj-node)
  ?expression)

(def-unparse parameter (e)
  (error "This can't appear in a Java file"))

(def-list-syntax parameter-list (linj-list-node) parameter
  :constructor make-parameter-list)

(defun make-parameter-list (category &key original-form elements)
  (declare (ignore category))
  (make-instance 'parameter-list
		 :original-form original-form
		 :elements (process-lambda-list elements)))

(defun process-lambda-list (original-arglist)
  (labels ((process-args (arglist type)
	     (if (endp arglist)
	       (list)
	       (let ((param (parameter-expression (first arglist))))
		 (case param
		   (&optional
		    (if (eq type :required)
		      (process-args (rest arglist) :optional)
		      (error "Misplaced ~A in lambda-list ~A" param original-arglist)))
		   (&rest
		    (if (or (eq type :required) (eq type :optional))
		      (process-args (rest arglist) :rest)
		      (error "Misplaced &rest in lambda-list ~A" original-arglist)))
		   (&key
		    (process-args (rest arglist) :keyword))
		   (&aux
		    (process-args (rest arglist) :auxiliary))
		   (t
		    (if (and (eq type :required)
			     (or (eq param 'this)
				 (and (consp param) (consp (cdr param)) (symbolp (cadr param))))) ;;found specializer
		      (let ((this (parse (if (eq param 'this)
					   param
					   `(,(first param) (the ,(second param))))
					 'variable-declaration)))
			(setf (get-option :specialized this) t)
			(setf (get-option :parameter-type this) type)
			(cons this (process-args (rest arglist) type)))
		      (multiple-value-bind (var-decl supplied-p)
			  (if (and (consp param) (consp (rest param)) (rest (rest param)))
			    (let ((sup (nth 2 param)))
			      (if (symbolp sup)
				(values (butlast param) sup)
				(error "Lambda parameter ~A in lambda-list ~A is not a symbol" sup original-arglist)))
			    (values param nil))
			(when (and supplied-p (or (eq type :required) (eq type :rest)))
			  (error "~:(~A~) parameter ~A in lambda-list ~A can't have supplied-p parameter" type param original-arglist))
			(let ((var (parse var-decl 'variable-declaration)))
			  (setf (get-option :parameter-type var) type)
			  (when (eq type :rest)
			    (unless (endp (rest arglist))
			      (error "Found garbage ~A in lambda-list ~A" (rest arglist) original-arglist))
			    (if (unknown-type-reference-p (inferred-type-node-type var))
			      (setf (inferred-type-node-type var) (with-parent (var) (cons-type))
				    (variable-declaration-initializer var) (with-parent (var) (parse ''() 'expression)))
			      (assert (cons-type-p (inferred-type-node-type var)))))
			  `(,var
			    ,@(if supplied-p
				(let ((new-param (parse `(,supplied-p (the boolean)) 'variable-declaration)))
				  (setf (supplied-p new-param) t)
				  (list new-param))
				(list))
			    ,@(process-args (rest arglist) type)))))))))))
    (process-args original-arglist :required)))

(def-unparse parameter-list (e)
  (format t "(~@<~{~:/ppinit/~^, ~:_~}~:>)"
	  (parameter-list-elements e)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Classes are made of type-body-declarations

(def-category type-body-declaration (declaration-options top-level-form)
  ())

;;This ugly syntax is just to make the parser happy

(def-syntax method-declaration (inferred-type-node type-body-declaration)
  (defmethod-0 ?type/type-reference ?name/linj-name ?parameters/parameter-list
	       ?declared-throws/type-reference-list
	       ?options . ?body/statement-list)
  :constructor make-method-declaration
  :slots ((receiver-parameter :reader receiver-parameter :initarg :receiver-parameter)
	  (receiver-position :reader receiver-position :initarg :receiver-position)
	  (simple-parameter-initializations-p :reader simple-parameter-initializations-p :initarg :simple-parameter-initializations-p)
	  (computed-throws :accessor method-declaration-computed-throws)))

;;We also want to visit the computed throws (e.g., to infer imports)
(defmethod visit-descendents :after ((meth method-declaration) (visitor t))
  (when (slot-boundp meth 'computed-throws)
    (visit (slot-value meth 'computed-throws) visitor)))

(defun make-method-declaration (category &key original-form type name parameters declared-throws options body)
  (declare (ignore category))
  (labels ((get-this (params &optional (pos 0) (prevs (list)))
	     (cond ((endp params)
		    (values nil nil (nreverse prevs)))
		   ((get-option :specialized (first params))
		    (values (first params) pos (nreconc prevs (rest params))))
		   (t
		    (get-this (rest params) (1+ pos) (cons (first params) prevs))))))
    (multiple-value-bind (param pos new-parameters)
	(get-this (parameter-list-elements parameters))
      (when param
	(setf (parameter-list-elements parameters) new-parameters))
      (multiple-value-bind (real-params aux-params)
	  (break-list-if #'(lambda (param) (eq (get-option :parameter-type param) :auxiliary)) new-parameters)
	(when aux-params
	  (setf (parameter-list-elements parameters) real-params))
	(make-instance 'method-declaration
		       :receiver-parameter (or param (parse 'this 'variable-declaration))
		       :receiver-position (or pos 0)
		       :simple-parameter-initializations-p (every #'(lambda (param)
								      (or (null (variable-declaration-initializer param))
									  (typep (variable-declaration-initializer param) 'literal)))
								  new-parameters)
		       :original-form original-form
		       :type type
		       :name name
		       :parameters parameters
		       :declared-throws declared-throws
		       :options `(:receiver-position ,pos ,@options)
		       :body (if aux-params (parse `((let* ,aux-params . ,body)) 'statement-list) body))))))

(defun method-declaration-specialized-to (meth)
  (and (uninitialized-variable-declaration-p (receiver-parameter meth))
       (uninitialized-variable-declaration-type (receiver-parameter meth))))

(defun computed-method-declaration-throws-p (meth)
  (slot-boundp meth 'computed-throws))

;;To consider the specialized parameter as an alias to 'this'

(defun alias-this-reference-p (reference)
  (and (reference-p reference)
       (let ((type-decl (containing-type-declaration reference)))
	 (and type-decl
	      (eq (find-declaration reference) type-decl)))))

;;To infer type information in methods, either we are specializing an
;;inherited method or we are defining a new one:

(defparameter *check-super-method-type* t)

(defmethod infer-type ((e method-declaration))
  (or (and *check-super-method-type* (super-method-type e))
      (infer-type-from-exit-points e)))

(defmethod super-method-type ((method method-declaration))
  (let ((class (containing-type-declaration method))
	(name (method-declaration-name method)))
    (let ((super-methods
	   (remove-if-not #'(lambda (meth)
			      (and (endp (get-option :qualifiers meth))
				   (same-signature-p meth method)))
			  (find-methods-named name class))))
      (if (null super-methods)
	nil
	(let ((super-method (first super-methods)))
	  (cond ((and (class-method-p method) (not (class-method-p super-method)))
		 (error "The method ~A~% cannot override the function ~A" method super-method))
		((and (not (class-method-p method)) (class-method-p super-method))
		 (error "The function ~A~% cannot override the method ~A" super-method method))
		(t
		 (get-type super-method))))))))

(defmethod same-signature-p ((e1 method-declaration) (e2 method-declaration))
  (and (eq (method-declaration-name e1)
	   (method-declaration-name e2))
       (let ((p1 (parameter-list-elements (method-declaration-parameters e1)))
	     (p2 (parameter-list-elements (method-declaration-parameters e2))))
	 (and (= (length p1) (length p2))
	      (every #'(lambda (par1 par2)
			 (equal-type-p (get-type par1) (get-type par2)))
		     p1 p2)))))

;;We can infer the method's return type from exit points

(defmethod infer-type-from-exit-points ((e method-declaration))
  (let ((exits (list))
	(cycle-p nil))
    (apply-to-method-exit-points
     e
     #'(lambda (exit)
	 (if (in-type-inference-p exit)
	   (setf cycle-p t)
	   (push exit exits))))
    (most-generic-type-for-exit-points (reverse exits) cycle-p)))

(defun most-generic-type-for-exit-points (exits cycle-p)
  (if (endp exits)
    (if cycle-p
      (cyclic-type)
      (void-type))
    (most-generic-type (mapcar #'get-type exits))))

(defmethod merge-types ((t1 class-or-interface-type-reference)
			(t2 class-or-interface-type-reference))
  ;;Least common superclass
  (cond ((null-type-p t1)
	 t2)
	((null-type-p t2)
	 t1)
	(t
	 (common-super-type t1 t2))))

(defmethod merge-types ((t1 array-type-reference)
			(t2 array-type-reference))
  (with-parent ((ast-node-parent t1))
    (make-instance 'array-type-reference
      :subtype (merge-types (array-type-reference-subtype t1)
			    (array-type-reference-subtype t2))
      :original-form 'automagically-generated-by-merge-types-for-arrays)))

;;But we, humans, prefer a different syntax, as follows

(defun extract-options-body (body)
  (labels ((options-body (body)
	     (cond ((not (listp body))
		    (list body))
		   ((endp body) 
		    (list body))
		   ((keywordp (first body))
		    (cons (first body) 
			  (cons (if (and (eq (first body) :throws)
					 (atom (second body))
					 (not (null (second body))))
				  (list (second body))
				  (second body))
				(options-body (rest (rest body))))))
		   ((and (stringp (first body)) (not (null (rest body))))
		    (cons :documentation 
			  (cons (first body)
				(list (rest body)))))
		   (t
		    (list body)))))
    (let ((result (options-body body)))
      (let ((options (butlast result))
	    (body (first (last result))))
	(let ((throws (getf options :throws))
	      (returns (or (getf options :returns) :infer-it)))
	  (remf options :throws)
	  (remf options :returns)
	  (values options
		  body
		  throws
		  returns))))))

(def-macro-transform method-declaration (defmethod ?name . ?body)
  (let ((after-qualifiers (member-if #'listp ?body)))
    (let ((qualifiers (ldiff ?body after-qualifiers))
	  (arglist (first after-qualifiers))
	  (options&body (rest after-qualifiers)))
      (multiple-value-bind (options body throws returns)
	  (extract-options-body options&body)
	(when (or (member :before qualifiers)
		  (member :after qualifiers))
	  (assert (eq returns :infer-it))
	  (setq returns (void-type)))
	`(defmethod-0 ,returns ,?name ,arglist ,throws 
	  (,@options :visibility :public :qualifiers ,qualifiers)
	  . ,body)))))

(defun class-method-p (meth-decl)
  (eq (get-option :allocation meth-decl) :class))

(defmethod find-declaration-in ((ref reference) (e method-declaration))
  (or (find (reference-name ref)
	    (parameter-list-elements (method-declaration-parameters e))
	    :key #'variable-declaration-name)
      (and (eq (reference-name ref) (variable-declaration-name (receiver-parameter e)))
	   (containing-type-declaration e))
      (call-next-method)))

(defmethod containing-method-declaration ((e method-declaration))
  e)

(defun accessor-declaration-p (e)
  (and (method-declaration-p e)
       (member (get-option :category e) '(:reader :writer))))

(defun constructor-declaration-p (e)
  (and (method-declaration-p e)
       (eq (get-option :category e) :initializer)))

(defun abstract-method-declaration-p (e)
  (and (method-declaration-p e)
       (eq (get-option :category e) :abstract)))

(defun write-documentation (e)
  (let ((documentation (get-option :documentation e)))
    (when documentation
      (if (or (string= "/*" documentation :end2 2)
	      (string= "//" documentation :end2 2))
	(format t "~@<~@;~A~:>~:@_" documentation)
	(format t "~@<// ~@;~A~:>~:@_" documentation)))))

(defun write-options (stream e at-p colon-p)
  (declare (ignore at-p colon-p))
  (let ((visibility (get-option :visibility e))
	(allocation (get-option :allocation e))
	(constant (get-option :final e)))
    (format stream "~A~:[~;static ~]~:[~;final ~]~:[~;synchronized ~]~:[~;native ~]~:[~;transient ~]~:[~;volatile ~]~:[~;strictfp ~]~{~A ~}"
	    (case visibility
	      ((:public) "public ") ((:protected) "protected ") ((:private) "private ") (t ""))
	    (or (eq allocation :class) (get-option :static e))
	    constant
	    (get-option :synchronized e)
	    (get-option :native e)
	    (get-option :transient e)
	    (get-option :volatile e)
	    (get-option :strictfp e)
	    (get-option :modifiers e))))

(def-unparse method-declaration (e)
  (write-documentation e)
  (format t "~@<~/write-options/~:[~;abstract ~]~
~:[~*~/pp/ ~/pp/~;~/pp/~2*~]~/pp/~@[ ~2I~:_throws ~{~/pp/~^, ~}~]~:[ ~/ppblk/~;;~]~:>"
	  e
	  (abstract-method-declaration-p e)
	  (constructor-declaration-p e)
	  (if (constructor-declaration-p e) 
	    (type-declaration-name (containing-type-declaration e))
	    nil)
	  (get-type e)
	  (method-declaration-name e)
	  (method-declaration-parameters e)
	  (method-declaration-throws-list e)
	  (abstract-method-declaration-p e)
	  (method-declaration-body e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Declaration processing is a nice option for those who really enjoy Common Lisp


;;We only expect to see things of the form 
;;(declare declaration-specifier*) where declaration-specifier is of the form

;;(type typespec variable*) and typespec is one of the Linj types
;;(final variable*)
;;(typespec variable*)
;;(returns typespec)
;;(throws typespec*)
;;(visibility spec) where spec is one of :public :protected :private
;;(category spec) where spec is one of :abstract :reader :writer
;;(modifiers spec*) where spec is one of :public :protected :private :abstract :static :final :synchronized :native :strictfp
;;(trace spec*) where spec is one of :time :args :return (if empty, defaults to :args :return)

(defun process-declare-specifiers (declaration-specifiers)
  (let ((type-decls (list))
	(finals (list))
	(returns nil)
	(throw-decls (list))
	(category nil)
	(visibility nil)
	(modifiers (list))
	(others (list)))
    (flet ((process-type (type vars)
	     (let ((type (translate-cl-type type)))
	       (dolist (var vars)
		 (if (assoc var type-decls)
		   (error "Duplicate declaration for variable ~S" var)
		   (push (cons var type) type-decls))))))
      (dolist (spec declaration-specifiers)
	(cond ((member (first spec) '(optimize ignore trace))
	       (push spec others))
	      ((eq (first spec) 'returns)
	       (if (and returns (not (eq (first spec) returns)))
		 (error "Incompatible duplicate declaration for returned type ~S ~S" returns spec)
		 (setf returns (second spec))))
	      ((eq (first spec) 'visibility)
	       (setf visibility (second spec)))
	      ((eq (first spec) 'throws)
	       (setf throw-decls (union throw-decls (rest spec))))
	      ((eq (first spec) 'category)
	       (setf category (second spec)))
	      ((eq (first spec) 'modifiers)
	       (setf modifiers (append modifiers (rest spec)))
	       (when (member :abstract modifiers)
		 (setf category :abstract)))
	      ((eq (first spec) 'final)
	       (setf finals (append (rest spec) finals)))
	      ((eq (first spec) 'type)
	       (process-type (second spec) (rest (rest spec))))
	      (t
	       (process-type (first spec) (rest spec))))))
    (values type-decls finals returns throw-decls category visibility modifiers others)))

;;Now, regarding the declare form, we will accept it everywhere
;;(and not only on the places allowed in Common Lisp). Our idea
;;is to think of declarations as a kind of void statement that
;;is used in a before parse step to assign types the the enclosed
;;variables.

(def-syntax declare-statement (statement)
  (declare . ?declarations))

(def-unparse declare-statement (e)
  ;;no translation to Java
  (error "Unprocessed declare statement ~A" e))

(defclass process-declares ()
  ())

(defmethod visit :before ((e declare-statement) (v process-declares))
  (multiple-value-bind (decls finals returns throws category visibility modifiers others)
      (process-declare-specifiers (declare-statement-declarations e))
    (dolist (decl decls)
      (let ((var (with-parent ((ast-node-parent e)) (parse (car decl) 'reference)))
	    (type (with-parent ((ast-node-parent e)) (parse (cdr decl) 'type-reference))))
	(let ((decl (strict-find-declaration var)))
	  (if (typep decl 'inferred-type-node)
	    (if (unknown-type-reference-p (inferred-type-node-type decl))
		(setf (inferred-type-node-type decl) type)
	      (if (equal-type-p (inferred-type-node-type decl) type)
		(warn "Duplicate type declaration for ~S" decl)
		(linj-error "Confliting type declarations for ~S (~S ~S)"
		       decl (inferred-type-node-type decl) type)))
	    (linj-error "Can't assert type of variable ~S" var)))))
    (dolist (final finals)
      (let ((var (with-parent ((ast-node-parent e)) (parse final 'reference))))
	(let ((decl (strict-find-declaration var)))
	  (setf (get-option :final decl) t))))
    (when (or returns throws category modifiers visibility others)
      (let ((meth (containing-method-declaration e)))
	(when returns
	  (setf (method-declaration-type meth)
		(with-parent (meth) (parse returns 'type-reference))))
	(dolist (throw throws)
	  (push (with-parent (meth) (parse throw 'type-reference))
		(type-reference-list-elements (method-declaration-declared-throws meth))))
	(when category
	  (setf (get-option :category meth) category))
	(when visibility
	  (setf (get-option :visibility meth) visibility))
	(setf (method-declaration-options meth)
	      (append (mapcan #'(lambda (mod) (list mod t)) modifiers)
		      (method-declaration-options meth)))
	(dolist (other others)
	  (setf (get-option (make-keyword (first other)) meth) other)))))
  ;;After processing, we remove ourselves from our parent
  (setf (statement-list-elements (ast-node-parent e))
	(delete e (statement-list-elements (ast-node-parent e)))))

;;;Trace
(defconstant +traced-name-posfix+ '-under-trace)

(defun make-traced-method-name (name)
  (conc-symbol name +traced-name-posfix+))

(defun generate-trace-method (meth)
  (let ((new-meth (with-parent ((ast-node-parent meth))
		    (make-trace-method meth (or (rest (needs-trace-method-p meth)) '(:args :returns))))))
    ;;we change the old method's name
    (setf (method-declaration-name meth)
	  (parse (make-traced-method-name (method-declaration-name meth)) 'linj-name))
    (apply-visitors 'after-parse new-meth)
    (apply-visitors 'before-dump new-meth)
    new-meth))

;;This is useful for tracing purposes
(defun parse-method (form)
  (parse form 'method-declaration))

(defun make-trace-method (meth-decl trace-options)
  (let ((name (method-declaration-name meth-decl))
	(parameters (parameter-list-elements (method-declaration-parameters meth-decl)))
	(options (method-declaration-options meth-decl)))
    (let ((parameter-names (mapcar #'variable-declaration-name parameters)))
      (let ((timed-output 
	     (if (member :time trace-options)
		 `((let ((end-time (in (the system) (current-time-millis))))
		     (in (the linj.trace) (out))
		     (format *trace-output*
			     ,(format nil "~A took ~~A milliseconds~~%" (linj-original-name name))
			     (- end-time start-time))
		     (in (the linj.trace) (print-level))))
		 '((in (the linj.trace) (out))))))
	(let ((body (if (void-type-p (get-type meth-decl))
			`(progn
			  (,(make-traced-method-name name) ,@parameter-names)
			  ,@timed-output
			  (format *trace-output* ,(format nil "~A returned void~~%" (linj-original-name name)) result))
			`(let ((result (,(make-traced-method-name name) ,@parameter-names)))
			  ,@timed-output
			  (format *trace-output* 
			   ,(if (member :returns trace-options)
				(format nil "~A returned ~~S~~%" (linj-original-name name))
				(format nil "~A returned ...~~%" (linj-original-name name)))
			   result)
			  result))))
	  (parse-method
	   `(defmethod ,name
	     ,parameter-names
	     ,@options
	     (declare 
	      ,@(mapcar #'(lambda (var)
			    `(,(get-type var) ,(variable-declaration-name var)))
			parameters)
	      (throws ,@(mapcar #'type-reference-name (method-declaration-throws-list meth-decl))))
	     (in (the linj.trace) (in))
	     (format *trace-output*
	      ,(if (member :args trace-options)
		   (format nil "(~A ~{~*~~S~^ ~})~~%" name parameters)
		   (format nil "(~A ...)~~%" name))
	      ,@parameter-names)
	     ,(if (member :time trace-options)
		  `(let ((start-time (in (the system) (current-time-millis))))
		    ,body)
		  body))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;To allow the use of (setf name) in the method name, we will use a simple macro:

(defconstant +accessor-name-prefix+ 'setf-)

(def-macro-transform method-declaration (defmethod-0 ?type (setf ?name) ?arglist . ?rest)
  `(defmethod-0 ,?type ,(conc-symbol +accessor-name-prefix+ ?name) ,?arglist . ,?rest))

;;When the l-value of a setf-expression is a method-call-expression, we rewrite it to a
;;setf-something method-call-expression.

(defparameter *standard-setf-places* (list 'aref 'slot-value 'the))

(def-macro-transform setf-expression (setf (?name . ?args) ?value)
  (unless (member ?name *standard-setf-places*)
    (when (unreserved-word-p ?name)
      `(,(conc-symbol +accessor-name-prefix+ ?name) ,?value ,@?args))))

;;(the ??) as place

(def-transform setf-expression (setf (the ?type ?var) ?val) (setf ?var (the ?type ?val)))

;;We want to be specific:
(def-macro-transform setf-expression (setq ?var ?val)
  (assert (symbolp ?var))
  `(setf ,?var ,?val))

(def-macro-transform statement (setq . ?vars-vals)
  (dolist-2 ((var val) ?vars-vals)
    (declare (ignore val))
    (assert (symbolp var)))
  `(setf . ,?vars-vals))

;;In some contexts (statements) we can accept a multi-argument setf

(def-macro-transform statement (setf ?var1 ?val1 . (?is ?others consp))
  (assert (evenp (length ?others)) ()
	  "Odd number of args to setq in ~a" `(setf ,?var1 ,?val1 . ,?others))
  `(progn
     ,@(loop for (var val) on `(,?var1 ,?val1 ,@?others) by #'cddr
	     collect `(setf ,var ,val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-syntax incf-decf-expression (statement-expression)
  ((?is ?oper (lambda (oper) (member oper '(incf decf post-incf post-decf)))) ?place/expression ?delta/expression))

(defmethod get-type ((e incf-decf-expression))
  (get-type (incf-decf-expression-place e)))

;; (def-syntax decf-expression (statement-expression)
;;   (decf ?place/expression ?delta/expression))

(defmethod visit-current ((e incf-decf-expression) (visitor cast-and-wrap))
  (check-not-final-variable (incf-decf-expression-place e))
  (let ((delta (incf-decf-expression-delta e)))
    (setf (incf-decf-expression-delta e)
	  (convert-type-to-lvalue (incf-decf-expression-place e) delta))))

(defun unparse-update-expression (oper place delta post-p)
  (check-not-final-variable place)
  (when *print-out-parenthesis*
    (format t "("))
  (if (and (literal-p delta) (eql (literal-value delta) 1))
    (if post-p
      (format t "~/ppexp/~A~A" place oper oper)
      (format t "~A~A~/ppexp/" oper oper place))
    (progn
      (assert (not post-p))
      (format t "~/ppexp/ ~A= ~:/ppexp/" place oper delta)))
  (when *print-out-parenthesis*
    (format t ")")))

(def-unparse incf-decf-expression (e)
  (unparse-update-expression
   (case (incf-decf-expression-oper e) 
     ((incf post-incf) "+") ((decf post-decf) "-"))
   (incf-decf-expression-place e)
   (incf-decf-expression-delta e)
   (member (incf-decf-expression-oper e) '(post-incf post-decf))))

;; (def-unparse decf-expression (e)
;;   (unparse-update-expression "-" (decf-expression-place e) (decf-expression-delta e)))

(def-transform statement-expression (incf ?place) (incf ?place 1))
(def-transform statement-expression (decf ?place) (decf ?place 1))
(def-transform statement-expression (post-incf ?place) (post-incf ?place 1))
(def-transform statement-expression (post-decf ?place) (post-decf ?place 1))
(def-transform statement-expression (setf (?is ?x symbolp) (+ ?x 1)) (incf ?x))
(def-transform statement-expression (setf (?is ?x symbolp) (1+ ?x)) (incf ?x))
(def-transform statement-expression (setf (?is ?x symbolp) (+ 1 ?x)) (incf ?x))
(def-transform statement-expression (setf (?is ?x symbolp) (1- ?x)) (decf ?x))
(def-transform statement-expression (setf (?is ?x symbolp) (- ?x 1)) (decf ?x))
;;HACK missing transforms for += and -= ?
				      					     
(def-transform statement-expression (incf (?is ?x consp) ?delta) (setf ?x (+ ?x ?delta)))
(def-transform statement-expression (decf (?is ?x consp) ?delta) (setf ?x (- ?x ?delta)))


;;Constructor declaration

(def-macro-transform method-declaration (defnew ?parameters . ?options&body)
  (multiple-value-bind (options body throws)
      (extract-options-body ?options&body)
    `(defmethod-0 ,(void-type-description) :initialize-instance ,?parameters ,throws
		  (,@options :category :initializer :visibility :public)
		  . ,body)))

(def-macro-transform method-declaration (defafternew . ?options&body)
  (multiple-value-bind (options body throws)
      (extract-options-body ?options&body)
    `(defmethod-0 ,(void-type-description) finish-initialize-instance () ,throws
		  (,@options
		   :category :initialize-after 
		   ;;this must be private to prevent override of this method in subclasses
		   :visibility :private)
		  . ,body)))

(defun after-new-declaration-p (e)
  (and (method-declaration-p e)
       (eq (get-option :category e) :initialize-after)))

;;Main entry point for a class file

;; (def-macro-transform method-declaration (defmain ?parameters . ?body)
;;   `(defmethod-0 ,(void-type-description) main
;;      ((,(if (or (null ?parameters)
;; 		(not (symbolp ?parameters)))
;; 	  'outside-args 
;; 	  ?parameters) (the ,(string-array-type-description))))
;;      ()
;;      (:allocation :class :visibility :public :category :main)
;;      . ,(if (or (symbolp ?parameters)
;; 		(null ?parameters))
;; 	  ?body
;; 	  `((unless (= (length outside-args) ,(length ?parameters))
;; 	      (error "Incorrect number of arguments"))
;; 	    (let (,@(mapcar #'(lambda (param i)
;; 				(if (name&type-p param)
;; 				  (multiple-value-bind (name type)
;; 				      (extract-name&type param)
;; 				    `(,name
;; 				      ,(from-string-type-to-type type
;; 								 `(aref outside-args ,i))))
;; 				  (error "The main method must use the simplified form of typed variables")))
;; 			    ?parameters
;; 			    (iota (length ?parameters))))
;; 	      . ,?body)))))

  
(def-transform method-declaration (defun main ?parameters . ?body)
  (defmethod main ?parameters
    :visibility :public :allocation :class :returns void :main t :no-key-method t
    . ?body))

(defmethod visit :before ((e method-declaration) (visitor parse-tree-finish))
  (when (get-option :main e)
    (setf (get-option :main e) nil)
    (let ((new-meth (from-normal-to-main-method e)))
      (setf (method-declaration-parameters e) (method-declaration-parameters new-meth)
	    (method-declaration-body e) (method-declaration-body new-meth)))))

(defun from-string-type-to-type (type expr)
  (cond ((string-type-p type)
	 expr)
	((char-type-p type)
	 `(char-at ,expr 0))
	((int-type-p type)
	 `(send (the java.lang.Integer) parse-int ,expr))
	((short-type-p type)
	 `(send (the java.lang.Short) parse-short ,expr))
	((long-type-p type)
	 `(send (the java.lang.Long) parse-long ,expr))
	((float-type-p type)
	 `(send (send (the Double) value-of ,expr) float-value))
	((double-type-p type)
	 `(send (send (the Double) value-of ,expr) double-value))
	((object-type-p type)
	 `(read (new 'linj.linj-reader (new 'string-reader ,expr))))
	((bignum-type-p type)
	 `(in (the linj.bignum) (value-of ,expr)))
	(t
	 (error "Don't know (yet) how to deal with conversion from string type to ~S" type))))

(defun from-normal-to-main-method (meth)
  (with-parent ((ast-node-parent meth))
    (parse-method
     `(defmethod main (outside-args/java.lang.string[])
       :visibility :public :allocation :class :returns void
       ,@(with-main-arguments-lexicals 'outside-args
				       (method-declaration-parameters meth)
				       (method-declaration-body meth))))))

(defun with-main-arguments-lexicals (outparam parameters body)
  (let ((params (parameter-list-elements parameters)))
    (labels ((resolve (params count)
	       (if (endp params)
		 body
		 (let ((param (first params)))
		   (ecase (get-option :parameter-type param)
		     (:required
		      `((let ((,(variable-declaration-name param)
			       ,(from-string-type-to-type (get-type param) `(aref ,outparam ,count))))
			  ,@(resolve (rest params)
				     (1+ count)))))
		     (:optional
		      `((let ((,(variable-declaration-name param)
			       (if (>= (length ,outparam) ,count)
				 ,(from-string-type-to-type (get-type param) `(aref ,outparam ,count))
				 ,(or (variable-declaration-initializer param)
				      (get-default-initializer (get-type param))))))
			  ,@(resolve (rest params)
				     (1+ count)))))
		     (:keyword
		      `((let ((keyargs (make-hash-table :test #'equals)))
			  (do ((i ,count (+ i 2)))
			      ((= i (length ,outparam)))
			    (setf (gethash (aref ,outparam i) keyargs) (aref ,outparam (1+ i))))
			  ,@(process-keyword-params params))))))))
	     (process-keyword-params (params)
	       (if (endp params)
		 body
		 (let ((param (first params)))
		   (let ((key (format nil "--~A" (ast-node-form (variable-declaration-name param)))))
		     (let ((form (from-string-type-to-type (get-type param) `(the java.lang.string (gethash ,key keyargs)))))
		       `((let ((,(variable-declaration-name param)
				,(if (variable-declaration-initializer param)
				   `(if (contains-key keyargs ,key)
				      ,form
				      ,(variable-declaration-initializer param))
				   form)))
			   ,@(process-keyword-params (rest params))))))))))
      (resolve params 0))))

;;Slots

(def-syntax slot-declaration (type-body-declaration)
  (defslot-0 ?statement/variable-declaration . ?options))

(defmethod get-type ((e slot-declaration))
  (get-type (slot-declaration-statement e)))

(def-unparse slot-declaration (e)
  (let ((var-decl (slot-declaration-statement e)))
    (write-documentation e)
    (format t "~@<~/write-options/~/pp/ ~/unlinj-name/~:[~; = ~4I~_~:/ppexp/~I~];~:>"
	    e
	    (get-type var-decl)
	    (variable-declaration-name var-decl)
	    (and (not (null (variable-declaration-initializer var-decl)))
		 (not (member :initarg (slot-declaration-options e)))) ;;avoid duplicate slot initialization
	    (variable-declaration-initializer var-decl))))

(defmethod constant-declaration-p ((e type-body-declaration))
  (get-option :final e))

;;Slots come in three varieties:
;;Normal instance slots
(def-macro-transform slot-declaration (defslot ?name ?initial-value-or-type . ?documentation)
  `(defslot-0 (,?name ,?initial-value-or-type)
    :visibility :protected
    ,@(when ?documentation `(:documentation ,@?documentation))))

;;Class slots
(def-macro-transform slot-declaration (defvar ?name ?initial-value-or-type . ?documentation)
  `(defslot-0 (,?name ,?initial-value-or-type)
    :allocation :class 
    :visibility :public
    ,@(when ?documentation `(:documentation ,@?documentation))))

(defmethod defvar-declaration-p ((e type-body-declaration))
  (eq (get-option :allocation e) :class))

(def-transform slot-declaration (defparameter ?name ?initial-value . ?documentation)
  (defvar ?name ?initial-value . ?documentation))

(def-macro-transform slot-declaration (defconstant ?name ?initial-value . ?documentation)
  `(defslot-0 (,?name ,?initial-value) 
    :allocation :class
    :final t
    :visibility :public 
    ,@(when ?documentation `(:documentation ,@?documentation))))

(defmethod defconstant-declaration-p ((e type-body-declaration))
  (and (eq (get-option :allocation e) :class)
       (get-option :final e)))

;;'Assembly' language, which might appear anywhere (as an expression or statement)
(def-syntax java-code (expression statement)
  (java (?is ?text stringp)))

(defmethod get-type ((e java-code))
  (error "BUM"))

(def-unparse java-code (e)
  (princ (java-code-text e)))

;; Java expressions
(def-syntax java-expression (expression)
  (java-expression ?type/type-reference (?is ?text stringp)))

(defmethod get-type ((e java-expression))
  (java-expression-type e))

(def-unparse java-expression (e)
  (princ (java-expression-text e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;throw

(def-syntax throw-statement (statement)
  (throw ?argument/expression))

(def-unparse throw-statement (e)
  (format t "throw ~/pp/;"
	  (throw-statement-argument e)))

;;Special Common Lisp case
(def-transform throw-statement (error (?is ?fmt stringp) . ?args)
  (throw (new 'error (format nil ?fmt . ?args))))

;;Now, several arguments but the first one is a symbol
(def-transform throw-statement (error '(?is ?type symbolp) . ?args)
  (throw (new '?type . ?args)))

;;Computed error
(def-transform throw-statement (error ?expr)
  (throw ?expr))

;;Note that the previous macros can only be used as statement and not as an
;;expression, which invalidates code like
;;(let ((x (if test result (error ...)))) ...)
;;However, the problem can be solved by defining a class method on some
;;utility class that throws the exception.  By the way, it looks
;;increasingly urgent to define such a class to hang up on it lots of stuff
;;that is generically usefull.  Such class could even include the cons
;;stuff that is already being intensively used.

;;;;;;;;;;;;;;;;
;;Method calls are a fundamental part of Linj.  The first type of method
;;call is similar to Smalltalks's messages being sent between objects.

(def-syntax method-call-expression (cached-declaration statement-expression)
  (send ?receiver/expression ?name/linj-name . ?arguments/argument-list))

(def-syntax class-method-call-expression (method-call-expression)
  (send (the ?receiver/type-reference) ?name/linj-name . ?arguments/argument-list))

(defmethod find-declaration ((e method-call-expression))
  (find-declaration-in e (get-type-declaration (method-call-expression-receiver e))))

(defmethod get-type ((e method-call-expression))
  (get-type (strict-find-declaration e)))

;;There is a special kind of method call that creates instances: when the
;;message :initialize-instance is send to a class it returns an instance of
;;that class.  These methods calls are named constructor-calls:

(defmethod constructor-call-p ((e method-call-expression))
  (eq (method-call-expression-name e) :initialize-instance))

;;When we emit the Java code we must distinguish between constructor-calls
;;and other calls as Java has different syntax for these: 
(def-unparse method-call-expression (e)
  (check-initialization e)
  (let ((receiver (method-call-expression-receiver e)))
    (cond ((constructor-call-p e)
	   (if (constructor-declaration-p (containing-method-declaration e))
	     ;;this is not entirely correct. Must check the class
	     (format t "~/pp/~/pp/" receiver (method-call-expression-arguments e))
	     (format t "new ~/pp/~/pp/" receiver (method-call-expression-arguments e))))
	  (t
	   (let ((real-receiver
		  (if (assert-type-expression-p receiver)
		    (assert-type-expression-expression receiver)
		    receiver)))
	     (format t
		     "~@<~:[~*~;~/ppexp/.~1I~_~I~]~/pp/~/pp/~:>"
		     (not (or (this-reference-p real-receiver)
			      (alias-this-reference-p real-receiver)
			      (this-type-reference-p real-receiver)))
		     receiver
		     (method-call-expression-name-for-unparse e)
		     (method-call-expression-arguments e)))))))

(defmethod check-initialization ((call method-call-expression))
  ;;do nothing
  ;;but subclasses will do something
  )

(defmethod method-call-expression-name-for-unparse ((e method-call-expression))
  (method-call-expression-name e))

(defun this-type-reference-p (reference)
  (and (type-reference-p reference)
;;;       (eq (get-option :allocation (containing-method-declaration reference)) :class)
       (let ((containing-type-declaration (containing-type-declaration reference)))
	 ;;We check existence because we might want to unparse a disconnected ast
	 (and containing-type-declaration
	      (equal-type-p reference (type-declaration-name containing-type-declaration))))))

(defmethod declares-p ((elem method-declaration) (call method-call-expression))
  (and (eq (method-call-expression-name call)
	   (method-declaration-name elem))
       (let ((params (parameter-list-elements (method-declaration-parameters elem)))
	     (args (argument-list-elements (method-call-expression-arguments call))))
	 (and (= (length params) (length args))
	      (every #'(lambda (param arg)
			 (super-type-p (get-type param)
				       (get-principal-type arg)))
		     params
		     args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Now, we introduce a more CommonLispy form of method call where the
;;arguments can have keywords and we must resolve the keywords.  Besides,
;;casts and wrappers are inserted as needed.

(def-syntax key-method-call-expression (method-call-expression)
  (key-send ?receiver/expression ?name/linj-name . ?arguments/argument-list)
  :slots ((args-passed :accessor method-call-expression-args-passed :initform :unknown)
	  (phase :accessor computing-phase)))

(def-syntax class-key-method-call-expression (key-method-call-expression)
  (key-send (the ?receiver/type-reference) ?name/linj-name . ?arguments/argument-list))

;;Keyword resolution is done in two phases: first, we look for a perfect
;;match, then we look for a match with casts and wrappers.
;;I'm abusing of the args-passed slot to contain the several search
;;phases. In the end, it will contain either a number or :all.
(defparameter *resolve-keyword-args* t) 

(defmethod check-initialization ((call key-method-call-expression))
  (if *resolve-keyword-args*
    (when (eq (method-call-expression-args-passed call) :unknown)
      ;;To avoid infinite regression
      (setf (method-call-expression-args-passed call) :computing)
      (respecify-method-call call (strict-find-declaration call)))
    (setf (method-call-expression-args-passed call) :all)))

;;To find a method declaration that satisfies a method call we must search
;;all methods implemented by the receiver for one that 'matches'  the
;;keyword parameters.  This might be a little difficult because of two things:
;; - overloading: there might be several methods with the same name,
;; specially in library classes
;;
;; - casts and wrapping: we might need to cast or wrap certain arguments in
;;                       order for them to satisfy the parameters.
;;
;;I think the best solution is to try several possibilities, that is, for
;;each method declaration that matches the method call message (thank God
;;there's something fixed), I'll try to resolve keyword arguments and cast
;;and/wrap arguments in a standard way. If this process fails, the
;;declaration can't be used and I try a different one.  The one declaration
;;that succeeds is permanently attached to the method call (via the
;;cached-declaration mixin) so that the process isn't repeated.
;;I must be carefull, however, to try the matching process first _without_
;;casts and wraps as there might be a method that matches exactly the
;;arguments and this should be prefered to others that need casts and wraps.

;;The tricky part in this process is that most (if not all) parse tree
;;operations are destructive, so I must have a way to 'undo' each failed
;;attempted matching.  Due to the automatic management of parent/child
;;links in the parse tree, I guess that it is enough to keep the original
;;argument list and restore it before each matching.

(defmethod find-declaration ((call key-method-call-expression))
  (some #'(lambda (phase)
	    (setf (computing-phase call) phase)
	    (call-next-method))
	(if (some #'keyword-reference-p
		  (argument-list-elements (key-method-call-expression-arguments call)))
	  '(:computing-keyword-match :computing-aprox-match)
	  '(:computing-perfect-match :computing-keyword-match :computing-aprox-match))))

;;We will use the args-passed slot, somehow violating Pitman's
;;two-bit rule but, for the moment, it seems ok.
(defmethod declares-p ((decl method-declaration) (call key-method-call-expression))
  (and (eq (method-call-expression-name call)
	   (method-declaration-name decl))
       (ecase (computing-phase call)
	 (:computing-perfect-match
	  (call-next-method)
;; 	  (resolve-cl-arguments
;; 	   call
;; 	   decl
;; 	   #'(lambda (param arg)
;; 	       (if (equal-type-p (get-type param)
;; 				 (get-principal-type arg))
;; 		 arg
;; 		 (return-from declares-p nil))))
	  )
	 (:computing-keyword-match
	  (resolve-cl-arguments
	   call
	   decl
	   #'(lambda (param arg)
	       (if (super-type-p (get-type param)
				 (get-principal-type arg))
		 arg
		 (return-from declares-p nil)))))
	 (:computing-aprox-match
	  (resolve-cl-arguments 
	   call
	   decl
	   #'(lambda (param arg)
	       (cast-and-wrap-if-needed
		(get-type param)
		(get-type arg)
		arg
		#'(lambda (expected current expr)
		    (declare (ignore expected current expr))
		    (return-from declares-p nil)))))))))

(defmethod respecify-method-call ((call key-method-call-expression) (decl method-declaration))
  (let ((arguments (key-method-call-expression-arguments call))
	(parameters (method-declaration-parameters decl)))
    (multiple-value-bind (args-passed resolved-args)
	(resolve-cl-arguments call
			       decl
			       #'(lambda (param arg)
				   (convert-to-type arg (get-type param) :force-cast nil)))
      (if args-passed
	(if (= (logcount args-passed)
	       (length (remove :required (parameter-list-elements parameters)
			       :key #'(lambda (param) (get-option :parameter-type param)))))
	  ;;all args used
	  (progn
	    (setf (method-call-expression-args-passed call) :all)
	    (setf (argument-list-elements arguments) resolved-args))
	  (progn
	    (setf (method-call-expression-args-passed call) args-passed)
	    (setf (argument-list-elements arguments)
		  (if (needs-key-method-p decl)
		    (nconc resolved-args (list (parse args-passed 'literal)))
		    resolved-args))))
	(linj-error "Not enough arguments in ~S" call)))))

(defmethod resolve-cl-arguments ((call key-method-call-expression) 
				 (decl method-declaration)
				 transform)
  (let ((args (argument-list-elements (key-method-call-expression-arguments call)))
	(params (parameter-list-elements (method-declaration-parameters decl))))
    (labels ((resolve (params count count-opts unkey-args prev-supplied-p present-args remaining-args)
	       (if (endp params)
		 (and (endp remaining-args)
		      (values present-args (nreverse unkey-args) remaining-args))
		 (let ((param (first params)))
		   (if (supplied-p param)
		     (resolve (rest params)
			      count
			      count-opts
			      (if (simple-parameter-initializations-p decl)
				(cons (parse (if prev-supplied-p 't 'nil) 'literal) unkey-args)
				unkey-args) ;;it will be dealt with in the key method
			      nil
			      present-args
			      remaining-args)
		     (ecase (get-option :parameter-type param)
		       (:required
			(multiple-value-bind (arg found-p)
			    (get-positional-argument count args)
			  (if found-p
			    (resolve (rest params)
				     (1+ count)
				     count-opts
				     (cons (funcall transform param arg)
					   unkey-args)
				     found-p
				     present-args;;(logior present-args (ash 1 count-opts))
				     (remove arg remaining-args :count 1))
			    nil))) ;;; This must fail so that another method can be tried
		       ;;;(linj-error "Missing required argument ~S in ~S" param call)
		       (:optional
			(multiple-value-bind (arg found-p)
			    (get-positional-argument count args)
			  (if found-p
			    (resolve (rest params)
				     (1+ count)
				     (1+ count-opts)
				     (cons (funcall transform param arg)
					   unkey-args)
				     found-p
				     (logior present-args (ash 1 count-opts))
				     (remove arg remaining-args :count 1))
			    (resolve (rest params)
				     (1+ count)
				     (1+ count-opts)
				     (cons (parse
					    (if (and (simple-parameter-initializations-p decl)
						     (initialized-variable-declaration-p param))
					      (ast-node-form (variable-declaration-initializer param))
					      (get-default-initializer (get-type param))) ;;only accept literals
					    'literal)
					   unkey-args)
				     found-p
				     present-args
				     remaining-args))))
		       (:keyword
			(multiple-value-bind (key arg)
			    (get-key-argument (make-keyword 
					       (linj-original-name 
						(variable-declaration-name param)))
					      args)
			  (if key
			    (resolve (rest params)
				     (1+ count)
				     (1+ count-opts)
				     (cons (funcall transform param arg)
					   unkey-args)
				     key
				     (logior present-args (ash 1 count-opts))
				     (remove key (remove arg remaining-args :count 1) :count 1))
			    (resolve (rest params)
				     (1+ count)
				     (1+ count-opts)
				     (cons (parse
					    (if (and (simple-parameter-initializations-p decl)
						     (initialized-variable-declaration-p param))
					      (ast-node-form (variable-declaration-initializer param))
					      (get-default-initializer (get-type param)))
					    'literal)
					   unkey-args)
				     key
				     present-args
				     remaining-args))))
		       (:rest
			(let ((arg (with-parent (call) (parse `(list ,@remaining-args) 'expression))))
			  (apply-visitors 'after-parse arg)
			  (resolve (rest params)
				   (1+ count)
				   (1+ count-opts)
				   (cons (funcall transform param arg)
					 unkey-args)
				   t
				   (logior present-args (ash 1 count-opts))
				   (list))))))))))
      (resolve params 0 0 (list) nil 0 args))))


(defun get-key-argument (key arg-list)
  (labels ((search-keys (args)
	     (cond ((endp args)
		    (values nil nil))
		   ((keyword-reference-p (first args))
		    (if (eq (reference-name (first args)) key)
		      (values (first args) (second args))
		      (search-keys (rest (rest args)))))
		   (t
		    (linj-error "Malformed argument list ~S" arg-list)))))
    (search-keys (member-if #'keyword-reference-p arg-list))))

(defun get-positional-argument (count arg-list)
  (cond ((endp arg-list)
	 (values nil nil))
	((keyword-reference-p (first arg-list))
	 (values nil nil))
	((zerop count)
	 (values (first arg-list) t))
	(t
	 (get-positional-argument (1- count) (rest arg-list)))))

(defmethod all-arguments-passed-p ((call method-call-expression))
  (not (numberp (method-call-expression-args-passed call))))

(defmethod method-call-expression-name-for-unparse ((e key-method-call-expression))
  (if (or (all-arguments-passed-p e)
	  (simple-parameter-initializations-p (find-declaration e)))
    (method-call-expression-name e)
    (parse (generate-key-method-name
	    (method-call-expression-name e)) 'linj-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cast-and-wrap-if-needed (expected current expr error-action &key (force-cast nil))
  (labels ((coercion-error ()
	     (funcall error-action expected current expr))
	   (wrap-for-types (type types)
	     (if (member current types :test #'equal-type-p)
	       ;;`(new ',(ast-node-form expected) ,expr)
	       (make-instance 'allocation-expression
		 :receiver (copy-type type)
		 :arguments (make-instance 'argument-list
			      :original-form 'auto-wrap-for-types
			      :elements (list expr))
		 :parent (ast-node-parent expr))
	       (coercion-error)))
	   (value-of-for-types (type types)
	     (if (member current types :test #'equal-type-p)
	       ;;`(value-of ,(ast-node-form expected) ,expr)
	       (make-instance 'method-call-expression
		 :receiver (copy-type type)
		 :name (parse 'value-of 'linj-name)
		 :original-form 'auto-value-of-for-types
		 :arguments (make-instance 'argument-list
			      :original-form 'auto-arg-value-of-for-types
			      :elements (list expr))
		 :parent (ast-node-parent expr))
	       (coercion-error)))
	   (unwrap-with-method-call (msg)
	       ;;`(send ,expr ',msg)
	     (make-instance 'method-call-expression
	       :receiver expr
	       :name (parse msg 'linj-name)
	       :arguments (parse '() 'argument-list)
	       :original-form 'auto-unwrap-with-method-call
	       :parent (ast-node-parent expr)))
	   (cast-to-expected-type ()
	     ;;`(the ,(ast-node-form expected) ,expr)
	     (make-instance 'cast-expression 
	       :original-form 'auto-cast-to-expected-type
	       :type (copy-type expected)
	       :expression expr
	       :parent (ast-node-parent expr)
	       :force-cast-p force-cast)))
    (cond ((equal-type-p expected current)
	   expr)
	  ((values-type-reference-p current)
	   (if (values-type-reference-p expected)
	     expr
	     (cast-and-wrap-if-needed
	      expected
	      (get-principal-type current)
	      (get-principal-value current expr)
	      error-action
	      :force-cast force-cast)))
	  ((and (primitive-type-reference-p expected)
		(primitive-type-reference-p current))
	   (if (super-type-p expected current)
	       (if force-cast
		   (cast-to-expected-type)
		   expr)
	       (if (super-type-p current expected)
		   (cast-to-expected-type)
		   (coercion-error))))
	  ((primitive-type-reference-p expected)
	   (cond ((equal-type-p current (char-wrapper-type))
		  (if (equal-type-p expected (char-type))
		    (unwrap-with-method-call 'char-value)
		    (coercion-error)))
		 ((equal-type-p current (boolean-wrapper-type))
		  (if (equal-type-p expected (boolean-type))
		    (unwrap-with-method-call 'boolean-value)
		    (coercion-error)))
		 ((or (equal-type-p current (int-wrapper-type))
		      (equal-type-p current (long-wrapper-type))
		      (equal-type-p current (float-wrapper-type))
		      (equal-type-p current (double-wrapper-type))
		      (equal-type-p current (number-wrapper-type))
		      ;;The next ones are a bit dangerous...
		      (equal-type-p current (big-integer-type))
		      (equal-type-p current (big-decimal-type))
		      (equal-type-p current (bignum-type)))
		  (cond ((equal-type-p expected (int-type))
			 (unwrap-with-method-call 'int-value))
			((equal-type-p expected (long-type))
			 (unwrap-with-method-call 'long-value))
			((equal-type-p expected (float-type))
			 (unwrap-with-method-call 'float-value))
			((equal-type-p expected (double-type))
			 (unwrap-with-method-call 'double-value))
			(t
			 (coercion-error))))
		 ((and (equal-type-p expected (boolean-type))
		       (class-or-interface-type-reference-p current))
		  (from-reference-type-to-boolean expr))
		 ((equal-type-p current (object-type))
		  (cast-and-wrap-if-needed
		   expected
		   (generic-wrapper-type expected)
		   (cast-and-wrap-if-needed
		    (generic-wrapper-type expected)
		    current
		    expr
		    error-action)
		   error-action))
		 (t
		  (coercion-error))))
	  ((primitive-type-reference-p current)
	   (cond ((and (boolean-type-p current) (false-literal-p expr))
		  ;;to allow (and a b c) where a b c return object
		  (make-literal (null-value)))
		 ((equal-type-p expected (char-wrapper-type))
		  (wrap-for-types expected (list (char-type))))
		 ((equal-type-p expected (boolean-wrapper-type))
		  (value-of-for-types expected (list (boolean-type))))
		 ((equal-type-p expected (int-wrapper-type))
		  ;;Integer only accepts int
		  (wrap-for-types expected (list (int-type))))
		 ((equal-type-p expected (long-wrapper-type))
		  ;;Long accepts long and int (which is automatically converted to long)
		  (wrap-for-types expected (list (int-type) (long-type))))
		 ((or (equal-type-p expected (float-wrapper-type))
		      ;;Float or Double accepts either double or float
		      (equal-type-p expected (double-wrapper-type)))
		  (wrap-for-types expected (list (float-type) (double-type))))
		 ;;Special case: bignums
		 ((equal-type-p expected (bignum-type))
		  (value-of-for-types expected (list (int-type) (long-type))))
		 ;;Special case: BigIntegers
		 ((equal-type-p expected (big-integer-type))
		  (value-of-for-types expected (list (int-type) (long-type))))
		 ((equal-type-p expected (big-decimal-type))
		  (if (or (equal-type-p current (float-type))
			  (equal-type-p current (double-type)))
		    ;;use allocation
		    (wrap-for-types expected (list (float-type) (double-type)))
		    ;;use value-of
		    (value-of-for-types expected (list (int-type) (long-type)))))
		 ((equal-type-p expected (number-wrapper-type))
		  (if (or (equal-type-p current (int-type))
			  (equal-type-p current (long-type))
			  (equal-type-p current (float-type))
			  (equal-type-p current (double-type)))
		    (wrap-for-types (wrapper-type current) (list current))
		    (coercion-error)))
		 ;;special case - from a primitive type to a generic
		 ;;reference type <object>
		 ((equal-type-p expected (object-type))
		  (cond ((and *bignum-arithmetic-p*
			      (or (int-type-p current) (long-type-p current)))
			 (value-of-for-types (bignum-type) (list (int-type) (long-type))))
			((boolean-type-p current)
			 (value-of-for-types (boolean-wrapper-type) (list current)))
			(t 
			 (wrap-for-types (wrapper-type current) (list current)))))
		 (t
		  (coercion-error))))
	  ;;both non-primitive
	  ((and (class-or-interface-type-reference-p expected)
		(class-or-interface-type-reference-p current))
	   (cond ((super-type-p expected current)
		  (if force-cast
		    (cast-to-expected-type)
		    expr))
		 ((super-type-p current expected)
		  ;;Here, we assume the programmer want's an automatic type-cast
		  (cast-to-expected-type))
		 ;;Special case BigInteger -> BigDecimal
		 ((equal-type-p expected (big-decimal-type))
		  (wrap-for-types expected (list (big-integer-type))))
		 ((equal-type-p expected (bignum-type))
		  (value-of-for-types expected (list (big-integer-type))))
		 ((and (equal-type-p expected (big-integer-type))
		       (equal-type-p current (bignum-type)))
		  (unwrap-with-method-call 'to-big-integer))
		 (t
		  (coercion-error))))
	  (t
	   (coercion-error)))))

(defun convert-to-type (e type &key (force-cast t))
  (cast-and-wrap-if-needed type
			   (get-type e)
			   e
			   #'(lambda (expected current expr)
			       (linj-error "Coercion error! Expr ~S has type ~S but expecting ~S" expr current expected))
			   :force-cast force-cast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Alocation

(def-syntax allocation-expression (key-method-call-expression)
  (new '?receiver/type-reference . ?arguments/argument-list)
  :slots ((name :initform (parse ':initialize-instance 'linj-name))))

(defmethod get-type ((e allocation-expression))
  (method-call-expression-receiver e))

(def-unparse allocation-expression (e)
  (check-initialization e)
  (format t "new ~/pp/~/pp/"
	  (method-call-expression-receiver e)
	  (method-call-expression-arguments e)))

;;;;;;;;;;;;;
;;Let's allow for a more Common Lispy way:

(def-macro-transform allocation-expression (make-instance '?type . ?args)
  `(new ',?type . ,?args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Let's replace super.???(...) with call-next-method

(def-syntax next-method-call-expression (method-call-expression)
  (call-next-method . ?arguments/argument-list))

(defmethod method-call-expression-receiver :before ((e next-method-call-expression))
  (check-initialization e))

(defmethod method-call-expression-name :before ((e next-method-call-expression))
  (check-initialization e))

(defmethod method-call-expression-arguments :before ((e next-method-call-expression))
  (check-initialization e))

(defmethod visit :before ((e next-method-call-expression) (visitor parse-tree-finish))
  (check-initialization e))

(defmethod check-initialization ((e next-method-call-expression))
  (unless (slot-boundp e 'receiver)
    (let ((method (containing-method-declaration e))
	  (args (argument-list-elements (next-method-call-expression-arguments e))))
      (if (endp args)
	(setf (method-call-expression-arguments e)
	      (parse (mapcar #'variable-declaration-name
			     (parameter-list-elements
			      (method-declaration-parameters method)))
		     'argument-list))
	(let ((receiver (find-if #'alias-this-reference-p args)))
	  (when receiver
	    (setf (argument-list-elements (next-method-call-expression-arguments e))
		  (delete receiver args)))))
      ;;We must distinguish the case of a recently 'adapted' :around-combination
      ;;and the general case
      (if (member :around-combination (get-option :qualifiers method))
	(setf (method-call-expression-name e) (parse (make-primary-name (method-declaration-name method)) 'linj-name)
	      (method-call-expression-receiver e) (parse 'this 'reference))
	(setf (method-call-expression-name e) (method-declaration-name method)
	      (method-call-expression-receiver e) (parse 'super 'reference))))))

;;When searching for the type of a next-method-call-expression we don't
;;need to look at the superclass.  We know the type is the same as the
;;return type of the containing method. This is specially important in
;;mixins, where we can't get the type of a next-method-call-expression
;;because the superclass doesn't exist.

(defmethod get-type ((e next-method-call-expression))
  (get-type (or (find-declaration e) (containing-method-declaration e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Slot access.  In theory, slot access will be mediated by readers and
;;writers.  It's easier, however, to implement basic means to access the
;;slot and define readers and writers on top of this.
(def-syntax slot-value-reference (expression cached-declaration)
  (slot-value ?object/expression '?slot/reference))

;;For legacy reasons, we also accept the following:
(def-syntax special-slot-value-reference (slot-value-reference)
  (slot-value (the ?object/type-reference) '?slot/reference))

;;HACK : Must use read-macro #s(?object slot)

(defmethod find-declaration ((e slot-value-reference))
  (find-declaration-in (slot-value-reference-slot e)
		       (get-type-declaration (slot-value-reference-object e))))

(defmethod find-declaration-in ((ref reference) (e slot-value-reference))
  (if (eq ref (slot-value-reference-slot e))
    (find-declaration e)
    (call-next-method)))

(defmethod get-type ((e slot-value-reference))
  ;;doesn't need to check this and super
  (get-type (strict-find-declaration e)))

(def-unparse slot-value-reference (e)
  (if (or (this-reference-p (slot-value-reference-object e))
	  (alias-this-reference-p (slot-value-reference-object e)))
    (let ((decl (find-declaration-in (slot-value-reference-slot e)
				     (ast-node-parent e)))
	  (class-decl (find-declaration e)))
      (if (and (not (null class-decl)) (eq decl class-decl))
	(format t "~/pp/" (slot-value-reference-slot e))
	(format t "~/ppexp/.~/unlinj-name/"
		(slot-value-reference-object e) (reference-name (slot-value-reference-slot e)))))
    (format t "~/ppexp/.~/unlinj-name/"
	    (slot-value-reference-object e) (reference-name (slot-value-reference-slot e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;We now(??) reach compilation-units. 
;;Each file is a compilation unit, which is composed by:
;;zero or one package declaration,
;;zero or more import statements,
;;one or more class declarations, including one public class declaration
;;which names the file.

;;In addition, (java lang *) is automatically imported on every compilation
;;unit (7.)

(def-syntax compilation-unit-member (top-level-form)
  (never-matches))

(defmacro with-compilation-options ((options) &rest body)
  (let ((vars '(*write-category-comment* 
		*check-super-method-type*
		*check-assignments-to-closure-variables*
		*parallel-assignment-can-reorder*)))
    `(let ,(mapcar #'(lambda (var)
		       `(,var ,var))
		   vars)
      (destructuring-bind (&key ,@(mapcar #'(lambda (var)
					      `((,(intern (subseq (symbol-name var) 1 (1- (length (symbol-name var))))
							  (find-package :keyword))
						 ,var) ,var))
					  vars))
	  ,options
	,@body))))

;;package

(def-syntax package-declaration (compilation-unit-member)
  (package ?name))

(defmethod package-declaration-components ((e package-declaration))
  (if (null (package-declaration-name e))
    (empty-package)
    (with-linj-syntax ()
      (mapcar #'read-from-string (parse-dots (princ-to-string (package-declaration-name e)))))))

(def-unparse package-declaration (e)
  (format t "package ~{~/pp/~^.~};" (package-declaration-components e)))

;;imports
(def-syntax import-declaration (compilation-unit-member)
  (import ?type/type-reference))

(defmethod visit ((n import-declaration) (visitor t))
  ;;don't visit the type
  (visit-current n visitor))

(defmethod visit :before ((n import-declaration) (visitor parse-tree-finish))
  (set-if-unknown-package (import-declaration-type n) (empty-package)))

(defun import-declaration-components (import)
  (let ((type (import-declaration-type import)))
    (append (class-or-interface-type-reference-package type)
	    (list (class-or-interface-type-reference-name type)))))

(def-unparse import-declaration (e)
  (let ((type (import-declaration-type e)))
    (if (empty-package-p (class-or-interface-type-reference-package type))
	(format t "import ~/unlinj-type/;"
		(class-or-interface-type-reference-name type))
	(format t "import ~{~A.~}~/unlinj-type/;"
		(class-or-interface-type-reference-package type)
		(class-or-interface-type-reference-name type)))))

(defun new-import-declaration (type)
  (make-instance 'import-declaration
    :type type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;in declarations

(def-syntax in-declaration (compilation-unit-member)
  (in (the ?object/type-reference)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;type declarations

(def-list-syntax type-body (linj-list-node) type-body-declaration)

(def-category type-declaration (declaration-options compilation-unit-member)
  ((name :reader type-declaration-name)
   (body :accessor type-declaration-body)))

(defmethod get-type ((e type-declaration))
  (type-declaration-name e))


(def-syntax class-declaration (type-body-declaration type-declaration)
  (defclass-0 ?name/type-reference
      ?superclass/type-reference
    ?mixins/type-reference-list
    ?options . ?body/type-body))

(def-syntax root-class-declaration (class-declaration)
  (defclass-0 ?name/type-reference nil
    ?mixins/type-reference-list
    ?options . ?body/type-body)
  :slots ((superclass :initform (make-instance 'class-or-interface-type-reference
				  :name nil
				  :inferred-package nil))))

(defmethod class-slots ((e class-declaration))
  (remove-if-not #'slot-declaration-p (type-body-elements (class-declaration-body e))))

(defmethod class-statics ((e class-declaration))
  (remove-if-not #'defvar-declaration-p (type-body-elements (class-declaration-body e))))

(defmethod self-or-containing-type-declaration ((e type-declaration))
  e)

;;;;;;;;;;;;;;

(defmethod declares-p ((elem t) (what t))
  nil)

(defmethod declares-p ((elem slot-declaration) (ref reference))
  (eq (reference-name ref)
      (variable-declaration-name (slot-declaration-statement elem))))

(defmethod declares-p ((elem type-declaration) (ref type-reference))
  (equal-type-p (type-declaration-name elem) ref))

(defmethod find-declaration-in (what (e class-declaration))
  (or ;;find in this class
      (find-declaration-in-type what e)
      ;;go through the superclass before going through the outerclass
      ;;except when we are at the root class
      (and (not (root-class-declaration-p e))
	   (or (find-declaration-in what (get-type-declaration (class-declaration-superclass e)))
	       ;;go through the mixins before going through the outerclass
	       (some #'(lambda (mixin)
			 (find-declaration-in what (get-type-declaration mixin)))
		     (type-reference-list-elements (class-declaration-mixins e)))
	       (call-next-method)))))

;;Allocation expressions have a special rule. If a constructor is found on
;;the current class, we can't use another one from a superclass.  Moreover,
;;if a constructor is not found, then the default constructor (that will be
;;built automatically by the Java compiler doesn't accept arguments.
(defparameter *default-constructor* (parse '(defnew ()) 'method-declaration))

(defmethod find-declaration-in ((alloc allocation-expression) (e class-declaration))
  ;;First, search on current class for constructors
  (let ((name (method-call-expression-name alloc)))
    (if (some #'(lambda (elem)
		  (and (method-declaration-p elem)
		       (eq name (method-declaration-name elem))))
	      (type-body-elements (class-declaration-body e)))
      ;;There are constructors. We _must_ find one here.
      (find-declaration-in-type alloc e)
      ;;Only allow for default constructor
      *default-constructor*)))


(defmethod find-declaration-in-type (what (e type-declaration))
  (find-if #'(lambda (elem)
	       (declares-p elem what))
	   (type-body-elements (type-declaration-body e))))

(defmethod find-declaration-in-type ((what type-reference) (e type-declaration))
  (if (equal-type-p what (type-declaration-name e))
    e
    (call-next-method)))

(defmethod find-declaration-in-type ((what method-call-expression) (e class-declaration))
  (let ((meths (remove-if-not #'(lambda (elem)
				  (declares-p elem what))
			      (type-body-elements (class-declaration-body e)))))
    (cond ((endp meths)
	   nil)
	  ((endp (rest meths))
	   (first meths))
	  (t
	   (first (sort meths ;;Must choose most specific method.
			#'(lambda (params1 params2)
			    (every #'super-type-p (mapcar #'get-type params2) (mapcar #'get-type params1)))
			:key #'(lambda (m)
				 (parameter-list-elements (method-declaration-parameters m)))))))))

(defmethod find-methods-named (name (e class-declaration) &optional (exclude-self-p t))
  (append
   (and (not exclude-self-p)
	(remove-if-not #'(lambda (elem)
			   (and (method-declaration-p elem)
				(eq name (method-declaration-name elem))))
		       (type-body-elements (class-declaration-body e))))
   (and (not (root-class-declaration-p e))
	(append
	 ;;go through the superclass
	 (find-methods-named name
			     (get-type-declaration (class-declaration-superclass e)) 
			     nil)
	 ;;go through the mixins
	 (reduce #'append
		 (type-reference-list-elements (class-declaration-mixins e))
		 :key #'(lambda (mixin)
			  (find-methods-named name (get-type-declaration mixin) nil))
		 :from-end t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *write-category-comment* t)

(defun write-type-body-elements (text elems)
  (unless (null elems)
    (if *write-category-comment*
      (format t "~:@_~:@_~A~:@_" text)
      (format t "~:@_"))
    (format t "~{~:@_~/pp/~^~:@_~}" elems)))

(defparameter *key-method-just-for-initialized-parameters* nil)

(defun needs-key-method-p (decl)
  (and (not (getf (method-declaration-options decl) :no-key-method))
       (some (if *key-method-just-for-initialized-parameters*
	       #'initialized-variable-declaration-p
	       #'(lambda (param)
		   (and (member (get-option :parameter-type param)
				'(:optional :keyword))
			(initialized-variable-declaration-p param)
			(not (typep (variable-declaration-initializer param) 'literal)))))
	     (parameter-list-elements (method-declaration-parameters decl)))))

;;Trace
(defun needs-trace-method-p (decl)
  (getf (method-declaration-options decl) :trace))

(defun forget-trace-method (decl)
  (setf (getf (method-declaration-options decl) :trace) nil))

(defun abstract-class-declaration-p (e)
  (or (eq (get-option :category e) :abstract)
      (some #'abstract-method-declaration-p
	    (type-body-elements (type-declaration-body e)))))

(defmethod class-allocated-reference-p ((e reference))
  (and (not (this-reference-p e))
       (not (super-reference-p e))
       (not (alias-this-reference-p e))
       (let ((decl (strict-find-declaration e)))
	 (and (slot-declaration-p decl)
	      (eq (get-option :allocation decl) :class)))))

(def-unparse class-declaration (e)
  (pprint-logical-block (*standard-output* nil)
    (write-documentation e)
    (format t "~:[~;abstract ~]~/write-options/class ~/pp/" 
	    (abstract-class-declaration-p e) e (class-declaration-name e))
    (let ((super (class-declaration-superclass e))
	  (mixins (type-reference-list-elements (class-declaration-mixins e))))
      (unless (root-class-declaration-p super)
	(format t " extends ~/pp/" super))
      (unless (null mixins)
	(format t " implements ~{~/pp/~^, ~}" mixins)))
    (unparse-type-body-declaration
     (type-body-elements (type-declaration-body e)))))

(defparameter *type-body-declarations-order*
  '(("// constructors" :constructors)
    ("// member classes" :member-classes)
    ("// finalizers" :finalizers)
    ("// accessors" :accessors)
    ("// methods" :methods)
    ("// key methods" :key-methods)
    ("// trace methods" :tracing-methods)
    ("// constants" :constants)
    ("// slots" :slots)
    ("// static blocks" :static-blocks)))

(defmethod unparse-type-body-declaration (elems)
  (format t " {~4I")
  (let ((cats (make-hash-table :test #'eq)))
    (flet ((add-to-category (elem cat)
	     (push elem (gethash cat cats (list))))
	   (category-elements (cat)
	     (reverse (gethash cat cats))))
      (dolist (e elems)
	(cond ((constructor-declaration-p e)
	       (add-to-category e :constructors)
	       (when (needs-key-method-p e)
		 (add-to-category (generate-key-method e) :key-methods)))
	      ((type-declaration-p e)
	       (add-to-category e :member-classes))
	      ((accessor-declaration-p e)
	       (add-to-category e :accessors))
	      ((method-declaration-p e)
	       (add-to-category e :methods)
	       (when (needs-key-method-p e)
		 (add-to-category (generate-key-method e) :key-methods))
	       (when (needs-trace-method-p e)
		 ;;save old-name
		 (add-to-category (cons (method-declaration-name e) e) :traced-methods)
		 ;;as it will be changed after this
		 (add-to-category (generate-trace-method e) :tracing-methods)))
	      ((constant-declaration-p e)
	       (add-to-category e :constants))
	      ((slot-declaration-p e)
	       (add-to-category e :slots))
	      ((static-block-p e)
	       (add-to-category e :static-blocks))
	      (t
	       (error "Unknown declaration ~W" e))))
      (loop for (comment category) in *type-body-declarations-order*
	    do (write-type-body-elements comment (category-elements category)))
      (format t "~I~:@_}")
      ;;restore traced methods
      (loop for (name . meth) in (category-elements :traced-methods)
	    do (setf (method-declaration-name meth) name)))))

;;method-declarations

(defun generate-key-method (meth)
  (let ((new-meth (with-parent ((ast-node-parent meth))
		    (make-key-method meth))))
    (apply-visitors 'after-parse new-meth)
    new-meth))

;;A strategy that is also applicable to constructors
(defun make-key-method (meth-decl)
  (let ((name (method-declaration-name meth-decl))
	(parameters (parameter-list-elements (method-declaration-parameters meth-decl)))
	(options (method-declaration-options meth-decl))
	(count 0)
	(last-name 'args-passed))
    (let ((new-name (generate-key-method-name name))
	  (call-expr
	   `(send this ,name
		  ,@(mapcar #'(lambda (param)
				(if (eq (get-option :parameter-type param) :required)
				  (linj-original-name (variable-declaration-name param))
				  (if (supplied-p param)
				    `(not (= (logand ,last-name ,(ash 1 (1- count))) 0))
				    (prog1
					`(if (= (logand ,last-name ,(ash 1 count)) 0)
					  ,(or (and (variable-declaration-initializer param)
						    (ast-node-form (variable-declaration-initializer param)))
					       (get-default-initializer (get-type param)))
					  ,(linj-original-name (variable-declaration-name param)))
				      (incf count)))))
			    parameters)))
	  (key-params (remove-if #'supplied-p parameters)))
      (parse-method
       `(defmethod ,new-name
	 (,@(mapcar #'variable-declaration-name key-params) ,last-name)
	 ,@options
	 (declare ,@(mapcar #'(lambda (var)
				`(,(get-type var) ,(variable-declaration-name var)))
			    key-params)
	  (,(int-type) ,last-name)
	  ;;missing throws (reported by Espen Wiborg <espenhw@grumblesmurf.org>)
	  (throws ,@(mapcar #'type-reference-name (method-declaration-throws-list meth-decl))))
	 ,call-expr)))))

(defun generate-key-method-name (name)
  (conc-symbol name '-key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-syntax nested-class-declaration (type-body-declaration)
  ?statement/class-declaration)

(def-unparse nested-class-declaration (e)
  (unparse-object (nested-class-declaration-statement e) *standard-output*))

;;Each slot is something like
;;(name :type type :initform init :visibility :public :allocation :class :reader get-name)
;;or, to follow the convention for variable declarations
;;(name/type :initform init :visibility :public :allocation :class :reader get-name)

(defun parse-slot-declaration (slot)
  ;;First, detect slot/type form
  (when (name&type-p (first slot))
    (multiple-value-bind (name type)
	(extract-name&type (first slot))
      (setf slot (list* name :type type (rest slot)))))
  (if (member :initform (rest slot))
    (values (first slot)
	    (getf (rest slot) :type)
	    ;;For the init we build a list because nil is also a possible init (meaning Java's false)
	    (list (getf (rest slot) :initform))
	    (rest slot))
    (values (first slot)
	    (or (getf (rest slot) :type) (object-type))
	    nil
	    (rest slot))))

(defconstant +reader-name-prefix+ 'get-)

(defconstant +writer-name-prefix+ 'set-)

(defun refine-slot-options (name options)
  (cond ((endp options)
	 (list))
	((endp (rest options))
	 (error "Malformed slot list terminating with ~A" options))
	((eq (second options) 't)
	 (ecase (first options)
	   ((:initarg)
	    (cons (first options)
		  (cons (make-keyword name)
			(refine-slot-options name (rest (rest options))))))
	   ((:reader)
	    (cons (first options)
		  (cons (conc-symbol +reader-name-prefix+ name)
			(refine-slot-options name (rest (rest options))))))
	   ((:writer)
	    (cons (first options)
		  (cons (conc-symbol +writer-name-prefix+ name)
			(refine-slot-options name (rest (rest options))))))
	   ((:accessor)
	    (refine-slot-options
	     name
	     (append (list :reader t :writer t)
		     (rest (rest options)))))
	   ((:type :initform :final :transient :strictfp)
	    (cons (first options)
		  (cons (second options)
			(refine-slot-options name (rest (rest options))))))))
	(t
	 (ecase (first options)
	   ((:accessor)
	    (refine-slot-options
	     name
	     (append (list :reader (second options) :writer `(setf ,(second options)))
		     (rest (rest options)))))
	   ((:reader :writer :type :documentation :initarg
	     :initform :final :transient :strictfp :visibility)
	    (cons (first options)
		  (cons (second options)
			(refine-slot-options name (rest (rest options))))))))))

(defun create-reader-method (reader-name name type)
  `(defmethod-0 ,(or type :infer-it) ,reader-name
     (this) ()
     ,(list :category :reader :visibility :public) ;;might be destructively modified.
     (slot-value this ',name)))

(defun create-writer-method (writer-name name type inits)
  `(defmethod-0 :infer-it ,writer-name
    ,(if (symbolp writer-name)
	 `(this ,(build-variable-declaration-description name type inits))
	 `(,(build-variable-declaration-description name type inits) this))
    ()
    ,(list :category :writer :visibility :public) ;;might be destructively modified.
    (setf (slot-value this ',name) ,name)))

(defun build-variable-declaration-description (name type inits)
  (cond ((and type inits) `(,name (the ,type ,(first inits))))
	(type `(,name (the ,type)))
	(inits `(,name (the :infer-it ,(first inits))))
	(t `(,name (the ,(object-type-description))))))

(defun unbuild-variable-declaration-description (decl)
  (values (first decl)
	  (second (second decl))
	  (and (rest (rest (second decl)))
	       (list (third (second decl))))))

(defun define-slots-and-accessors (slots)
  (mapcan #'(lambda (slot)
	      (multiple-value-bind (name type inits options)
		  (parse-slot-declaration slot)
		(let ((options (refine-slot-options name options)))
		  (cons `(defslot-0 
			     ,(build-variable-declaration-description name type inits)
			     ,@options :visibility :protected)
			(let ((meths (list)))
			  (dolist-2 ((key value) options)
			    (case key
			      ((:reader)
			       (push (create-reader-method value name type) meths))
			      ((:writer)
			       (push (create-writer-method value name type inits) meths))
			      (t
			       )))
			  (nreverse meths))))))
	  slots))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;There are several things we want to do on class-declarations, namely
;;automatically generate constructors and also importing mixin code.

;;One of the most stupid things about the java language is the need for
;;constructors with as many parameters as the non-initialized class slots
;;and which just assign to 'this.slot' the value of the parameter 'slot'.
;;We will mechanize this.

;;Another of the (many) shortcomings of the Java language is the
;;non-inheritable constructors.  It would be very nice if all subclasses
;;could be initialized using the same constructor signature of some
;;superclass.
(defparameter *no-mixin-inheritance* nil
  "Don't use mixin inheritance.")

(defmethod visit :before ((e class-declaration) (visitor parse-tree-finish))
  ;;Mixins
  (unless *no-mixin-inheritance*
    (let ((mixins (mapcar #'get-type-declaration
			  (type-reference-list-elements (class-declaration-mixins e)))))
      (copy-mixins-declarations mixins e)))
  ;;Default constructors:
;;;Anonymous classes deserve special treatment:
;;;15.9.5.1 Anonymous Constructors
;;;An anonymous class cannot have an explicitly declared
;;;constructor. Instead, the compiler must automatically provide an
;;;anonymous constructor for the anonymous class. 
  (unless (or (get-option :no-default-constructor e)
	      (anonymous-class-declaration-p e) ;;don't apply this to anonymous classes!!
	      (find-if #'constructor-declaration-p
		       (type-body-elements (class-declaration-body e))))
    (add-constructors e))
  ;;Method combination:
  (unless (or (get-option :no-method-combinations e)
	      (mixin-declaration-p (find-declaration (class-declaration-superclass e))))
    (operate-methods-combination e))
  ;;Multiple dispatch:
  (unless (get-option :no-multiple-dispatch e)
    (operate-method-multi-dispatch e))
  ;;Parse methods.
  (when (or (get-option :make-xml-parse-method e)
	    (get-option :make-parse-method e))
    (let ((return-type (type-declaration-name e))
	  (slots (remove-if-not #'(lambda (elem)
				    (and (slot-declaration-p elem)
					 (not (constant-declaration-p elem))))
				(collect-all-slots e))))
      (let ((methods
	     (if (get-option :make-parse-method e)
	       (generate-parse-methods return-type slots)
	       (generate-xml-parse-methods return-type slots)))
	    (elems (type-body-elements (class-declaration-body e))))
	(dolist (meth methods)
	  (push (with-parent (elems) (parse-method meth))
		(type-body-elements (class-declaration-body e))))))))

;;To be used just after importing the mixins
(defmethod find-redefined-method (method (class class-declaration) &optional (exclude-self-p t))
  (or (and (not exclude-self-p)
	   (find-if #'(lambda (elem)
			(and (method-declaration-p elem)
			     (same-signature-p method elem)))
		    (type-body-elements (class-declaration-body class))))
      (and (not (root-class-declaration-p class))
	   ;;go through the superclass
	   (find-redefined-method method
				  (get-type-declaration (class-declaration-superclass class))
				  nil))))

(defmethod primary-method-p ((meth method-declaration))
  (endp (get-option :qualifiers meth)))

(defmethod before-method-p ((meth method-declaration))
  (member :before (get-option :qualifiers meth)))

(defmethod after-method-p ((meth method-declaration))
  (member :after (get-option :qualifiers meth)))

(defmethod around-method-p ((meth method-declaration))
  (member :around (get-option :qualifiers meth)))

(defun make-primary-name (name)
  (conc-symbol 'primary- name))

(defun make-before-name (name class-name)
  (conc-symbol 'before- name '- class-name))

(defun make-after-name (name class-name)
  (conc-symbol 'after- name '- class-name))

(defun operate-methods-combination (class)
  (let ((same-signature-methods (list))
	(class-name (type-declaration-name class)))
    (dolist (elem (type-body-elements (class-declaration-body class)))
      (when (method-declaration-p elem)
	(let ((bind (assoc elem same-signature-methods :test #'same-signature-p)))
	  (if bind
	    (setf (cdr bind) (cons elem (cdr bind)))
	    (setf same-signature-methods (acons elem nil same-signature-methods))))))
    (dolist (methods same-signature-methods)
      (let ((befores (remove-if-not #'before-method-p methods))
	    (afters (remove-if-not #'after-method-p methods))
	    (around (find-if #'around-method-p methods))
	    (primary (find-if #'primary-method-p methods))
	    (name (ast-node-form (method-declaration-name (first methods))))
	    (parameters (method-declaration-parameters (first methods)))
	    (redef (find-redefined-method (first methods) class)))
	(let ((redef-combination (and redef (intersection '(:standard-combination :around)
							  (get-option :qualifiers redef)))))
	  (dolist (method methods)
	    (cond ((primary-method-p method)
		   (when (or befores afters redef-combination)
		     (setf (method-declaration-name method)
			   (parse (make-primary-name name) 'linj-name))))
		  ((before-method-p method)
		   (setf (method-declaration-name method)
			 (parse (make-before-name name class-name) 'linj-name)))
		  ((after-method-p method)
		   (setf (method-declaration-name method)
			 (parse (make-after-name name class-name) 'linj-name)))
		  ((around-method-p method)
		   (when (or befores afters primary)
		     (error "Can't mix primary, before or after method with an around on the same class"))
		   (unless redef-combination
		     ;;Around without previous method combination.
		     ;;Must use around but converting call-next-method to call the primary
		     ;;We will leave that to the next-method-call
		     (push :around-combination (get-option :qualifiers method))))
		  (t
		   (error "Unknown method ~A" method))))
	  (when (or befores afters around)
	    (let ((args (mapcar #'variable-declaration-name (parameter-list-elements parameters))))
	      (unless (or primary redef-combination)
		(setf primary
		      (parse-method
		       `(defmethod ,(make-primary-name name) ,(ast-node-form parameters)
			 (send super ,name ,@args))))
		(push primary (type-body-elements (class-declaration-body class))))
	      (unless around
		;;Must build a method combination
		(let ((call (if redef-combination
			      `(call-next-method)
			      `(send this ,(make-primary-name name) ,@args))))
		  (push (parse-method
			 `(defmethod ,name :standard-combination ,(ast-node-form parameters)
			   ,@(mapcar #'(lambda (before)
					 (declare (ignore before))
					 ;;HACK: this must be improved to concat all the befores
					 `(send this ,(make-before-name name class-name) ,@args))
				     befores)
			   ,@(if afters
				 ;;HACK: the same thing must be done here...
				 (let ((forms `(,call
						(send this ,(make-after-name name class-name) ,@args))))
				   (if (void-type-p (get-type (or primary redef)))
				     forms
				     `((prog1 ,@forms))))
				 `(,call))))
			(type-body-elements (class-declaration-body class))))))))))))

;; (defun make-class-name (name class-name)
;;   (conc-symbol name '- class-name))

(defun specialized-parameters (meth)
  (remove-if-not #'(lambda (param)
		     (get-option :specialized param))
		 (parameter-list-elements (method-declaration-parameters meth))))

(defun operate-method-multi-dispatch (class)
  (let ((multi-methods (list)))
    (dolist (elem (type-body-elements (class-declaration-body class)))
      (when (and (method-declaration-p elem)
		 (some #'(lambda (param)
			   (get-option :specialized param))
		       (parameter-list-elements (method-declaration-parameters elem)))
		 (not (some #'(lambda (multi-method-list)
				(member elem multi-method-list))
			    multi-methods)))
	;;This method needs double dispatch
	(let ((ref-params (parameter-list-elements (method-declaration-parameters elem))))
	  (let ((generic-function-methods
		 (remove-if-not
		  #'(lambda (meth)
		      (let ((params
			     (parameter-list-elements
			      (method-declaration-parameters meth))))
			(and (= (length ref-params) (length params))
			     (every #'(lambda (ref-param param)
					(let ((ref-specialized (get-option :specialized ref-param))
					      (specialized (get-option :specialized param)))
					  (eq (and ref-specialized t)
					      (and specialized t))))
				    ref-params
				    params))))
		  (find-methods-named (method-declaration-name elem) class nil))))
	    (push (stable-sort
		   (remove-duplicates generic-function-methods :test #'same-signature-p :from-end t)
		   #'(lambda (params1 params2)
		       (every #'super-type-p (mapcar #'get-type params1) (mapcar #'get-type params2)))
		   :key #'specialized-parameters)
		  multi-methods)))))
    (dolist (generic-function-methods multi-methods)
      (let ((bodies (mapcar (compose #'ast-node-form #'method-declaration-body) generic-function-methods)))
	(loop
	 for (base-meth . meths) on generic-function-methods
	 and base-body in bodies 
;;	 do (print (list base-meth meths))
	 do (let* ((name (ast-node-form (method-declaration-name base-meth)))
		   (base-params (parameter-list-elements (method-declaration-parameters base-meth)))
		   (base-param (find-if #'(lambda (param) (get-option :specialized param)) base-params))
		   (dispatch-position (position base-param base-params))
		   (supertype
		    (reduce #'merge-types
			    (cons base-meth meths)
			    :key #'(lambda (meth)
				     (get-type
				      (nth dispatch-position
					   (parameter-list-elements
					    (method-declaration-parameters meth)))))))
		   (more-specific
		    (position-if #'(lambda (m)
				     ;;(format *trace-output* "Method: ~A~%base-param: ~A" m base-param)
				     (and (member m (type-body-elements (class-declaration-body class)))
					  (let* ((base-params (parameter-list-elements (method-declaration-parameters m)))
						 (base-param2 (find-if #'(lambda (param) (get-option :specialized param)) base-params)))
					    ;;(format *trace-output* "base-param2: ~A~%" base-param2)
					    (super-type-p (get-type base-param2) (get-type base-param)))))
				 generic-function-methods
				 :from-end t)))
	      (if (equal-type-p supertype (get-type base-param))
		(if (member base-meth (type-body-elements (class-declaration-body class)))
		  (setf (method-declaration-body base-meth)
			(parse (list (dispatch-body
				      name
				      (first (ast-node-form base-param))
				      dispatch-position
				      (reverse meths)
				      (if more-specific
					`((macrolet ((call-next-method ()
						       `,',(let ((new-params (parameter-list-elements (method-declaration-parameters (nth more-specific generic-function-methods)))))
								`(send this ,name
								  ,@(mapcar #'(lambda (param)
										(if (get-option :specialized param)
										  (parse `(force-the ,(copy-type (get-type param)) ,(first (ast-node-form base-param)))
											 'cast-expression)
										  (first (ast-node-form param))))
									    new-params)))))
					    ,@base-body))
					base-body)))
			       'statement-list)
			(inferred-type-node-type base-param) (with-parent (base-param) supertype))
		  (push (parse-method
			 `(defmethod ,name (,(first
					      (ast-node-form
					       (method-declaration-parameters base-meth)))
					    ,@(mapcar #'(lambda (param)
							  (if (eq param base-param)
							    `(,(first (ast-node-form param))
							      (the ,supertype))
							    param))
						      base-params))
			   ,(dispatch-body
			     name
			     (first (ast-node-form base-param))
			     dispatch-position
			     (reverse meths)
			     ;;This method came from a superclass. Do we
			     ;;have a more specific version (that is, a
			     ;;method belonging to this class that is
			     ;;specialized in a superclass of the
			     ;;current parameter specializer)?
			     (let ((more-specific
				    (position-if #'(lambda (m)
						     ;;(format *trace-output* "Method: ~A~%base-param: ~A" m base-param)
						     (and (member m (type-body-elements (class-declaration-body class)))
							  (let* ((base-params (parameter-list-elements (method-declaration-parameters m)))
								 (base-param2 (find-if #'(lambda (param) (get-option :specialized param)) base-params)))
							    ;;(format *trace-output* "base-param2: ~A~%" base-param2)
							    (super-type-p (get-type base-param2) (get-type base-param)))))
						 generic-function-methods
						 :from-end t)))
			       (if more-specific
				 (nth more-specific bodies)
				 `((call-next-method)))))))
			(type-body-elements (class-declaration-body class))))
		(push (parse-method
		       `(defmethod ,name (this
					  ,@(mapcar #'(lambda (param)
							(if (eq param base-param)
							  `(,(first (ast-node-form param))
							    (the ,supertype))
							  param))
						    base-params))
			 ,(dispatch-body
			   name
			   (first (ast-node-form base-param))
			   dispatch-position
			   (reverse (cons base-meth meths))
			   `((error ,(format nil "No methods applicable for ~A with arg ~~A" name)
			      ,(first (ast-node-form base-param)))))))
		      (type-body-elements (class-declaration-body class))))))))))

(defun dispatch-body (name spec-param dispatch-position new-meths body)
  (if new-meths
    (let ((clauses
	   (mapcar #'(lambda (ref-meth)
		       (let ((new-params (parameter-list-elements (method-declaration-parameters ref-meth))))
			 (let ((new-specialized-param (nth dispatch-position new-params)))
			   `((or (null ,spec-param)
			      (typep ,spec-param ',(copy-type (get-type new-specialized-param))))
			     (send this ,name
			      ,@(mapcar #'(lambda (param)
					    (if (get-option :specialized param)
					      (parse `(force-the ,(copy-type (get-type param)) ,spec-param)
						     'cast-expression)
					      (first (ast-node-form param))))
					new-params))))))
		   new-meths)))
      `(cond ,@clauses
	(t ,@body)))
    `(progn ,@body)))

(defun initable-slots (slots)
  (remove-if #'(lambda (slot)
		 (or (eq (get-option :allocation slot) :class)
		     (null (get-option :initarg slot))
		     (get-option :final slot)))
	     slots))

(defun merge-default-initargs (slots default-initargs)
  (if default-initargs
    (mapcar #'(lambda (slot)
		(let ((init-name (get-option :initarg slot)))
		  (if init-name
		    (let ((init (getf default-initargs init-name)))
		      (if init
			(let ((defslot-0-form (ast-node-form slot)))
			  ;;(defslot-0 (name (the <type> ...)) :etc etc)
			  (multiple-value-bind (name type inits)
			      (unbuild-variable-declaration-description (second defslot-0-form))
			    (declare (ignore inits))
			    (with-parent ((ast-node-parent slot))
					 (parse `(defslot-0 
						   ,(build-variable-declaration-description name type (list init))
						   ,@(rest (rest defslot-0-form)))
						'slot-declaration))))
			slot))
		    slot)))
	    slots)
    slots))

(defmethod collect-all-slots ((e class-declaration) &optional (defaults (list)))
  (if (root-class-declaration-p e)
    (list)
    (append (merge-default-initargs (class-slots e) defaults) 
	    (collect-all-slots (get-type-declaration (class-declaration-superclass e)) 
			       (append (get-option :default-initargs e (list))
				       defaults)))))

;;To define constructors, we will consider several strategies

;;0 - the basic strategy is to use a defmethod form on something
;;related to 'initialize-instance' (currently, defnew and defafternew
;;forms) that is solely responsible for initializing the current slots
;;and call-next-method.  Let's call these constructors 'NEW
;;constructors'.

;;1 - the classical strategy is to collect initargs from current and
;;superclasses and define a constructor with all initargs that calls
;;the super construtor with all super initargs and sets the current
;;slots.  Being repeated on every class, this creates a natural set of
;;constructors.  However, there are no fancy initializations, e.g.,
;;computing initializations from other initializations.  Let's call
;;these constructors 'KEY constructors'.

;;2 - the 'Java' strategy is to allow a kind of 'BOA constructor'
;;where we specify the parameter list, presumably using some &aux
;;parameters to specify dependent initializations.  It's obvious that
;;we can initialize our own slots using the arguments passed to the
;;BOA lambda list.  The problem is the initialization of the inherited
;;slots.  We cannot simply set them to the corresponding arguments
;;because not calling the superclass constructor entails that a
;;default constructor is automatically called in the superclass, and
;;this default constructor might not even exist.  There are several
;;options here:

;;2.0 - the superclass implements a KEY constructor.  This is good,
;;because we can simply call that constructor with all its initarg
;;slots that have a corresponding value in the BOA lambda list.

;;2.1 - the superclass implements one or more BOA constructors.  We
;;can choose from that set one that closely matches the non-local
;;slots that are available in the BOA lambda list.  Note that this
;;identification is done using the slot names (and maybe also their
;;types).

;;2.2 - superclass implements one or more NEW constructors.  We could
;;try to apply the same logic of identifying the best constructor
;;according to the names (and types) of the parameters but,
;;unfortunately, NEW constructors have too much latitude of
;;implementation to allow a reliable selection.

;;Final considerations:

;; - if there are initargs in the current class, there must be a KEY
;; constructor.

;; - if there are no initargs in the current class, neither there are
;; BOA constructors or NEW constructors, but there is a KEY
;; constructor in the superclass, there must be a KEY constructor.

(defmethod add-constructors ((e class-declaration))
  (let ((superclass (get-type-declaration (class-declaration-superclass e))))
    (let ((boas (get-option :constructor e))
	  (super-boas (get-option :constructor superclass)))
      (let ((default-initargs (get-option :default-initargs e (list))))
	(let ((inits (initable-slots (merge-default-initargs (class-slots e) default-initargs)))
	      (super-inits (initable-slots (collect-all-slots superclass default-initargs))))
	  (let ((lambda-lists
		 (if (or (not (endp inits))  ;;we have initargs or
			 (and (not (endp super-inits)) ;;we have super initargs
			      (endp boas)              ;;and no BOA constructor nor NEW constructor
			      (notany #'constructor-declaration-p (type-body-elements (class-declaration-body e)))))
		   (cons (make-lambda-list-from-inits (append inits super-inits))
			 boas)
		   boas)))
	    (dolist (constructor (make-constructors lambda-lists (class-slots e) super-boas super-inits))
	      (push constructor (type-body-elements (class-declaration-body e)))
	      (apply-visitors 'after-parse constructor))))))))
;; 	  (unless (and (endp slots) (endp superclasses-slots)) ;;a default constructor
;; 	    ;;is assured by java itself, as long as there is no other constructor
;; 	    ))))))

(defun initparam (slot)
  (let ((ast (ast-node-form (slot-declaration-statement slot)))
	(name (get-option :initarg slot)))
    ;;bc the symbol is usually in the package keyword
    (if name `(,(read-from-string (princ-to-string name)) ,@(rest ast)) ast)))

(defun make-lambda-list-from-inits (inits)
  `(&key ,@(mapcar #'initparam inits)))

(defun in-lambda-list-p (name lambda-list)
  (member name lambda-list :key #'(lambda (param) (if (consp param) (first param) param))))

(defun find-super-boa (boas lambda-list)
  (let ((good-boas
	 (sort 
	  (remove-if-not #'(lambda (params)
			     (every #'(lambda (param)
					(in-lambda-list-p (if (consp param) (first param) param) lambda-list))
				    (break-list-if #'(lambda (param)
						       (member param lambda-list-keywords))
						   params)))
			 boas)
	  #'>
	  :key #'length)))
    (if good-boas
      (let ((args
	     (loop for param in (first good-boas)
		   until (member param lambda-list-keywords)
		   when (in-lambda-list-p (if (consp param) (first param) param) lambda-list)
		   collect param)))
	(if args
	  (list `(call-next-method ,@args))
	  (list)))
      (list))))

(defun make-constructors (lambda-lists slots super-boas super-inits)
  (let ((superslots-params (mapcar #'initparam super-inits)))
    (mapcar #'(lambda (lambda-list)
		(let ((slots (remove-if-not #'(lambda (slot) (in-lambda-list-p (first (initparam slot)) lambda-list)) slots))
		      (superslots-params (remove-if-not #'(lambda (slot) (in-lambda-list-p (first slot) lambda-list)) superslots-params)))
		  (parse-method
		   `(defnew ,lambda-list
		     ,@(if (null super-inits) ;;no KEY constructor on superclass
			   (find-super-boa super-boas lambda-list)
			   `((call-next-method ,@(mapcar #'first superslots-params))))
		     ,@(mapcar
			#'(lambda (slot)
			    (let ((slot-name
				   (linj-original-name (variable-declaration-name (slot-declaration-statement slot)))))
			      (let ((key-name (or (get-option :initarg slot) slot-name)))
				`(setf (slot-value this ',slot-name)
				  ,(read-from-string (princ-to-string key-name))))))
			slots)))))
	    lambda-lists)))

(defun copy-mixins-declarations (mixins e)
  (dolist (mixin mixins)
    (copy-mixins-declarations (mapcar #'get-type-declaration
				      (type-reference-list-elements (mixin-declaration-supermixins mixin)))
			      e)
    (let ((mixin-elems (type-body-elements (mixin-declaration-body mixin))))
      (dolist (elem mixin-elems)
	(cond ((or (defvar-declaration-p elem)
		   (abstract-method-declaration-p elem))
	       nil) ;;do-nothing
	      ((or (method-declaration-p elem)
		   (slot-declaration-p elem))
	       (let ((copy (with-parent (e)
			     (parse (ast-node-form elem)
				    'type-body-declaration)))) ;;HACK This is
		 ;;wrong bc it might cause package capture problems.
		 ;;There's also the problem of not dealing with local-macros.
		 (setf (type-body-elements (class-declaration-body e))
		       (nconc (type-body-elements (class-declaration-body e)) (list copy)))
;		 (apply-visitors 'after-parse copy)
		 ))
	      (t
	       (linj-error "What's this? (~W)" elem)))))))

;;This must be improved to include the package name in the external representation
(defun generate-parse-methods (type-ref slots)
  (let ((type (ast-node-form type-ref)))
    (let ((name (class-or-interface-type-reference-name type-ref))
	  (pkg (class-or-interface-type-reference-package type-ref)))
      (labels ((make-initarg-keyword (initarg name)
		 (if (eq initarg t)
		   (make-keyword name)
		   initarg))
	       (make-init-and-let-binding (slot)
		 (let ((initarg (get-option :initarg slot)))
		   (when initarg 
		     (let ((var (slot-declaration-statement slot)))
		       (let ((name (variable-declaration-name var))
			     (init (variable-declaration-initializer var))
			     (type (get-type var)))
			 (let ((initarg-key (make-initarg-keyword initarg name)))
			   (list
			    `(,type ,name ,initarg-key ,init)))))))))
	(let ((inits-binds (mapcan #'make-init-and-let-binding slots)))
	  (list
	   `(defun parse (internal-raw-data/cons)
	     (let* ((internal-raw-data-keys (rest internal-raw-data)))
	       (if (not (eq ',name (first internal-raw-data)))
		 (error 'class-parse-exception
			(format nil "Error parsing class ~S object: ~S"
				',name internal-raw-data))
		 (make-instance
		     ',type
		   ,@(mapcan #'(lambda (init-bind)
				 (let ((initarg-key (third init-bind))
				       (init (fourth init-bind)))
				   `(,initarg-key
				     (getf internal-raw-data-keys
				      ',initarg-key
				      ,(or (and init (ast-node-form init))
					   'null)))))
			     inits-binds)))))
	   `(defmethod to-string ()
	     (format nil
	      ,(format nil "#S(~A ~{:~A ~~A~^ ~})" 
		       name
		       (mapcar #'(lambda (init-bind)
				   (linj-original-name (second init-bind)))
			       inits-binds))
	      ,@(mapcar #'(lambda (init-bind)
			    (let ((type (first init-bind))
				  (name (second init-bind)))
			      (if (string-type-p type)
				`(format nil "\"~A\"" ,name)
				name)))
			inits-binds)))))))))

(defun generate-xml-parse-methods (type-ref slots)
  type-ref;;to be finished
  (list
   `(defmethod-0 object from-xml (xml/java.lang.string) ()
      (:visibility :public)
      (new 'object))
   `(defmethod-0 java.lang.string to-xml () ()
      (:visibility :public)
      (format nil
	      ,(format nil "~{<~A>~%~~A~%</~:*~A>~%~}" 
		       (mapcar #'(lambda (slot)
				   (variable-declaration-name (slot-declaration-statement slot)))
			       slots))
	      ,@(mapcar #'(lambda (slot)
			    (variable-declaration-name (slot-declaration-statement slot)))
			slots)))))

(defun process-type-options (slots options)
  (declare (ignore slots))
  (let ((body (list))
	(new-options (list)))
    (dolist (option options)
      (ecase (first option)
	(:body (setf body (append body (rest option))))
	;; see <../2001/20010324#* Linj> for a discussion on the following
	((:make-parse-method :make-xml-parse-method :visibility :documentation :category)
	 (setf new-options (append option new-options)))
	((:default-initargs :constructor)
	 (setf new-options (cons (first option) (cons (rest option) new-options))))))
    (values new-options body)))

;;HACK: It would be more helpfull to be able to inherit exclusively
;;from mixins without being forced to also include object!
(def-macro-transform class-declaration
    (defclass ?class ?superclasses ?slots . ?options&body)
    (multiple-value-bind (options body)
	(process-type-options ?slots ?options&body)
      `(defclass-0 ,?class ,(or (first (last ?superclasses)) (object-type-description))
	 ,(butlast ?superclasses)
	 ,options
	 ,@(define-slots-and-accessors ?slots)
	 ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;The defstruct is sometimes usefull

;;Warning: this doesn't allow for the specification of a superclass.  To be
;;able to do that I would have to instruct the Linj compiler to save the
;;defstruct slots of each definition so that they might be used latter.
;;Nothing impossible but I don't feel like opening that door right
;;now. (Anyway, with the (lisp ...) and (lisp-effect ...) forms it seems
;;quite easy to do that.
(def-macro-transform class-declaration
  (defstruct ?name . ?slots)
  (multiple-value-bind (name options)
      (if (atom ?name)
	(values ?name (list))
	(values (first ?name) (rest ?name)))
    (multiple-value-bind (slots body)
	(break-list-if #'(lambda (form) (and (consp form) (eq (first form) :body))) ?slots)
      (let ((struct
	     `(defclass ,name ()
		,(mapcar #'(lambda (slot)
			     (multiple-value-bind (slot-name initform options)
				 (if (symbolp slot)
				   (values slot nil nil)
				   (values (first slot) (second slot) (rest (rest slot))))
			       (let ((accessor (conc-symbol name '- slot-name))
				     (initarg (make-keyword slot-name)))
				 (if (or (atom slot) (endp (rest slot)))	;;slot or (slot)
				   `(,slot-name :type object :accessor ,accessor :initarg ,initarg)
				   (let ((reader-or-accessor (if (getf options :read-only) :reader :accessor)))
				     `(,slot-name :initform ,initform
						  ,@(and (getf options :type) `(:type ,(getf options :type)))
						  ,reader-or-accessor ,accessor
						  :initarg ,initarg))))))
			 slots)
		;;HACK should this be set by default?       (:make-parse-method t)
		,@body)))
	(let ((constructors (remove :constructor options :key #'first :test-not #'eq)))
	  (if constructors
	    `(progn
	       ,struct
	       ,@(mapcar #'(lambda (constructor)
			     (destructuring-bind (constructor-name lambda-list)
				 (rest constructor)
			       `(defun ,constructor-name ,lambda-list
				  (make-instance ',name
						 ,@(mapcan #'(lambda (param)
							       (cond ((consp param) (list (make-keyword (first param)) (first param)))
								     ((member param lambda-list-keywords) (list))
								     (t (list (make-keyword param) param))))
							   lambda-list)))))
			 constructors))
	    struct))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;We must keep package information available at runtime.
;;We will represent packages as a sequence of hashtables.

(defparameter *packages* (make-hash-table :test #'equal))

(defun add-package-type-info (package type info)
  (labels ((add-info (table package)
	     (if (endp package)
	       (setf (gethash type table) info)
	       (let ((subtable (gethash (first package) table)))
		 (unless subtable
		   (setq subtable (make-hash-table :test #'equal))
		   (setf (gethash (first package) table) subtable))
		 (add-info subtable (rest package))))))
    (add-info *packages* package)))

(defun get-package-type-info (package type)
  (labels ((get-info (table package)
	     (if (endp package)
	       (gethash type table)
	       (let ((subtable (gethash (first package) table)))
		 (and subtable
		      (get-info subtable (rest package)))))))
    (get-info *packages* package)))

(defun add-type-declaration-info (type-decl)
  (let ((type-def (type-declaration-name type-decl)))
    (add-package-type-info (class-or-interface-type-reference-package type-def)
			   (class-or-interface-type-reference-name type-def)
			   type-decl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;(defparameter *java-server-command* 
;;;    (format nil "cd ~ALMI; java JavaToLMI"
;;;            (make-pathname :directory (pathname-directory *load-truename*))))

(defparameter *java-server-process* (create-server-process))

(defun get-package-type-info-from-java-server (package name)
  (with-linj-syntax () ;;The class loader needs $ to disambiguate between package.class and class.inner-class
    (let ((type (format nil "~{~A.~}~{~A~^$~}" package (split-at-char (princ-to-string name) #\/))))
      (let ((decl (ask-server *java-server-process* type)))
	(when decl
	  (format *trace-output* "Reading info for ~A~%" type)
	  (let ((type (parse decl 'type-declaration)))
	    (save-foreign-type-declaration type)
	    type))))))

(defmethod save-foreign-type-declaration ((type type-declaration))
  (setf (get-option :foreign type) t)
  (add-type-declaration-info type))

(defmethod foreign-type-p (type)
  (get-option :foreign type nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;HACK:: All this anonymous stuff must be changed
;;Anonymous type references are just like normal references but
(defun anonymous-type-reference-p (type-reference)
  (null (symbol-package (class-or-interface-type-reference-name type-reference))))

(defmethod get-type-declaration ((type class-or-interface-type-reference))
  (if (anonymous-type-reference-p type)
    (ast-node-parent type)
    (or (let ((containing-type-declaration (containing-type-declaration type)))
  	  (and containing-type-declaration
  	       (find-declaration-in type containing-type-declaration)))
	(let ((package (class-or-interface-type-reference-package type))
	      (name (class-or-interface-type-reference-name type)))
	  (or (get-package-type-info package name)
	      (let ((location (find-package-type-location package name)))
		(and location
		     (let ((unit (get-compilation-unit-from-location location)))
		       (unless (package= package
					 (package-declaration-components
					  (compilation-unit-package unit)))
			 (error "Inferred package: ~A vs compilation unit package: ~A"
				package
				(package-declaration-name
				 (compilation-unit-package unit))))
		       (find type (compilation-unit-type-declarations unit)
			     :key #'type-declaration-name
			     :test #'equal-type-p))))
	      (get-package-type-info-from-java-server package name)
	      (linj-error "Couldn't find type declaration for ~S in package ~S"
			  type package))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *package-locations* ())

(defun add-package-location (location)
  (push (parse-namestring location) *package-locations*))

(defun find-package-type-location (package type)
  (with-linj-syntax ()
    (let ((path (make-pathname
		 :directory (cons :relative
				  (if (empty-package-p package)
				    ()
				    (mapcar #'linj-name-to-linj-file-name package)))
		 :name (first (split-at-char (princ-to-string (linj-original-name type)) #\/))
		 :type "linj")))
      (dolist (loc *package-locations* nil)
	(let ((path (merge-pathnames path loc)))
	  (when (probe-file path)
	    (return path)))))))

(defun add-package-location-from-linjpath ()
  (let ((linjpath (or (get-env "LINJPATH")
		      (directory-namestring *load-truename*))))		       
    (dolist (path (split-at-char linjpath #\:))
      (let ((comps (remove "" (split-at-char path #\/) :test #'string=)))
	(unless (null comps)
	  (push (make-pathname :directory
			       (if (string= (first comps) ".")
				 (cons :relative (rest comps))
				 (cons :absolute comps)))
		*package-locations*))))))


(add-package-location-from-linjpath)

;;;;;;;;;;;;;;Reading compilation units;;;;;;;;;;;;;;;;;;;;

(def-list-syntax compilation-unit-list (linj-list-node) compilation-unit-member)

(def-syntax compilation-unit (linj-node)
  (compilation-unit . ?components/compilation-unit-list)
  :slots
  ((name
    :accessor compilation-unit-name
    :initform nil)
   (location
    :accessor compilation-unit-location
    :initform nil)
   (time-stamp
    :accessor compilation-unit-time-stamp
    :initform nil)
   (parameters
    :accessor compilation-unit-options
    :initform nil)
   (package 
    :accessor compilation-unit-package 
    :initform nil)
   (imports
    :accessor compilation-unit-imports 
    :initform (list))
   (ins
    :accessor compilation-unit-ins
    :initform (list))
   (type-declarations
    :accessor compilation-unit-type-declarations
    :initform (list))))

(def-unparse compilation-unit (e)
  (unless (empty-package-p (package-declaration-components (compilation-unit-package e)))
    (format t "~/pp/~%~%" (compilation-unit-package e)))
  (let ((imports (sort (remove (java.lang.*-type)
			       (compilation-unit-imports e)
			       :test #'equal-type-p
			       :key #'import-declaration-type)
		       #'string<
		       :key #'(lambda (import)
				(format nil "~/pp/" import)))))
    (unless (endp imports)
      (format t "~{~/pp/~%~}~%" imports)))
  (format t "~{~/pp/~^~3%~}" (compilation-unit-type-declarations e)))

(defmethod containing-compilation-unit ((e compilation-unit))
  e)

(defmethod containing-compilation-unit ((e linj-node))
  (containing-compilation-unit (ast-node-parent e)))

;;Escaped from ast-tree
(defmethod containing-compilation-unit ((e null))
  (error "Empty parent"))

(defun list-difference (l1 l2)
  (remove-if #'(lambda (e) 
		 (member e l2))
	     l1))

(defun split-if (pred list)
  (let ((list-pred (remove-if-not pred list)))
    (values list-pred (list-difference list list-pred))))

(defmethod refine-compilation-unit ((e compilation-unit))
  (let ((types (compilation-unit-type-declarations e)))
    (let ((public-type
	   (or (find (compilation-unit-name e) types
		     :key (compose #'type-reference-name #'type-declaration-name))
	       (first types))))
      (assert (or (null (get-option :visibility public-type))
		  (eq (get-option :visibility public-type) :public)))
      (setf (get-option :visibility public-type) :public) ;;one of them must be public
      (dolist (type (remove public-type types)) ;;and the others have package visibility
	(unless (get-option :visibility type)
	  (setf (get-option :visibility type) :package))))
    (dolist (type types)
      ;;Top-level type declarations have the compilation-unit package
      (set-type-definition-package
       (type-declaration-name type)
       (package-declaration-components (compilation-unit-package e)))
      ;;All the others have a null package (they're accessed through the
      ;;containing class
      (set-all-unknown-package-type-definition type (empty-package)))
    e))

;;;To define the package in type declarations:

(defmethod set-type-definition-package ((e class-or-interface-type-reference) package)
  (setf (class-or-interface-type-reference-inferred-package e) package))

(defclass set-unknown-package-visitor ()
  ((package :initarg :package :reader visitor-package)))

(defmethod visit :before ((e type-declaration) (visitor set-unknown-package-visitor))
  (set-if-unknown-package (type-declaration-name e) (visitor-package visitor)))

(defmethod set-all-unknown-package-type-definition ((type type-declaration) package)
  (visit type (make-instance 'set-unknown-package-visitor :package package)))

;;To resolve static references:

(defmethod find-declaration-in ((ref reference) (e compilation-unit))
  (or (some #'(lambda (type)
		(find-declaration-in-type ref type))
	    (remove-if-not #'type-declaration-p
			   (compilation-unit-list-elements
			    (compilation-unit-components e))))
      (some #'(lambda (in-decl)
		(find-declaration-in-type ref (get-type-declaration (in-declaration-object in-decl))))
	    (remove-if-not #'in-declaration-p
			   (compilation-unit-list-elements
			    (compilation-unit-components e))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *compilation-units* (make-hash-table :test #'equal))

(defun add-loaded-compilation-unit (unit)
  (setf (gethash (compilation-unit-location unit) *compilation-units*)
    unit))

(defparameter *always-load-p* nil)

(defun get-loaded-compilation-unit (location)
  (and (not *always-load-p*)
       (gethash location *compilation-units*)))

(defun get-compilation-unit-from-location (location &optional (error-value :error))
  (let ((unit (get-loaded-compilation-unit location)))
    (if (or (null unit)
	    (let ((time-stamp (compilation-unit-time-stamp unit)))
	      (and (probe-file location) ;;might have been deleted...
		   (> (file-write-date location) time-stamp)
		   (progn
		     (format *trace-output* "Rereading location ~A~%" location)
		     t))))
      (read-compilation-unit-from-location location error-value)
      unit)))

(defun read-compilation-unit-from-location (location &optional (error-value :error))
  (with-linj-syntax ()
    (with-open-file (f location
		       :direction :input
		       :if-does-not-exist error-value)
      (when f
	(format *trace-output* "Reading file ~A~%" location)
	(let ((unit (read-compilation-unit-from-stream
		     f
		     location
		     (file-write-date location))))
	  (add-loaded-compilation-unit unit)
	  (apply-visitors 'after-parse unit)
	  unit)))))

(defun read-one-form (stream eof-errorp eof-value)
  (read stream eof-errorp eof-value))
;;This is experimental to accept inlined Java code between Linj code
;   (let ((ch (peek-char t stream eof-errorp eof-value)))
;     (cond ((eq ch eof-value)
; 	   eof-value)
; 	  ((char= ch #\()
; 	   (read stream eof-errorp eof-value))
; 	  ((char= ch #\;)
; 	   (read-line stream eof-errorp eof-value)
; 	   (read-one-form stream eof-errorp eof-value))
; 	  (t
; 	   (let ((buffer (make-string-output-stream)))
; 	     (do ((line (read-line stream eof-errorp eof-value) (read-line stream eof-errorp eof-value)))
; 		 ((or (eq line eof-value)
; 		      (char= (peek-char t stream eof-errorp eof-value) #\())
; 		  (get-output-stream-string buffer))
; 	       (write-line line buffer)))))))


(defun read-all-forms-until-eof (stream)
  (labels ((read-forms-until-eof
	    ()
	    (let ((form (read-one-form stream nil :eof)))
	      (cond ((eq form :eof)
		     (list))
		    ((stringp form)  ;;Parse here or in the parse methods?
		     (cons form (read-forms-until-eof)))
		    (t
		     (cons form (read-forms-until-eof)))))))
    (read-forms-until-eof)))

;;We impose some restrictions on what we expect:
;;first, an optional package declaration,
;;second, optional package imports,
;;third, an optional type declaration, if not present a defstruct is generated
;;then all the remaining mess that will belong to the first class above it or, in
;;case of properly specialized methods, to the referred type
(defun combine-compilation-unit-forms (unit forms &optional (expecting :package))
  (if (endp forms)
    (if (eq expecting :types)
      ;;The end! Define the components
      (setf (compilation-unit-components unit)
	    (parse (cons (compilation-unit-package unit)
			 (append (compilation-unit-imports unit)
				 (append (compilation-unit-ins unit)
					 (compilation-unit-type-declarations unit))))
		   'compilation-unit-list))
      (error "Was expecting a type declaration but found none"))
    (let ((form (first forms)))
      (ecase expecting
	(:package
	 (cond ((package-declaration-p form)
		(setf (compilation-unit-package unit) form)
		(combine-compilation-unit-forms unit (rest forms) :imports))
	       (t
		(setf (compilation-unit-package unit)
		      (with-parent (unit) (parse `(package nil) 'package-declaration)))
		(combine-compilation-unit-forms unit forms :imports))))
	(:imports
	 (cond ((package-declaration-p form)
		(error "Unexpected package declaration ~A" form))
	       ((import-declaration-p form)
		(setf (compilation-unit-imports unit) (nconc (compilation-unit-imports unit) (list form)))
		(combine-compilation-unit-forms unit (rest forms) :imports))
	       ((in-declaration-p form)
		(setf (compilation-unit-ins unit) (nconc (compilation-unit-ins unit) (list form)))
		(combine-compilation-unit-forms unit (rest forms) :imports))
	       (t 
		(combine-compilation-unit-forms unit forms :types))))
	(:types
	 (cond ((package-declaration-p form)
		(error "Unexpected package declaration ~A" form))
	       ((import-declaration-p form)
		(error "Unexpected import statement ~A" form))
	       ((in-declaration-p form)
		(error "Unexpected in declaration ~A" form))
	       ((type-declaration-p form)
		(setf (compilation-unit-type-declarations unit) (nconc (compilation-unit-type-declarations unit) (list form)))
		(combine-compilation-unit-forms unit (rest forms) :types))
	       ((type-body-declaration-p form)
		(let ((types (compilation-unit-type-declarations unit)))
		  (if (endp types)
		      (combine-compilation-unit-forms unit
						      (cons (with-parent (unit)
							      (parse `(defstruct ,(compilation-unit-name unit)) 'type-declaration)) forms)
						      expecting)
		      (let ((type
			     (or (and (method-declaration-p form)
				      (let ((type (method-declaration-specialized-to form)))
					(and type
					     ;;we search for equal type names because we can't infer yet the type packages
					     (or (find (type-reference-name type) types
						       :from-end t
						       :key (compose #'type-reference-name #'type-declaration-name)
						       :test #'equal-type-reference-name)
						 (error "Method specialized for unknown type ~A" form)))))
				 (first (last types)))))
			(setf (type-body-elements (type-declaration-body type))
			      (nconc (type-body-elements (type-declaration-body type)) (list form)))
			(combine-compilation-unit-forms unit (rest forms) expecting)))))
	       (t
		(error "Impossible syntatic category ~A" form))))))))

(def-syntax macro-definition (compilation-unit-member)
  (defmacro ?name . ?body))

(def-syntax syntax-macro-definition (compilation-unit-member)
  (defmacro-syntax ?name ?cat . ?body))

(defgeneric add-macro-definition (macro-def)
  (:method ((macro-def macro-definition))
    (let ((name (macro-definition-name macro-def))
	  (body (macro-definition-body macro-def)))
      (funcall (compile nil `(lambda () (def-macro ,name ,@body))))))
  (:method ((macro-def syntax-macro-definition))
    (let ((name (syntax-macro-definition-name macro-def))
	  (cat (syntax-macro-definition-cat macro-def))
	  (body (syntax-macro-definition-body macro-def)))
      (funcall (compile nil `(lambda () (def-syntax-macro ,name ,cat ,@body)))))))

(def-list-syntax top-level-forms (linj-list-node) top-level-form)

;;;But we also accept progns
(def-syntax progn-code (top-level-form)
  (progn . ?elements/top-level-forms))

(defun flatten-progn-code (form)
  (if (progn-code-p form)
    (mapcan #'flatten-progn-code (top-level-forms-elements (progn-code-elements form)))
    (list form)))

(defparameter *default-in-declarations*
  '(java.lang.math
    java.lang.system
    java.lang.thread
    linj.util))

(defun default-in-declarations ()
  (mapcar #'(lambda (type)
	      (parse `(in (the ,type)) 'in-declaration))
	  *default-in-declarations*))

(defun read-compilation-unit-from-stream (stream location time)
  (let ((type-form (read-from-string (with-<> (pathname-name location)))))
    (let ((name (type-reference-name (parse type-form 'type-reference)))
	  (forms (read-all-forms-until-eof stream)))
      ;;Each form can be:
      ;;a compilation-unit-member (package declaration, imports, class or mixin definitions)
      ;;a type-body-declaration (defslots, defconstants, defmethods, defuns, static blocks, etc)
      ;;a macro-definition (defmacro)
      ;;a progn that results from the expansion of a macro

      ;;First, we will collect all linj-options to prepare the compiler
      (multiple-value-bind (options forms)
	  (split-if #'(lambda (form) (let-pattern (((linj-options . ?ignore) form)) t)) forms)
	;;Next, we apply the options and we start parsing:
	(let ((all-options (reduce #'append options :key #'rest)))
	  (with-compilation-options (all-options)
	    (let ((parsed (mapcan #'(lambda (form)
				      (let ((macro (or (try-to-parse form 'macro-definition)
						       (try-to-parse form 'syntax-macro-definition))))
					(cond (macro
					       (unless (eq macro *ignore-parse*)
						 (add-macro-definition macro))
					       (list))
					      (t
					       (flatten-progn-code
						(or (try-to-parse form 'top-level-form)
						    (error "Couldn't parse form ~A" form)))))))
				  forms)))
	      ;;Now we must reorganize the forms so that type declarations belong to the compilation unit
	      ;;and type-body-declarations belong to the correct type-declaration
	      (let ((unit (make-instance 'compilation-unit)))
		(setf (compilation-unit-name unit) name)
		(setf (compilation-unit-location unit) location)
		(setf (compilation-unit-time-stamp unit) time)
		(setf (compilation-unit-imports unit) (list (new-import-declaration (java.lang.*-type))))
		(setf (compilation-unit-ins unit) (default-in-declarations))
		(setf (compilation-unit-options unit) all-options)
		(combine-compilation-unit-forms
		 unit
		 (mapcar #'(lambda (form) (with-parent (unit) form)) parsed))
		(refine-compilation-unit unit)
		(dolist (type (compilation-unit-type-declarations unit))
		  (add-type-declaration-info type))
		unit))))))))

(defmethod imported-type-p ((e class-or-interface-type-reference) (unit compilation-unit))
  (some #'(lambda (import)
	    (match-type
	     (import-declaration-type import)
	     e))
	(compilation-unit-imports unit)))

(defun linj-pathname (pathname)
  (with-simple-restart (abort "Abort Linjing pathname ~A" pathname)
    (loop
     (with-simple-restart (retry "Linj again pathname ~A" pathname)
       (let ((unit (get-compilation-unit-from-location pathname)))
	 (when (null unit)
	   (error "Couldn't find file ~A" pathname))
	 (return (write-compilation-unit unit)))))))

(defun linj2java (name &optional (directory (get-current-directory)))
  (unless (typep directory 'pathname)
    (setq directory (parse-namestring directory)))
  (linj-pathname (merge-pathnames (make-pathname :name (if (symbolp name) ;;backward compatability
							 (format nil "~(~A~)" name)
							 (format nil "~A" name))
						 :type "linj")
				  directory)))

(defun linj2java-directory (&optional (directory (get-current-directory)))
  (unless (typep directory 'pathname)
    (setq directory (parse-namestring directory)))
  (let ((pathnames
	 (directory
	  (make-pathname :directory (pathname-directory directory)
			 :name :wild :type "linj"))))
    (with-simple-restart (abort "Abort Linjing directory ~A" directory)
      (mapc #'linj-pathname pathnames))))

(defun java-name-from-class-name (name)
  (format nil "~W.java" name))

(defmethod write-compilation-unit ((unit compilation-unit))
  (with-linj-syntax ()
    (let ((name (linj-name-to-java-type-name (pathname-name (compilation-unit-location unit)))))
      (let ((location (merge-pathnames (make-pathname :name name :type "java")
				       (compilation-unit-location unit))))
	(with-open-file (f location :direction :output :if-exists :supersede)
	  (format *trace-output* "(Re)Writing file ~a~%" location)
	  (write-compilation-unit-to-stream unit f)
	  name)))))

(defmethod write-compilation-unit-to-stream (unit stream)
  (with-compilation-options ((compilation-unit-options unit))
    (apply-visitors 'before-dump unit)
    (unparse-linj unit stream)))

(defun unparse-linj (ast &optional (stream *standard-output*))
  (let ((*print-right-margin* 120)
 	(*print-miser-width* nil)  ;;The Java text editor never uses miser style
	(*print-length* nil)
	(*print-level* nil)
	(*print-pretty* t))
    (with-linj-syntax ()
      (unparse-object ast stream))))
  

(defun dump-loaded-compilation-units ()
  (maphash #'(lambda (path unit)
	       (declare (ignore path))
	       (write-compilation-unit unit))
	   *compilation-units*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;The visitors scheme is based on transformations that are applied
;;;immediately after parse and immediately before unparse.  This separation
;;;is useful to avoid transformations that affect other transformations.

(defparameter *visitors* (make-hash-table))

(defun add-visitor (type visitor &optional (position :end))
  (setf (gethash type *visitors*)
    (cond ((eq position :end)
	   (nconc (gethash type *visitors*) (list visitor)))
	  ((eq position :begin)
	   (cons visitor (gethash type *visitors*)))
	  (t
	   (linj-error "Don't understand ~A" position)))))

(defun remove-visitor (type visitor)
  (setf (gethash type *visitors*)
    (delete visitor (gethash type *visitors*))))

(defvar *previous-visitors* (list))

(defun apply-visitors (type ast)
  (let ((visitors (gethash type *visitors*)))
    (assert (not (null visitors)))
    (let ((*previous-visitors* (list)))
      (dolist (visitor-class visitors ast)
	(let ((visitor (make-instance visitor-class)))
	  (visit ast visitor)
	  (push visitor *previous-visitors*))))))

(defun apply-previous-visitors (ast)
  (dolist (visitor *previous-visitors*)
    (visit ast visitor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Overriding methods:

;;Sometimes, we need to override a method.  Unfortunately, as soon as we
;;change the type of an argument or the number of arguments, we are
;;overriding nothing.  It would be interesting to be sure that a method
;;declaration is, in fact, overriding another method declaration on some
;;super-super-\ldots{}-class.  The idea to implement this is just to define
;;a visitor that walks the parse tree and that, on each method
;;declaration, checks the existence of an identical method (in terms of its
;;name) in the superclass path.  If the type of the parameters in both
;;method disagree, we generate an error:

;; (defclass a () ())
;; (defclass b (a) ())
;; (defclass c (b) ())
;; (defclass d (c) ())
;; (defclass e (c) ())

;; (defmethod xpto ((x a))
;;   1)

;; (defmethod xpto :after ((x b))
;;   (print 'after-b))

;; (defmethod xpto :before ((x c))
;;   (print 'before-c))

;; (defmethod xpto :after ((x c))
;;   (print 'after-c))

;; (defmethod xpto ((x d))
;;   2)

;; (defmethod xpto ((x e))
;;   3)

;; (defmethod xpto :before ((x e))
;;   (print 'before-e))

;; (defmethod xpto :after ((x e))
;;   (print 'after-e))
;; -------------------------------

;; (defmethod xpto ((x a))
;;   1)

;; (defmethod xpto ((x b))
;;   (prog1
;;       (primary-xpto x)
;;     (after-xpto-b x)))

;; (defmethod primary-xpto ((x b))
;;   (xpto super))

;; (defmethod after-xpto-b ((x b))
;;   (print 'after))

;; (defmethod xpto ((x c))
;;   (before-xpto-c x)
;;   (prog1
;;       (call-next-method)
;;     (after-xpto-c x)))

;; (defmethod primary-xpto ((x d))
;;   2)

;; (defmethod primary-xpto ((x e))
;;   3)

;; (defmethod xpto ((x e))
;;   (before-xpto-e x)
;;   (prog1
;;       (call-next-method)
;;     (after-xpto-e x)))
;; -------------------------------


;; (defclass overrides-visitor ()
;;   ())

;; (defmethod visit ((method method-declaration) (visitor overrides-visitor))
;;   (unless (constructor-declaration-p method)
;;     (let ((class (containing-type-declaration method)))
;;       (when (class-declaration-p class) ;;it might be a mixin
;; 	(let ((super-method (find-declaration-in method class)))
;; 	  (unless (or (null super-method)
;; 		      (same-signature-p method super-method))
;; 	    (override-error method super-method)))))))

;; (defun override-error (method super-method)
;;   (linj-error "The method ~W 
;; do not obeys the signature specified by the same method ~W
;; in the superclass ~W"
;; 	 method 
;; 	 super-method 
;; 	 (class-declaration-name (containing-type-declaration super-method))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;We want to be able to define :after methods on default constructors
(defclass link-afternew-transform ()
  ())

(defmethod visit :after ((e class-declaration) (visitor link-afternew-transform))
  (let ((after-new-decl (find-if #'after-new-declaration-p
				   (type-body-elements (class-declaration-body e)))))
    (when (not (null after-new-decl))
      (add-after-new-call (method-declaration-name after-new-decl) e))))


(defmethod add-after-new-call (after-new-name (e class-declaration))
  (if (anonymous-class-declaration-p e)
    (linj-error "Anonymous classes can't have defafternew declarations~%")
    (let ((constructors (remove-if-not #'constructor-declaration-p
				       (type-body-elements (class-declaration-body e)))))
      (if (null constructors)
	;; We have to generate the default one to call the after-new
	(let ((c (parse-method
		  `(defnew ()
		    (call-next-method)
		    (,after-new-name this)))))
	  (push c (type-body-elements (class-declaration-body e)))
	  (apply-visitors 'after-parse c))
	(dolist (c constructors)
	  (unless (eq :initialize-instance-key (method-declaration-name c))
	    (let ((s (parse `(,after-new-name this) 'statement)))
	      (setf (statement-list-elements (method-declaration-body c))
		    (append (statement-list-elements (method-declaration-body c))
			    (list s)))
	      (apply-visitors 'after-parse s))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;To access the class of a type reference
;;To access the class of an object
(def-transform expression (class-of ?expr) (get-class ?expr))

(def-syntax class-of-class-expression (expression)
  (get-class (the ?class/type-reference)))

(defmethod get-type ((e class-of-class-expression))
  (parse 'java.lang.Class 'type-reference))

(def-unparse class-of-class-expression (e)
  (format t "~/ppexp/.class" (class-of-class-expression-class e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;The use of a generic function approach on a non-multi-method system has
;;some drawbacks which I really would like to solve.  Consider the
;;following example:  we want to define a method that adds an element to a
;;list.  Since, in linj, we must attach methods to classes, we must decide
;;where to put it.  Either we put it on our 'root' class or we put it on a
;;<list> class. The first option forbids us to insert other objects
;;belonging to the Java 'root' class.  The second options forces us to
;;define the method in such a way that the first argument of the invocation
;;is the list and the second argument is the object we want to add,
;;i.e. (cons <list> <object>).

;;While the first option is plainly impossible to use, the second one is a
;;source of bugs.  Remember that linj tries to copy Common Lisp as far as
;;possible, thus most operations over lists (cons, remove, mapcar, etc)
;;that accept a non-list first argument must be rewritten with a different
;;calling syntax, and everybody (me, specially) tend to forget it.

;;My idea for this transformation is to allow methods to identify which
;;parameter is the 'receiver' (more about this latter) and then treat all
;;method-calls as generic calls that must be further refined in order to
;;properly identify the correct receiver.  To this end, we must check all
;;arguments and identify wether an argument's class define such a method.
;;In case more we have more than one definition we must abort because we
;;don't have multi-methods (or else we can use some metric such as
;;inheritance-path-length to pick up the preferred one).  In case we have
;;just one method, we check which parameter is the receiver and we remove the
;;correspondent argument from the arg list and put it as method-call-receiver.

;;Let's go back a little and think about out to identify the receiver on
;;the parameter list of a method.  A simple idea is to find a parameter
;;whose type is the same as the class where the method is being defined.
;;This has a problem when there is a method with more than one such
;;parameter and the method does some side-effects. Changing the receiver or
;;the 'other'  is a totally different matter. Without side-effects there
;;seems to be no problem.  Another idea (from JCachopo) is to use the word
;;'this' to distinguish it.  This one has the advantage that we don't have
;;to alpha-convert the receiver parameter name in the method body because
;;it coincides with Java's expectations.  Let's try to use this 'this'.

;; (def-macro-transform method-declaration
;;     (defmethod-0 ?type ?name ?parameters ?throws ?options . ?body)
;;   (let ((receiver-position (position 'this ?parameters)))
;;     (when receiver-position
;;       `(defmethod-0 ,?type ,?name ,(remove 'this ?parameters) ,?throws
;; 		    (:receiver-position ,receiver-position . ,?options) . ,?body))))

;;Generic calls are a kind of generic function calls, `a la' CLOS

(def-syntax generic-call-expression (key-method-call-expression)
  (?name/unreserved-linj-name . ?arguments/argument-list)
  :slots ((receiver :initform nil)) ;;don't bother to visit this now. It's on the args, anyway
  :components (?receiver))

;;The inherited check-initialization is enough. We just have to further
;;specify the define-arguments method:

(defparameter *resolve-generic-call* t)

(defmethod check-initialization ((call generic-call-expression))
  (if *resolve-generic-call*
    (call-next-method)
    (let ((args (generic-call-expression-arguments call)))
      (setf (method-call-expression-receiver call)
	(with-parent (call) (first (argument-list-elements args))))
      (setf (argument-list-elements args)
	(rest (argument-list-elements args)))
      (setf (method-call-expression-args-passed call) :all))))

;;The idea, now, is just to try (and I repeat 'try') to find a class in one
;;of the arguments that implements the method.  This argument becomes then
;;the receiver.

;;Note that there are only a few possibilities, namely, the receiver must
;;be of a reference type and we can't use keyword args as receivers (this
;;is consistent with CLOS)

(defun possible-receivers (args)
  (let ((combs (list)))
    (dotimes (i (length args))
      (let ((arg (nth i args)))
	(if (keyword-reference-p arg)
	  (return)
	  (let ((type (get-principal-type arg)))
	    (unless (or (primitive-type-reference-p type)
			(null-type-p type))
	      (push (cons arg (append (subseq args 0 i) (subseq args (1+ i))))
		    combs))))))
    (nreverse combs)))

(defun get-method (call receiver args)
  (find-declaration
   (with-parent ((ast-node-parent call))
     (make-instance 'key-method-call-expression
		    :name (method-call-expression-name call)
		    :receiver receiver
		    :arguments (make-instance 'argument-list :elements args)
		    :original-form `(trying ,(ast-node-form (method-call-expression-name call))
				     on
				     ,(ast-node-form receiver)
				     with-args
				     (,@(mapcar #'ast-node-form args)))))))

(defmethod find-declaration ((call generic-call-expression))
  (let ((gen-args (argument-list-elements (method-call-expression-arguments call))))
    (flet ((instance-call (comb)
	     (destructuring-bind (receiver . args) comb 
	       ;;which argument's class implement such method?
	       (let ((method (get-method call receiver args)))
		 (when (and method
			    ;;we use gen-args because the (method-call-expression-arguments call)
			    ;;might have been changed in a recursive computation.
			    (= (position receiver gen-args)
			       (receiver-position method)))
		   (setf (argument-list-elements (method-call-expression-arguments call))
			 args)
		   (setf (method-call-expression-receiver call)
			 (with-parent (call) receiver))
		   method))))
	   (class-call (receiver)
	     (let ((method (get-method call receiver gen-args)))
	       (when (and method
			  (or (null (method-call-expression-receiver call))
			      (equal-type-p (method-call-expression-receiver call) receiver)))
		 (setf (method-call-expression-receiver call)
		       (with-parent (call)
			 (copy-type receiver)))
		 method))))
      (or (some #'instance-call (possible-receivers gen-args))
	  ;;Use 'in' forms first
	  (some #'class-call (mapcar #'get-type (collect-upward-in-forms call)))
	  ;;and the containing classes second
	  (some #'class-call (mapcar #'get-type (containing-type-declarations call)))
	  ;;and all top-level classes and in declarations (bottom up in the file)
	  (some #'class-call (mapcar #'get-type (top-level-type-declarations call)))))))

(defun containing-type-declarations (node)
  (let ((containing (containing-type-declaration node)))
    (if containing
      (cons containing (containing-type-declarations containing))
      (list))))

(defun top-level-type-declarations (node)
  (let ((collect (list)))
    (dolist (elem (compilation-unit-list-elements (compilation-unit-components (containing-compilation-unit node))))
      (cond ((type-declaration-p elem)
	     (push elem collect))
	    ((in-declaration-p elem)
	     (push (in-declaration-object elem) collect))))
    collect))

;;The defun macro is just syntatic sugar for a class allocated method:

(def-transform method-declaration 
    (defun ?name ?arglist . ?body)
  (defmethod ?name ?arglist :allocation :class . ?body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Mixins: There's no multiple inheritance in Java, but we can avoid copying
;;and pasting of code.  The idea is to use mixins.
;;First, we define a new kind of class declaration, the mixin:

(def-syntax mixin-declaration (type-body-declaration type-declaration)
  (defmixin-0 ?name/type-reference
      ?supermixins/type-reference-list 
    ?options . ?body/type-body)
  :strict-p t)

(def-macro-transform mixin-declaration
    (defmixin ?name ?supermixins ?slots . ?options&body)
    (multiple-value-bind (options body)
	(process-type-options ?slots ?options&body)
      `(defmixin-0 ,?name ,?supermixins
	 ,options
	 ,@(define-slots-and-accessors ?slots)
	 ,@body)))

;;As in Java, this class contains constant declarations and abstract
;;methods.  Let's define the abstract methods by syntax extension:

(def-macro-transform method-declaration
    (defabstract ?name ?arglist . ?options&body)
  (multiple-value-bind (options body throws returns)
      (extract-options-body ?options&body)
    (when body
      (linj-error "Abstract methods can't have a body"))
    `(defmethod ,?name ,?arglist 
		:category :abstract :throws ,throws :returns ,returns ,@options)))

;;Besides having constants and abstract methods, the mixin can have
;;'normal' slots and methods, which are copied to the inheriting classes
;;However, during unparse, these slots and method must be invisivel because
;;this class unparses in the form of a Java interface, which can't contain them.

(def-unparse mixin-declaration (e)
  (pprint-logical-block (*standard-output* nil)
    (format t "interface ~/pp/" (mixin-declaration-name e))
    (let ((mixins (type-reference-list-elements (mixin-declaration-supermixins e))))
      (unless (null mixins)
	(format t " extends ~{~/pp/~^, ~}" mixins)))
    ;;HACK: This isn't elegant but, for now, it is sufficient
    (let ((non-abstract (remove-if-not #'(lambda (elem)
					   (and (method-declaration-p elem)
						(primary-method-p elem)))
				       (type-body-elements (type-declaration-body e)))))
      (unparse-type-body-declaration
       (translate-for-interface (type-declaration-body e)))
      (dolist (elem non-abstract)
	(setf (get-option :category elem) nil)))))

(defmethod translate-for-interface (body)
  (mapcan #'(lambda (elem)
	      (cond ((or (constant-declaration-p elem)
			 (abstract-method-declaration-p elem))
		     (list elem))
		    ((and (method-declaration-p elem)
			  (primary-method-p elem))
		     ;;HACK: this isn't elegant.
		     (setf (get-option :category elem) :abstract)
		     (list elem))
		    (t
		     (list))))
	  (type-body-elements body)))

(defmethod find-declaration-in (what (e mixin-declaration))
  (or (find-declaration-in-type what e)
      ;;go through the mixins before going through the outerclass
      (some #'(lambda (mixin)
		(find-declaration-in what (get-type-declaration mixin)))
	    (type-reference-list-elements (mixin-declaration-supermixins e)))
      (call-next-method)))

(defmethod find-methods-named (name (e mixin-declaration) &optional (exclude-self-p t))
  (append
   (and (not exclude-self-p)
	(remove-if-not #'(lambda (elem)
			   (and (method-declaration-p elem)
				(eq name (method-declaration-name elem))))
		       (type-body-elements (mixin-declaration-body e))))
   ;;go through the mixins
   (reduce #'append
	   (type-reference-list-elements (mixin-declaration-supermixins e))
	   :key #'(lambda (mixin)
		    (find-methods-named name (get-type-declaration mixin) nil))
	   :from-end t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unwrap-if-needed (type form)
  (if (primitive-type-reference-p type)
    `(send (the ,(generic-wrapper-type type) ,form)
	   ,(type-to-type-message type))
    (if (object-type-p type)
      form
      `(the ,(copy-type type) ,form))))

(defun wrapper-type (type)
  (cond ((boolean-type-p type) (boolean-wrapper-type))
	;; HACK!!  wrapper-types for int and float are Long and Double
	;; because that's what linj-reader returns and it allows
	;; mixing ints and longs (resp. floats and doubles)
	((int-type-p type) (long-wrapper-type))
	((long-type-p type) (long-wrapper-type))
	((char-type-p type) (char-wrapper-type))
	((double-type-p type) (double-wrapper-type))
	((float-type-p type) (double-wrapper-type))
	(t (error "Don't know wrapper type for ~S" type))))

(defun generic-wrapper-type (type)
  (cond ((boolean-type-p type) 
	 (boolean-wrapper-type))
	((char-type-p type)
	 (char-wrapper-type))
	((or (int-type-p type) 
	     (long-type-p type)
	     (float-type-p type)
	     (double-type-p type))
	 (number-wrapper-type))
	(t 
	 (error "Don't know wrapper type for ~S" type))))

(defun type-to-type-message (type)
  (cond ((boolean-type-p type) 'boolean-value)
	((int-type-p type) 'int-value)
	((long-type-p type) 'long-value)
	((char-type-p type) 'char-value)
	((double-type-p type) 'double-value)
	((float-type-p type) 'float-value)
	(t (error "Don't know wrapper type for ~S" type))))

;;One simple idea would be to extend the previous argument list to deal with keyword arguments

;;Unfortunately,
;;these can't be combined with the previous keyword arguments bc it is
;;impossible to distinguish what is a keyword and what is part of the rest
;;arguments. But that's a minor problem.  Having both ...[TO BE CONTINUED]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;To implement symbols, I though about converting symbols to ints (or
;;longs) but I couldn't find a sufficiently small solution that satisfied me.
;;The convertion to ints would have the advantage to allow a case
;;implementation based on a Java switch.  On the other hand, Common Lisp
;;never had such an implementation, being its case based on sucessive eq
;;comparisons.  As a result, I decided to implement symbols based on
;;interned strings (as they should be) and provide a case which isn't
;;mapped on a switch but is, instead, just like the Common Lisp case.

(def-macro-transform expression '?argument
  ;;numbers and strings are self-evaluating
  (with-linj-syntax ()
    (cond ((or (numberp ?argument)
	       (stringp ?argument)
	       (characterp ?argument)
	       (simple-vector-p ?argument))
	   ?argument)
	  ((keywordp ?argument)
	   `(intern ,(format nil ":~A" ?argument)))
	  ((and (symbolp ?argument) (not (null ?argument)))
	   ;;stupid mix between NIL and ()
	   `(intern ,(princ-to-string ?argument)))
	  (t 
	   ;;fail
	   nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;It is ridicule to try to work with only half of Common Lisp.  (I should say
;;half of the Common Lisp I known, as it may be just 1% of the real thing).
;;One missing thing is multiple-value-bind and values.
;;To implement this, I could translate the code to CPS, but I would be
;;unable to unparse it properly.  Don't forget that I want readable Java code!
;;An alternative is to use the old trick of asking the callee to collect
;;all return values into an array and then dismantle it in the caller to
;;obtain the values. 

(def-syntax values-expression (expression)
  (values . ?arguments/argument-list))

;;We will introduce a special type to deal with a values expression as we
;;want to recover all the arguments types on the caller.

(def-list-syntax values-type-reference (type-reference linj-list-node) type-reference
  :slots
  ((original-form :initform nil)
   (name :initform :values-type)))

;;We have a different principal type
(defmethod get-principal-type ((e values-type-reference))
  (let ((types (values-type-reference-elements e)))
    (if (endp types)
      (void-type)
      (first types))))

(defmethod array-or-values-type-reference-subtype ((e values-type-reference) (dim literal))
  ;;(nth (literal-value dim) (values-type-reference-elements e)))
  (let ((common-type (common-type-or-nil e)))
    (if (not (null common-type))
      common-type
      (object-type))))

(defmethod copy-type ((e values-type-reference))
  (make-instance (type-of e) ;;also used in subclasses
    :elements (values-type-reference-elements e)
    :original-form `(copy-of ,(ast-node-form e))))

(defmethod equal-type-p ((t1 values-type-reference) (t2 values-type-reference))
  (every #'equal-type-p
	 (values-type-reference-elements t1)
	 (values-type-reference-elements t2)))

(defmethod super-type-p ((type1 values-type-reference) (type2 values-type-reference))
  (every #'super-type-p 
	 (values-type-reference-elements type1)
	 (values-type-reference-elements type2)))

(defmethod merge-types ((t1 values-type-reference) (t2 values-type-reference))
  (if (= (length (values-type-reference-elements t1))
	 (length (values-type-reference-elements t2)))
    (with-parent ((ast-node-parent t1))
      (make-instance (type-of t1) ;;also used in subclasses
       :elements (mapcar #'merge-types
			 (values-type-reference-elements t1)
			 (values-type-reference-elements t2))
       :original-form `(merge-type ,(ast-node-form t1) ,(ast-node-form t2))))
    (linj-error "Can't merge values with diferent arity")))


(defmethod get-type ((e values-expression))
  (with-parent ((ast-node-parent e))
    (make-instance 'values-type-reference
      :elements (mapcar #'get-principal-type
			(argument-list-elements (values-expression-arguments e)))
      :original-form `(values-type ,(ast-node-form e)))))

(defmethod common-type-or-nil ((e values-type-reference))
  (let ((types (values-type-reference-elements e)))
    (if (every #'(lambda (other-type) 
		   (equal-type-p (first types) other-type))
	       (rest types))
      (first types)
      nil)))

(def-unparse values-type-reference (e)
  (let ((common-type (common-type-or-nil e)))
    (if common-type
      (format t "~/pp/[]" common-type)
      (format t "Object[]"))))

(def-unparse values-expression (e)
  (let ((common-type (common-type-or-nil (get-type e))))
    (if common-type
      (unparse-vector-inits
       common-type
       (mapcar #'principal-value (argument-list-elements (values-expression-arguments e))))
      (unparse-vector-inits
       (with-parent (e) (object-type))
       (mapcar #'(lambda (arg)
		   (let ((type (get-principal-type arg)))
		     (if (primitive-type-reference-p type)
			 (if (boolean-type-p type)
			     (with-parent (e)
			       (make-instance 'method-call-expression
					      :receiver (boolean-wrapper-type)
					      :name (parse 'value-of 'linj-name)
					      :original-form 'auto-value-of-for-types
					      :arguments (make-instance 'argument-list
									:original-form 'auto-arg-value-of-for-types
									:elements (list arg))))
			     (with-parent (e)
			       (make-instance 'allocation-expression
					      :receiver (wrapper-type type)
					      :arguments (make-instance 'argument-list :elements (list arg)))))
			 (principal-value arg))))
	       (argument-list-elements (values-expression-arguments e)))))))

(def-syntax nth-value-expression (expression)
  (nth-value (?is ?n numberp) ?argument/expression))

(defmethod get-type ((e nth-value-expression))
  (let ((type (get-type (nth-value-expression-argument e))))
    (if (values-type-reference-p type) 
      (nth (nth-value-expression-n e) (values-type-reference-elements type))
      type)))

(def-unparse nth-value-expression (e)
  (let ((type (get-type (nth-value-expression-argument e))))
    (if (values-type-reference-p type)
      (let ((common-type (common-type-or-nil type)))
	(if common-type
	  (format t "~/pp/"
		  (with-parent (e) ;;HACK: This is dangerous bc it doesn't
	            ;;consider local macros.
		    (make-instance 'array-reference
				   :original-form :computed
				   :expression (nth-value-expression-argument e)
				   :indexes (make-instance
					     'argument-list
					     :elements (list (make-instance
							      'literal
							      :value (nth-value-expression-n e)))))))
	  (format t "~/pp/"
		  (with-parent (e)
		    (parse (unwrap-if-needed (copy-type (get-type e))
					     `(aref ,(nth-value-expression-argument e) ,(nth-value-expression-n e)))
			   'expression)))))
      (unparse-linj (nth-value-expression-argument e)))))


(defmethod get-principal-value ((e values-type-reference) expr)
  (make-instance 'nth-value-expression
		 :original-form 'auto-nth-value-expression
		 :n 0
		 :argument expr))


(def-macro-transform statement (multiple-value-bind ?parameters 
				   ?values-form
				 . ?body)
  (cond ((endp ?parameters)
	 `(progn ,?values-form ,@?body))
	((endp (rest ?parameters))
	 `(let ((,(first ?parameters) ,?values-form)) ,@?body))
	(t
	 (let ((multiple-results 'multiple-results))
	   `(let ((,multiple-results ,?values-form))
	      (let ,(loop for count from 0 and param in ?parameters
			  collect `(,param (nth-value ,count ,multiple-results)))
		,@?body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Futures

(def-syntax future (expression)
  (future ?expression/expression)
  :components
  (?translation)
  :slots
  ((translation :accessor future-translation :initarg :translation))
  :constructor make-future)

(defun make-future (category &key original-form expression)
  (declare (ignore category))
  (make-instance 'future
    :original-form original-form
    :expression expression
    :translation (parse `(new 'linj.future #'(lambda () ,(ast-node-form expression))) 'expression)))

;;We will use the values-type-reference machinery
(def-category future-type-reference (values-type-reference)
  ())

(defmethod future-type-reference-subtype ((e future-type-reference))
  (first (values-type-reference-elements e)))

(defmethod get-type-declaration ((e future-type-reference))
  (get-type-declaration (parse 'linj.future 'type-reference)))

(defmethod get-type ((e future))
  (with-parent ((ast-node-parent e))
    (make-instance 'future-type-reference
      :original-form `(future-type ,(ast-node-form e))
      :elements (list (get-principal-type (future-expression e))))))

(def-unparse future-type-reference (e)
  ;;this should not be used
  (format t "~/pp/" (with-parent (e) (parse 'linj.future 'type-reference))))

(def-unparse future (e)
;;;  (format t "Hummm")
  (format t "~/pp/" (future-translation e)))

(def-syntax make-future-present (expression)
  (touch ?argument/expression))

(defmethod get-type ((e make-future-present))
  (let ((type (get-type (make-future-present-argument e))))
    (if (future-type-reference-p type)
      (future-type-reference-subtype type)
      type)))

(def-unparse make-future-present (e)
  (let ((type (get-type (make-future-present-argument e))))
    (if (future-type-reference-p type)
      (format t "~/pp/"
	      (with-parent (e)
		(parse (unwrap-if-needed (copy-type (get-type e))
					 `(send ,(make-future-present-argument e) get-value))
		       'expression))))))

(defmethod get-principal-value ((e future-type-reference) expr)
  (make-instance 'make-future-present
		 :original-form 'auto-make-future-present
		 :argument expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;The try statement is a fundamental one.
;;The agreed syntax is something similar to the Common Lisp's handler-case
;; (try (form1 form2 formn) ((var1 type1) . body1) ((var2 type2) . body2)
;; ...  (finally . bodyn))

(def-syntax default-handler (linj-node)
  (finally . ?body/statement-list))

(def-unparse default-handler (e)
  (format t "finally ~/ppblk/"
	  (default-handler-body e)))

(def-syntax handler-variable-declaration (variable-declaration)
  (?name/linj-name ?type/type-reference))

(def-syntax handler (default-handler)
  (?binding/handler-variable-declaration . ?body/statement-list))

(def-unparse handler (e)
  (format t "catch (~/pp/) ~/ppblk/"
	  (handler-binding e)
	  (handler-body e)))

(defmethod find-declaration-in ((ref reference) (e handler))
  (if (eq (reference-name ref) (variable-declaration-name (handler-binding e)))
    (handler-binding e)
    (call-next-method)))

(def-list-syntax handler-list (linj-list-node) default-handler)

(def-unparse handler-list (e)
  (format t "~{ ~/pp/~}" (handler-list-elements e)))

(def-syntax try-statement (statement)
  (try ?form/statement . ?handlers/handler-list))

(defun handled-throws (try)
  (let ((throws (list)))
    (dolist (clause (handler-list-elements (try-statement-handlers try)))
      (when (handler-p clause)
	(push (handler-variable-declaration-type (handler-binding clause)) throws)))
    throws))

(def-unparse try-statement (e)
  (format t "~@<try ~/ppstm/~/pp/~:>"
	  (try-statement-form e)
	  (try-statement-handlers e)))

;;; Macro HANDLER-CASE
;;; Syntax:
;;; handler-case expression [[{error-clause}* | no-error-clause]] => result*
;;; clause::= error-clause | no-error-clause 
;;; error-clause::= (typespec ([var]) declaration* form*) 
;;; no-error-clause::= (:no-error lambda-list declaration* form*) 

(defun translate-handler-case-clause (clause ?form ?clauses)
  (if (eq (first clause) :no-error)
    (error "Linj handler case does not support a :no-error clause")
    `((,(if (endp (second clause))
	    (gen-new-name 'e (cons ?form ?clauses))
	    (first (second clause)))
       ,(first clause))
      ,@(rest (rest clause)))))

(def-macro-transform try-statement
    (handler-case ?form . ?clauses)
  `(try
    ,?form
    ,@(mapcar #'(lambda (clause)
		  (translate-handler-case-clause clause ?form ?clauses))
	      ?clauses)))
;;;Synchronization

(def-syntax synchronized-statement (statement)
  (with-lock (?expression/expression) . ?body/statement-list))

(def-unparse synchronized-statement (e)
  (when (primitive-type-reference-p (get-type (synchronized-statement-expression e)))
    (linj-error "Can't lock a member of a primitive type: ~A" e))
  (format t "~@<synchronized (~:/ppexp/) ~/ppblk/~:>"
	  (synchronized-statement-expression e)
	  (synchronized-statement-body e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Now, some helpfull macros
; (def-transform try-statement (unwind-protect ?form . ?cleanup-forms)
;   (try ?form
;        ((generated-exception <exception>))
;        (finally . ?cleanup-forms)))

(def-macro-transform try-statement (unwind-protect ?form . ?cleanup-forms)
  (or (let-pattern (((handler-case ?form . ?clauses) ?form))
	`(try ,?form
	  ,@(mapcar #'(lambda (clause)
			(translate-handler-case-clause clause ?form ?clauses))
		    ?clauses)
	  (finally ,@?cleanup-forms)))
      `(try ,?form
	(finally ,@?cleanup-forms))))

(def-macro-transform try-statement (ignore-errors . ?forms)
  `(try (progn . ,?forms) ((,(gen-new-name 'e ?forms) throwable))))

(def-macro-transform try-statement (without-checked-exceptions . ?forms)
  (let ((var (gen-new-name 'e ?forms)))
    `(try (progn . ,?forms) ((,var exception) (print-stack-trace ,var) (error "Can't continue")))))

(def-macro-transform let-statement (fluid-let ?bindings . ?body)
  `(let ,(mapcar #'(lambda (binding)
		     `(,(conc-symbol 'fluid-let- (car binding)) ,(car binding)))
	  ?bindings)
     (unwind-protect
	 (progn
	   ,@(mapcar #'(lambda (binding)
			 `(setf ,@binding))
		     ?bindings)
	   ,@?body)
       ,@(mapcar #'(lambda (binding)
		     `(setf ,(car binding) ,(conc-symbol 'fluid-let- (car binding))))
		 ?bindings))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;x
;;static blocks are like top-level code:

; (def-syntax static-block (type-body-declaration)
;   (static-block . ?body/statement-list))

; (def-unparse static-block (e)
;   (format t "~@<static ~/ppstm/~:>"
; 	  (static-block-body e)))

;;I prefer to separate each statement in a unique static block.  You have
;;progn, anyway.

(def-syntax static-block (type-body-declaration)
  ?statement/statement)

(def-unparse static-block (e)
  (format t "~@<static {~4I~:@_~/ppblk/~I~:@_}~:>"
	  (static-block-statement e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;x
;;A kind of code-walker:

(defmethod apply-to-method-exit-points ((e method-declaration) f)
  (apply-to-exit-point (method-declaration-body e) f (list 'normal-flow (method-declaration-name e))))

(defmethod apply-to-exit-point ((e statement-list) f blocks)
  (if (endp (statement-list-elements e))
    (unless (member 'non-normal-flow blocks)
      (funcall f (make-instance 'explicit-type-node :type (void-type))))
    (progn
      (dolist (statement (butlast (statement-list-elements e)))
	(apply-to-exit-point statement f (cons 'non-normal-flow blocks)))
      (apply-to-exit-point (first (last (statement-list-elements e))) f blocks))))

(defmethod apply-to-exit-point ((e statement) f blocks)
  (declare (ignore f blocks)))

(defmethod apply-to-exit-point ((e when-statement) f blocks)
  (apply-to-exit-point (when-statement-body e) f blocks)
  ;;the when-else is like an empty else in an if-then-else
  (apply-to-exit-point (make-instance 'statement-list :elements (list)) f blocks))
  ;;This is necessary so that when-statements force methods to have void return type
;;In fact, it is not needed bc the statement-list already does that.
;;   (unless (member 'non-normal-flow blocks)
;;     (funcall f (make-instance 'explicit-type-node :type (void-type)))))

(defmethod apply-to-exit-point ((e expression-statement) f blocks)
  (unless (member 'non-normal-flow blocks)
    (funcall f e)))

(defmethod apply-to-exit-point ((e block-statement) f blocks)
  (apply-to-exit-point (block-statement-body e)
		       f
		       (cons (block-statement-name e) blocks)))

(defmethod apply-to-exit-point ((e return-from-statement) f blocks)
  (unless (member 'non-normal-flow (rest (member (return-from-statement-label e) blocks)))
    (funcall f e)))

(defmethod apply-to-exit-point ((e return-statement) f blocks)
  (declare (ignore blocks))
  (funcall f e))

(defmethod apply-to-exit-point ((e for-statement) f blocks)
  (apply-to-exit-point (for-statement-body e)
		       f
		       (cons 'non-normal-flow blocks)))

(defmethod apply-to-exit-point ((e let-statement) f blocks)
  (apply-to-exit-point (let-statement-body e) f blocks))

(defmethod apply-to-exit-point ((e cond-statement) f blocks)
  (dolist (clause (clause-list-elements (cond-statement-clauses e)))
    (apply-to-exit-point (clause-body clause) f blocks)))

(defmethod apply-to-exit-point ((e case-statement) f blocks)
  (dolist (clause (case-clause-list-elements (case-statement-clauses e)))
    (apply-to-exit-point (default-case-clause-body clause) f blocks)))

(defmethod apply-to-exit-point ((e if-statement) f blocks)
  (apply-to-exit-point (if-statement-then e) f blocks)
  (apply-to-exit-point (if-statement-else e) f blocks))

(defmethod apply-to-exit-point ((e try-statement) f blocks)
  (apply-to-exit-point (try-statement-form e) f blocks)
  (dolist (handler (handler-list-elements (try-statement-handlers e)))
    (if (handler-p handler)
      (apply-to-exit-point (default-handler-body handler) f blocks)
      (apply-to-exit-point (default-handler-body handler) f (cons 'non-normal-flow blocks)))))

(defmethod apply-to-exit-point ((e in-statement) f blocks)
  (apply-to-exit-point (in-statement-body e) f blocks))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Extensions that depend on java support code

;;The idea is to translate lambda expressions to java inner classes
;;depending on the return type (in particular, distinguishing the classical
;;cases of boolean-predicates, Object-functions and void-procedures) and on
;;the number of parameters (distinguishing unary, binary, etc).  Notice
;;that non-unary methods have not been used in the past because it was
;;ackward to define all of them.

;;Here is an example of a lambda expression translated into java
;; (lambda boolean ((component c))
;;   (return (is-visible c)))

;;new Predicate() { 
;;  public boolean funcall(Object arg) { 
;;     Component c = (Component)arg; 
;;     return c.isVisible(); }}


;;As a preliminary, we should define the concept of anonymous inner class
;;expression:

;;this is a simplification justified by the fact that, in general,
;;anonymous superclasses do not define new methods, they just override
;;them

(def-macro-transform class-declaration (class ?class . ?body)
  (let ((real-class (parse ?class 'type-reference)))
    `(defclass-0 (class-or-interface-type 
		  ,(empty-package)
		  ,(gensym (princ-to-string (conc-symbol 'anonymous- (ast-node-form (class-or-interface-type-reference-name real-class))))))
       ,real-class () ()
       . ,?body)))

(defmethod anonymous-class-declaration-p ((class class-declaration))
  (anonymous-type-reference-p (class-declaration-name class)))

(def-syntax anonymous-class-allocation-expression (allocation-expression)
  (new ?class/class-declaration . ?arguments/argument-list)
  :slots ((receiver :initform (list))))

(defmethod method-call-expression-receiver ((e anonymous-class-allocation-expression))
  (class-declaration-superclass (anonymous-class-allocation-expression-class e)))

(defmethod get-type-declaration ((e anonymous-class-allocation-expression))
  (anonymous-class-allocation-expression-class e))

;;Sometimes we need to locate a default constructor on a mixin-declaration
(defmethod find-declaration-in ((alloc anonymous-class-allocation-expression) (e mixin-declaration))
  *default-constructor*)


(def-unparse anonymous-class-allocation-expression (e)
  (format t "~@<new ~/pp/~/pp/ {~4I~{~:@_~/pp/~^~:@_~}}~:>"
	  (class-declaration-superclass (anonymous-class-allocation-expression-class e))
	  (allocation-expression-arguments e)
	  (type-body-elements (class-declaration-body (anonymous-class-allocation-expression-class e)))))

;; (defmethod get-type ((e anonymous-class-allocation-expression))
;;   (get-type (anonymous-class-allocation-expression-class e)))

(defclass free-references-collector ()
  ((form :initarg :form :reader get-form)
   (references :initform (list) :accessor get-references)))

(defun contained-in (e form)
  (or (eq e form)
      (and e (contained-in (ast-node-parent e) form))))

(defmethod visit :before ((e reference) (visitor free-references-collector))
  (unless (or (type-reference-p e)
	      (this-reference-p e)
	      (super-reference-p e)
	      (keyword-reference-p e)
	      (alias-this-reference-p e))
    (let ((decl (strict-find-declaration e)))
      (unless (slot-declaration-p decl)
	(unless (contained-in decl (get-form visitor))
	  (push e (get-references visitor)))))))

(defun free-references (form)
  (let ((visitor (make-instance 'free-references-collector :form form)))
    (visit form visitor)
    (get-references visitor)))


(defclass define-finals-visitor ()
  ())

(defmethod visit :after ((e anonymous-class-allocation-expression) (visitor define-finals-visitor))
  (dolist (reference (free-references (class-declaration-body (anonymous-class-allocation-expression-class e))))
    (let ((decl (strict-find-declaration reference)))
      (define-final-on-variable-declaration decl))))

(defun define-final-on-variable-declaration (var-decl)
  (setf (get-option :final var-decl) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Extensions:

(def-transform expression *standard-input* (slot-value (the java.lang.System) 'in))
(def-transform expression *standard-output* (slot-value (the java.lang.System) 'out))
(def-transform expression *error-output* (slot-value (the java.lang.System) 'err))
(def-transform expression *trace-output* *error-output*)

(def-transform setf-expression (setf *standard-input* ?stream) (send (the java.lang.System) set-in ?stream))
(def-transform setf-expression (setf *standard-output* ?stream) (send (the java.lang.System) set-out ?stream))
(def-transform setf-expression (setf *error-output* ?stream) (send (the java.lang.System) set-err ?stream))
(def-transform setf-expression (setf *trace-output* ?stream) (setf *error-output* ?stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass imports-visitor ()
  ((packages :accessor imports-visitor-packages :initform (make-hash-table))))

(defun add-import-info (type visitor)
  (unless (empty-package-p (class-or-interface-type-reference-package type))
    (pushnew type
	     (gethash (class-or-interface-type-reference-name type)
		      (imports-visitor-packages visitor)
		      '())
	     :test #'(lambda (e1 e2)
		       (equal-type-reference-package (class-or-interface-type-reference-package e1)
						     (class-or-interface-type-reference-package e2))))))


;;To determine imports, the method-call-expression must be initialized
(defmethod visit :before ((e method-call-expression) (visitor imports-visitor))
  (check-initialization e))

;;nodes with type information
(defmethod visit-current ((e class-or-interface-type-reference) (visitor imports-visitor))
  (unless (null (class-or-interface-type-reference-package e)) ;;arrays don't have package
    (add-import-info e visitor)))

;;but excluding in-declarations (their type information is spread of its uses).
(defmethod visit ((e in-declaration) (visitor imports-visitor)))

;;some literals correspond to class instances
(defmethod visit-current ((e literal) (visitor imports-visitor))
  (let ((type (get-type e)))
    (unless (primitive-type-reference-p type)
      (add-import-info type visitor))))

(defclass determine-imports ()
  ())

(defparameter *determine-imports* t)

(defmethod visit ((e compilation-unit) (v determine-imports))
  (when *determine-imports*
    (let ((visitor (make-instance 'imports-visitor)))
      (visit e visitor)
      (let ((unit-package (compilation-unit-package e))
	    (previous-imports (compilation-unit-imports e))
	    (extra-imports (list)))
	(maphash #'(lambda (name types)
		     (declare (ignore name))
		     (when (endp (rest types))
		       (let ((type (first types)))
			 (when (not (or (package= (class-or-interface-type-reference-package type)
						  (package-declaration-components unit-package))
					(some #'(lambda (import)
						  (match-type
						   (import-declaration-type import)
						   type))
					      previous-imports)))
			   (push (first types) extra-imports)))))
		 (imports-visitor-packages visitor))
	(when extra-imports
	  (format *trace-output* "Add imports:~%")
	  (setf (compilation-unit-imports e)
		(append previous-imports
			(mapcar #'(lambda (import)
				    (let ((import (parse `(import ,(copy-type import)) 'import-declaration)))
				      (unparse-linj import *trace-output*)
				      (terpri *trace-output*)
				      import))
				extra-imports))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Visitors' sequence

(add-visitor 'after-parse 'adjust-parents)
(add-visitor 'after-parse 'process-declares)
(add-visitor 'after-parse 'parse-tree-finish)
(add-visitor 'after-parse 'expand-macros-visitor)
;;(add-visitor 'after-parse 'mixin-transform)
;;(add-visitor 'after-parse 'inherit-constructors-transform)
(add-visitor 'after-parse 'return-type-inference-visitor)
(add-visitor 'after-parse 'link-afternew-transform)
(add-visitor 'after-parse 'process-return-from-statements)
(add-visitor 'after-parse 'compress-blocks)

(add-visitor 'before-dump 'remove-unneded-blocks)
;;(add-visitor 'before-dump 'overrides-visitor)
(add-visitor 'before-dump 'define-finals-visitor)
(add-visitor 'before-dump 'cast-and-wrap)
(add-visitor 'before-dump 'add-method-throws-visitor)
(add-visitor 'before-dump 'determine-imports)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Standard method combination:

;;We want to provide the Common Lisp standard method combination, based on
;;primary, before and after methods.

;;The idea is that when we have an :after method (for example), we can
;;automatically generate a method that invokes a call-next-method.  We can
;;even make some simplifying assumptions such as avoiding the return type
;;and the argument list (although I'm not so sure this is a good idea).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;The function concept:  each function is a static method on the class
;;file.  Each invocation is replaced by the correspondent method
;;invocation.  Each file must import other files to be able to use its
;;functions. Let's do it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;For the emacs-lisp connection
#+allegro
(defclass linj-request (lep::compile/load-file-request)
  ())

#+allegro
(defmethod lep::compute-reply ((session linj-request) 
			       &key filename directory)
  (format *trace-output* "Filename: ~A~%" filename)
  (let ((curr-dir (format nil "~A" (get-current-directory))))
    (excl:chdir directory)
    (prog1
	(linj2java filename)
      (excl:chdir curr-dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Containing blocks

(defmethod containing-block-named ((e linj-node) name)
  (containing-block-named (ast-node-parent e) name))

(defmethod containing-block-named ((e method-declaration) name)
  (if (eq name (method-declaration-name e))
    e
    (linj-error "Can't find block named ~A in method ~A" name e)))

(defmethod containing-block-named ((e block-statement) name)
  (if (eq (block-statement-name e) name)
    e
    (call-next-method)))


(defmethod jumped-over-blocks ((e linj-node) name)
  (jumped-over-blocks (ast-node-parent e) name))

(defmethod jumped-over-blocks ((e method-declaration) name)
  (declare (ignore name))
  nil)

(defmethod jumped-over-blocks ((e block-statement) name)
  (if (eq (block-statement-name e) name)
    (list)
    (cons (block-statement-name e)
	  (call-next-method))))

(defmethod jumped-over-blocks ((e case-statement) name)
  (declare (ignore name))
  ;;Warn: we must consider case-statements as invisible blocks to
  ;;avoid incorrect breaks.
  (cons (gensym)
	(call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Collect throws
;;Must be done before unparsing so that the correct import statements can be derived.
(defclass add-method-throws-visitor ()
  ())

(defparameter *infer-method-throws* t)

(defmethod visit :after ((e method-declaration) (visitor add-method-throws-visitor))
  (when *infer-method-throws*
    (method-declaration-throws-list e)))

(defparameter +error-type+ (parse 'java.lang.error 'type-reference))

(defparameter +runtime-exception-type+ (parse 'java.lang.runtime-exception 'type-reference))

(defmethod method-declaration-throws-list ((e method-declaration))
  (cond ((computed-method-declaration-throws-p e)
	 (method-declaration-computed-throws e))
	(*infer-method-throws*
	 (setf (method-declaration-computed-throws e)
	       (infer-method-throws e))
	 (method-declaration-computed-throws e))
	(t
	 (type-reference-list-elements (method-declaration-declared-throws e)))))

(defun checked-exceptions (types)
  (remove-if #'(lambda (type)
		 (or (super-type-p +error-type+ type)
		     (super-type-p +runtime-exception-type+ type)))
	     types))

(defun type-union (types)
  (let ((new-types (list)))
    (mapl #'(lambda (types)
	      (let ((first-type (first types)))
		(unless (or (some #'(lambda (other-type)
				      (super-type-p other-type first-type))
				  (rest types))
			    (some #'(lambda (other-type)
				      (super-type-p other-type first-type))
				  new-types))
		  (push first-type new-types))))
	  types)
    (sort new-types #'string< :key #'type-reference-name)))

(defclass collect-throws-visitor ()
  ((throws :initform (list) :accessor throws)))

(defmethod visit ((e try-statement) (visitor collect-throws-visitor))
  ;;First, the form:
  (let ((old-throws (throws visitor))
	(handled-throws (handled-throws e)))
    (setf (throws visitor) (list))
    (visit (try-statement-form e) visitor)
    (let ((new-throws (throws visitor)))
      ;;remove all those exceptions that are handled by the try stratement
      (setf (throws visitor)
	    (union (remove-if #'(lambda (new-throw)
				  (some #'(lambda (handled-throw)
					    (super-type-p handled-throw new-throw))
					handled-throws))
			      new-throws)
		   old-throws
		   :test #'equal-type-p))))
  ;;Now, add all throws in the handlers:
  (visit (try-statement-handlers e) visitor))

(defmethod visit :after ((e method-call-expression) (visitor collect-throws-visitor))
  (unless (or (anonymous-class-allocation-expression-p e)
	      (next-method-call-expression-p e))
    (setf (throws visitor)
	  (union (method-declaration-throws-list (strict-find-declaration e))
		 (throws visitor)
		 :test #'equal-type-p))))

(defmethod visit :after ((e throw-statement) (visitor collect-throws-visitor))
  (setf (throws visitor)
	(adjoin (get-type (throw-statement-argument e))
		(throws visitor)
		:test #'equal-type-p)))

(defparameter *computing-throws-stack* (list))

(defun infer-method-throws (meth-decl)
  (if (member meth-decl *computing-throws-stack*)
    (list)
    (let ((*computing-throws-stack* (cons meth-decl *computing-throws-stack*)))
      (let ((visitor (make-instance 'collect-throws-visitor)))
	(visit meth-decl visitor)
	(let ((explicit-throws (type-reference-list-elements (method-declaration-declared-throws meth-decl))))
	  (let ((checked-throws
		 (mapcar #'(lambda (type)
			     (with-parent (meth-decl)
					  (copy-type type)))
			 (checked-exceptions (set-difference (throws visitor) explicit-throws :test (complement #'super-type-p))))))
	    (if (endp checked-throws)
	      explicit-throws
	      (with-linj-syntax ()
		(warn "Method ~S throws ~{~/pp/~^, ~}" (method-declaration-name meth-decl) checked-throws)
		(type-union (union explicit-throws checked-throws :test #'equal-type-p))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun linj2undoc-java-directory (&rest args)
  (let ((*type-body-declarations-order*
	 '(("// constants" :constants)
	   ("// slots" :slots)
	   ("// constructors" :constructors)
	   ("// member classes" :member-classes)
	   ("// finalizers" :finalizers)
	   ("// accessors" :accessors)
	   ("// methods" :methods)
	   ("// key methods" :key-methods)
	   ("// trace methods" :tracing-methods)
	   ("// static blocks" :static-blocks)))
	(*write-category-comment* nil))
    (apply #'linj2java-directory args)))