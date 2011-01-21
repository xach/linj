;;; -*- Mode: Common-Lisp; Syntax: Common-Lisp; Package: LINJ; Base: 10 -*-

;;; Copyright (C) Antonio Menezes Leitao Created on Sun Apr 21 18:38:01 2002
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

#|
;;The idea is to parse code in the appropriate context but then use a
;;special macro-expand step to translate the forms:

;;Here an example

(def-linj-macro expression (1+ ?arg/expression)
  `(+ 1 ,?arg))

;;This expands into

(def-syntax |1+ ?arg/expression| (expression)
  (1+ ?arg/expression))

(defmethod visit-current ((ast-node |1+ ?arg/expression|) (visitor expand-macros-visitor))
  (let ((new-ast-node 
	 (with-parent ((ast-node-parent ast-node))
	   (parse-and-visit
	    `(+ 1 ,(|1+ ?arg/expression|-arg ast-node))
	    'expression
	    visitor))))
    (transform-instance ast-node new-ast-node)))
|#

(defclass expand-macros-visitor ()
  ())

;;CLISP seems to have problems with combinations of with-slots and
;;change-class. As I only read the slot values (and never modify them)
;;I'll change the with-slots to a let + slot-value.
(defmacro def-linj-macro (super-category pattern &body body)
  (destructuring-bind (super-category &key (phase :pre-syntax))
      (if (consp super-category) super-category (list super-category))
    (let ((category (intern (format nil "~A-~A" super-category pattern)))
	  (ast-node (gensym)) (visitor (gensym)) (new-ast-node (gensym))
	  (new-form (gensym)))
      (multiple-value-bind (super-category parsing-category)
	  (if (consp super-category)
	      (values (first super-category) (second super-category))
	      (values super-category nil))
	`(progn
	  (def-syntax ,category (,super-category)
	    ,pattern
	    :accessors nil
	    :phase ,phase)
	 
	  (defmethod expand-macro-call ((,ast-node ,category))
	    ,(let* ((pat-vars (collect-pattern-vars pattern))
		    (slots (mapcar #'(lambda (pat-var-cat)
				       (let ((pat-var (pat-var-name-categorie pat-var-cat)))
					 `(,pat-var (slot-value ,ast-node ',(var-name pat-var)))))
				   pat-vars)))
		   `(let ((,new-form
			   (let ,slots
			     (macrolet ((fail () '',new-form))
			       ,@body))))
		     (let ((,new-ast-node
			    (if (eq ,new-form ',new-form) ;;fail was used
				(with-parent ((ast-node-parent ,ast-node))
				  (avoiding-parse-rule
				      ',category
				    (let ,slots
				      (parse (sublis (pairlis ',pat-vars (list ,@(mapcar #'first slots)))
						     ',(simplify-pattern-tree pattern))
					     (ast-node-parse-category ,ast-node)))))
				(with-parent ((ast-node-parent ,ast-node))
				  (parse ,new-form ,(if parsing-category
							`',parsing-category
							`(ast-node-parse-category ,ast-node))))))) ;;to avoid infinite recursion
		       (become-instance ,ast-node ,new-ast-node)
		       (apply-after-parse-visitors ,ast-node)))))

	  ;;There are some situations that force macroexpansion
	  (defmethod get-type ((,ast-node ,category))
	    (expand-macro-call ,ast-node)
	    (get-type-after-expand ,ast-node))
	 
	  (defmethod apply-to-exit-point ((,ast-node ,category) f blocks)
	    (expand-macro-call ,ast-node)
	    (apply-to-exit-point-after-expand ,ast-node f blocks))

	  (defmethod visit ((,ast-node ,category) (,visitor collect-type-info))
	    (expand-macro-call ,ast-node)
	    (visit-after-expand ,ast-node ,visitor))

	  (defmethod visit ((,ast-node ,category) (,visitor expand-macros-visitor))
	    (expand-macro-call ,ast-node)
	    (visit-after-expand ,ast-node ,visitor)))))))

;; These are necessary to deal with the new CMUCL optimized CLOS.  The
;; reason is explicit in the hyperspec: The generic function
;; change-class has several semantic difficulties. First, it performs
;; a destructive operation that can be invoked within a method on an
;; instance that was used to select that method. When multiple methods
;; are involved because methods are being combined, the methods
;; currently executing or about to be executed may no longer be
;; applicable. Second, some implementations might use compiler
;; optimizations of slot access, and when the class of an instance is
;; changed the assumptions the compiler made might be violated. This
;; implies that a programmer must not use change-class inside a method
;; if any methods for that generic function access any slots, or the
;; results are undefined.
(defun apply-to-exit-point-after-expand (node f blocks)
  (apply-to-exit-point node f blocks))

(defun get-type-after-expand (node)
  (get-type node))

(defun apply-after-parse-visitors (ast-node)
  (apply-previous-visitors ast-node))

(defun visit-after-expand (node visitor)
  (visit node visitor))

(defparameter *numerical-object-is* :bignum)

(def-linj-macro binary-operator-expression
  ((?is ?name (lambda (oper) (member oper '(< <= > >= = /=))))
   ?arg1/expression
   ?arg2/expression)
  (let ((type1 (get-principal-type ?arg1))
	(type2 (get-principal-type ?arg2)))
    (cond ((and (or (float-type-p type1)
		    (double-type-p type1))
		(not (super-type-p type1 type2)))
	   `(,?name ,?arg1 ,(convert-to-type ?arg2 type1)))
	  ((and (or (float-type-p type2)
		    (double-type-p type2))
		(not (super-type-p type2 type1)))
	   `(,?name ,(convert-to-type ?arg1 type2) ,?arg2))
	  ((or (big-decimal-type-p type1)
	       (big-decimal-type-p type2))
	   `(,?name (compare-to ,(convert-to-type ?arg1 (big-decimal-type))
				,(convert-to-type ?arg2 (big-decimal-type)))
		    0))
	  ((or (bignum-type-p type1)
	       (bignum-type-p type2))
	   `(,?name (compare-to ,(convert-to-type ?arg1 (bignum-type))
				,(convert-to-type ?arg2 (bignum-type)))
		    0))
	  ((or (big-integer-type-p type1)
	       (big-integer-type-p type2))
	   `(,?name (compare-to ,(convert-to-type ?arg1 (big-integer-type))
				,(convert-to-type ?arg2 (big-integer-type)))
		    0))
	  ((or (long-wrapper-type-p type1)
	       (long-wrapper-type-p type2))
	   `(,?name (maybe-convert-to-type ,?arg1 ,(long-type))
		    (maybe-convert-to-type ,?arg2 ,(long-type))))
	  ((or (double-wrapper-type-p type1)
	       (double-wrapper-type-p type2))
	   `(,?name (maybe-convert-to-type ,?arg1 ,(double-type))
		    (maybe-convert-to-type ,?arg2 ,(double-type))))
	  ((or (object-type-p type1)
	       (object-type-p type2))
	   (case *numerical-object-is*
	     ((:int)
	      `(,?name (maybe-convert-to-type ,?arg1 ,(int-type))
		       (maybe-convert-to-type ,?arg2 ,(int-type))))
	     ((:long)
	      `(,?name (maybe-convert-to-type ,?arg1 ,(long-type))
		       (maybe-convert-to-type ,?arg2 ,(long-type))))
	     ((:bignum)
	      `(,?name (compare-to (maybe-convert-to-type ,?arg1 ,(bignum-type))
				   (maybe-convert-to-type ,?arg2 ,(bignum-type)))
		       0))))
	  (t
	   (fail)))))

(def-linj-macro binary-operator-expression
  ((?is ?name (lambda (oper) (member oper '(rem floor logand logior logxor))))
   ?arg1/expression
   ?arg2/expression)
  (let ((type1 (get-type ?arg1))
	(type2 (get-type ?arg2)))
    (cond ((and (eq ?name 'floor)
		(primitive-type-reference-p type1))
	   `(the ,(copy-type type1) (floor (/ ,?arg1 ,?arg2))))
	  ((and (eq ?name 'floor)
		(equal-type-p type1 type2)
		(not (object-type-p type1)))
	   (fail))
	  (t
	   (let ((name (case ?name 
			 ((rem) 'remainder)
			 ((logand) 'and)
			 ((logior) 'or)
			 ((logxor) 'xor)
			 (t ?name))))
	     (cond ((or (bignum-type-p type1)
			(bignum-type-p type2))
		    `(,name ,(convert-to-type ?arg1 (bignum-type))
		      ,(convert-to-type ?arg2 (bignum-type))))
		   ((or (big-integer-type-p type1)
			(big-integer-type-p type2))
		    `(,(if (eq name 'floor) 'divide name)
		      ,(convert-to-type ?arg1 (big-integer-type))
		      ,(convert-to-type ?arg2 (big-integer-type))))
		   ((or (long-wrapper-type-p type1)
			(long-wrapper-type-p type2))
		    `(,?name (maybe-convert-to-type ,?arg1 ,(long-type))
		      (maybe-convert-to-type ,?arg2 ,(long-type))))
		   ((or (object-type-p type1)
			(object-type-p type2))
		    (case *numerical-object-is*
		      ((:int)
		       `(,?name (maybe-convert-to-type ,?arg1 ,(int-type))
			 (maybe-convert-to-type ,?arg2 ,(int-type))))
		      ((:long)
		       `(,?name (maybe-convert-to-type ,?arg1 ,(long-type))
			 (maybe-convert-to-type ,?arg2 ,(long-type))))
		      ((:bignum)
		       `(,?name (maybe-convert-to-type ,?arg1 ,(bignum-type))
			 (maybe-convert-to-type ,?arg2 ,(bignum-type))))))
		   (t
		    (fail))))))))

(def-linj-macro binary-operator-expression (logbitp ?arg1/expression ?arg2/expression)
  (let ((type2 (get-type ?arg2)))
    (cond ((primitive-type-reference-p type2)
	   `(not (zerop (logand ,?arg2 (ash 1L ,?arg1)))))
	  ((or (bignum-type-p type2)
	       (big-integer-type-p type2))
	   `(test-bit ,?arg2 ,?arg1))
	  (t
	   (fail)))))


(defun convert-wrapper (oper exprs type1 type2)
  `(maybe-convert-to-type
    (,oper ,@(mapcar #'(lambda (expr)
			 `(maybe-convert-to-type ,expr ,type1))
		     exprs))
    ,type2))

(defun wrapping-conversion (name object-name types args)
  (cond ((find-if #'double-wrapper-type-p types)
	 (convert-wrapper name args (double-type) (double-wrapper-type)))
	((find-if #'float-wrapper-type-p types)
	 (convert-wrapper name args (float-type) (float-wrapper-type)))
	((find-if #'long-wrapper-type-p types)
	 (convert-wrapper name args (long-type) (long-wrapper-type)))
	((find-if #'int-wrapper-type-p types)
	 (convert-wrapper name args (int-type) (int-wrapper-type)))
	((find-if #'object-type-p types)
	 (case *numerical-object-is*
	   (:int 
	    (convert-wrapper object-name args (int-type) (int-type)))
	   (:long
	    (convert-wrapper object-name args (long-type) (long-type)))
	   (:bignum
	    (convert-wrapper object-name args (bignum-type) (bignum-type)))))
	(t
	 nil)))

(def-linj-macro unary-operator-expression (lognot ?operand/expression)
  (let ((type (get-type ?operand)))
    (cond ((or (bignum-type-p type) (big-integer-type-p type))
	   `(not ,?operand))
	  (t
	   (fail)))))

(defun combine-maybe-convert-args (name args type)
  (reduce #'(lambda (arg1 arg2)
	      `(key-send ,arg1
			 ,name
			 (maybe-convert-to-type ,arg2 ,type)))
	  (rest args)
	  :initial-value `(maybe-convert-to-type ,(first args) ,type)))


(def-linj-macro n-ary-operator-expression
    ((?is ?name (lambda (oper) (member oper '(+ - * /)))) . ?arguments/argument-list)
  (let ((args (argument-list-elements ?arguments)))
    (let ((types (remove-if #'cyclic-type-p (mapcar #'get-principal-type args))))
      (cond ((and (not (endp args)) (endp (rest args))) ;;unary case
	     (if (endp types)
	       (error "BUM")
	       (if (eq ?name '-)
		 (let ((type (first types)) (operand (first args)))
		   (cond ((or (bignum-type-p type) (big-integer-type-p type) (big-decimal-type-p type))
			  `(negate ,operand))
			 (t
			  (or (wrapping-conversion '- 'negate (list type) (list operand))
			      (fail)))))
		 (fail))))
	    ((and (member-if #'double-type-p types)
		  (member-if (complement #'(lambda (type)
					     (super-type-p (double-type) type)))
			     types))
	     `(,?name ,@(mapcar #'(lambda (arg) (convert-to-type arg (double-type))) args)))
	    ((and (member-if #'float-type-p types)
		  (member-if (complement #'(lambda (type)
					     (super-type-p (float-type) type)))
			     types))
	     `(,?name ,@(mapcar #'(lambda (arg) (convert-to-type arg (float-type))) args)))
	    ((find-if #'bignum-type-p types)
	     (let ((name (case ?name ((+) 'add) ((-) 'subtract) ((*) 'multiply) ((/) 'divide))))
	       (combine-maybe-convert-args name args (bignum-type))))
	    ((find-if #'big-decimal-type-p types)
	     (let ((type (big-decimal-type)))
	       (if (eq ?name '/)
		 (reduce #'(lambda (arg1 arg2)
			     `(key-send ,arg1
					divide
					(maybe-convert-to-type ,arg2 ,type)
					(in (the java.math.big-decimal) +round-half-up+)))
			 (rest args)
			 :initial-value `(maybe-convert-to-type ,(first args) ,type))
		 (let ((name (ecase ?name ((+) 'add) ((-) 'subtract) ((*) 'multiply))))
		   (combine-maybe-convert-args name args type)))))
	    ((find-if #'big-integer-type-p types)
	     (let ((name (ecase ?name ((+) 'add) ((-) 'subtract) ((*) 'multiply))))
	       (combine-maybe-convert-args name args (big-integer-type))))
	    (t
	     (or (wrapping-conversion ?name ?name types args)
		 (fail)))))))

(def-linj-macro binary-operator-expression (ash ?arg1/expression ?arg2/expression)
  (let ((type1 (get-type ?arg1)))
    (if (or (bignum-type-p type1)
	    (big-integer-type-p type1))
	(if (and (literal-p ?arg2) (< (literal-value ?arg2) 0))
	    `(shift-right ,?arg1 ,(- (literal-value ?arg2)))
	    `(shift-left ,?arg1 (maybe-convert-to-type ,?arg2 ,(int-type))))
	(if (not (literal-p ?arg2))
	    `(in (the linj.util)
	      (shift ,?arg1 ,?arg2))
	    (fail)))))


(def-syntax maybe-convert-expression (expression)
  (maybe-convert-to-type ?expression/expression ?type/type-reference))

(defmethod get-type ((e maybe-convert-expression))
  (maybe-convert-expression-type e))

(def-unparse maybe-convert-expression (e)
  (let ((expr (maybe-convert-expression-expression e))
	(type (maybe-convert-expression-type e)))
    (if (equal-type-p (get-type expr) type)
      (unparse-object expr
		      *standard-output*)
      (unparse-object (with-parent ((ast-node-parent e))
			(convert-to-type expr type))
		      *standard-output*))))

(defmethod visit :before ((cast maybe-convert-expression) (collect collect-type-info))
  (when (and (reference-p (maybe-convert-expression-expression cast))
	     (eq (find-declaration (maybe-convert-expression-expression cast))
		 (collect-type-info-declaration collect)))
    (push (maybe-convert-expression-type cast)
	  (collect-type-info-found collect))))

(def-transform anonymous-class-allocation-expression
    (lambda ?parameters . ?body)
  (lambda-method (defmethod funcall ?parameters . ?body)))

(def-linj-macro anonymous-class-allocation-expression 
    (lambda-method ?method/method-declaration)
  (let ((parameters (parameter-list-elements (method-declaration-parameters ?method)))
	(body (ast-node-form (method-declaration-body ?method))))
    (let ((new-names (mapcar #'(lambda (parameter)
				 (if (object-type-p (get-type parameter));;don't replace it
				   nil
				   (parse (conc-symbol 'generic- (variable-declaration-name parameter)) 'linj-name)))
			     parameters)))
      (let ((casts (mapcan #'(lambda (parameter new-name)
			       (if new-name
				 `((,(variable-declaration-name parameter)
				    (the ,(get-type parameter) ,new-name)))
				 ()))
			   parameters new-names)))
	(let ((type (get-type ?method)))
	  ;;restrict to acceptable types
	  (let ((type (if (or (boolean-type-p type)
			      (void-type-p type))
			type
			(object-type))))
	    `(new (class
		   ,(compute-class-name (length parameters) type)
		   (defmethod funcall
		       ,(mapcar #'(lambda (name parameter)
				    `(,(or name (variable-declaration-name parameter))
				      (the ,(object-type))))
				new-names parameters)
		       ;;We use a returns to stop type-inference
		       :returns ,type
		       ,@(if casts
			   `((let ,casts ,@body))
			   body))))))))))

(defun compute-class-name (args type)
  (cond ((= args 0)
	 (cond ((boolean-type-p type)
		'<predicate-0>)
	       ((void-type-p type)
		'<procedure-0>)
	       ((object-type-p type)
		'<function-0>)
	       (t
		(error "Bronca"))))
	((= args 1)
	 (cond ((boolean-type-p type)
		'<predicate>)
	       ((void-type-p type)
		'<procedure>)
	       ((object-type-p type)
		'<function>)
	       (t
		(error "Bronca"))))
	((= args 2)
	 (cond ((boolean-type-p type)
		'<predicate-2>)
	       ((void-type-p type)
		'<procedure-2>)
	       ((object-type-p type)
		'<function-2>)
	       (t
		(error "Dupla Bronca"))))
	(t
	 (error "Tripla bronca"))))


;;;array operations

(def-linj-macro array-reference (aref ?expression/expression . ?indexes/argument-list)
  (let ((expr-type (get-type ?expression)))
    (if (and (endp (rest (argument-list-elements ?indexes)))
	     (or (string-buffer-type-p expr-type) (string-type-p expr-type)))
      `(char-at ,?expression ,@(argument-list-elements ?indexes))
      (fail))))

(def-linj-macro setf-expression (setf (aref ?expression/expression . ?indexes/argument-list) ?r-value/expression)
  (if (and (endp (rest (argument-list-elements ?indexes)))
	   (string-buffer-type-p (get-type ?expression)))
    `(set-char-at ,?expression ,@(argument-list-elements ?indexes) ,?r-value)
    (fail)))

;;HACK!! This one is buggy!!!!!
;; (def-linj-macro expression
;;     (find ?expression/expression ?sequence/expression . ?options)
;;   (let ((seq-type (get-type ?sequence)))
;;     (cond ((string-type-p seq-type)
;; 	   (assert (char-type-p (get-type ?expression)))
;; 	   (let ((from-end (getf ?options :from-end))
;; 		 (other-options (remf ?options :from-end)))
;; 	     (assert (endp other-options))
;; 	     (if from-end
;; 	       `(last-index-of ,?sequence ,?expression)
;; 	       `(index-of ,?sequence ,?expression))))
;; 	  (t
;; 	   `(find ,?expression ,?sequence . ,?options)))))

(def-linj-macro generic-call-expression (abs ?x/expression)
  (if (primitive-type-reference-p (get-principal-type ?x))
    `(send (the java.lang.Math) abs ,?x)
    ;;;HACK: Important discussion:
    ;;similarly to what happens above, we would like to write the case as
    ;;`(abs ,?x)  Unfortunately, any syntax-defined form (as is the def-linj-macro) 'reserves' the
    ;;operator so that the form can't be parsed as a generic method call expression.  Until now, this wasn't a
    ;;problem bc we were only introducing linj-macros for forms that were also syntax-defined.
    `(send ,?x abs)))

(defun assert-side-effect-free (expression operator)
  (unless (or (literal-p expression)
	      (reference-p expression))
    (linj-error "The operator ~A needs to evaluate ~A multiple times but I can't be sure this is possible"
		operator expression)))

(def-linj-macro generic-call-expression (eql ?x/expression ?y/expression)
  (let ((type-x (get-principal-type ?x))
	(type-y (get-principal-type ?y)))
    (cond ((or (primitive-type-reference-p type-x)
	       (super-type-p (number-wrapper-type) type-x)
	       (primitive-type-reference-p type-y)
	       (super-type-p (number-wrapper-type) type-y))
	   `(= ,?x ,?y))
	  ((or (not (super-type-p type-x (number-wrapper-type)))
	       (not (super-type-p type-y (number-wrapper-type))))
	   `(eq ,?x ,?y))
	  (t
	   (assert-side-effect-free ?x "eql")
	   (assert-side-effect-free ?y "eql")
	   `(or (eq ,?x ,?y) (and (numberp ,?x) (numberp ,?y) (equals ,?x ,?y)))))))

(def-linj-macro expression (length ?x/expression)
  (if (array-type-reference-p (get-type ?x))
    `(vector-length ,?x)
    (fail)))

(def-linj-macro generic-call-expression
    ((?is ?name (lambda (oper) (member oper '(max min)))) . ?arguments/argument-list)
  (let ((args (argument-list-elements ?arguments)))
    (let ((types (mapcar #'get-type args)))
      (cond ((find-if #'bignum-type-p types)
	     (combine-maybe-convert-args ?name args (bignum-type)))
	    ((find-if #'big-decimal-type-p types)
	     (combine-maybe-convert-args ?name args (big-decimal-type)))
	    ((find-if #'big-integer-type-p types)
	     (combine-maybe-convert-args ?name args (big-integer-type)))
	    ((find-if #'double-wrapper-type-p types)
	     (convert-wrapper ?name args (double-type) (double-wrapper-type)))
	    ((find-if #'float-wrapper-type-p types)
	     (convert-wrapper ?name args (float-type) (float-wrapper-type)))
	    ((find-if #'long-wrapper-type-p types)
	     (convert-wrapper ?name args (long-type) (long-wrapper-type)))
	    ((find-if #'int-wrapper-type-p types)
	     (convert-wrapper ?name args (int-type) (int-wrapper-type)))
	    ((find-if #'object-type-p types)
	     (convert-wrapper ?name args (bignum-type) (bignum-type)))
	    (t
	     (labels ((distribute (args)
				  `(key-send (the java.lang.math)
					     ,?name
					     ,(first args)
					     ,(if (endp (rest (rest args)))
						(second args)
						(distribute (rest args))))))
	       (distribute args)))))))


;;Extra unary operators: must distinguish types shorter than int
(def-linj-macro expression ((?is ?name (lambda (oper) (member oper '(1+ 1-)))) ?argument/expression)
  (let ((type (get-type ?argument))
	(new-oper (ecase ?name (1+ '+) (1- '-))))
    (let ((new-expr `(,new-oper ,?argument 1)))
      (cond ((byte-type-p type)
	     `(the byte ,new-expr))
	    ((short-type-p type)
	     `(the short ,new-expr))
	    (t
	     new-expr)))))

(def-linj-macro incf-decf-expression
  ((?is ?oper (lambda (oper) (member oper '(incf decf)))) ?place/expression ?delta/expression)
  (let ((type (get-type ?place)))
    (if (primitive-type-reference-p type)
      (fail)
      `(setf ,?place (,(if (eq ?oper 'incf) '+ '-) ,?place ,?delta)))))


(defun make-values-method-call (arg1 args)
  (with-parent ((ast-node-parent args))
	       (make-instance 'method-call-expression
			      :name (parse 'values 'linj-name)
			      :receiver arg1
			      :arguments (make-instance 'argument-list :elements (argument-list-elements args))
			      :original-form '???)))

(def-linj-macro expression (values ?arg1/expression . ?args/argument-list)
  (if (and (not (primitive-type-reference-p (get-principal-type ?arg1)))
	   (find-declaration (make-values-method-call ?arg1 ?args)))
    `(send ,?arg1 values . ,?args)
    (fail)))


;;HACK: There's a bug here where repeated variables are not properly renamed.
;;Consider
;;(for-each (a b)
;;  (for-each (c a)
;;    ...))

;;(def-linj-macro statement (for-each (?var ?form/expression) . ?body/statement-list)
;;We delay parsing the body because it tries to do things that can't be done before the macro expansion.
(def-linj-macro statement (for-each (?var ?form/expression) . ?body)
  (let ((form-type (get-type ?form)))
    (cond ((cons-type-p form-type)
	   `(dolist (,?var ,?form) . ,?body))
	  ((array-type-reference-p form-type)
	   `(dovector (,?var ,?form) . ,?body))
	  ((super-type-p (iterator-type) form-type)
	   `(let ((iter ,?form))
	      (while (has-next iter)
		(let ((,?var (next iter))) . ,?body))))
	  ((super-type-p (enumeration-type) form-type)
	   `(let ((enum ,?form))
	      (while (has-more-elements enum)
		(let ((,?var (next-element enum))) . ,?body))))
	  (t
	   (error "Unknown type for iteration ~A" form-type)))))


(def-linj-macro (expression :phase :pos-syntax) ?form/statement
  (let ((free-vars (free-references ?form)))
    (let ((unique-vars (reverse (remove-duplicates free-vars :test #'eq :key #'reference-name))))
      (let ((params (mapcar #'reference-name unique-vars))
	    (declares (mapcar #'(lambda (var)
				  `(declare (type ,(copy-type (get-type var)) ,(reference-name var))
					    (final ,(reference-name var))))
			      unique-vars)))
	;;They will get new local declarations => invalidate the cached declaration
	(dolist (ref free-vars)
	  (setf (cached-declaration ref) nil))
	`(funcall (new (class object (defmethod funcall ,params ,@declares ,?form)))
  		  ,@params)))))