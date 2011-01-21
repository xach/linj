;;; -*- Mode: Common-Lisp; Syntax: Common-Lisp; Package: LINJ; Base: 10 -*-

;;; Copyright (C) Antonio Menezes Leitao Created on Mon May  3 18:53:07 2004

;;Adapted from the public domain CMU LOOP: CMU Implementation of ANSI Loop Macro

(in-package "LINJ")

;;This file requires the Linj readtable
(eval-when (:compile-toplevel :load-toplevel)
  (setq *readtable* *linj-readtable*))


;;;; Specials used during the parse.

;;; These specials hold the different parts of the result as we are generating
;;; them.
;;; 
(defvar *loop-name*
  "The name used in the 'named' clause")

(defvar *outside-bindings*
  "Bindings established before the prologue and before the loop")

(defvar *prologue*
  "Forms related to the 'initially' clause")

(defvar *inside-bindings*
  "Bindings established in the loop body")

(defvar *body-forms*
  "Forms that go inside the loop but before the iteration forms")

(defvar *iteration-forms*
  "Forms that go inside the loop but after the iteration forms")

(defvar *epilogue*
  "Forms related to the 'finally' clause")

(defvar *result-var*
  "Variable used for the result of accumulation clauses")

(defvar *return-value*
  "The value to return in the 'finally' clause")

(defvar *default-return-value*
  "The value to return as the result of some clauses ('always', 'never', etc)")

(defvar *accumulation-variables*
  "The variables used for the iterations of accumulation clauses")

(defvar *it-referenced*
  "Was the loop keyword 'it' used?")

;;; This special holds the remaining stuff we need to parse.
;;; 
(defvar *remaining-stuff*)

;;; This special holds a value that is EQ only to itself.
;;; 
(defvar *magic-cookie* (list 'magic-cookie))


;;;; Utility functions/macros used by the parser.

(defun maybe-car (thing)
  (if (consp thing) (car thing) thing))

(defun maybe-cdr (thing)
  (if (consp thing) (cdr thing) thing))

(defmacro push-end (form var)
  `(setf ,var (nconc ,var (list ,form))))

(defun loop-keyword-p (thing &rest more-keywords)
  (when (endp more-keywords)
    (setf more-keywords '(named
			  for as and from downfrom upfrom to downto upto below
			  above by in on = then across being each the hash-key
			  hash-keys hash-value hash-values of using symbol
			  present-symbol internal-symbol external-symbol symbols
			  present-symbols internal-symbols external-symbols
			  repeat
			  while until always never thereis
			  collect collecting append appending nconc nconcing
			  count counting sum summing maximize maximizing
			  minimize minimizing into
			  with
			  if when unless else end it
			  do doing return
			  of-type
			  initially finally
			  ;;extra ones, for Linj
			  tokenizing
			  iterating
			  enumerating)))
  (and (symbolp thing)
       (member (string thing) more-keywords :test #'string-equal :key #'string)))

(defun preposition-p (prep)
  (when (loop-keyword-p (car *remaining-stuff*) prep)
    (pop *remaining-stuff*)
    t))

(defun splice-in-subform (form subform)
  (if (eq form *magic-cookie*)
      subform
      (labels ((sub-splice-in-subform (form path)
		 (cond ((atom form)
			nil)
		       ((member form path)
			nil)
		       ((eq (car form) *magic-cookie*)
			(setf (car form) subform)
			t)
		       (t
			(let ((new-path (cons form path)))
			  (or (sub-splice-in-subform (car form) new-path)
			      (sub-splice-in-subform (cdr form) new-path)))))))
	(if (sub-splice-in-subform form nil)
	    form
	    (error "Couldn't find the magic cookie in:~% ~S~%Loop is broken."
		   form)))))

(defmacro queue-var (where name type &key (initer nil initer-p) (stepper nil stepper-p))
  `(push (list ,name ,type ,initer-p ,initer ,stepper-p ,stepper)
	 ,where))

(defun pick-default-value (var type)
  (if (null var)
    (list)
    (if (consp var)
      (cons (pick-default-value (car var) (maybe-car type))
	    (pick-default-value (cdr var) (maybe-cdr type)))
      (get-default-initializer (parse type 'type-reference)))))

(defun only-simple-types (type-spec)
  (if (atom type-spec)
    (and (symbolp type-spec) ;;linj accepts many types  (member type-spec '(fixnum float t nil))
	 (not (loop-keyword-p type-spec)))
    (and (only-simple-types (car type-spec))
	 (only-simple-types (cdr type-spec)))))

(defun build-let-expression (vars)
  (if (null vars)
      (values *magic-cookie* *magic-cookie*)
      (let ((inside nil)
	    (outside nil)
	    (steppers nil)
	    (sub-lets nil))
	(labels
	    ((process (name type initial-p initial stepper-p stepper)
;; 		      (format *trace-output* "name ~A type ~A initial-p ~A initial ~A stepper-p ~A stepper ~A~%"
;; 			      name type initial-p initial stepper-p stepper)
	       (setq initial (simplify-quote initial)
		     stepper (simplify-quote stepper))
	       (cond ((atom name)
		      (cond ((not stepper-p)
			     (push (list type name initial) outside))
			    ((not initial-p)
			     (push (list type name stepper) inside))
			    (t
			     (push (list type name initial) outside)
			     (setf steppers ;;AML swapped nconc order because it's only used with a psetq (and I don't care about side effects)
				   (nconc (list name stepper) steppers)))))
		     ((and (car name) (cdr name))
		      (let ((temp
			     (if (and (consp initial)) ;;HACK Remove nil
			       (let ((temp (gen-new-name 'temp *remaining-stuff*)))
				 (process temp 'cons initial-p initial stepper-p stepper)
				 temp)
			       initial)))
			(push (if stepper-p
				(list (car name) (maybe-car type) nil nil t `(car ,temp))
				(list (car name) (maybe-car type) t `(car ,temp) nil nil))
			      sub-lets)
			(push (if stepper-p
				(list (cdr name) (maybe-cdr type) nil nil t `(cdr ,temp))
				(list (cdr name) (maybe-cdr type) t `(cdr ,temp) nil nil))
			      sub-lets)))
		     ((car name)
		      (process (car name)
			       (maybe-car type)
			       initial-p `(car ,initial)
			       stepper-p `(car ,stepper)))
		     ((cdr name)
		      (process (cdr name)
			       (maybe-cdr type)
			       initial-p `(cdr ,initial)
			       stepper-p `(cdr ,stepper))))))
	  (dolist (var vars)	      
	    (process (first var) (second var) (third var)
		     (fourth var) (fifth var) (sixth var))))
	(when steppers
	  (push-end (cons 'psetq steppers)
		    *iteration-forms*))
	(multiple-value-bind (sub-outside sub-inside)
	    (build-let-expression sub-lets)
	  (values (build-bindings outside sub-outside)
		  (build-bindings inside sub-inside))))))

(defun build-bindings (vars guts)
  (if (null vars)
      guts
      (let ((declares (mapcar #'build-declare (remove t vars :key #'car))))
	`(let ,(mapcar #'cdr vars)
	   ,@(when declares `((declare ,@declares)))
	   ,guts))))

(defun build-declare (var)
  `(type ,(if (consp (car var)) 'cons (car var)) ,(cadr var)))



;;;; LOOP itself.
(def-macro-transform for-statement (loop . ?body)
  (if (some #'atom ?body)
    (parse-loop ?body)
    `(block nil (for (() t ()) . ,?body))))

;;We also need to delay some processing to the semantic-macro-phase

(def-linj-macro statement (when-return-it ?test/expression ?var ?name)
  (cond ((boolean-type-p (get-type ?test))
	 `(when ,?test
	    (return-from ,?name t)))
	((or (reference-p ?test) (literal-p ?test))
	 `(when ,?test
	    (return-from ,?name ,?test)))
	(t
	 `(let ((,?var ?test))
	    (when ,?var
	      (return-from ,?name ,?var))))))

;;;; The parser.

;;; Top level parser.  Bind the specials, and call the other parsers.

(defparameter *generate-improved-loop* t)

(defun parse-loop (stuff)
  (let* ((*prologue* nil)
	 (*outside-bindings* *magic-cookie*)
	 (*inside-bindings* *magic-cookie*)
	 (*body-forms* nil)
	 (*iteration-forms* nil)
	 (*epilogue* nil)
	 (*result-var* nil)
	 (*return-value* nil)
	 (*default-return-value* nil)
	 (*accumulation-variables* nil)
	 (*remaining-stuff* stuff)
	 (*loop-name* (parse-named)))
    (loop
      (when (null *remaining-stuff*)
	(return))
      (let ((clause (pop *remaining-stuff*)))
	(cond ((not (symbolp clause))
	       (error "Invalid clause, ~S, must be a symbol." clause))
	      ((loop-keyword-p clause 'initially)
	       (setf *prologue* (nconc *prologue* (parse-expr-list))))
	      ((loop-keyword-p clause 'finally)
	       (parse-finally))
	      ((loop-keyword-p clause 'return)
	       (let ((expr (maybe-parse-it (pop *remaining-stuff*))))
		 (push-end `(return-from ,*loop-name* ,expr)
			   *iteration-forms*)))
	      ((loop-keyword-p clause 'with)
	       (parse-with))
	      ((loop-keyword-p clause 'for 'as)
	       (parse-for-as))
	      ((loop-keyword-p clause 'repeat)
	       (parse-repeat))
	      ((loop-keyword-p clause 'while)
	       (let ((conditional (pop *remaining-stuff*)))
		 (unless (eq conditional 't)
		   (push-end `(unless ,conditional (loop-finish))
			     *body-forms*))))
	      ((loop-keyword-p clause 'until)
	       (push-end `(when ,(pop *remaining-stuff*) (loop-finish))
			 *body-forms*)
;; 	       (push `(when ,(pop *remaining-stuff*) (loop-finish))
;;  		     *iteration-forms*)
	       )
	      ((loop-keyword-p clause 'always)
	       (push-end `(unless ,(pop *remaining-stuff*)
			    (return-from ,*loop-name* nil))
			 *body-forms*)
	       (setf *default-return-value* '(t)))
	      ((loop-keyword-p clause 'never)
	       (push-end `(when ,(pop *remaining-stuff*)
			    (return-from ,*loop-name* nil))
			 *body-forms*)
	       (setf *default-return-value* '(t)))
	      ((loop-keyword-p clause 'thereis)
	       (push-end (let ((temp (gen-new-name 'thereis *remaining-stuff*)))
			   `(when-return-it ,(pop *remaining-stuff*)
					    ,temp
					    ,*loop-name*))
			 *body-forms*)
	       (setf *default-return-value* '(nil)))
	      (t
	       (push clause *remaining-stuff*)
	       (return)))))
    (loop
      (when (null *remaining-stuff*)
	(return))
      (let ((clause (pop *remaining-stuff*)))
	(cond ((not (symbolp clause))
	       (error "Invalid clause, ~S, must be a symbol." clause))
	      ((loop-keyword-p clause 'initially)
	       (setf *prologue* (nconc *prologue* (parse-expr-list))))
	      ((loop-keyword-p clause 'finally)
	       (parse-finally))
	      ((loop-keyword-p clause 'while)
	       (push-end `(unless ,(pop *remaining-stuff*)
			    (loop-finish))
			 *body-forms*))
	      ((loop-keyword-p clause 'until)
	       (push-end `(when ,(pop *remaining-stuff*) (loop-finish))
			 *body-forms*))
	      ((loop-keyword-p clause 'always)
	       (push-end `(unless ,(pop *remaining-stuff*)
			    (return-from ,*loop-name* nil))
			 *body-forms*)
	       (setf *default-return-value* '(t)))
	      ((loop-keyword-p clause 'never)
	       (push-end `(when ,(pop *remaining-stuff*)
			    (return-from ,*loop-name* nil))
			 *body-forms*)
	       (setf *default-return-value* '(t)))
	      ((loop-keyword-p clause 'thereis)
	       (push-end (let ((temp (gen-new-name 'thereis *remaining-stuff*)))
			   `(when-return-it ,(pop *remaining-stuff*)
					    ,temp
					    ,*loop-name*))
			 *body-forms*)
	       (setf *default-return-value* '(nil)))
	      (t
	       (push clause *remaining-stuff*)
	       (or (maybe-parse-unconditional)
		   (maybe-parse-conditional)
		   (maybe-parse-accumulation) 
		   (error "Unknown clause, ~S" clause))))))
;;     (format *trace-output* "Outside: ~A~%Prologue: ~A~%Inside: ~A~%Body: ~A~%Iteration: ~A~%Epilogue: ~A~%Return: ~A~%"
;; 	    *outside-bindings* *prologue* *inside-bindings* *body-forms* *iteration-forms* *epilogue* (or *return-value* *default-return-value* *result-var*))
    (if *generate-improved-loop*
      (let ((iteration-forms *iteration-forms*))
	(let ((last-iteration (first (last iteration-forms))))
	  (let ((for-update
		 (let-pattern (((psetq . ?var-vals) last-iteration))
			      (let ((expansion (expand-psetq-psetf ?var-vals)))	;;can we do it without extra lets?
				(let-pattern (((setf . ?var-vals) expansion))
					     (loop for (var val) on ?var-vals by #'cddr collect `(setf ,var ,val)))))))
	    (let ((iteration-forms (if for-update (butlast iteration-forms) iteration-forms)))
	      (multiple-value-bind (for-init outside-bindings)
		  (loop for i upfrom 0
			do (destructuring-bind (for-init outside-bindings)
			       (inner-binding *outside-bindings* i)
			     (if (endp for-init) 
				 (return (values (list) *outside-bindings*))
				 (unless (expression-references-p `(or ,*prologue* ,*epilogue* ,*return-value* ,*default-return-value* ,*result-var*)
								  (first (first for-init)))
				   (return (values for-init outside-bindings))))))
		(let ((inside-bindings (compact-if-or *inside-bindings*)))
		  (let ((test
			 (or (let-pattern (((if ?test (loop-finish) ?ignore) inside-bindings))
					  `(not ,?test))
			     't)))
		    (let ((inside-bindings (if (eq test 't) inside-bindings (fourth inside-bindings))))
		      `(block ,*loop-name*
			 ,@(simplify-let
			    (splice-in-subform
			     outside-bindings
			     `(progn
				,@*prologue*
			       (let-macro-transform ((statement (loop-finish) `(break ,',*loop-name*)))
				(for (,for-init ,test ,for-update)
				 ,@(simplify-progn
				    (splice-in-subform
				     inside-bindings
				     `(progn
				       ,@*body-forms*
				       ,@iteration-forms)))))
				,@*epilogue*
				,@(unless (or (member 'return *epilogue* :key #'car)
					      (member 'return-from *epilogue* :key #'car)
					      (and (eq test 't)
						   (not (expression-references-p
							 `(or ,@*body-forms* ,@iteration-forms)
							 'loop-finish))))
				    (let ((return (or *return-value* *default-return-value* *result-var*)))
				      (when return
					return
					;;We don't need this return-from because it's the last statement of the block
					;; `((return-from ,*loop-name* ,@return))
					)))))))))))))))
      `(block ,*loop-name*
	 ,(splice-in-subform
	   *outside-bindings*
	   `(let-macro-transform ((statement (loop-finish) `(break)))
	     ,@*prologue*
	     (while t
	       ,(splice-in-subform
		 *inside-bindings*
		 `(progn
		   ,@*body-forms*
		   ,@(nreverse *iteration-forms*))))
	     ,@*epilogue*
	     ,@(let ((return (or *return-value* *default-return-value* *result-var*)))
		    (when return
		      `((return-from ,*loop-name* ,return))))))))))

(defvar *accept-it* nil
  "Used to enforce the ANSI rule that the it variable can only be used in the first clause after the conditional")
  
(defun maybe-parse-it (expr)
  (when (and *accept-it* (loop-keyword-p expr 'it))
    (setf expr (gen-new-name 'it *remaining-stuff*))
    (setf *it-referenced* expr))
  expr)

(defun simplify-progn (form)
  (or (let-pattern (((progn . ?forms) form))
	 ?forms)
      (list form)))

(defun simplify-let (form)
  (or (let-pattern (((let () . ?forms) form))
	 ?forms)
      (let-pattern (((progn ?form) form))
	(list ?form))
      (list form)))

(defun compact-if-or (form)
  (or (let-pattern-special
       (((if (or . ?tests1) ?conseq (if (or . ?tests2) ?conseq ?altern)) form))
       (compact-if-or `(if (or ,@?tests1 ,@?tests2) ,?conseq ,?altern)))
      form))

(defun inner-binding (form n &optional (inner-p nil))
  (or (let-pattern (((let ?binds1 ?subform1) form))
		   (let-pattern (((let ?ignore . ?ignore) ?subform1))
				(destructuring-bind (&optional binding inner)
				    (inner-binding ?subform1 n t)
				  (list binding `(let ,?binds1 ,inner)))))
      (let-pattern (((let ?binds1 (declare (type . ?rest-decl)) ?subform1) form))
		   (let-pattern (((let ?ignore . ?ignore) ?subform1))
				(destructuring-bind (&optional binding inner)
				    (inner-binding ?subform1 n t)
				  (list binding `(let ,?binds1
						   (declare (type . ,?rest-decl))
						   ,inner)))))
      (let-pattern (((let ?binds ?form) form))
		   (and (or inner-p
			    (not (let-pattern (((let ?ignore ?ignore) ?form)) t)))
			(< n (length ?binds))
			(let ((bind (nth n ?binds)))
			  (list (list bind) 
				(let ((remaining-binds (remove bind ?binds)))
				  (if (endp remaining-binds)
				      ?form
				      `(let ,remaining-binds ,?form)))))))
      (list (list) form)))

(defun parse-named ()
  (when (loop-keyword-p (car *remaining-stuff*) 'named)
    (pop *remaining-stuff*)
    (if (symbolp (car *remaining-stuff*))
	(pop *remaining-stuff*)
	(error "Loop name ~S is not a symbol." (car *remaining-stuff*)))))


(defun parse-expr-list ()
  (let ((results nil))
    (loop
      (when (or (endp *remaining-stuff*) (loop-keyword-p (car *remaining-stuff*)))
	(return (nreverse (delete-if #'atom results)))) ;;to remove constants (doesn't consider symbol-macrolet)
      (push (pop *remaining-stuff*) results))))

(defun parse-finally ()
  (let ((sub-clause (pop *remaining-stuff*)))
    (if (loop-keyword-p sub-clause 'return)
	(cond (*return-value*
	       (error "Cannot specify two FINALLY RETURN clauses."))
	      ((null *remaining-stuff*)
	       (error "FINALLY RETURN must be followed with an expression."))
	      (t
	       (setf *return-value* (list (maybe-parse-it (pop *remaining-stuff*))))))
	(progn
	  (unless (loop-keyword-p sub-clause 'do 'doing)
	    (push sub-clause *remaining-stuff*))
	  (setf *epilogue* (nconc *epilogue* (parse-expr-list)))))))

(defvar *magic-quote* 'quote);;(list 'magic-quote))

(defun simplify-quote (expr) expr
  (or (let-pattern (((car ?subexpr) expr))
        (if (and (consp ?subexpr) (eq (first ?subexpr) *magic-quote*))
	  (if (consp (car (second ?subexpr)))
	    `(,*magic-quote* ,(car (second ?subexpr)))
	    (car (second ?subexpr)))
	  expr))
      (let-pattern (((cdr ?subexpr) expr))
	(if (and (consp ?subexpr) (eq (first ?subexpr) *magic-quote*))
	  (if (consp (cdr (second ?subexpr)))
	    `(,*magic-quote* ,(cdr (second ?subexpr)))
	    (cdr (second ?subexpr)))
	  expr))
      expr))

(defun parse-with ()
  (let ((vars nil))
    (loop
      (multiple-value-bind (var type) (parse-var-and-type-spec)
	(let ((initial
	       (if (loop-keyword-p (car *remaining-stuff*) '=)
		   (progn
		     (pop *remaining-stuff*)
		     (pop *remaining-stuff*))
		   (if (consp var)
		     `(,*magic-quote* ,(pick-default-value var type))
		     (pick-default-value var type)))))
	  (queue-var vars var type :initer initial)))
      (if (loop-keyword-p (car *remaining-stuff*) 'and)
	  (pop *remaining-stuff*)
	  (return)))
    (multiple-value-bind (outside inside)
	(build-let-expression vars)
      (setf *outside-bindings*
	    (splice-in-subform *outside-bindings* outside))
      (setf *inside-bindings*
	    (splice-in-subform *inside-bindings* inside)))))

(defun parse-var-and-type-spec ()
  (values (pop *remaining-stuff*)
	  (parse-type-spec t)))

(defun parse-type-spec (default)
  (cond ((preposition-p 'of-type)
	 (translate-cl-type (pop *remaining-stuff*)))
	((and *remaining-stuff*
	      (only-simple-types (car *remaining-stuff*)))
	 (translate-cl-type (pop *remaining-stuff*)))
	(t
	 default)))



;;;; FOR/AS stuff.

;;; These specials hold the vars that need to be bound for this FOR/AS clause
;;; and all of the FOR/AS clauses connected with AND.  All the *for-as-vars*
;;; are bound in parallel followed by the *for-as-sub-vars*.
;;; 
(defvar *for-as-vars*)
(defvar *for-as-sub-vars*)

;;; These specials hold any extra termination tests.  *for-as-term-tests* are
;;; processed after the *for-as-vars* are bound, but before the
;;; *for-as-sub-vars*.  *for-as-sub-term-tests* are processed after the
;;; *for-as-sub-vars*.

(defvar *for-as-term-tests*)
(defvar *for-as-sub-term-tests*)
(defvar *for-as-iter-vars*)


(defun parse-for-as ()
  (let ((*for-as-vars* nil)
	(*for-as-term-tests* nil)
	(*for-as-sub-vars* nil)
	(*for-as-sub-term-tests* nil)
	(*for-as-iter-vars* nil))
    (loop
      (multiple-value-bind (name type) (parse-var-and-type-spec)
	(let ((sub-clause (pop *remaining-stuff*)))
	  (cond ((loop-keyword-p sub-clause 'from 'downfrom 'upfrom 'to 'downto 'upto 'below 'above 'by)
		 (parse-arithmetic-for-as sub-clause name type))
		((loop-keyword-p sub-clause 'in)
		 (parse-in-for-as name type))
		((loop-keyword-p sub-clause 'on)
		 (parse-on-for-as name type))
		((loop-keyword-p sub-clause '=)
		 (parse-equals-for-as name type))
		((loop-keyword-p sub-clause 'across)
		 (parse-across-for-as name type))
		((loop-keyword-p sub-clause 'being)
		 (parse-being-for-as name type))
		((loop-keyword-p sub-clause 'tokenizing)
		 (parse-token-for-as name type))
		((loop-keyword-p sub-clause 'enumerating)
		 (parse-token-for-as name type))
		((loop-keyword-p sub-clause 'iterating)
		 (parse-token-for-as name type))
		(t
		 (error "Invalid FOR/AS subclause: ~S" sub-clause)))))
      (if (loop-keyword-p (car *remaining-stuff*) 'and)
	  (pop *remaining-stuff*)
	  (return)))
;;     (format *trace-output* "For-as-vars:~A For-as-sub-vars:~A~%" *for-as-vars* *for-as-sub-vars*)
    (multiple-value-bind (outside inside)
	(build-let-expression *for-as-vars*)
      (multiple-value-bind (sub-outside sub-inside)
	  (build-let-expression *for-as-sub-vars*)
	(multiple-value-bind (iter-outside iter-inside)
	    (build-let-expression *for-as-iter-vars*)
;; 	(format *trace-output* "Outside:~A Inside:~A Sub-outside:~A Sub-inside:~A" outside inside sub-outside sub-inside)
	;; (setf *outside-bindings*
;; 	      (splice-in-subform *outside-bindings*
;; 				 (splice-in-subform outside sub-outside)))
	(setf *outside-bindings*
	      (splice-in-subform *outside-bindings*
				 (splice-in-subform outside sub-outside)))
	(let ((inside-body
	       (if *for-as-term-tests*
		   `(if (or ,@(nreverse *for-as-term-tests*))
			(loop-finish)
			,*magic-cookie*)
		   *magic-cookie*))
	      (sub-inside-body
	       (if *for-as-sub-term-tests*
		   `(if (or ,@(nreverse *for-as-sub-term-tests*))
			(loop-finish)
			,*magic-cookie*)
		   *magic-cookie*)))
	  (setf *inside-bindings*
		(splice-in-subform
		 *inside-bindings*
		 (splice-in-subform
		  inside
		  (splice-in-subform
		   inside-body
		   (splice-in-subform
		    (splice-in-subform
		     sub-inside
		     (splice-in-subform
		      sub-inside-body
		      iter-inside))
		    iter-outside)))))))))))

(defun parse-arithmetic-for-as (sub-clause name type)
  (unless (atom name)
    (error "Cannot destructure arithmetic FOR/AS variables: ~S" name))
  (let (start stop inc dir exclusive-p)
    (push sub-clause *remaining-stuff*)
    (let ((from nil) (to nil))
      (macrolet ((update (var val)
			 `(if ,var
			    (error "Can't mix ~A and ~A in ~S." ,var ,val name)
			    (setf ,var ,val))))
	(loop
	 (cond ((preposition-p 'from)
		(update from 'from)
		(setf start (pop *remaining-stuff*)))
	       ((preposition-p 'downfrom)
		(update from 'downfrom)
		(setf start (pop *remaining-stuff*)))
	       ((preposition-p 'upfrom)
		(update from 'upfrom)
		(setf start (pop *remaining-stuff*)))
	       ((preposition-p 'to)
		(update to 'to)
		(setf stop (pop *remaining-stuff*)))
	       ((preposition-p 'downto)
		(update to 'downto)
		(setf stop (pop *remaining-stuff*)))
	       ((preposition-p 'upto)
		(update to 'upto)
		(setf stop (pop *remaining-stuff*)))
	       ((preposition-p 'above)
		(update to 'above)
		(setf stop (pop *remaining-stuff*))
		(setf exclusive-p t))
	       ((preposition-p 'below)
		(update to 'below)
		(setf stop (pop *remaining-stuff*))
		(setf exclusive-p t))
	       ((preposition-p 'by)
		(setf inc (pop *remaining-stuff*)))
	       (t
		(return)))))
      (unless inc
	(setf inc '1))
      (cond ((eq from 'from))
	    ((eq from 'downfrom)
	     (setf dir :down))
	    ((eq from 'upfrom)
	     (setf dir :up)))
      (cond ((eq to 'to))
	    ((eq to 'downto)
	     (if (eq dir :up)
	       (error "Can't mix UPFROM and DOWNTO in ~S." name)
	       (setf dir :down)))
	    ((eq to 'upto)
	     (if (eq dir :down)
	       (error "Can't mix DOWNFROM and UPTO in ~S." name)
	       (setf dir :up)))
	    ((eq to 'above)
	     (if (eq dir :up)
	       (error "Can't mix UPFROM and ABOVE in ~S." name)
	       (setf dir :down)))
	    ((eq to 'below)
	     (if (eq dir :down)
	       (error "Can't mix DOWNFROM and BELOW in ~S." name)
	       (setf dir :up))))
      (when (and (eq dir :down) (null start))
	(error "No default starting value for decremental stepping."))
      (unless (numberp inc)
	(let ((temp (gen-new-name (conc-symbol 'step- name) *remaining-stuff*)))
	  (queue-var *for-as-sub-vars* temp type :initer inc)
	  (setq inc temp)))
      (queue-var *for-as-sub-vars* name type
		 :initer (or start 0)
		 :stepper `(,(if (eq dir :down) '- '+) ,name ,inc))
      (when stop
	(unless (or (numberp stop) (and (symbolp stop) (not (expression-references-p *remaining-stuff* stop))))
	  (let ((stop-var (gen-new-name (conc-symbol 'limit- name) *remaining-stuff*)))
	    (queue-var *for-as-sub-vars* stop-var type :initer stop)
	    (setq stop stop-var)))
	(push (list (if (eq dir :down)
		      (if exclusive-p '<= '<)
		      (if exclusive-p '>= '>))
		    name stop)
	      *for-as-sub-term-tests*)))))

(defun parse-in-for-as (name type)
  (let* ((temp (gen-new-name (conc-symbol 'list- (if (symbolp name) name 'elems)) *remaining-stuff*))
	 (initer (pop *remaining-stuff*))
	 (stepper (if (preposition-p 'by)
		    (build-funcall (pop *remaining-stuff*) temp)
		    `(rest ,temp))))
    (queue-var *for-as-vars* temp 'cons :initer initer :stepper stepper)
    (queue-var *for-as-iter-vars* name type :initer `(first ,temp))
    (push `(null ,temp) *for-as-sub-term-tests*)))

(defun parse-on-for-as (name type)
  (let* ((temp (if (atom name)
		 name
		 (gen-new-name 'list-elems *remaining-stuff*)))
	 (initer (pop *remaining-stuff*))
	 (stepper (if (preposition-p 'by)
		    (build-funcall (pop *remaining-stuff*) temp)
		    `(rest ,temp))))
    (cond ((atom name)
	   (queue-var *for-as-sub-vars* name type :initer initer :stepper stepper)
	   (push `(endp ,name) *for-as-sub-term-tests*))
	  (t
	   (queue-var *for-as-vars* temp type :initer initer :stepper stepper)
	   (queue-var *for-as-sub-vars* name type :initer temp :stepper temp)
	   (push `(endp ,temp) *for-as-term-tests*)))))

(defun build-funcall (func arg)
  (or (let-pattern ((#'?f func))
	(and (symbolp ?f)
	     `(,?f ,arg)))
      `(funcall ,func ,arg)))

(defun parse-equals-for-as (name type)
  (let ((initer (pop *remaining-stuff*)))
    (if (preposition-p 'then)
	(queue-var *for-as-sub-vars* name type
		   :initer initer :stepper (pop *remaining-stuff*))
	(queue-var *for-as-vars* name type :initer initer :stepper initer))))

(defun parse-across-for-as (name type)
  (let* ((expr (pop *remaining-stuff*))
	 (temp (if (atom expr) nil (gen-new-name (conc-symbol 'vector- name) *remaining-stuff*)))
	 (length (gen-new-name (conc-symbol 'length- name) *remaining-stuff*))
	 (index (gen-new-name (conc-symbol 'index- name) *remaining-stuff*)))
    (when temp
      (queue-var *for-as-vars* temp 't :initer expr))
    (queue-var *for-as-sub-vars* length 'int :initer `(length ,(or temp expr)))
    (queue-var *for-as-vars* index 'int :initer 0 :stepper `(1+ ,index))
    (queue-var *for-as-sub-vars* name type :stepper `(aref ,(or temp expr) ,index))
    (push `(>= ,index ,length) *for-as-term-tests*)))

(defun parse-token-for-as (name type)
  (let* ((expr (pop *remaining-stuff*))
	 (temp (if (atom expr) nil (gen-new-name (conc-symbol 'tokenizer- name) *remaining-stuff*))))
    (when temp
      (queue-var *for-as-vars* temp 't :initer expr))
    (queue-var *for-as-sub-vars* name type :stepper `(next-token ,(or temp expr)))
    (push `(not (has-more-tokens ,(or temp expr))) *for-as-term-tests*)))

(defun parse-enumerating-for-as (name type)
  (let* ((expr (pop *remaining-stuff*))
	 (temp (if (atom expr) nil (gen-new-name (conc-symbol 'enumerator- name) *remaining-stuff*))))
    (when temp
      (queue-var *for-as-vars* temp 't :initer expr))
    (queue-var *for-as-sub-vars* name type :stepper `(next-element ,(or temp expr)))
    (push `(not (has-more-elements ,(or temp expr))) *for-as-term-tests*)))

(defun parse-iterating-for-as (name type)
  (let* ((expr (pop *remaining-stuff*))
	 (temp (if (atom expr) nil (gen-new-name (conc-symbol 'iterator- name) *remaining-stuff*))))
    (when temp
      (queue-var *for-as-vars* temp 't :initer expr))
    (queue-var *for-as-sub-vars* name type :stepper `(next ,(or temp expr)))
    (push `(not (has-next ,(or temp expr))) *for-as-term-tests*)))

(defun parse-being-for-as (name type)
  (let ((clause (pop *remaining-stuff*)))
    (unless (loop-keyword-p clause 'each 'the)
      (error "BEING must be followed by either EACH or THE, not ~S"
	     clause)))
  (let ((clause (pop *remaining-stuff*)))
    (cond ((loop-keyword-p clause 'hash-key 'hash-keys
 			   'hash-value 'hash-values)
	   (let ((prep (pop *remaining-stuff*)))
	     (unless (loop-keyword-p prep 'in 'of)
	       (error "~A must be followed by either IN or OF, not ~S"
		      (symbol-name clause) prep)))
	   (let ((table (pop *remaining-stuff*))
		 (iterator (gensym (format nil "~A-ITERATOR-" name)))
		 (exists-temp (gensym (format nil "~A-EXISTS-TEMP-" name)))
		 (key-temp (gensym (format nil "~A-KEY-TEMP-" name)))
		 (value-temp (gensym (format nil "~A-VALUE-TEMP-" name))))
	     (setf *outside-bindings*
		   (splice-in-subform
		    *outside-bindings*
		    `(with-hash-table-iterator (,iterator ,table)
					       ,*magic-cookie*)))
	     (multiple-value-bind (using using-type)
		 (when (preposition-p 'using)
		   ;; ### This is wrong.
		   (parse-var-and-type-spec))
	       (multiple-value-bind (key-var key-type value-var value-type)
		   (if (loop-keyword-p clause 'hash-key 'hash-keys)
		       (values name type using using-type)
		       (values using using-type name type))
		 (setf *inside-bindings*
		       (splice-in-subform
			*inside-bindings*
			`(multiple-value-bind (,exists-temp ,key-temp ,value-temp)
			     (,iterator)
			   ,@(unless (and key-var value-var)
			       `((declare (ignore ,@(if (null key-var)
							(list key-temp))
						  ,@(if (null value-var)
							(list value-temp))))))
			   ,*magic-cookie*)))
		 (push `(not ,exists-temp) *for-as-term-tests*)
		 (when key-var
		   (queue-var *for-as-sub-vars* key-var key-type
			      :stepper key-temp))
		 (when value-var
		   (queue-var *for-as-sub-vars* value-var value-type
			      :stepper value-temp))))))
	  (t
	   (error
	    "Unknown sub-clause, ~A, for BEING.  Must be one of:~%  ~
	     HASH-KEY HASH-KEYS HASH-VALUE HASH-VALUES"
	    (symbol-name clause))))))



;;;;

(defun parse-repeat ()
  (let ((temp (gen-new-name 'repeat *remaining-stuff*)))
    (setf *outside-bindings*
	  (splice-in-subform *outside-bindings*
			     `(let ((,temp ,(pop *remaining-stuff*)))
				,*magic-cookie*)))
    (setf *inside-bindings*
	  (splice-in-subform *inside-bindings*
			     `(if (< (decf ,temp) 0)
				  (loop-finish)
				  ,*magic-cookie*)))))


(defun maybe-parse-unconditional ()
  (let ((clause (car *remaining-stuff*)))
    (cond ((loop-keyword-p clause 'do 'doing)
	   (pop *remaining-stuff*)
	   (setf *body-forms* (nconc *body-forms* (parse-expr-list)))
	   t)
	  ((loop-keyword-p clause 'return)
	   (pop *remaining-stuff*)
	   (let ((expr (maybe-parse-it (pop *remaining-stuff*))))
	     (push `(return-from ,*loop-name* ,expr)
		   *body-forms*))
	   t))))

(defun maybe-parse-conditional ()
  (let ((clause (pop *remaining-stuff*)))
    (cond ((loop-keyword-p clause 'if 'when)
	   (parse-conditional (pop *remaining-stuff*))
	   t)
	  ((loop-keyword-p clause 'unless)
	   (parse-conditional `(not ,(pop *remaining-stuff*)))
	   t)
	  (t
	   (push clause *remaining-stuff*)
	   nil))))

(defun parse-conditional (condition)
  (let ((*it-referenced* nil))
    (let ((clauses (parse-and-clauses))
	  (else-clauses (when (preposition-p 'else)
			  (parse-and-clauses))))
      (flet ((build-body (condition)
	       (cond ((and clauses else-clauses)
		      `(cond (,condition ,@clauses)
			     (t ,@else-clauses)))
		     (clauses
		      `(when ,condition
			 ,@clauses))
		     (else-clauses
		      `(unless ,condition
			 ,@clauses))
		     (t
		      condition))))
	(setf *body-forms*
	      (nconc *body-forms*
		     (if *it-referenced*
		       `((let ((,*it-referenced* ,condition))
			   ,(build-body *it-referenced*)))
		       `(,(build-body condition)))))
	(preposition-p 'END)))))

(defun parse-and-clauses ()
  (let ((*body-forms* nil))
    (setf *accept-it* t)
    (loop
      (or (maybe-parse-unconditional)
	  (maybe-parse-conditional)
	  (maybe-parse-accumulation)
	  (error "Invalid clause for inside a conditional: ~S" (car *remaining-stuff*)))
      (setf *accept-it* nil)
      (unless (preposition-p 'and)
	(return *body-forms*)))))


;;;; Accumulation stuff

(defun maybe-parse-accumulation ()
  (when (loop-keyword-p (car *remaining-stuff*)
		       'collect 'collecting
		       'append 'appending 'nconc 'nconcing
		       'count 'counting 'sum 'summing
		       'maximize 'maximizing 'minimize 'minimizing)
    (parse-accumulation)
    t))

(defun parse-accumulation ()
  (let* ((clause (pop *remaining-stuff*))
	 (expr (maybe-parse-it (pop *remaining-stuff*)))
	 (var (if (preposition-p 'into)
		  (pop *remaining-stuff*)
		  (or (and *result-var* (car *result-var*))
		      (progn
			(setf *result-var*
			      (list (gen-new-name (intern (string clause) *package*) *remaining-stuff*)))
			(car *result-var*)))))
	 (info (assoc var *accumulation-variables*))
	 (type nil)
	 (initial nil))
    (cond ((loop-keyword-p clause 'collect 'collecting 'append 'appending
			   'nconc 'nconcing)
	   (setf initial '(list))
	   (setf type 'cons)
	   (let ((aux-var
		  (or (caddr info)
		      (let ((aux-var (gen-new-name 'tail *remaining-stuff*)))
			(setf *outside-bindings*
			      (splice-in-subform `(let ((,var (list))
							(,aux-var (list)))
						    (declare (type cons
								   ,var
								   ,aux-var))
						    ,*magic-cookie*);;AML swapped
						 *outside-bindings*))
			(if (null info)
			    (push (setf info (list var 'cons aux-var))
				  *accumulation-variables*)
			    (setf (cddr info) (list aux-var)))
			aux-var)))
		 (value
		  (cond ((loop-keyword-p clause 'collect 'collecting)
			 `(list ,expr))
			((loop-keyword-p clause 'append 'appending)
			 `(copy-list ,expr))
			((loop-keyword-p clause 'nconc 'nconcing)
			 expr)
			(t
			 (error "Bug in loop?")))))
	     (setf *body-forms*
		   (nconc *body-forms*
			  `((cond ((endp ,var)
				   (setf ,var ,value)
				   (setf ,aux-var (last ,var)))
				  (t
				   (nconc ,aux-var ,value)
				   (setf ,aux-var (last ,aux-var)))))))))
	  ((loop-keyword-p clause 'count 'counting)
	   (setf type (parse-type-spec (or (cadr info) 'int)));;fixnum))
	   (setf initial '0)
	   (setf *body-forms*
		 (nconc *body-forms*
			(if (eq expr 't)
			    `((incf ,var))
			    `((when ,expr (incf ,var)))))))
	  ((loop-keyword-p clause 'sum 'summing)
	   (setf type (parse-type-spec (or (cadr info) 'long)))
	   (setf initial '0)
	   (setf *body-forms*
		 (nconc *body-forms*
			`((incf ,var ,expr)))))
	  ((loop-keyword-p clause 'maximize 'maximizing)
	   (setf type (parse-type-spec (or (cadr info) 'long)))
	   (setf initial (case type
			   ((byte) `(in (the java.lang.Byte) +min-value+))
			   ((short) `(in (the java.lang.Short) +min-value+))
			   ((int) `(in (the java.lang.Integer) +min-value+))
			   ((long) `(in (the java.lang.Long) +min-value+))
			   ((char) `(in (the java.lang.Character) +min-value+))
			   ((double) `(in (the java.lang.Double) +negative-infinity+))
			   ((float) `(in (the java.lang.Float) +negative-infinity+))
			   (t 'null)))
	   (push-end (generate-max-min-update 'max '> expr initial var) *body-forms*))
	  ((loop-keyword-p clause 'minimize 'minimizing)
	   (setf type (parse-type-spec (or (cadr info) 'long)))
	   (setf initial (case type
			   ((byte) `(in (the java.lang.Byte) +max-value+))
			   ((short) `(in (the java.lang.Short) +max-value+))
			   ((int) `(in (the java.lang.Integer) +max-value+))
			   ((long) `(in (the java.lang.Long) +max-value+))
			   ((char) `(in (the java.lang.Character) +max-value+))
			   ((double) `(in (the java.lang.Double) +positive-infinity+))
			   ((float) `(in (the java.lang.Float) +positive-infinity+))
			   (t 'null)))
	   (push-end (generate-max-min-update 'min '< expr initial var) *body-forms*))
	  (t
	   (error "Invalid accumulation clause: ~S" clause)))
    (cond (info
	   (unless (equal type (cadr info))
	     (error "Attempt to use ~S for both types ~S and ~S."
		    var type (cadr info))))
	  (t
	   (push (list var type) *accumulation-variables*)
	   (setf *outside-bindings*
		 (splice-in-subform `(let ((,var ,initial))
				       (declare (type ,type ,var))
				       ,*magic-cookie*);;AML swapped
				    *outside-bindings*))))))

(defun generate-max-min-update (max-min oper expr initial var) 
  (if (atom expr)
    `(when ,(if (eq initial 'null)
	      `(or (eq ,var null) (,oper ,expr ,var))
	      `(,oper ,expr ,var))
       (setf ,var ,expr))
    (let ((temp (gen-new-name max-min *remaining-stuff*)))
      `(let ((,temp ,expr))
	 ,(generate-max-min-update max-min oper temp initial var)))))
