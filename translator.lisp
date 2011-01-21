;;; -*- Mode: Common-Lisp; Syntax: Common-Lisp; Package: LINJ; Base: 10 -*-

;;; Copyright (C) Antonio Menezes Leitao Created on Fri Oct  6 18:50:23 2000
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

;;Syntactic elements

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;It's important to leave the responsability of parsing to the
;;transformation application so that the transformations can modify the
;;parsing behaviour before parsing recursively.  This is necessary, at
;;least, to properly implement the Linj equivalent of macrolet, namely
;;let-macro-transform and let-transform.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Macros.

(defparameter *transforms* (make-hash-table))

(defstruct transformation
  category
  name
  pattern
  function)

(defun form-to-name (form)
  (if (consp form) (car form) form))

(defun find-transformation (cat pat)
  (find-if #'(lambda (trans)
	       (and (eq (transformation-category trans) cat)
		    (subsumes (transformation-pattern trans) pat)
		    (subsumes pat (transformation-pattern trans))))
	   (gethash (form-to-name pat) *transforms*)))

(defun add-new-transformation (cat pat func)
  (let ((name (form-to-name pat)))
    (setf (gethash name *transforms*)
      (nconc (gethash name *transforms* (list)) 
	     (list (make-transformation :category cat
					:pattern pat
					:function func))))))

(defun delete-transformation (cat pat)
  (let ((name (form-to-name pat)))
    (setf (gethash name *transforms*)
      (delete (find-transformation cat pat)
	      (gethash name *transforms*)))))

;;Reserved words

(defparameter *statement-words* (list))

(defun add-statement-word (word)
  (pushnew word *statement-words*))

(defparameter *expression-words* (list))

(defun add-expression-word (word)
  (pushnew word *expression-words*))

(defun unreserved-word-p (word)
  (or (member word *expression-words*)
      (not (member word *statement-words*))))
;;

(defun add-transformation (cat pat func)
  (when (and (consp pat)
	     (symbolp (first pat))
	     (not (pat-var? (first pat)))
	     (subcategory-p cat 'statement))
    (add-statement-word (first pat)))
  (let ((old-transform (find-transformation cat pat)))
    (if old-transform
      (progn
	(setf (transformation-function old-transform) func)
	'redefined)
      (progn
	(add-new-transformation cat pat func)
	'defined))))

(defun apply-transformation (transf form)
  (funcall (transformation-function transf) form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Parse rules

(defstruct parse-rule
  category
  pattern
  function)

(defun set-category-super-categories (category super-categories)
  (setf (get category :super-categories) (or super-categories '(t))))

(defun category-super-categories (category)
  (get category :super-categories))

(defun category-precedence-list (category)
  (labels ((deep-first (cat)
             (if (null cat)
               ()
               (cons cat (reduce #'append
				 (mapcar #'deep-first (category-super-categories cat))
				 :from-end t)))))
    (remove-duplicates (deep-first category) :from-end t)))

(defparameter *parse-rules-database* 
  (list (cons :pre-syntax (make-hash-table :test #'eq))
	(cons :syntax (make-hash-table :test #'eq))
	(cons :pos-syntax (make-hash-table :test #'eq))))

(defun get-parse-rules-database-for-phase (phase)
  (cdr (assoc phase *parse-rules-database*)))

(defvar *avoided-parse-rules* (list))

(defmacro avoiding-parse-rule (category &body body)
  `(let ((*avoided-parse-rules* (cons ,category *avoided-parse-rules*)))
     ,@body))

(defun applicable-parse-rule (rule form)
  (and (not (member (parse-rule-category rule) *avoided-parse-rules*))
       (funcall (parse-rule-function rule) form)))

(defun add-parse-rule (rule &optional (phase :syntax))
  (let ((category (parse-rule-category rule)))
    (if (atom-first-p (parse-rule-pattern rule))
      (dolist (cat (category-precedence-list category))
	(let ((atom-rule (find-if #'atom-first-rule-p
				  (or (gethash cat (get-parse-rules-database-for-phase phase))
				      (setf (gethash cat (get-parse-rules-database-for-phase phase))
					    (list (create-atom-first-rule category)))))))
	  (add-to-atom-first-rule rule atom-rule)))
      (dolist (cat (category-precedence-list category))
	(setf (gethash cat (get-parse-rules-database-for-phase phase))
	      (add-to-rules-sequence
	       rule
	       (or (gethash cat (get-parse-rules-database-for-phase phase))
		   (list (create-atom-first-rule category)))))))))

(defun get-parse-rules (category &optional (phase :syntax))
  (gethash category (get-parse-rules-database-for-phase phase)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;One optimization for patterns with an atom as first element
(defun atom-first-p (pattern)
  (and (consp pattern)
       (atom (first pattern))
       (not (pat-var? (first pattern)))))

(defstruct (atom-first-rule (:include parse-rule))
  subrules)

(defun create-atom-first-rule (category)
  (let ((subrules (make-hash-table :test #'eq)))
    (make-atom-first-rule
     :category category
     :pattern '(?name . ?args)
     :function #'(lambda (original-form)
		   (try-match ((?name . ?ignore) original-form)
		     (let ((rules (gethash ?name subrules (list))))
		       (some #'(lambda (rule)
				 (applicable-parse-rule rule original-form))
			     rules))))
     :subrules subrules)))

(defun add-to-atom-first-rule (rule atom-first-rule)
  (let ((key (first (parse-rule-pattern rule))))
    (let ((rule (make-parse-rule :category (parse-rule-category rule)
				 :pattern (rest (parse-rule-pattern rule))
				 :function (parse-rule-function rule))))
      (setf (gethash key (atom-first-rule-subrules atom-first-rule))
	    (add-to-rules-sequence
	     rule
	     (gethash key (atom-first-rule-subrules atom-first-rule) (list)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-to-rules-sequence (rule rules)
  (cond ((null rules)
	 (list rule))
	((strict-subsumes (first rules) rule)
	 (cons rule rules))
	(t
         (cons (first rules) (add-to-rules-sequence rule (rest rules))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro let-pattern-special ((&rest bindings) &body body)
  "This should be merged with the next."
  (if (null bindings)
    (if (endp (rest body)) (first body) `(progn ,@body))
    (compile-pattern-special (caar bindings)
			     (cadar bindings)
			     `(let-pattern-special ,(cdr bindings) ,@body))))

(defmacro let-pattern ((&rest bindings) &body body)
  (if (null bindings)
    (if (endp (rest body)) (first body) `(progn ,@body))
    (compile-pattern (caar bindings)
                     (cadar bindings)
		     nil
                     `(let-pattern ,(cdr bindings) ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Parsing

(defun try-to-parse (form &optional (+category nil))
  ;;This allows parsing of already parsed objects
  (if (if +category (typep form +category) t)
    form
    (try-to-parse-in-category form +category)))

(defun try-to-parse-in-category (form +category)
  (let ((ast (or (apply-transformations form +category)
		 (apply-parse-rules form +category :pre-syntax)
		 (apply-parse-rules form +category :syntax)
		 (apply-parse-rules form +category :pos-syntax))))
    (when (and ast (typep ast 'ast-node))
      (setf (ast-node-parse-category ast) +category))
    ast))

(defun apply-parse-rules (form +category phase)
  (some #'(lambda (rule)
	    (applicable-parse-rule rule form))
	(get-parse-rules +category phase)))

(defun simple-pattern? (pat)
  (and (consp pat)
       (eq (car pat) '?is)))

(defun compatible-binds (a1 a2)
  (if (some #'(lambda (e1)
		(let ((e2 (find (car e1) a2 :key #'car)))
		  (and e2
		       (not (equal (cdr e1) (cdr e2))))))
	    a1)
    nil
    (union a1 a2 :test #'equal)))

(defconst +empty-bind+ (list (cons (gensym) (gensym))))

(defun subsumes (pat1 pat2)
  (cond ((and (null pat1) (null pat2)) +empty-bind+)
        ((atom pat1)
         (if (pat-var? pat1)
	   (if (eq pat1 '?ignore)
	     +empty-bind+
	     (list (cons pat1 pat2)))
	   (if (eq pat1 pat2)
	     +empty-bind+
	     nil)))
        ((atom pat2)
         nil)
	((simple-pattern? pat2)
	 +empty-bind+)
	((simple-pattern? pat1)
	 nil)
        (t
         (let ((a1 (subsumes (car pat1) (car pat2))))
	   (if a1
	     (let ((a2 (subsumes (cdr pat1) (cdr pat2))))
	       (if a2
		 (compatible-binds a1 a2)
		 nil))
	     nil)))))

(defun strict-subsumes (rule1 rule2)
  (let ((pat1 (parse-rule-pattern rule1))
	(pat2 (parse-rule-pattern rule2)))
    (and (subsumes pat1 pat2)
	 (or (not (subsumes pat2 pat1))
	     (subcategory-p (parse-rule-category rule2)
			    (parse-rule-category rule1))))))

(defun subcategory-p (sub super)
  (or (eq sub super)
      (and (not (null sub))
	   (subtypep (find-class sub) (find-class super)))))

;;;Some extra syntactic sugar
;;;
;;; (def-syntax if-statement (statement)
;;;   (if (?is ?test/formession numberp) ?then/statement ?else/statement))

; (defun convert-symbols (tree)
;   (cond ((null tree) 
; 	 ())
; 	((consp tree)
; 	 (cons (convert-symbols (car tree))
; 	       (convert-symbols (cdr tree))))
; 	((and (symbolp tree) (not (pat-var? tree)))
; 	 (intern (string-downcase (symbol-name tree))))
; 	(t
; 	 tree)))

(defmacro def-category (category super-categories slots &rest options)
  `(progn
     (set-category-super-categories ',category ',super-categories)
     (defclass ,category ,super-categories ,slots ,@options)
     (defmethod ,(conc-symbol category '-p) ((e ,category))
       t)
     (defmethod ,(conc-symbol category '-p) ((e t))
       nil)))

(defmacro def-syntax (category super-categories pattern &key 
		      (strict-p nil)
		      (slots nil)
		      (components nil)
		      (constructor 'make-instance)
		      (accessors t)
		      (phase :syntax))
  (let ((pat-vars (collect-pattern-vars pattern)))
    (let ((cat-pat-vars (remove-if-not #'(lambda (pat-var)
					   (nth-value 1 (pat-var-name-categorie pat-var)))
				       pat-vars)))
      `(progn
	 (def-category ,category ,super-categories
	   (,@(mapcar #'(lambda (pat-var)
			  (multiple-value-bind (name type)
			      (pat-var-name-categorie pat-var)
			    (declare (ignore type))
			    (let ((name (var-name name)))
			      `(,name
				:initarg ,(intern (symbol-name name) (find-package "KEYWORD"))
				,@(when accessors
				    `(:accessor ,(conc-symbol category '- name)))
				;; :type ,(or type t)
				))))
		      pat-vars)
	      ,@slots))
	 ;;set-parent
	 ,@(mapcar #'(lambda (pat-var)
		       (let ((name (var-name (pat-var-name-categorie pat-var))))
			 `(defmethod (setf ,(conc-symbol category '- name))
			      :after (new-val (obj ,category))
			    (setf (ast-node-parent new-val) obj))))
		   (append cat-pat-vars components))
	 (defmethod set-parent-on-slots :after ((obj ,category))
	   ,@(mapcar #'(lambda (pat-var)
			 (let ((name (var-name (pat-var-name-categorie pat-var))))
			   `(when (slot-boundp obj ',name)
			      (setf (ast-node-parent (slot-value obj ',name)) obj))))
		     (append cat-pat-vars components)))
	 ;;Statements are like reserved words
	 ,@(when (and (consp pattern)
		      (symbolp (first pattern))
		      (not (pat-var? (first pattern))))
	     `((when (subcategory-p ',category 'statement)
		 (add-statement-word ',(first pattern)))
	       (when (subcategory-p ',category 'expression)
		 (add-expression-word ',(first pattern)))))
	 ;;Visitor
	 (defmethod visit-descendents ((n ,category) (visitor t))
	   ,(let ((slots (mapcar #'(lambda (pat-var)
				     (var-name (pat-var-name-categorie pat-var)))
				 (append pat-vars components))))
	      `(with-slots ,slots n
		 ,@(mapcar #'(lambda (slot) `(visit ,slot visitor))
			   slots))))

	 (def-parse-rule ,category ,pattern ,phase
	   (original-form)
	   (,(if strict-p 'match-force-match 'try-match) (,pattern original-form)
	     (,constructor ',category
	       :original-form original-form
	       ,@(mapcan #'(lambda (pat-var)
			     (let ((name (pat-var-name-categorie pat-var)))
			       `(,(intern (string (var-name name)) (find-package "KEYWORD"))
				 ,name)))
			 pat-vars))))))))
  
;;NOTE: the parent initialization is being done in initialize-instance but
;;probably there are better places to put it.  def-list-syntax, for
;;instance, defines it in a different way.

;;;;;;;;;;;;;;;;;;;;
;;The translator 'back-door':

(defparameter *ignore-parse* (gensym))

(defun list-or-ignore-parse (parse)
  (if (eq parse *ignore-parse*)
    nil
    (list parse)))

;;To allow a 'nice' kind of gensym we use two different functions:
;;- gen-temp receives a string and generates a name with a terminal numeric index
;;- gen-iter doesn't receive arguments and generates the 'next'  logical
;;  iteration variable according to the sequence (i j k l m n o p q r s t)

;;The idea to implement the iteration variables is to establish a dynamic
;;binding containing the available variables and to pop this binding after
;;each gen-iter.  The binding is re-established (with its lattest value) on
;;each macro-expansion.  The hope is that the lexical environment is
;;enought to prevent colision of variables.

(defvar *gen-iter-names* '(i j k l m n o p q r s t))

(defvar *gen-temp-index* -1)

(defmacro gen-iter (&optional (prefix '||))
  `(conc-symbol ',prefix (pop *gen-iter-names*)))

(defmacro gen-temp (prefix) ;; not optional to force read case sensitiveness
  `(conc-symbol ,prefix (incf *gen-temp-index*)))

(defmacro with-new-names (names &rest body)
  (let ((new-body (gensym)))
    `(let (,@(mapcar #'(lambda (name)
			 `(,name (gensym)))
		     names))
       (let ((,new-body (list ,@body)))
	 (let (,@(mapcar #'(lambda (name)
			     `(,name (gen-new-name ',name ,new-body)))
			 names))
	   ,@body)))))

(defvar *new-names* (list))

(defun gen-new-name (name body)
  (labels ((references-p (form name)
	     (if (atom form)
	       (eq form name)
	       (or (references-p (car form) name)
		   (references-p (cdr form) name)))))
    (do ((i 0 (1+ i))
	 (new-name name (read-from-string (format nil "~A~D" name i))))
	((and (not (references-p body new-name))
	      (not (member new-name *new-names*)))
	 (push new-name *new-names*)
	 new-name))))


(defvar *linj-traced-macros* (list))

(defun linj-trace-macro (name)
  (push name *linj-traced-macros*))

(defun linj-untrace-macro (name)
  (setf *linj-traced-macros* (delete name *linj-traced-macros*)))

(defun linj-eval (form)
  (eval form))

(defun apply-transformations (form category)
  (or (let-pattern (((lisp ?form) form)) (try-to-parse (eval ?form) category))
      (let-pattern (((lisp-effect ?form) form)) (linj-eval ?form) *ignore-parse*)
      ;;This is an optimization allowed by the fact that we always 'start'
      ;;transformation patterns with a symbol.
      (let ((*gen-iter-names* *gen-iter-names*)  ;;for gen-iter
	    (*gen-temp-index* *gen-temp-index*)
	    (*new-names* *new-names*))  ;;for gen-temp
	(let ((transfs (gethash (form-to-name form) *transforms*)))
	  (or 	      ;;Deal with let-macro-transform as it changes the current transformations:
	   (define-and-apply-local-transformations form category)
	   (let ((result
		  (some #'(lambda (transf)
			    (and (or (and (null (transformation-category transf))
					  (or (subcategory-p 'expression category)
					      (subcategory-p 'statement category)
					      (subcategory-p 'top-level-form category)))
				     (subcategory-p (transformation-category transf) category))
				 (let ((transf (apply-transformation transf form)))
				   (and transf
					(let ((result (try-to-parse transf category)))
					  (when (and result (member (form-to-name form) *linj-traced-macros*))
					    (format *trace-output* "~&In category ~A ~A->~A" category form transf)
					    (format *trace-output* " and the parse is ~A~%" result))
					  result)))))
			transfs)))
	     
	     result))))))

;;To use in the editor:
(defun linj-macroexpand-1 (form)
  (apply-transformations form t))


(defmacro def-transformation (cat pat body)
  (let ((form (gensym)))
    `(add-transformation
      ',cat
      ',pat
      #'(lambda (,form)
	  (let-pattern-special ((,pat ,form))
			       ,body)))))


;; (defun define-and-apply-local-transformations (form category)
;;   (let-pattern (((let-macro-transform ?transforms ?form) form))
;;     (let ((transfs
;; 	   (mapcar #'(lambda (transf)
;; 		       (let ((cat (first transf))
;; 			     (pat (second transf))
;; 			     (body (third transf)))
;; 			 `(,cat
;; 			   ,pat
;; 			   ,(compile nil
;; 				     (let ((form (gensym)))
;; 				       `(lambda (,form)
;; 					  (let-pattern-special ((,pat ,form))
;; 					    ,body)))))))
;; 		   ?transforms)))
;;       (unwind-protect
;; 	  (progn (dolist (transf transfs)
;; 		   (apply #'add-transformation transf))
;; 		 (try-to-parse ?form category))
;; 	(dolist (transf transfs)
;; 	  (apply #'delete-transformation (butlast transf)))))))


(defun define-and-apply-local-transformations (form category)
  (or 
   (let-pattern (((let-macro-transform ?transforms ?form) form))
     (let ((transfs
	    (mapcar #'(lambda (transf)
			(let ((cat (first transf)) (pat (second transf)) (body (third transf)))
			  `(,cat
			    ,pat
			    ,(compile nil
				      (let ((form (gensym)))
					`(lambda (,form)
					  (let-pattern-special ((,pat ,form))
					    ,body)))))))
		    ?transforms)))
       (let ((old-functions (mapcar #'(lambda (transf)
					(let ((cat (first transf)) (pat (second transf)))
					  (let ((transf-struct (find-transformation cat pat)))
					    (and transf-struct (transformation-function transf-struct)))))
				    transfs)))
	 (dolist (transf transfs)
	   (apply #'add-transformation transf))
	 (unwind-protect
	      (try-to-parse ?form category)
	   (mapc #'(lambda (transf old-function)
		     (let ((cat (first transf)) (pat (second transf)))
		       (if old-function
			   (add-transformation cat pat old-function)
			   (delete-transformation cat pat))))
		 transfs 
		 old-functions)))))
   (let-pattern (((unlet-macro-transform ?transforms ?form) form))
     (let ((binds ?transforms))
       (let ((old-functions (mapcar #'(lambda (bind)
					(let ((cat (first bind)) (pat (second bind)))
					  (let ((transf-struct (find-transformation cat pat)))
					    (and transf-struct (transformation-function transf-struct)))))
				    binds)))
	 (dolist (bind binds)
	   (delete-transformation (first bind) (second bind)))
	 (unwind-protect
	      (try-to-parse-in-category ?form category)
	   (mapc #'(lambda (bind old-function)
		     (let ((cat (first bind)) (pat (second bind)))
		       (when old-function
			 (add-transformation cat pat old-function))))
		 binds
		 old-functions)))))))

(defun eval-replicate (form)
  (cond ((null form) 
         nil)
        ((consp form) 
         `(cons ,(eval-replicate (car form)) 
                ,(eval-replicate (cdr form))))
        ((pat-var? form)
         form)
        (t 
         `',form)))

(defmacro def-macro-transform (cat pat &body body)
  `(def-transformation ,cat ,pat (progn ,@body)))

(defmacro def-transform (cat pat1 pat2)
  (assert (symbolp cat))
  `(def-macro-transform ,cat ,pat1 ,(eval-replicate pat2)))

;;Classical macros
;;;HACK: The category should be ast-node but it doesn't work

(defmacro def-macro (name arglist &body body)
  (let ((form '#:?form))
    `(def-macro-transform nil (,name . ,form)
       (destructuring-bind ,arglist ,form
	 ,@body))))

(defmacro def-syntax-macro (name syntax arglist &body body)
  (let ((form '#:?form))
    `(def-macro-transform ,syntax (,name . ,form)
       (destructuring-bind ,arglist ,form
	 ,@body))))

;;Everybody is asking for macrolet

(def-macro macrolet ((&rest forms) &body body)
  (let ((form '#:?form))
    `(let-macro-transform ,(mapcar #'(lambda (macrolet-form)
				       (destructuring-bind (name arglist &body body) 
					   macrolet-form
					 `(nil (,name . ,form)
					   (destructuring-bind ,arglist ,form
					     ,@body))))
				   forms)
      (progn ,@body))))

(def-macro symbol-macrolet ((&rest symbol-expansion-pairs) &body ?body)
  `(let-macro-transform ,(mapcar #'(lambda (symbol-expansion)
				     (destructuring-bind (name body) 
					 symbol-expansion
				       `(expression ,name
					 '(unlet-macro-transform ((expression ,name))
					   ,body))))
				 symbol-expansion-pairs)
    (progn ,@?body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Fundamental classes
(def-category ast-node ()
  ((original-form :initarg :original-form :accessor ast-node-form)
   (parent :initarg :parent :accessor ast-node-parent)
   (parse-category :accessor ast-node-parse-category :initform :unparsed))
  (:default-initargs
      :parent nil
      :original-form :no-original-form))

;;Primary to hang all the afters
(defmethod set-parent-on-slots ((node ast-node))
  nil)

;;Let's initialize the parent slot of all the initargs
(defmethod initialize-instance :after ((node ast-node) &key)
  (set-parent-on-slots node))

;;To automatically assign a parent

(defmacro with-parent ((parent) &body body)
  (let ((form (gensym))
	(new-parent (gensym)))
    `(let ((,new-parent ,parent))
       (let ((,form (progn ,@body)))
	 (setf (ast-node-parent ,form) ,new-parent)
	 ,form))))

;;A starting rule
(def-parse-rule ast-node ?form :syntax (form)
  form)

;;Lists of things:

(defmacro def-list-syntax (category super-categories sub-category &key
			   (slots nil)
			   (constructor 'make-instance)
			   (phase :syntax))
  (let ((accessor (conc-symbol category '- 'elements)))
    `(progn
       (def-category ,category ,super-categories
	 ((elements :initarg :elements 
		    :accessor ,accessor)
	  ,@slots))
       (defmethod visit-descendents ((n ,category) (visitor t))
	 (visit (,accessor n) visitor))
       (defmethod (setf ,accessor) :after (new-val (obj ,category))
	 (dolist (elem new-val)
	   (setf (ast-node-parent elem) obj)))
       (defmethod set-parent-on-slots :after ((obj ,category))
	 (dolist (elem (slot-value obj 'elements))
	   (setf (ast-node-parent elem) obj)))
       (def-parse-rule ,category ?form ,phase
	 (original-form)
	 (when (listp original-form)
	   (,constructor ',category
	     :original-form original-form
	     :elements (labels ((parse-elems (elems)
				  (cond ((null elems)
					 (list))
					((,(conc-symbol category '-p) elems)
					 (,accessor elems))
					(t
					 (append (list-or-ignore-parse (parse (first elems) ',sub-category))
						 (parse-elems (rest elems)))))))
			 (parse-elems original-form))))))))
  
;;We need to visit and transform the parse tree.
;;Some of the operations can be better seen as post-initializations that
;;can only be done after everything on the parse tree is on its correct place.

(defmethod visit ((object t) (visitor t))
  (visit-descendents object visitor)
  (visit-current object visitor))

(defmethod visit-current ((object t) (visitor t))
  'do-nothing)

(defmethod visit-descendents ((object t) (visitor t))
  'do-nothing)

(defmethod visit-descendents ((l list) (visitor t)) 
  (dolist (e l)
    (visit e visitor)))




;;;;;;;;;;;;;;;;
;;The visitors:

;;Visitor to correct parent pointers
(defclass adjust-parents ()
  ())

;;All sorts of adjustments.  Probably needs to be divided into
;;sub-visitors
(defclass parse-tree-finish ()
  ())

;;Some casts and wrappings that need to be done prior to unparsing.
(defclass cast-and-wrap ()
  ())



;;;To transform instances of ast-nodes

(defun become-instance (ast-node new-ast-node)
  (let ((class (class-of new-ast-node)))
    (change-class ast-node class)
    (dolist (slot-def (mop-class-slots class))
      (let ((slot (mop-slot-definition-name slot-def)))
	(if (slot-boundp new-ast-node slot)
	  (setf (slot-value ast-node slot)
	    (slot-value new-ast-node slot))
	  (slot-makunbound ast-node slot))))
    ast-node))

;;To copy ast-nodes
;;BE CAREFULL. This can't understand node semantics and it might produce infinite regress easily.
;;Use it only for simpler things like statements
(defmethod copy-ast ((node t) visited)
  (let ((bind (assoc node visited :test #'eq)))
    (if bind
      (values (cdr bind) visited)
      (cond ((consp node)
	     (let ((new-node (cons :car :cdr)))
	       (let ((visited (acons node new-node visited)))
		 (multiple-value-bind (car visited)
		     (copy-ast (car node) visited)
		   (multiple-value-bind (cdr visited)
		       (copy-ast (cdr node) visited)
		     (setf (car new-node) car
			   (cdr new-node) cdr)
		     (values new-node visited))))))
	    ((ast-node-p node)
	     (let ((class (class-of node)))
	       (let ((new-node (make-instance class)))
		 (let ((visited (acons node new-node visited)))
		   (dolist (slot-def (mop-class-slots class))
		     (let ((slot (mop-slot-definition-name slot-def)))
		       (if (slot-boundp node slot)
			 (multiple-value-bind (new-value new-visited)
			     (copy-ast (slot-value node slot) visited)
			   (setf (slot-value new-node slot) new-value
				 visited new-visited))
			 (slot-makunbound new-node slot))))
		   (values new-node visited)))))
	    (t
	     (values node visited))))))

(defun copy-with-parent (node parent)
  (copy-ast node (acons (ast-node-parent node) parent (list))))

(defun mop-class-slots (class)
  #+(or cmu allegro) (mop::class-slots class)
  #+sbcl (sb-mop::class-slots class)
  #+clisp (clos::class-slots class)
  #+lispworks (hcl::class-slots class))

(defun mop-slot-definition-name (slot-def)
  #+(or cmu allegro) (mop::slot-definition-name slot-def)
  #+sbcl (sb-mop::slot-definition-name slot-def)
  #+clisp (clos::slotdef-name slot-def)
  #+lispworks (hcl::slot-definition-name slot-def))