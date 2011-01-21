;;; -*- Mode: Common-Lisp; Syntax: Common-Lisp; Package: LINJ; Base: 10 -*-

;;; Copyright (C) Antonio Menezes Leitao Created on Thu May  2 20:02:40 2002
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


(defmethod force-type ((e t) (type type-reference))
  (error "The method force-type must be specialized for class ~a (instance ~a)" (type-of e) e))

(defmethod force-type :around ((e expression) (type type-reference))
  (cond ((super-type-p type (get-type e))
	 e) ;;do nothing
	(t
	 (call-next-method))))
;;I don't do this because some numeric types can be forced but they do not belong to the same type hierarchy.
;; 	((super-type-p (get-type e) type)
;; 	 (call-next-method))
;; 	(t
;; 	 (linj-error "In expression ~S can't force type ~S to become ~S" e (get-type e) type))))

;; (defun convert-instance-to-type (inst type)
;;   (let ((new-inst (make-instance 'ast-node)))
;;     (become-instance new-inst inst)
;;     (let ((forced-type-inst
;; 	   (with-parent ((ast-node-parent inst))
;; 	     (convert-to-type new-inst type))))
;;       (become-instance inst forced-type-inst))))

(defmethod force-type ((e literal) (type type-reference))
  (if (null-literal-p e)
    e
    (with-parent ((ast-node-parent e))
      (assignment-convertion
       e
       type
       #'(lambda (expected current expr)
	   (linj-error "Can't make convert to type ~S~%the expression ~S of type ~S"
		       expected
		       expr
		       current))))))

(defmethod force-type ((e expression) (type type-reference))
  (with-parent ((ast-node-parent e))
    (convert-to-type e type)))

(defmethod force-type ((e explicit-type-node) (type type-reference))
  (unless (equal-type-p (get-type e) type)
    (linj-error "Can't force type in an explicit-type-node ~A of type ~A (to type ~A)"
		e (get-type e) type)))

;; (defmethod force-type ((e method-call-expression) (type type-reference))
;;   (convert-instance-to-type e type))

;; (defmethod force-type ((e n-ary-operator-expression) (type type-reference))
;;   (convert-instance-to-type e type))

;;Java forces conditional expression consequent and alternative to be type-compatible
(defmethod visit-current ((e conditional-expression) (visitor parse-tree-finish))
  (let ((then-type (get-type (conditional-expression-then e)))
	(else-type (get-type (conditional-expression-else e))))
    (unless (or (null-type-p then-type)
		(super-type-p then-type else-type)
		(null-type-p else-type)
		(super-type-p else-type then-type))
      (force-type e (get-type e)))))

;;Must override the generic around because type casts are not optional in
;;conditional expressions
(defmethod force-type :around ((e conditional-expression) (type type-reference))
  (setf (conditional-expression-then e)
    (with-parent (e)
      (convert-to-type (conditional-expression-then e) type)))
  (setf (conditional-expression-else e)
    (with-parent (e)
      (convert-to-type (conditional-expression-else e) type)))
  e)


(defmethod force-type ((e expression-statement) (type type-reference))
  (setf (expression-statement-expression e)
	(with-parent (e)
	  (force-type (expression-statement-expression e) type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;We can also force the exit points to have a pre-determined type:

(defmethod force-type ((e method-declaration) (type type-reference))
;;  (unless (void-type-p type)
  (let ((exits (list))
	(cycle-p nil))
    (unless (abstract-method-declaration-p e)
      (with-new-type-inference ()
	(apply-to-method-exit-points
	 e
	 #'(lambda (exit)
	     (if (in-type-inference-p exit)
	       (setf cycle-p t)
	       (push exit exits)))))
      (if cycle-p
	(error "Can't force type to exit points in method ~S" (method-declaration-name e))
	(force-exit-type exits type)))))

(defun force-exit-type (exits type)
  (dolist (exit exits)
    (force-type exit type)))


(defmethod force-type ((e non-void-return-statement) (type type-reference))
  (setf (non-void-return-statement-expression e)
	(with-parent (e)
	  (force-type (non-void-return-statement-expression e) type))))

(defmethod force-type ((e return-from-statement) (type type-reference))
  (if (null (return-from-statement-expression e))
    (linj-error "Can't force type on non-valued return statement ~A" e)
    (setf (return-from-statement-expression e)
	  (with-parent (e)
	    (force-type (return-from-statement-expression e) type)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;One of the best things I would like to have in linj would be to avoid most of
;;the nasty 'return' statements. One could write just expressions and the proper
;;'return' would be inserted wherever appropriate.

;;The idea is that when a method has a return type, we must follow the
;;execution path and check whether the last component is a
;;expression-statement.  In this case, we add a return.  Note that in
;;conditional statements there are multiple potential execution paths.
;;A last warning: this does not apply to constructors or abstract
;;methods. They never return!

;;Not that all return-from-statements that are exit points for a
;;method are transformed into void-return-statements or
;;non-void-return-statements.  The remaining return-from-statements
;;must be returning from some inner block.  In this case, either they
;;don't return a value (i.e., they just break the block), or there's
;;an error because in linj you can't return a value for an inner
;;block.
(defclass return-type-inference-visitor ()
  ())

(defmethod visit :after ((e method-declaration) (visitor return-type-inference-visitor))
  (unless (or (constructor-declaration-p e)
	      (abstract-method-declaration-p e))
    ;;Compute merged return type for method and force it on every exit point
    (if (void-type-p (method-declaration-type e))
      (apply-to-method-exit-points
       e
       #'(lambda (stat)
	   (when (return-from-statement-p stat)
	     ;;Just to be sure, ensure that we have a proper block
	     (containing-block-named stat (return-from-statement-label stat))
	     (if (null (return-from-statement-expression stat))
	       (change-class stat 'void-return-statement)
	       (linj-error "In ~S, can't return a value from a void method" stat)))))
      (progn
	(force-type e (get-type e))
	(apply-to-method-exit-points
	 e
	 #'make-return-point)))))

(defun make-return-point (stat)
  (if (expression-statement-p stat)
    (change-class stat 'non-void-return-statement)
    (when (return-from-statement-p stat)
      ;;Just to be sure, ensure that we have a proper block
      (containing-block-named stat (return-from-statement-label stat))
      (if (null (return-from-statement-expression stat))
	(change-class stat 'void-return-statement)
	(change-class stat 'non-void-return-statement)))))
