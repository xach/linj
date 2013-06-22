;;; -*- Mode: Common-Lisp; Syntax: Common-Lisp; Package: LINJ; Base: 10 -*-

;;; Copyright (C) Antonio Menezes Leitao Created on Thu May  2 19:57:45 2002
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

;;How to infer types:
;;We need a stack to deal with recursive and mutual recursive code.

(defparameter *in-type-inference* (list))

(defun in-type-inference-p (e)
  (member e *in-type-inference*))

(defmacro with-new-type-inference ((&rest args) &body body)
  (declare (ignore args))
  `(let ((*in-type-inference* (list)))
     ,@body))

(defmethod get-type :around ((e t))
  (let ((*in-type-inference* (cons e *in-type-inference*)))
    (call-next-method)))

(defmethod get-type ((e t))
  (error "Don't know how to get the type of ~S" e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Now, the types: primitive or reference.  In the reference case, it can be
;;array or class-or-interface.

;;Type references are more specific than regular references.

(def-syntax type-reference (linj-node cached-declaration)
  (?is ?name type-reference-name-p)
  :constructor make-type-reference)

(defun type-reference-name-p (name)
  (and (symbolp name)
       (or (eq name :infer-it)
           (not (keywordp name)))))

(defun make-type-reference (category &key original-form name)
  (declare (ignore category))
  (if (eq name :infer-it)
      (make-instance 'type-reference :name :infer-it)
      (let ((parse (parse (parse-java-type (princ-to-string (translate-cl-type name))) 'type-reference)))
        (setf (ast-node-form parse) original-form) ;;HACK: this mix node-form/original-form must change
        parse)))

;;Translating Common Lisp types
(defparameter *bignum-arithmetic-p* t)

(defun translate-cl-type (type)
  (case type
    ((base-char) 'char)
    ((bit) 'int)
    ((fixnum) 'long)
    ((integer) (if *bignum-arithmetic-p* 'linj.Bignum 'java.math.BigInteger))
    ((real) 'java.math.BigDecimal)
    ((rational) 'linj.Bignum)
    ((short-float single-float) 'float)
    ((double-float long-float) 'double)
    ;;    ((list) 'linj.Cons)
    ((base-string simple-string) 'string)
    ((simple-vector) 'java.lang.Object[])
    ;;    ((bit-vector simple-bit-vector) 'long)
    ((t) 'java.lang.Object)
    (t type)))

(defmethod unknown-type-reference-p ((e type-reference))
  (eq (type-reference-name e) :infer-it))

(defmethod containing-type-declaration ((e type-reference)) ;;if it's a type declaration name, jump over
  (let ((up (ast-node-parent e)))
    (and up
         (cond ((or (and (class-declaration-p up)
                         (or (eq (class-declaration-name up) e)
                             (eq (class-declaration-superclass up) e)))
                    (and (mixin-declaration-p up)
                         (eq (mixin-declaration-name up) e)))
                (containing-type-declaration up))
               ((type-reference-list-p up)
                (let ((up-up (ast-node-parent up)))
                  (if (or (and (class-declaration-p up-up)
                               (eq (class-declaration-mixins up-up) up)
                               (member e (type-reference-list-elements up)))
                          (and (mixin-declaration-p up-up)
                               (eq (mixin-declaration-supermixins up-up) up)
                               (member e (type-reference-list-elements up))))
                      (containing-type-declaration up-up)
                      (self-or-containing-type-declaration up))))
               (t
                (self-or-containing-type-declaration up))))))

(defmethod find-declaration ((e type-reference))
  (get-type-declaration e))

(def-unparse type-reference (e)
  (error "Can't unparse [TYPE-REF ~A]" (reference-name e)))

;;types have themselves as type
(defmethod get-type ((e type-reference))
  e)

(defmethod equal-type-p ((type1 type-reference)
                         (type2 type-reference))
  nil)

(def-syntax primitive-type-reference (type-reference)
  (primitive-type ?name))

(def-unparse primitive-type-reference (e)
  (format t "~A" (primitive-type-reference-name e)))

;;Primitive types don't have a declaration
(defmethod get-type-declaration ((e primitive-type-reference))
  (error "Primitive type ~W doesn't have type declaration!" e))

(defun equal-primitive-type-reference-name (n1 n2)
  ;;Unique names for primitive types
  (eq n1 n2))

(defmethod equal-type-p ((type1 primitive-type-reference)
                         (type2 primitive-type-reference))
  (equal-primitive-type-reference-name
   (primitive-type-reference-name type1)
   (primitive-type-reference-name type2)))

(defmethod copy-type ((type primitive-type-reference))
  (make-instance 'primitive-type-reference
                 :name (primitive-type-reference-name type)
                 :original-form (ast-node-form type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Reference types are more interesting that primitive types because:
;;- they are composed by a package and a name
;;- they are programmer definable
;;- the package can be omited and Linj is able to derive it (under certain conditions)
;;- they can be used as a kind of expressions in method calls and slot-value expressions

;;First, we need syntax to define reference types:
(def-syntax class-or-interface-type-reference (type-reference)
  (class-or-interface-type ?inferred-package ?name/linj-type-name))

;;We must compute packages on demand bc the programmer might not specify them.
;;The rules are the following

                                        ; 6.5.5.1 Simple Type Names

                                        ; If a type name consists of a single Identifier, then the identifier must
                                        ; occur in the scope of a declaration of a type with this name, or a
                                        ; compile-time error occurs.

                                        ; It is possible that the identifier occurs within the scope of more than
                                        ; one type with that name, in which case the type denoted by the name is
                                        ; determined as follows:

                                        ; If the simple type name occurs within the scope of a visible local class
                                        ; declaration (§14.3) with that name, then the simple type name denotes
                                        ; that local class type.  Otherwise, if the simple type name occurs within
                                        ; the scope of exactly one visible member type (§8.5, §9.5), then the
                                        ; simple type name denotes that member type.  Otherwise, if the simple type
                                        ; name occurs within the scope of more than one visible member type, then
                                        ; the name is ambiguous as a type name; a compile-time error occurs.
                                        ; Otherwise, if a type with that name is declared in the current
                                        ; compilation unit (§7.3), either by a single-type-import declaration
                                        ; (§7.5.1) or by a declaration of a class or interface type (§7.6), then
                                        ; the simple type name denotes that type.  Otherwise, if a type with that
                                        ; name is declared in another compilation unit (§7.3) of the package (§7.1)
                                        ; containing the identifier, then the identifier denotes that type.
                                        ; Otherwise, if a type of that name is declared by exactly one
                                        ; type-import-on-demand declaration (§7.5.2) of the compilation unit
                                        ; containing the identifier, then the simple type name denotes that type.
                                        ; Otherwise, if a type of that name is declared by more than one
                                        ; type-import-on-demand declaration of the compilation unit, then the name
                                        ; is ambiguous as a type name; a compile-time error occurs.  Otherwise, the
                                        ; name is undefined as a type name; a compile-time error occurs.

                                        ; This order for considering type declarations is designed to choose the
                                        ; most explicit of two or more applicable type declarations.

                                        ; 6.5.5.2 Qualified Type Names

                                        ; If a type name is of the form Q.Id, then Q must be either a type name or
                                        ; a package name. If Id names exactly one type that is a member of the type
                                        ; or package denoted by Q, then the qualified type name denotes that
                                        ; type. If Id does not name a member type (§8.5, §9.5) within Q, or the
                                        ; member type named Id within Q is not accessible (§6.6), or Id names more
                                        ; than one member type within Q, then a compile-time error occurs.

(defmethod class-or-interface-type-reference-package ((e class-or-interface-type-reference))
  (when (unknown-package-p (class-or-interface-type-reference-inferred-package e))
    (setf (class-or-interface-type-reference-inferred-package e)
          (infer-class-or-interface-type-reference-package e)))
  (class-or-interface-type-reference-inferred-package e))

(defun find-package-or-null (name decl)
  (and (equal-type-reference-name
        name
        (class-or-interface-type-reference-name (type-declaration-name decl)))
       (class-or-interface-type-reference-package (type-declaration-name decl))))

(defparameter *accept-isolated-type-references* nil
  "Don't try to infer package for isolated type references")

(defmethod infer-class-or-interface-type-reference-package ((e class-or-interface-type-reference))
  (let ((containing-decl (containing-type-declaration e)))
    (let ((containing-unit (or (and containing-decl
                                    (containing-compilation-unit containing-decl))
                               (containing-compilation-unit e))))
      (let ((name (class-or-interface-type-reference-name e)))
        (or
         (and t
              (or
               ;;First, the containing type definition
               (labels ((scoped-declarations (e)
                          (let ((containing-decl (containing-type-declaration e)))
                            (and containing-decl
                                 (or (find-package-or-null name containing-decl)
                                     (some #'(lambda (elem)
                                               (and (type-declaration-p elem)
                                                    (find-package-or-null name elem)))
                                           (type-body-elements (type-declaration-body containing-decl)))
                                     (scoped-declarations containing-decl))))))
                 (scoped-declarations e))
               ;;Second, type definitions on the same compilation unit
               (some #'(lambda (decl)
                         (and (not (eq decl containing-decl))
                              (find-package-or-null name decl)))
                     (compilation-unit-type-declarations containing-unit))))
         ;;Third, imported types
         (some #'(lambda (import)
                   (find-import-type-declarations name import))
               (compilation-unit-imports containing-unit))
         ;;Fourth, other compilation units on the same package
         (find-in-package-compilation-units-type-declarations name containing-unit)
         ;;Fifth, guess some usual imports
         (guess-package name)
         ;;Sixth, bum.
         (if *accept-isolated-type-references*
             (empty-package)
             (error "Couldn't infer package for type ~A" name)))))))

;; (defmethod infer-class-or-interface-type-reference-package ((e class-or-interface-type-reference))
;;   (let ((containing-decl (containing-type-declaration e)))
;;     (let ((containing-unit (or (and containing-decl
;;                                  (containing-compilation-unit containing-decl))
;;                             (containing-compilation-unit e))))
;;       (let ((name (class-or-interface-type-reference-name e)))
;;      (or
;;       (and containing-decl
;;            (or
;;             ;;First, the containing type definition
;;             (find-package-or-null name containing-decl)
;;             ;;Some inner type definition
;;             (some #'(lambda (elem)
;;                       (and (type-declaration-p elem)
;;                            (find-package-or-null name elem)))
;;                   (type-body-elements (type-declaration-body containing-decl)))
;;             ;;Second, type definitions on the same compilation unit
;;             (some #'(lambda (decl)
;;                       (and (not (eq decl containing-decl))
;;                            (find-package-or-null name decl)))
;;                   (compilation-unit-type-declarations containing-unit))))
;;       ;;Third, imported types
;;       (some #'(lambda (import)
;;                 (find-import-type-declarations name import))
;;             (compilation-unit-imports containing-unit))
;;       ;;Fourth, other compilation units on the same package
;;       (find-in-package-compilation-units-type-declarations name containing-unit)
;;       ;;Fifth, guess some usual imports
;;       (guess-package name)
;;       ;;Sixth, bum.
;;       (and *accept-isolated-type-references*
;;            (empty-package)))))))

(defun set-if-unknown-package (type-ref package)
  (when (unknown-package-p (class-or-interface-type-reference-inferred-package type-ref))
    (setf (class-or-interface-type-reference-inferred-package type-ref) package)))

;;After parsing, we should resolve unknown packages
;;Linj inheritance copies code.  The copied code must have all packages
;;properly resolved to avoid improper package capture.

(defmethod visit :before ((e class-or-interface-type-reference) (visitor parse-tree-finish))
  (class-or-interface-type-reference-package e)) ;;this is enough to force package computation.

(defparameter *prefered-packages* (list))

(defparameter *standard-packages* (make-hash-table))

(defparameter *check-duplicate-standard-packages* nil)

(defun add-standard-type (type)
  (if (and *check-duplicate-standard-packages*
           (gethash (class-or-interface-type-reference-name type) *standard-packages* nil))
      (error "Duplicate standard type addition ~A" (class-or-interface-type-reference-name type))
      (setf (gethash (class-or-interface-type-reference-name type) *standard-packages*)
            (class-or-interface-type-reference-package type))))

(defun guess-package (name)
  (gethash name *standard-packages*))

(defmacro add-standard-types (&rest pkg-names)
  `(progn
     ,@(mapcar #'(lambda (pkg-name)
                   `(add-standard-type (parse ',pkg-name 'class-or-interface-type-reference)))
               pkg-names)))

;;;;;;;;;;;;;;;;;;;;;;;

(defun get-package-compilation-units (package)
  (error "Get package compilation units for ~A must be finished!" package))

;;;;;;;;;;;;;;;;;;;;;;;

(defun find-in-package-compilation-units-type-declarations (name containing-unit)
  ;;hyper short route: the file was already loaded
  (let ((pkg (package-declaration-components (compilation-unit-package containing-unit))))
    (and ;;(not (empty-package-p pkg))
     (or (and (get-package-type-info pkg name)
              pkg)
         ;;short route: there's a file with the same name
         (let ((path (merge-pathnames (make-pathname :name (princ-to-string (linj-original-name name))
                                                     :type "linj")
                                      (compilation-unit-location containing-unit))))
           (and (probe-file path)
                (let ((unit (get-compilation-unit-from-location path nil)))
                  (or (some #'(lambda (decl)
                                (find-package-or-null name decl))
                            (compilation-unit-type-declarations unit))
                      (error "Couln't find the compilation unit ~A!" name)))))
         ;;ask server
         (and (get-package-type-info-from-java-server pkg name)
              pkg)))))
                                        ;   ;;long route: we must check all files:
                                        ;   (some #'(lambda (unit)
                                        ;           (some #'(lambda (decl)
                                        ;                     (find-package-or-null e decl))
                                        ;                 (compilation-unit-type-declarations unit)))
                                        ;       (and (compilation-unit-package containing-unit)
                                        ;            (get-package-compilation-units
                                        ;             (package-declaration-components
                                        ;              (compilation-unit-package containing-unit))))))

(defmethod find-import-type-declarations (name import)
  (let ((last (class-or-interface-type-reference-name (import-declaration-type import)))
        (pkg (class-or-interface-type-reference-package (import-declaration-type import))))
    (cond ((eq last name)
           pkg)
          ((and (eq last '*)
                (or (get-package-type-info pkg name)
                    (find-package-type-location pkg name)
                    (get-package-type-info-from-java-server pkg name)))
           pkg)
          (t nil))))

(defun merge-package-type (package type)
  (let ((last (first (last package))))
    (cond ((eq last type)
           package)
          ((eq last '*)
           (append (butlast package) (list type)))
          (t
           (error "Can't merge ~S with ~S" package type)))))

(defun match-type (import type)
  (and (package= (class-or-interface-type-reference-package import)
                 (class-or-interface-type-reference-package type))
       (or (eq (class-or-interface-type-reference-name import) '*)
           (eq (class-or-interface-type-reference-name import)
               (class-or-interface-type-reference-name type)))))

(defparameter *debug-p* nil
  "Change this to t if a simpler unparse is needed")

(defparameter *no-package-type-references-p* nil
  "Don't try to resolve type references")

(def-unparse class-or-interface-type-reference (e)
  (if (cyclic-type-p e)
      (linj-error "Circularity while inferring type information." e)
      (if *no-package-type-references-p*
          (format t "~/unlinj-type/" (class-or-interface-type-reference-name e))
          (let ((pkg (if *debug-p*
                         (class-or-interface-type-reference-inferred-package e)
                         (class-or-interface-type-reference-package e))))
            (if (unknown-package-p pkg)
                (format t "???.~/unlinj-type/" (class-or-interface-type-reference-name e))
                (if (or (empty-package-p pkg)
                        (let ((unit (containing-compilation-unit e)))
                          (and unit
                               (or (let ((unit-pkg (compilation-unit-package unit)))
                                     (package= pkg (package-declaration-components unit-pkg)))
                                   (imported-type-p e unit)))))
                    (format t "~/unlinj-type/" (class-or-interface-type-reference-name e))
                    (format t "~{~A.~}~/unlinj-type/"
                            (class-or-interface-type-reference-inferred-package e)
                            (class-or-interface-type-reference-name e))))))))

(defun equal-type-reference-name (n1 n2)
  (eq n1 n2))

(defun equal-type-reference-package (p1 p2)
  (equal p1 p2))

(defmethod equal-type-p ((type1 class-or-interface-type-reference)
                         (type2 class-or-interface-type-reference))
  ;;We test the name first to avoid resolving package if uneeded
  (and (equal-type-reference-name
        (class-or-interface-type-reference-name type1)
        (class-or-interface-type-reference-name type2))
       (equal-type-reference-package
        (class-or-interface-type-reference-package type1)
        (class-or-interface-type-reference-package type2))))

(defmethod copy-type ((type class-or-interface-type-reference))
  (make-instance 'class-or-interface-type-reference
                 :name (class-or-interface-type-reference-name type)
                 :inferred-package (class-or-interface-type-reference-package type)
                 :original-form (ast-node-form type)))


(defmethod super-type-p ((type1 type-reference) (type2 type-reference))
  nil)

(defparameter *primitive-types-order*
  (list (double-type) (float-type) (long-type) (int-type) (char-type) (short-type) (byte-type)))

(defmethod super-type-p ((type1 primitive-type-reference) (type2 primitive-type-reference))
  (or (equal-type-p type1 type2)
      (member type2 (member type1 *primitive-types-order*
                            :test #'equal-type-p)
              :test #'equal-type-p)))

(defmethod super-type-p ((type1 class-or-interface-type-reference) (type2 class-or-interface-type-reference))
  (or (equal-type-p type1 type2)
      (equal-type-p type2 (null-type))
      (and (not (equal-type-p type1 (null-type)))
           (or (root-class-declaration-p (get-type-declaration type1))
               (and (not (root-class-declaration-p (get-type-declaration type2)))
                    (let ((decl (get-type-declaration type2)))
                      (cond ((class-declaration-p decl)
                             (or (super-type-p type1 (class-declaration-superclass decl))
                                 (some (curry #'super-type-p type1)
                                       (type-reference-list-elements (class-declaration-mixins decl)))))
                            ((mixin-declaration-p decl)
                             (some (curry #'super-type-p type1)
                                   (type-reference-list-elements (mixin-declaration-supermixins decl))))
                            (t
                             (error "Unknown type of declaration ~S" decl)))))))))

(defmethod common-super-type ((t1 class-or-interface-type-reference)
                              (t2 class-or-interface-type-reference))
  (cond ((super-type-p t1 t2)
         t1)
        ((super-type-p t2 t1)
         t2)
        (t
         (or (common-super-type (class-declaration-superclass
                                 (get-type-declaration t1))
                                t2)
             (common-super-type t1
                                (class-declaration-superclass
                                 (get-type-declaration t2)))))))


;;;;;;;;;;;;;;
;;Array types
(def-syntax array-type-reference (class-or-interface-type-reference)
  (array-type ?subtype/type-reference)
  :slots ((name :initform :array)
          (inferred-package :initform (empty-package))
          (original-form :initform nil)))

(def-unparse array-type-reference (e)
  (format t "~/pp/[]" (array-type-reference-subtype e)))

;;Array types have a extremely small declaration which is a class composed
;;by a unique final instance variable containing the length of the array as
;;an int.
(defparameter *array-type-declaration* nil)

;;Arrays are subclasses of <object>
(defmethod get-type-declaration ((e array-type-reference))
  (unless *array-type-declaration*
    (setf *array-type-declaration*
          (parse '(defclass-0 java.lang.array java.lang.object () ()
                   (defslot-0 (length (the int)) :visibility :public))
                 'type-declaration)))
  *array-type-declaration*)
                                        ;  (error "Array type ~W doesn't have declaration!" e))

(defmethod equal-type-p ((type1 array-type-reference)
                         (type2 array-type-reference))
  (equal-type-p (array-type-reference-subtype type1)
                (array-type-reference-subtype type2)))

(defmethod super-type-p ((type1 array-type-reference)
                         (type2 array-type-reference))
  (super-type-p (array-type-reference-subtype type1)
                (array-type-reference-subtype type2)))

(defmethod copy-type ((type array-type-reference))
  (make-instance 'array-type-reference
                 :subtype (copy-type (array-type-reference-subtype type))
                 :original-form (ast-node-form type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-list-syntax type-reference-list (linj-list-node) type-reference)

(def-unparse type-reference-list (e)
  (format t "(~@<~{~/pp/~^, ~_~}~:>)"
          (type-reference-list-elements e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Besides the get-type protocol, there's a parallel protocol that
;;deals with "special" types such as multiple values or futures or
;;whatever.  The idea is that these types should be processed as
;;normal types (e.g., to infer a method return type) but must 'decay'
;;onto other basic types in order to be usable, e.g., as arguments to
;;numerical operations.  To give a more concrete example, let's think
;;about (+ (values 1 2) 3).  Although the values expression might be
;;implemented as an array of a certain type, the '+' operation just
;;wants to use the principal value and this value is of int type.

;;To implement this, get-type will return the non-decayed type and
;;get-principal-type will return the decayed type.

;;For any expression, the principal type is the principal type of the expression's type
(defmethod get-principal-type ((e expression))
  (get-principal-type (get-type e)))

;;By omisson, get-principal-type is synonym with get-type
(defmethod get-principal-type ((e type-reference))
  (get-type e))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;To help assign types to the arguments of some forms

(defun real-the-form-p (form)
  (and (consp form)
       (eq (first form) 'real-the)))

(defmacro def-real-the-form (name &rest types)
  (let ((args (mapcar #'(lambda (n)
                          (conc-symbol '?arg n))
                      (iota (1- (length types))))))
    `(def-macro-transform expression (,name ,@args)
       (unless (or ,@(mapcar #'(lambda (arg)
                                 `(real-the-form-p ,arg))
                             args))
         `(,',name ,,@(mapcar #'(lambda (arg type)
                                  (if (eq type 't)
                                      arg
                                      ``(real-the ,',type ,,arg)))
                              args
                              types))))))
