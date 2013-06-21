;;; -*- Mode: Common-Lisp; Syntax: Common-Lisp; Package: LINJ; Base: 10 -*-

;;; Copyright (C) Antonio Menezes Leitao Created on Mon Feb 26 00:23:11 2001
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;First of all, types:
;;There are primitive types and reference types.  Primitive types have the syntax
;;(primitive-type name), where name is one of "boolean", "byte", "short",
;;"int", "long", "char", "float" and "double".
;;Example: (primitive-type "int").
;;There are two types of references types: class-or-interface types and
;;array-types.  Array types have the syntax (array-type type) where type is
;;another type (primitive or reference).  Class or Interface types are of the form
;;(class-or-interface-type package name), where package is a list of strings and name is a string.
;;Example: (class-or-interface-type (java lang) Vector)
;;A more complex example is:
;;(array-type (array-type (class-or-interface-type (java lang) Object)))
;;In Java notation, this would be "java.lang.Object[][]".

;;Since it is a little painfull to write such a long syntax, we will
;;provide a more compact form.  In fact, we will implement precisely the
;;Java syntax within "<" and ">".
;;A type such as <java.lang.Vector[][]> will be read as
;;(array-type (array-type (class-or-interface-type (java lang) Vector)))

;;One interesting caracteristic of the primitive types is that they can't
;;be confused with any other types (independently of packages).  This
;;allows us to use unique Lisp objects so that we can compare them with eq.

;;NOTE: There's a huge difference between the linj language _description_ of a
;;type and the parsed result of such description.  Don't confuse them.

(defun read-until-char (stream char &optional (test #'identity))
  (let ((read-char (read-char stream nil char)))
    (assert (funcall test read-char))
    (if (char= read-char char)
        ()
        (cons read-char (read-until-char stream char test)))))

(defun read-string-until-char (stream char &optional (test #'identity))
  (coerce (read-until-char stream char test) 'string))

(defun split-at-char (string char)
  (let ((pos (position char string)))
    (if (null pos)
        (list string)
        (cons (subseq string 0 pos)
              (split-at-char (subseq string (1+ pos)) char)))))

(defun parse-dots (descr)
  (split-at-char descr #\.))

(defvar *primitive-types-description* nil
  "The list of primitive types")

(defun primitive-type-description-p (type-descr)
  (member type-descr *primitive-types-description*))

(defun unknown-package ()
  :unknown)

(defun unknown-package-p (pkg)
  (eq pkg :unknown))

(defun empty-package ()
  '(empty))

(defun empty-package-p (pkg)
  (package= pkg (empty-package)))

(defun package= (pkg1 pkg2)
  (assert (and (listp pkg1) (listp pkg2)))
  (equal pkg1 pkg2))

(defun standard-package-p (pkg)
  (package= pkg '(java lang)))

(defparameter *infer-unknown-packages* t)

(defun parse-java-type (descr)
  (let ((array-pos (position #\[ descr :from-end t)))
    (if array-pos
        `(array-type ,(parse-java-type (subseq descr 0 array-pos)))
        (let ((sub-descrs (mapcar #'read-from-string (parse-dots descr))))
          (let ((name (first (last sub-descrs)))
                (pkg (butlast sub-descrs)))
            (let ((prim (member name *primitive-types-description*
                                :test #'eq
                                :key #'second)))
              (if prim
                  (if (not (null pkg)) ;;this could be an error, but it allows
                      ;;a convention where, e.g., <java.lang.long> represents
                      ;;<java.lang.Long>, while <long> is just the primitive type
                      `(class-or-interface-type
                        ,pkg
                        ,name)
                      (first prim))
                  `(class-or-interface-type
                    ,(if (null pkg)
                         (if *infer-unknown-packages*
                             (unknown-package)
                             (empty-package))
                         pkg)
                    ,name))))))))

;;Java to linj names:

(defun valid-in-java-name-p (char)
  (or (alphanumericp char)
      (member char '(#\_ #\. #\$) :test #'char=)))

                                        ; (defun java-mixed-case-to-linj-name (java-name)
                                        ;   (coerce
                                        ;    (cons (char-upcase (char java-name 0))
                                        ;        (loop
                                        ;          :for i :from 1 :below (length java-name)
                                        ;          :for ch := (char java-name i)
                                        ;          :for previous-ch := (char java-name (1- i))
                                        ;          :if (and (alphanumericp previous-ch)
                                        ;                   (or (upper-case-p ch)
                                        ;                       (digit-char-p ch)))
                                        ;          :collect #\- :and :collect ch
                                        ;          :else :if (valid-in-java-name-p ch) :collect (char-upcase ch)
                                        ;          :else :do (error "unknown char '~a' in java name" ch)))
                                        ;    'string))

(defun java-mixed-case-to-linj-name (java-name)
  (coerce
   (let ((new-chars (list (char-downcase (char java-name 0)))))
     (dotimes (i (1- (length java-name)) (nreverse new-chars))
       (let ((previous-ch (char java-name i))
             (ch (char java-name (1+ i))))
         (cond ((and (alphanumericp previous-ch)
                     (or (upper-case-p ch)
                         (digit-char-p ch)))
                (push #\- new-chars)
                (push (char-downcase ch) new-chars))
               ((valid-in-java-name-p ch)
                (push (char-downcase ch) new-chars))
               (t
                (error "unknown char '~a' in java name" ch))))))
   'string))

(defun java-name-to-linj-name-string (java-name)
  (if (and (some #'upper-case-p java-name)
           (some #'lower-case-p java-name))
      (java-mixed-case-to-linj-name java-name)
      ;; else all upper or lower possibly with #\_
      (substitute #\- #\_ (string-downcase java-name))))

(defun linj-name-to-linj-file-name (name)
  (princ-to-string name))

;;;;;;;;;;;;;;;;
(defparameter *standard-read-table* (copy-readtable))

(defparameter *linj-type-readtable* (copy-readtable))
(setf (readtable-case *linj-type-readtable*) :invert)

(defun read-java-type-or-symbol (stream char)
  (unread-char char stream)
  (let ((*readtable* *linj-type-readtable*))
    (let ((token (read stream t nil t)))
      (if (symbolp token)
          (let ((str (princ-to-string token)))
            (if (potential-java-type-token str)
                (parse-java-type (subseq str 1 (1- (length str))))
                token))
          token))))

(defparameter *linj-readtable* (copy-readtable *linj-type-readtable*))

(set-macro-character #\< #'read-java-type-or-symbol t *linj-readtable*)

;;All parses must use this readtable (with-linj-syntax does it)

(defun parse (form &optional (+category nil))
  (with-linj-syntax ()
    (or (try-to-parse form +category)
        (error "Couldn't parse form ~S in +category ~A"
               form +category))))

(defun potential-java-type-token (tok)
  (and (char= (char tok 0) #\<)
       (let ((last-pos (1- (length tok))))
         (and (char= (char tok last-pos) #\>)
              (do ((i 1 (1+ i)))
                  ((= i last-pos) t)
                (let ((char (char tok i)))
                  (unless (or (alphanumericp char)
                              (member char '(#\- #\_ #\. #\$ #\[ #\] #\/) :test #'char=))
                    (return nil))))))))

;;Regarding primitive types, we need some extra operations: easy naming,
;;easy recognizing and promotion:

;;We need primitive type descriptions and ast-recognizers:

(defmacro define-type-expansion (name form)
  (let ((parameter-name (conc-symbol '* name '-type-description*)))
    (let ((constructor-name (conc-symbol name '-type-description))
          (recognizer-name (conc-symbol name '-type-description-p)))
      `(progn
         (defvar ,parameter-name ',form)
         (defun ,constructor-name ()
           ,parameter-name)
         (defun ,recognizer-name (obj)
           (or (eq obj ,parameter-name) ;;We use eq bc the reader uniquify primitive types
               (equal obj ,parameter-name)))
         (defun ,(conc-symbol name '-type) ()
           (parse ,parameter-name 'type-reference))
         (defun ,(conc-symbol name '-type-p) (obj)
           (equal-type-p obj (parse ,parameter-name 'type-reference)))
;;;        (,recognizer-name (ast-node-form obj)))
         ',parameter-name))))


;;Every symbol of the form <name> is a type name
(defun type-description-p (any)
  (and (listp any)
       (not (null any))
       (member (first any) '(primitive-type array-type class-or-interface-type))))

;;;Primitive types:
;;this must be done prior to using the *linj-readtable*

(defmacro def-primitive-type (name)
  (let ((form `(primitive-type ,name)))
    `(progn
       (define-type-expansion ,name ,form)
       (pushnew ',form *primitive-types-description*))))

(def-primitive-type byte)
(def-primitive-type short)
(def-primitive-type int)
(def-primitive-type long)
(def-primitive-type boolean)
(def-primitive-type float)
(def-primitive-type double)
(def-primitive-type char)
(def-primitive-type void)
