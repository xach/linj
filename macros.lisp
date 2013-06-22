;;; -*- Mode: Common-Lisp; Syntax: Common-Lisp; Package: LINJ; Base: 10 -*-

;;; Copyright (C) Antonio Menezes Leitao Created on Thu Nov 23 20:09:09 2000
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

;;Basic constants

(defmethod java-constant-name ((value t))
  (princ-to-string value))

(defmacro def-basic-constant (name &optional
                                     (value name)
                                     (print-name (string-downcase (symbol-name name))))
  (let ((constant-name (conc-symbol '* name '-value*)))
    `(progn
       (defparameter ,constant-name ',value)
       (defun ,(conc-symbol name '-value) ()
         ,constant-name)
       (defun ,(conc-symbol name '-value-p) (obj)
         (eq obj ,constant-name))
       (defmethod java-constant-name ((value (eql ',value)))
         ,print-name))))

(defun without-<> (str)
  (unless (and (char= (char str 0) #\<) (char= (char str (1- (length str))) #\>))
    (error "~A is not a class name" str))
  (subseq str 1 (- (length str) 1)))

(defun with-<> (str)
  (when (or (char= (char str 0) #\<) (char= (char str (1- (length str))) #\>))
    (error "~A is a class name" str))
  (concatenate 'string "<" str ">"))

(defun linj-name-to-java-name (name)
  (if (char= (char name 0) #\+) ;;constants
      (linj-name-to-java-name (substitute #\_ #\- (string-upcase (remove #\+ name))))
      (replace-java-invalid-chars name)))

(defun replace-java-invalid-chars (name)
  (labels ((iter (i)
             (if (= i (length name))
                 (list)
                 (let ((c (char name i)))
                   (let ((special
                           (case c
                             (#\- "")
                             (#\! "bang")
                             (#\# "sharp")
                             (#\% "percent")
                             (#\& "and")
                             (#\* "star")
                             (#\+ "plus")
                             (#\. "dot")
                             (#\/ "slash")
                             (#\{ "lbracket")
                             (#\} "rbracket")
                             (#\< "less")
                             (#\= "equal")
                             (#\> "greater")
                             (#\^ "up")
                             (#\~ "tilde")
                             (#\@ "at")
                             (#\? "p")
                             (#\: "colon")
                             (t nil))))
                     (if special
                         (nconc (coerce (if (= i 0) special (string-capitalize special)) 'list)
                                (let ((others (iter (1+ i))))
                                  (and others
                                       (cons (char-upcase (first others))
                                             (rest others)))))
                         (cons c
                               (iter (1+ i)))))))))
    (coerce (iter 0) 'string)))


(defun linj-name-to-java-type-name (name)
  (if (some #'upper-case-p name)
      name
      (delete #\- (string-capitalize name))))


(defun filename-from-class-name (id)
  (format nil "~(~A~).linj" id))

;;pretty-print expression
(defparameter *print-out-parenthesis* t)

(defparameter *print-out-init-braces* t)

(defun pp (stream obj colonp atp)
  (declare (ignore colonp atp))
  ;;   (let ((*print-out-parenthesis* (not colonp))
  ;;    (*print-out-init-braces* (not atp)))
  (unparse-object obj stream));)

(defun ppexp (stream exp colonp atp)
  (let ((*print-out-parenthesis* (not colonp))
        (*print-out-init-braces* (not atp)))
    (unparse-object exp stream)))

(defun ppmultiargs (stream oper-args colonp atp)
  (let ((args (rest oper-args)))
    (if (endp (rest args))
        (ppexp stream (first args) colonp atp)
        (let ((operator-unparse (first oper-args))
              (*print-out-parenthesis* (not colonp))
              (*print-out-init-braces* (not atp)))
          (format stream (if *print-out-parenthesis*
                             "~@<(~/ppmultiargs/ ~A~_ ~/ppexp/)~:>"
                             "~@<~/ppmultiargs/ ~A~_ ~/ppexp/~:>")
                  (cons operator-unparse (butlast args))
                  operator-unparse
                  (first (last args)))))))

;;pretty-print statement and block
(defparameter *print-out-braces* t)

(defun ppstm (stream exp colonp atp)
  (declare (ignore atp))
  (let ((*print-out-braces* (not colonp)))
    (if *print-out-braces*
        (format stream "{~4I~:@_~/pp/~I~:@_}" exp)
        (format stream "~/pp/" exp))))

(defun ppblk (stream exp colonp atp)
  (declare (ignore atp))
  (let ((*print-out-braces* (not colonp)))
    (unparse-object exp stream)))

;;pretty-print initializer
(defparameter *print-out-initializer* t)

(defun ppinit (stream exp colonp atp)
  (declare (ignore atp))
  (let ((*print-out-initializer* (not colonp)))
    (unparse-object exp stream)))


;;to build keywords respecting the readtable-case

(defun make-keyword (sym)
  (read-from-string (format nil ":~A" sym)))

(defmacro with-linj-syntax ((&rest args) &body body)
  (declare (ignore args))
  `(let ((*readtable* *linj-readtable*)
         (*read-default-float-format* 'short-float)
         (*package* (find-package "LINJ")))
     ,@body))

(defun expand-call-function (f)
  (cond ((and (consp f) (eq (first f) 'function)) `(,(second f)))
        ((and (consp f) (eq (first f) 'lambda)) `(,f))
        (t `(funcall ,f))))

(defmacro compose (f g)
  `#'(lambda (arg)
       (,@(expand-call-function f) (,@(expand-call-function g) arg))))


(defmacro curry (f arg1)
  `(let ((#1=#:arg1-val ,arg1))
     #'(lambda (arg2)
         (,@(expand-call-function f) #1# arg2))))

(defmacro rcurry (f arg2)
  `(let ((#1=#:arg2-val ,arg2))
     #'(lambda (arg1)
         (,@(expand-call-function f) arg1 #1#))))
