;;; -*- Mode: Common-Lisp; Syntax: Common-Lisp; Package: LINJ; Base: 10 -*-

;;; Copyright (C) Antonio Menezes Leitao Created on Thu Apr  5 12:03:16 2001
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;A special statement for debugging:
(def-macro println (arg &optional (stream '*standard-output*))
  `(send ,stream println ,arg))

(def-macro print (arg &optional (stream '*standard-output*))
  `(send ,stream print (concat #\newline ,arg " ")))

(def-macro pprint (arg &optional (stream '*standard-output*))
  `(send ,stream print (concat #\newline ,arg)))

(def-macro princ (arg &optional (stream '*standard-output*))
  `(send ,stream print ,arg))

(def-macro prin1 (arg &optional (stream '*standard-output*))
  `(princ ,arg ,stream))

(def-macro warn (&rest args)
  `(format *trace-output* ,@args))

(def-macro terpri (&optional (stream '*standard-output*))
  `(send ,stream println))

(def-macro fresh-line (&optional (stream '*standard-output*))
  `(terpri ,stream))

(def-macro write-char (char &optional (stream '*standard-output*))
  `(princ (real-the char ,char) ,stream))

(defun subseq-string-form (string start end)
  (cond ((or (/= start 0) end)
         `(send ,(subseq-string-form string 0 nil) substring ,start ,@(and end (list end))))
        ((stringp string)
         string)
        (t
         `(real-the java.lang.string ,string))))

(def-macro write-string (string &optional (stream '*standard-output*)
                                &key (start '0) end)
  `(send ,stream print ,(subseq-string-form string start end)))

(def-macro write-line (string &optional (stream '*standard-output*)
                              &key (start '0) end)
  `(send ,stream println ,(subseq-string-form string start end)))

(def-macro write-byte (byte stream)
  `(send ,stream write (real-the byte ,byte)))

;; (def-macro read-line (&optional (stream '*standard-input*))
;;   (if (eq stream '*standard-input*)
;;     `(send ,stream read-line)
;;     `(send (new 'java.io.buffered-reader (new 'java.io.input-stream-reader ,stream)) read-line)))

(def-macro read-line (&optional (stream '*standard-input*) eof-error-p eof-value recursive-p)
  (assert (null eof-error-p)) (assert (null eof-value)) (assert (not recursive-p))
  (if (eq stream '*standard-input*)
      `(send (new 'java.io.buffered-reader (new 'java.io.input-stream-reader *standard-input*)) read-line)
      `(send ,stream read-line)))

;;Format

;; (defun old-emit-format-code (type fmt stream full-args)
;;   (let ((length (length fmt)))
;;     (labels ((iterate (i args)
;;             (if (= i length)
;;               (list)
;;               (case (char fmt i)
;;                 ((#\~) ;;'dispatch' char
;;                  (incf i)
;;                  (let ((numeric-arg nil)
;;                        (colonp nil)
;;                        (atp nil))
;;                    (when (char<= #\0 (char fmt i) #\9)
;;                      (multiple-value-bind (num pos)
;;                          (parse-integer fmt :start i :junk-allowed t)
;;                        (setf numeric-arg num
;;                              i pos)))
;;                    (case (char fmt i)
;;                      ((#\:) ;;optional args
;;                       (setf colonp t)
;;                       (incf i)))
;;                    (case (char fmt i)
;;                      ((#\@) ;;optional args
;;                       (setf atp t)
;;                       (incf i)))
;;                    (case (char fmt i)
;;                      ((#\c #\C) ;;Character
;;                       (cond ((endp args)
;;                              (error "Not enough arguments for format string ~S ~S" fmt full-args))
;;                             (numeric-arg
;;                              (error "~~C does not accept numeric prefixes (in ~S)" fmt))
;;                             (t
;;                              (cons `((the char ,(first args)))
;;                                    (iterate (+ i 1) args)))))
;;                      ((#\%  ;;Newline
;;                        #\&) ;;Fresh-line
;;                       (nconc (make-list (or numeric-arg 1) :initial-element `newline)
;;                              (iterate (+ i 1) args)))
;;                      ((#\|) ;;Page
;;                       (nconc (make-list (or numeric-arg 1) :initial-element `(#\Page))
;;                              (iterate (+ i 1) args)))

;;                      ((#\~) ;;Tilde
;;                       (nconc (make-list numeric-arg :initial-element `(#\~))
;;                              (iterate (+ i 1) args)))
;;                      ((#\d #\D ;;Decimal
;;                        #\S #\s
;;                        #\A #\a) ;for the moment, they are all equal
;;                       (if (endp args)
;;                         (error "Not enough arguments for format string ~S ~S" fmt full-args)
;;                         (cons `(,(first args))
;;                               (iterate (+ i 1) args))))
;;                      ((#\*) ;;Go-to
;;                       (iterate (+ i 1)
;;                                (cond (atp
;;                                       (nthcdr (or numeric-arg 0) full-args))
;;                                      (colonp
;;                                       (nthcdr (- (length (ldiff full-args args)) (or numeric-arg 1)) args))
;;                                      (t
;;                                       (nthcdr (or numeric-arg 1) args)))))
;;                      (otherwise
;;                       (error "Unknown format directive ~~~C" (char fmt i))))))
;;                 (otherwise
;;                  (do ((j i (+ j 1)))
;;                      ((or (= j length)
;;                           (char= (char fmt j) #\~))
;;                       (if (= j i)
;;                         (list)
;;                         (cons `(,(subseq fmt i j))
;;                               (iterate j args)))))))))
;;           (emit-statements (exprs)
;;             (cond ((null exprs)
;;                    (list))
;;                   ((null (rest exprs))
;;                    (if (eq (first exprs) 'newline)
;;                      (list `(send ,stream println))
;;                      (list `(send ,stream print ,@(first exprs)))))
;;                   ((and (not (eq (first exprs) 'newline))
;;                         (eq (second exprs) 'newline))
;;                    (cons `(send ,stream println ,@(first exprs))
;;                          (emit-statements (nthcdr 2 exprs))))
;;                   ((eq (first exprs) 'newline)
;;                    (cons `(send ,stream println)
;;                          (emit-statements (rest exprs))))
;;                   (t
;;                    (cons `(send ,stream print ,@(first exprs))
;;                          (emit-statements (rest exprs))))))
;;           (emit-args (exprs result)
;;             (cond ((null exprs)
;;                    result)
;;                   ((null (rest exprs))
;;                    (if (eq (first exprs) 'newline)
;;                      `(,@result #\newline)
;;                      `(,@result ,@(first exprs))))
;;                   ((and (not (eq (first exprs) 'newline))
;;                         (eq (second exprs) 'newline))
;;                    (emit-args (nthcdr 2 exprs)
;;                               `(,@result ,@(first exprs) #\newline)))
;;                   (t
;;                    (emit-args (rest exprs)
;;                               `(,@result ,@(first exprs))))))
;;           (emit-expression (exprs)
;;             `(concat ,@(emit-args (rest exprs) (first exprs)))))
;;       (let ((elems (iterate 0 full-args)))
;;      (if (eq type :statement)
;;        (emit-statements elems)
;;        (emit-expression elems))))))

;;The following macro shows that the expansion depends on the parser
;;expectations.  In this particular case, we either want to deal with
;;statements or with expressions.
;;This also makes sense from the macro expansion point of view, as it knows
;;what it's trying to parse.  So the solution is to, somehow, include a
;;kind of 'environment' arg `a la Common Lisp.  Another option is to define
;;syntax specific versions of a macro. Not only is the right thing as it
;;also allows for a much more efficient version of the macro expansions.

;; (def-macro-transform statement (format nil ?fmt . ?args)
;;   (assert (stringp ?fmt))
;;   `(let ((buf (new 'string-buffer)))
;;      ,(emit-format-code :string-buffer
;;                      ?fmt
;;                      'buf
;;                      ?args)
;;      (to-string buf)))


(defun make-format-method-call (stream fmt args)
  (with-parent ((ast-node-parent stream))
    (make-instance 'method-call-expression
                   :name (parse 'format 'linj-name)
                   :receiver stream
                   :arguments (make-instance 'argument-list :elements (cons fmt (argument-list-elements args)))
                   :original-form '???)))

(defun exists-format-method-p (stream fmt args)
  (and (not (primitive-type-reference-p (get-type stream)))
       (find-declaration (make-format-method-call stream fmt args))))

;;Statements are tried first
(def-linj-macro statement (format ?stream-expression/expression ?fmt/expression . ?args/argument-list)
  (if (exists-format-method-p ?stream-expression ?fmt ?args)
      (fail)
      (let ((args (argument-list-elements ?args)))
        (assert (and (literal-p ?fmt) (stringp (literal-value ?fmt))))
        (let ((fmt (literal-value ?fmt)))
          (cond ((false-literal-p ?stream-expression)
                 `(let ((buf (new 'string-buffer)))
                    ,(emit-format-code :string-buffer
                                       fmt
                                       'buf
                                       args)
                    (to-string buf)))
                ((string-buffer-type-p (get-type ?stream-expression))
                 (emit-format-code :string-buffer fmt ?stream-expression args))
                ((or (literal-p ?stream-expression)
                     (reference-p ?stream-expression)
                     (slot-declaration-p (find-declaration ?stream-expression))
                     (endp (rest (emit-format-code :statement fmt ?stream-expression args))))
                 (emit-format-code :statement
                                   fmt
                                   (if (true-literal-p ?stream-expression) '*standard-output* ?stream-expression)
                                   args))
                (t
                 (let ((stream 'format-internal-stream))
                   `(let ((,stream ,?stream-expression))
                      ,(emit-format-code :statement
                                         fmt
                                         stream
                                         args)))))))))

;;Expressions second
(def-linj-macro expression (format ?stream-expression/expression ?fmt/expression . ?args/argument-list)
  (if (exists-format-method-p ?stream-expression ?fmt ?args)
      (fail)
      (let ((args (argument-list-elements ?args)))
        (assert (and (literal-p ?fmt) (stringp (literal-value ?fmt))))
        (let ((fmt (literal-value ?fmt)))
          (if (false-literal-p ?stream-expression)
              (emit-format-code :expression fmt 'stream args)
              `(send ,(if (true-literal-p ?stream-expression) '*standard-output* ?stream-expression)
                     print
                     ,(emit-format-code :expression fmt 'stream args)))))))


;;The following macros hide too much when errors happen...
(def-transform statement (with-open-thing (?var ?stream) . ?body)
  (let ((?var ?stream))
    (unwind-protect
         (progn . ?body)
      (unless (eq ?var null)
        (close ?var)))))

(def-transform statement (with-open-stream . ?args)
  (with-open-thing . ?args))


(def-macro with-open-file ((stream
                            filespec
                            &key
                            (direction :input)
                            (stream-class nil stream-class-p)
                            (decorator-classes nil decorator-classes-p))
                           &body body)
  (unless stream-class-p
    (setq stream-class
          (ecase direction
            ((:output) 'java.io.file-writer)
            ((:input nil) 'java.io.file-reader))))
  (unless decorator-classes-p
    (setq decorator-classes
          (ecase direction
            ((:output) '(java.io.print-writer))
            ((:input nil) '(java.io.buffered-reader)))))
  `(with-open-stream (,stream ,(reduce #'(lambda (decorator rest)
                                           `(new ',decorator ,rest))
                                       decorator-classes
                                       :from-end t
                                       :initial-value `(new ',stream-class ,filespec)))
     ,@body))

(def-transform method-call-expression (read-from-string ?string)
  (read (new 'linj-reader (new 'string-reader ?string))))


(def-linj-macro expression (to-base ?expr/expression ?base)
  (let ((type (get-type ?expr)))
    (let ((expr
            (if (object-type-p type)
                (ecase *numerical-object-is*
                  ((:int)
                   `(maybe-convert-to-type ,?expr ,(int-type)))
                  ((:long)
                   `(maybe-convert-to-type ,?expr ,(long-type)))
                  ((:bignum)
                   `(maybe-convert-to-type ,?expr ,(bignum-type))))
                ?expr)))
      (let ((method-call
              (case ?base
                ((10)  expr)
                ((2)   `(to-binary-string ,expr))
                ((8)   `(to-octal-string ,expr))
                ((16)  `(to-upper-case (to-hex-string ,expr)))
                (t     `(to-upper-case (to-string ,expr ,?base))))))
        (cond ((int-type-p type)
               `(in (the java.lang.Integer) ,method-call))
              ((or (long-type-p type) (double-type-p type) (float-type-p type))
               `(in (the java.lang.Long) ,method-call))
              ((or (big-integer-type-p type) (bignum-type-p type) (object-type-p type))
               (if (and (numberp ?base) (<= ?base 10))
                   `(to-string ,expr ,?base)
                   `(to-upper-case (to-string ,expr ,?base))))
              (t
               (error "Can't print object ~A in base ~A" expr ?base)))))))
