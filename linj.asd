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

(in-package :asdf)

(defmethod output-files ((op compile-op) (c java-source-file))
  (list (make-pathname :type "class" :defaults (component-pathname c))))

(defmethod perform ((op compile-op) (c java-source-file))
  (unless (= 0 (run-shell-command
		"javac -d ~S ~S"
		(namestring (make-pathname :name nil :type nil
					   :defaults (car (output-files op c))))
		(namestring (component-pathname c))))
    (error 'operation-error :operation op :component c)))

(defmethod perform ((operation load-op) (c java-source-file))
  t)



(defsystem :linj
    :components
    ((:file "package")
     (:file "util"
	    :depends-on ("package"))
     (:file "translator-macros" 
	    :depends-on ("util"))
     (:file "translator" 
            :depends-on ("translator-macros" "util"))
     (:file "macros" 
            :depends-on ("translator"))
     (:file "reader"
	    :depends-on ("macros"))
     (:file "backq"
	    :depends-on ("reader"))
     (:java-source-file  "JavaToLMI")
     (:file "server"
            :depends-on ("package"))
     (:file "basics"
	    :depends-on ("reader" "macros" "backq"))
     (:file "types"
	    :depends-on ("reader" "macros" "basics"))
     (:file "linj"
	    :depends-on ("reader" "macros" "types" "server"))
     (:file "type-inference"
	    :depends-on ("reader" "macros" "types" "linj"))
     (:file "imports"
	    :depends-on ("linj"))
     (:file "semantic-macros"
	    :depends-on ("linj"))
     (:file "chars"
	    :depends-on ("linj"))
     (:file "functions"
	    :depends-on ("linj" "semantic-macros"))
     (:file "loop"
	    :depends-on ("linj" "semantic-macros"))
     (:file "format"
	    :depends-on ("linj"))
     (:file "io"
	    :depends-on ("linj" "format" "semantic-macros"))
     (:file "list"
	    :depends-on ("linj" "semantic-macros"))
     (:file "vector"
	    :depends-on ("linj"))
     (:file "linj-macros"
	    :depends-on ("linj"))
     (:file "swing"
	    :depends-on ("linj"))))