;;; -*- Mode: Common-Lisp; Syntax: Common-Lisp; Package: LINJ; Base: 10 -*-

;;; Copyright (C) Antonio Menezes Leitao Created on Thu May  2 20:01:15 2002
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

(defstruct server-process
  stream
  process)

(defun get-current-directory ()
  #+cmu (ext:default-directory)
  #+openmcl (ccl::current-directory-name)
  #+clisp (ext::default-directory)
  #+allegro (excl:current-directory)
  #+lispworks (hcl:get-working-directory)
  #+sbcl (sb-unix:posix-getcwd/)
  #-(or cmu openmcl clisp allegro lispworks sbcl) (error "Lisp implementation not supported"))

(defun get-env (var)
  #+cmu (cdr (assoc var ext:*environment-list* :test #'equalp :key #'string))
  #+openmcl (ccl::getenv var)
  #+clisp (ext::getenv var)
  #+allegro (sys::getenv var)
  #+lispworks (lw:environment-variable var)
  #+sbcl (sb-ext:posix-getenv var)
  #-(or cmu openmcl clisp allegro lispworks sbcl) (error "Lisp implementation not supported"))

(defparameter *server-dir-path*
  (let ((classpath (get-env "CLASSPATH"))
	(file-dir (namestring
		   (make-pathname :host (pathname-host *load-truename*)
				  :device (pathname-device *load-truename*)
				  :directory (pathname-directory *load-truename*)))))
    (if classpath
      (concatenate 'string
		   file-dir
		   #+(and allegro mswindows) ";"
		   #-(and allegro mswindows) ":"
		   classpath)
      file-dir)))

(defun create-server-process ()
  (make-server-process))

(defun get-server-process-stream (process)
  (unless (server-process-stream process)
    #+cmu
    (let ((proc (ext:run-program "java" `("-classpath" ,*server-dir-path* "JavaToLMI")
				 :input :stream
				 :output :stream
				 :error *trace-output*
				 :wait nil)))
      (unless proc
	(error "Couldn't start Java process.  Is J2SDK properly installed?"))
      (setf (server-process-process process) proc)
      (setf (server-process-stream process)
	    (make-two-way-stream (ext:process-output proc)
				 (ext:process-input proc))))

    #+openmcl
    (let ((proc (ccl:run-program "java" `("-classpath" ,*server-dir-path* "JavaToLMI")
				 :input :stream
				 :output :stream
				 :error *trace-output*
				 :wait nil)))
      (unless proc
        (error "Couldn't start Java process.  Is J2SDK properly installed?"))
      (setf (server-process-process process) proc)
      (setf (server-process-stream process)
            (make-two-way-stream (ccl:external-process-output-stream proc)
                                 (ccl:external-process-input-stream proc))))
    
    #+clisp
    (let ((proc-stream
	   (ext:run-program "java"
			    :arguments `("-classpath" ,*server-dir-path* "JavaToLMI")
			    :input :stream
			    :output :stream
			    :wait t)))
      (setf (server-process-stream process) proc-stream))

    #+sbcl
    (let ((proc (sb-ext:run-program "java" `("-classpath" ,*server-dir-path* "JavaToLMI")
				    :search t
				    :input :stream
				    :output :stream
				    :error *trace-output*
				    :wait nil)))
      (unless proc
	(error "Couldn't start Java process.  Is J2SDK properly installed?"))
      (setf (server-process-process process) proc)
      (setf (server-process-stream process)
	    (make-two-way-stream (sb-ext:process-output proc)
				 (sb-ext:process-input proc))))
    
    #+allegro
    (multiple-value-bind (proc-stream err-stream)
	#+mswindows (excl:run-shell-command (format nil "java -classpath ~S JavaToLMI" *server-dir-path*)
					    :input :stream
					    :output :stream
					    :wait nil
					    :show-window :hide)
	#-mswindows (excl:run-shell-command `#("java" "java" "-classpath" ,*server-dir-path* "JavaToLMI")
					    :input :stream
					    :output :stream
					    ;;:error-output *trace-output* slime changes this stream
					    :wait nil)
      (declare (ignore err-stream))
      (setf (server-process-stream process) proc-stream))

    #+lispworks
    (let ((proc-stream
	   (sys::open-pipe (format nil "java -classpath ~A JavaToLMI" *server-dir-path*)
			   :direction :io
			   :buffered nil)))
      (setf (server-process-stream process) proc-stream)))
  (server-process-stream process))

(defun kill-server-process (process)
  (close (server-process-stream process))
  #+cmu (ext:process-close (server-process-process process))
  #+openmcl (ccl:signal-external-process (server-process-process process) 9)
  #+allegro (system:os-wait)
  (setf (server-process-stream process) nil))
  

(defun ask-server (process what)
  (handler-case (let ((proc-stream (get-server-process-stream process)))
		  (format proc-stream "~a~%" what)
		  (force-output proc-stream)
		  (read proc-stream))
    (error (e) 
      (warn "~&The server got an error ~A." e)
      (kill-server-process process))))
