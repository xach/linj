(sb-ext:run-program "javac" (list "JavaToLMI.java")
                    :search t :output t)

(assert (probe-file "JavaToLMI.class"))

(load "load")

(linj2java-directory "linj/")

(let* ((*default-pathname-defaults* (merge-pathnames "linj/"))
       (files (directory "*.java")))
  (format t "Compiling ~D java files...~%" (length files))
  (sb-ext:run-program "javac" (mapcar #'file-namestring files)
                      :search t :output t
                      :directory *default-pathname-defaults*))

(format t "DONE~%")
