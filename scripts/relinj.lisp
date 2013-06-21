(sb-ext:run-program "javac" (list "JavaToLMI.java") :search t)

(assert (probe-file "JavaToLMI.class"))

(load "load")

(linj2java-directory "linj/")

(format t "DONE~%")
