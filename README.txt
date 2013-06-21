README for Linj_1.x

January 22, 2011



Overview
--------

Linj is a Common Lisp-like language that tries to be as similar to
Common Lisp as possible but allowing Linj programs to be compiled into
human-readable Java code.

Linj was originally created by eValuator, Lda, and this code was last
updated in 2004. The software is no longer supported or maintained by
eValuator or António Menezes Leitão. In January, 2011, António Menezes
Leitão gave Zachary Beane permission to publish the sources under the
MIT license.



License
-------

This Linj distribution is available under the MIT license. See the
file LICENSE.txt for details.



Requirements
------------

The Linj compiler is written in Common Lisp and currently runs on

 - CMU Common Lisp 19a for Linux
 - SBCL 0.8.15 for Linux and MacOSX
 - Allegro CL 6.2 for Linux and Windows

You should download the version that matches your working environment.

Linj requires a JDK that conforms to the Java 1.1 Core API.



Installation
-----------

Assuming that you extracted the files in the Linj distribution to the
path <path>, start your Common Lisp environment and evaluate the
expression:

(load "<path>/load")



Use
----------

Put your Linj program into a file named according to the Linj
conventions for classes (lowercase with hyphens), e.g., test.linj and
invoke the Linj compiler using the (quoted) name of the file (without
extension) and (optionally) the pathname to the directory containing
that file.  As an example, assuming that test.linj is in /tmp, use:

(linj2java 'test "/tmp/")

If the file test.linj is in the current directory, you can omit the
second argument.

If you have several linj files in a directory you can compile all of
them using

(linj2java-directory <path to the directory>)

The <path to the directory> defaults to the current directory.



Support Files
-------------

This Linj distribution also includes several pre-defined supporting
classes (defined in Linj itself).  You will find them on the
<path>/linj/ subdirectory.  For these classes to be useful, you must
define your CLASSPATH to include the directory containing the file you
are just reading.  As an example, if your Linj distribution is on the
path "/home/user/linj_1.0/" then you should set your CLASSPATH as
follows:

export CLASSPATH=/home/user/linj1.0/:$CLASSPATH



Documentation
-------------

You can find a Linj tutorial in doc/tutorial.pdf.

One (unfinished) manual can be found at doc/linj-manual.pdf



Feedback & Support
------------------

Linj is no longer officially supported or maintained and is available
only on an as-is basis.


-----------------------------------------------------------------------
"Java" is a registered trademark of Sun Microsystems.
