;;; This file is part of Cedilla.
;;; Copyright (C) 2002 by Juliusz Chroboczek.

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

(in-package "CL-USER")

(load "package.lisp")

(defparameter *cedilla-sources*
  '("package.lisp" "macros.lisp" "struct.lisp" "generics.lisp"
    "unicode.lisp" "data.lisp" "data-add.lisp"
    "afm.lisp" "resources.lisp" "typeset.lisp"
    "glyphs.lisp" "ps.lisp" "encodings.lisp" "magic.lisp"
    "named-font.lisp" "fixed-font.lisp" "space-font.lisp" "built-in-font.lisp"
    "transformed.lisp"
    "cedilla.lisp"))

(defun compile-cedilla ()
  (mapc #'(lambda (filename) (load (compile-file filename)
                                   :verbose nil))
        *cedilla-sources*)
  (load "cedilla-config.lisp"))

(defun load-cedilla ()
  (mapc #'(lambda (filename) (load (compile-file-pathname filename)
                                   :verbose nil))
        *cedilla-sources*)
  (load "cedilla-config.lisp"))

(defun make-cedilla-binary (clisp binary installer bindir libdir etcdir mandir)
  (labels ((dir (s) (if (eql #\/ (aref s (- (length s) 1)))
                        s 
                        (concatenate 'string s "/")))
           (make-dir (dir out) (format out "mkdir -p \"$TARGET\"'~A'~%"
                                       (dir dir)))
           (target (file dir) 
             (make-pathname :defaults (pathname file)
                            :directory (pathname (dir dir))))
           (install (file dir out &optional executable (name file))
             (format out "cp -f '~A' \"$TARGET\"'~A'~%" file (target name dir))
             (when executable
               (format out "chmod +x \"$TARGET\"'~A'~%" (target name dir)))))
    (with-open-file (out binary :direction :output)
      (format out "#!~A -ansi -q~%" clisp)
      (format out ";;; Automatically generated file -- do not modify.~%")
      (print '(setq *load-verbose*  nil) out)
      (dolist (f *cedilla-sources*)
        (print `(load ,(target (compile-file-pathname f) libdir)) out))
      (print `(load ,(target "cedilla-config.lisp" etcdir)) out)
      (print `(cedilla:cedilla-main) out)
      (fresh-line out))
    (with-open-file (out installer :direction :output)
      (format out "#!/bin/sh~%")
      (format out "# Automatically generated file -- do not modify.~%")
      (make-dir bindir out)
      (make-dir libdir out)
      (make-dir etcdir out)
      (dolist (f *cedilla-sources*)
        (install (compile-file-pathname f) libdir out))
      (install "cedilla-config.lisp" etcdir out)
      (install "cedilla.man" mandir out nil "cedilla.1")
      (install "cedilla" bindir out t)
      (fresh-line out))))
