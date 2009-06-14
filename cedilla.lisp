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

(in-package "CEDILLA")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *external-format-utf8*
    #+clisp 'charset:utf-8
    #+ccl (ccl:make-external-format :character-encoding :utf-8)))

(defun cedilla (in-filename &optional (out-filename *standard-output*)
                &key
                (wrap nil)
                (header nil) (header-baseline (/ 72 2.54))
                (footer nil) (footer-baseline (/ 72 2.54))
                (fontset "courier")
                (paper-size "a4")
                (size 11)
                (baselineskip size))
  "Main Cedilla entry point."
  (let ((font-list (find-fontset fontset))
        (paper (find-paper-size paper-size)))
    (let ((*known-glyphs* (make-hash-table :test 'equal)))
      (dolist (s (append header footer))
        (when s (compute-header-glyphs s font-list)))
      (with-open-file (in in-filename :external-format *external-format-utf8*)
        (compute-stream-glyphs in font-list))
      (with-open-file (in in-filename :external-format *external-format-utf8*)
        (with-open-file-or-stream (out out-filename
                                       :direction :output
                                       :if-exists :supersede)
          (typeset-file in out font-list
                        paper
                        :wrap wrap
                        :header header :footer footer
                        :header-baseline header-baseline
                        :footer-baseline footer-baseline
                        :size size :baselineskip baselineskip))))))

(defun cedilla-list-all (&optional (fontset "courier"))
  (let ((font-list (find-fontset fontset)))
    (let ((*known-glyphs* (make-hash-table :test 'equal)))
      (loop for i from 0 upto #xFFFF
            do (let* ((c (code-char i))
                      (ccs (normalise-ccs c))
                      (g (compute-glyph ccs font-list nil)))
                 (when g
                   (if (ccs-combining-class ccs)
                       (format t "~04X:  ~C ~S~%" i c c)
                     (format t "~04X: ~C ~S~%" i c c))))))))

(defun find-fontset (name)
  (let ((spec (get (intern (string-upcase name) "CEDILLA") 'fontset)))
    (when (null spec)
      (error "Unknown fontset ~A" name))
    (or
     (mapcan
      #'(lambda (entry)
          (let ((font
                 (ecase (car entry)
                   ((:afm) (apply #'read-afm-file (cdr entry)))
                   ((:space) (apply #'make-space-font (cdr entry)))
                   ((:built-in) (apply #'make-built-in-font (cdr entry))))))
           (if font (list font) '())))
      spec)
     (error "Fontset ~A contains no useable fonts" name))))
                 
(defun find-paper-size (name)
  (or (get (intern (string-upcase name) "CEDILLA") 'paper-size)
      (error "Unknown paper size ~A" name)))

(defun split-comma-string (string)
  (do* ((strings '())
        (s 0 (and e (+ e 1)))
        (e (position #\, string) (and s (position #\, string :start s))))
      ((not s) (nreverse strings))
    (push (subseq string s e) strings)))

(defun parse-header (string)
  (let ((l (split-comma-string string)))
    (case (length l)
      ((1) (list nil (car l) nil))
      ((2) (list (car l) (cadr l) nil))
      ((3) l)
      (t (error "A header or footer may have at most three elements")))))

(defun cedilla-with-args (args)
  (let ((fontset nil) (paper-size nil) (size nil) (baselineskip nil)
        (wrap nil)
        (header nil) (header-baseline nil) (footer nil) (footer-baseline nil)
        (list nil))
    (loop
     (cond
      ((null args) (return nil))
      ((equal (car args) "-fs")
       (setf fontset (cadr args) args (cddr args)))
      ((equal (car args) "-p")
       (setf paper-size (cadr args) args (cddr args)))
      ((equal (car args) "-s")
       (let ((s (read-from-string (cadr args))))
         (unless (realp s)
           (error "Size should be a number, not ~A" (cadr args)))
         (setf size s args (cddr args))))
      ((equal (car args) "-bs")
       (let ((b (read-from-string (cadr args))))
         (unless (realp b)
           (error "Baselineskip should be a number, not ~A" (cadr args)))
         (setf baselineskip b args (cddr args))))
      ((equal (car args) "-w")
       (setf wrap t args (cdr args)))
      ((equal (car args) "-h")
       (setf header (parse-header (cadr args)) args (cddr args)))
      ((equal (car args) "-hb")
       (let ((hb (read-from-string (cadr args))))
         (unless (realp hb)
           (error "Header-baseline should be a number, not ~A" (cadr args)))
         (setf header-baseline hb args (cddr args))))
      ((equal (car args) "-f")
       (setf footer (parse-header (cadr args)) args (cddr args)))
      ((equal (car args) "-fb")
       (let ((fb (read-from-string (cadr args))))
         (unless (realp fb)
           (error "Footer-baseline should be a number, not ~A" (cadr args)))
         (setf footer-baseline fb args (cddr args))))
      ((equal (car args) "-v")
       (setf *cedilla-verbose* t args (cdr args)))
      ((equal (car args) "-l")
       (setf list t args (cdr args)))
      ((equal (car args) "-V")
       (format *error-output* 
               "Cedilla v. ~A, by Juliusz Chroboczek.~%" *cedilla-version*)
       (quit 0))
      ((or (equal (car args) "-?")
           (equal (car args) "-help") (equal (car args) "--help"))
       (format *error-output*
               "cedilla [-v] [-fs fontset] [-p papersize] [-s size] ~
                   [-bs baselineskip]~%")
       (format *error-output*
               "        [-h header] [-hb header-baseline ~
                         [-f footer] [-fb footer-baseline]~%")
       (format *error-output*
               "        infile [outfile]~%")
       (quit 0))
      ((equal (car args) "--")
       (setf args (cdr args)) (return nil))
      ((eql (aref (car args) 0) #\-)
       (error "Unknown option ~A -- try \"cedilla -?\" for help" (car args)))
      (t (return nil))))
    (cond
     (list (apply #'cedilla-list-all (and fontset (list fontset))))
     (t    
      (unless (<= 1 (length args) 2)
        (error "Wrong number of arguments -- try \"cedilla -?\" for help"
               (length args)))
      (apply #'cedilla
             (car args) (or (cadr args) *standard-output*)
             (nconc
              (if fontset (list :fontset fontset))
              (if wrap (list :wrap wrap))
              (if header (list :header header))
              (if header-baseline (list :header-baseline header-baseline))
              (if footer (list :footer footer))
              (if footer-baseline (list :footer-baseline footer-baseline))
              (if paper-size (list :paper-size paper-size))
              (if size (list :size size))
              (if baselineskip (list :baselineskip baselineskip))))))))

#+clisp
(defun cedilla-main ()
  "Establish an error-handler and run Cedilla."
  (handler-bind ((error 
                  #'(lambda (c) 
                      (format *error-output* "Cedilla: ~A." c)
                      (quit 1))))
    (cedilla-with-args *args*)
    (quit 0)))
