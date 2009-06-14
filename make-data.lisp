;;; This file is part of Cedilla.
;;; Copyright (C) 2002-2009 by Juliusz Chroboczek.

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

(defpackage "MAKE-CEDILLA-DATA"
  (:use "CL")
  (:import-from "CEDILLA"
                "*COMBINING-CLASS-TABLE*"
                "*CANONICAL-DECOMPOSITION-TABLE*"
                "*GLYPH-NAMES*"
                "NORMALISE-CCS")
  (:export "MAKE-DATA"))

(in-package "MAKE-CEDILLA-DATA")

(defun split-string (string &optional (separator #\;))
  (do ((pos (position separator string)
            (and pos (position separator string :start (+ pos 1))))
       (old-pos -1 pos)
       (strings '()))
      ((null old-pos) (nreverse strings))
    (push (subseq string (+ 1 old-pos) pos) strings)))

(defun read-data-line (in &optional (separator #\;))
  (let ((line (read-line in nil in)))
    (cond
      ((eql line in) in)
      ((zerop (length line)) nil)
      ((eql (aref line 0) #\#) nil)
      (t (split-string line separator)))))

(defun read-unicode-data-line (stream)
  (let ((line (read-data-line stream)))
    (declare (list line))
    (cond
      ((eql line stream) stream)
      ((null line) nil)
      (t
       (unless (>= (length line) 10)
         (error "Malformed UnicodeData line"))
       (let ((codepoint
              (let ((*read-base* 16)) (read-from-string (car line))))
             (combining-class 
              (if (zerop (length (nth 3 line)))
                  nil
                  (and (equal (nth 4 line) "NSM")
                       (read-from-string (nth 3 line)))))
             (decomp (parse-decomp (nth 5 line))))
         (list codepoint combining-class decomp))))))

(defun parse-decomp (string)
  (if (zerop (length string))
      nil
      (let* ((start 
              (if (eql (aref string 0) #\<)
                  (+ 1 (position #\Space string))
                  0))
             (type
              (if (> start 0)
                  (intern (string-upcase (subseq string 1 (- start 2)))
                          "KEYWORD")
                  nil))
             (vals
              (with-input-from-string (in string :start start)
                (let ((v '()))
                  (loop
                   (let ((val 
                          (let ((*read-base* 16)) (read in nil in))))
                     (when (eql val in)
                       (return (nreverse v)))
                     (push val v)))))))
        (cons type vals))))

(defun read-data-file (filename parser)
  (let ((data '()))
    (with-open-file (in filename)
      (loop
       (let ((line (funcall parser in)))
         (when (eql line in)
           (return (nreverse data)))
         (when line
           (push line data)))))))

(defun read-glyph-list-line (stream)
  (let ((line (read-data-line stream)))
    (cond
      ((eql line stream) stream)
      ((null line) nil)
      (t
       (unless (>= (length line) 2)
         (error "Malformed Glyph List line"))
       (let ((codepoint
              (let ((*read-base* 16)) (read-from-string (cadr line))))
             (name (car line)))
         (if (<= #xE000 codepoint #xF8FF)
             nil
             (list codepoint name)))))))

(defun list-to-ccs-or-string (char list)
  "Takes a list of codepoints, returns a CCS-OR-STRING."
  (flet ((ccs (base pre-ccs)
           (if pre-ccs (append pre-ccs base) base))
         (check-consistency (base pre-ccs)
           (when (or (gethash char *combining-class-table*) 
                     (null pre-ccs))
             (unless (eql (not base)
                          (not (null (gethash char *combining-class-table*))))
               (warn "Inconsistent decomposition for U+~4,'0X"
                     (char-code char))
               (return-from list-to-ccs-or-string nil)))))
    (do ((l list (cdr l)) (base nil) (pre-ccs '()) (ccs-list '()))
        ((null l)
         (unless (and (null pre-ccs) (null base))
           (when (null ccs-list)
             (check-consistency base pre-ccs))
           (cond
             (base (push (ccs base pre-ccs) ccs-list))
             (t (push (ccs (car pre-ccs) (cdr pre-ccs)) ccs-list))))
         (when (null ccs-list) (error "No CCS in list"))
         (if (null (cdr ccs-list))
             (car ccs-list)
             (coerce (nreverse ccs-list) 'vector)))
      (let ((char (code-char (car l))))
        (cond
          ((gethash char *combining-class-table*)
           (push char pre-ccs))
          (t
           (unless (and (null pre-ccs) (null base))
             (when (null ccs-list)
               (check-consistency base pre-ccs))
             (cond
               (base (push (ccs base pre-ccs) ccs-list))
               (t (push (ccs (car pre-ccs) (cdr pre-ccs)) ccs-list))))
           (setf base char pre-ccs '())))))))

(defun printable-ccs (ccs)
  (etypecase ccs
    (character (char-code ccs))
    (symbol ccs)
    (cons (cons (printable-ccs (car ccs)) (printable-ccs (cdr ccs))))
    (vector (map 'vector #'printable-ccs ccs))))

(defun make-data (out-filename unicode-data-filename glyph-list-filename)
  (let ((unicode-data
         (read-data-file unicode-data-filename #'read-unicode-data-line))
        (glyph-list
         (read-data-file glyph-list-filename #'read-glyph-list-line))
        (combining-classes '())
        (canon-decomps '())
        (good-decomps-table (make-hash-table :test 'equal))
        (bad-decomps-table (make-hash-table :test 'equal))
        (good-decomps '()) (bad-decomps '()) (glyph-names '())
        (*combining-class-table* (make-hash-table :test 'equal))
        (*canonical-decomposition-table* (make-hash-table :test 'equal))
        (*glyph-names* (make-hash-table :test 'equal)))
    (dolist (entry unicode-data)
      (let ((char (code-char (car entry)))
            (class (cadr entry)))
        (when (cadr entry)
          (when char
            (setf (gethash char *combining-class-table*) class)
            (push (cons char class) combining-classes)))))
    ;; Build the canonical decomposition table.
    (dolist (entry unicode-data)
      (let ((char (code-char (car entry)))
            (decomp-type (car (caddr entry)))
            (decomp (cdr (caddr entry))))
        (when (and char decomp)
          (let ((ccs (list-to-ccs-or-string char decomp)))
            (when ccs
              (cond
                ((null decomp-type) 
                 (setf (gethash char *canonical-decomposition-table*)
                       ccs))))))))
    ;; Build the other tables.
    (dolist (entry unicode-data)
      (let ((char (code-char (car entry)))
            (decomp-type (car (caddr entry)))
            (decomp (cdr (caddr entry))))
        (when (and char decomp)
          (let ((ccs (list-to-ccs-or-string char decomp)))
            (when ccs
              (let ((n-char (normalise-ccs char))
                    (n-ccs (normalise-ccs ccs)))
                (cond
                  ((null decomp-type) 
                   (push (cons char n-ccs) canon-decomps))
                  ((member decomp-type '(:compat))
                   (push n-ccs (gethash n-char good-decomps-table))
                  ;; provide inverse mapping for spacing diacritics
                  (when (and (listp ccs) (eql (cdr ccs) #\Space))
                    (push char (gethash (normalise-ccs (car ccs))
                                        good-decomps-table))))
                  ((member decomp-type '(:circle :sub :super
                                         :small :narrow :wide))
                   (push (cons decomp-type n-ccs) 
                         (gethash n-char good-decomps-table)))
                  (t (push n-ccs (gethash n-char bad-decomps-table))))))))))
    (dolist (entry glyph-list)
      (let ((ccs (normalise-ccs (code-char (car entry))))
            (name (cadr entry)))
        (push name (gethash ccs *glyph-names*))))
    (setf combining-classes (nreverse combining-classes)
          canon-decomps (nreverse canon-decomps))
    (maphash
     #'(lambda (key val) (push (cons key val) good-decomps))
     good-decomps-table)
    (maphash
     #'(lambda (key val) (push (cons key val) bad-decomps))
     bad-decomps-table)
    (maphash
     #'(lambda (key val) (push (cons key val) glyph-names))
     *glyph-names*)
    (with-open-file (out out-filename :direction :output :if-exists :supersede)
      (with-standard-io-syntax
        (let ((*print-pretty* nil)
              (*package* (find-package "CEDILLA")))
          (format out ";;; Automatically generated file -- do not modify~%")
          (print '(in-package "CEDILLA") out)
          (print
           '(defun cedilla::parse-ccs (cedilla::ccs)
             (etypecase cedilla::ccs
               (integer (code-char cedilla::ccs))
               (vector (map 'vector #'cedilla::parse-ccs cedilla::ccs))
               (cons (cons
                      (if (integerp (car cedilla::ccs))
                          (code-char (car cedilla::ccs))
                          (car cedilla::ccs))
                      (cedilla::parse-ccs (cdr cedilla::ccs))))))
           out)
          (print
           `(eval-when (load eval)
             (let ((cedilla::cct cedilla::*combining-class-table*))
               (flet ((cedilla::frob (cedilla::code cedilla::class)
                        (setf (gethash (code-char cedilla::code) cedilla::cct)
                              cedilla::class)))
                 ,@(mapcar
                    #'(lambda (entry) 
                        `(cedilla::frob ',(char-code (car entry))
                                        ',(cdr entry)))
                    combining-classes))))
           out)
          (print
           `(eval-when (load eval)
             (let ((cedilla::cdt cedilla::*canonical-decomposition-table*))
               (flet ((cedilla::frob (cedilla::code cedilla::ccs)
                       (setf (gethash (code-char cedilla::code) cedilla::cdt)
                             (cedilla::parse-ccs cedilla::ccs))))
                 ,@(mapcar
                    #'(lambda (entry) 
                        `(cedilla::frob ',(char-code (car entry))
                                        ',(printable-ccs (cdr entry))))
                    canon-decomps))))
           out)
          (print
           `(eval-when (load eval)
             (let ((cedilla::at cedilla::*alternatives-table*))
               (flet ((cedilla::frob (cedilla::code cedilla::list)
                        (setf (gethash (code-char cedilla::code) cedilla::at)
                              (mapcar #'cedilla::parse-ccs cedilla::list))))
                 ,@(mapcar
                    #'(lambda (entry) 
                        `(cedilla::frob
                          ',(char-code (car entry))
                          ',(nreverse (mapcar #'printable-ccs (cdr entry)))))
                    good-decomps))))
           out)
          (print
           `(eval-when (load eval)
              (let ((cedilla::ft cedilla::*fallbacks-table*))
                (flet ((cedilla::frob (cedilla::code cedilla::list)
                         (setf (gethash (code-char cedilla::code) cedilla::ft)
                               cedilla::list)))
                 ,@(mapcar
                    #'(lambda (entry)
                        `(cedilla::frob
                          ',(char-code (car entry))
                          ',(nreverse
                             (mapcar #'printable-ccs (cdr entry)))))
                    bad-decomps))))
           out)
          (print
           `(eval-when (load eval)
             (let ((cedilla::gn cedilla::*glyph-names*))
               (flet ((cedilla::frob (cedilla::code cedilla::name)
                       (setf (gethash (cedilla::parse-ccs cedilla::code)
                                      cedilla::gn)
                             cedilla::name)))
                 ,@(mapcar
                    #'(lambda (entry)
                        `(cedilla::frob
                          ',(printable-ccs (car entry))
                          ',(reverse (cdr entry))))
                    glyph-names))))
           out)))))
  nil)

(defun read-unicode-data-line-for-printing (stream)
  (let ((line (read-data-line stream)))
    (declare (list line))
    (cond
      ((eql line stream) stream)
      ((null line) nil)
      (t
       (unless (>= (length line) 10)
         (error "Malformed UnicodeData line"))
       (list (let ((*read-base* 16)) (read-from-string (car line)))
             (cadr line))))))

(defun make-unicode-sample (out-filename unicode-data-filename
                            &optional (from #x20) (to #x2FFF))
  (let ((data
         (read-data-file unicode-data-filename
                         #'read-unicode-data-line-for-printing)))
    (let ((a (make-array #x10000 :initial-element nil)))
      (dolist (e data)
        (when (and e (< (car e) #x10000))
          (setf (aref a (car e)) (cadr e))))
      (with-open-file (out out-filename :direction :output
                           :external-format cedilla:*external-format-utf8*)
        (loop for i from from upto to
              when (aref a i)
              do (format out "~4,'0X ~A ~A~%"
                         i (code-char i) (aref a i)))))))

;;; (make-data "data.lisp" 
;;;            "/usr/local/share/doc/unicode/UNIDATA/UnicodeData.txt" 
;;;            "/usr/local/share/doc/adobe/glyphlist.txt")
