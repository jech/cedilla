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

;;; A CCS is either a single non-combining character, or an improper
;;; list with all CARs combining characters and the last CDR a
;;; non-combining character.  I think of it as the CAR applied to the
;;; CDR.

;;; As a special exception, in order to provide for lone combining CCS
;;; (needed by the normalisation algorithm), a CCS can also be
;;; terminated by a combining character.

;;; A CCS is normal when it consists entirely of canonical characters
;;; and is sorted according to the opposite of what Unicode says.
;;; Normal CCS can be compared using EQUAL, which makes it possible to
;;; use them as keys in hashtables.

(deftype ccs () '(or char cons))

(defun ccs-base (ccs)
  (if (atom ccs) ccs (ccs-base (cdr ccs))))

;;; A CCS string is a simple-vector of non-combining CCS.  It need
;;; not be a general vector; in particular, every simple-string
;;; consisting entirely of non-combining characters is a CCS string.
;;; We only manipulate normal CCS strings.

(deftype ccs-string () 'simple-vector)

(deftype ccs-or-string () '(or ccs ccs-string))

;;; A CCS-STRING-OR-MAGIC is either a CCS-OR-STRING or a cons of a
;;; type of magic (a symbol in the KEYWORD package) and a
;;; CCS-STRING-OR-MAGIC.

(deftype CCS-STRING-OR-MAGIC () '(or ccs ccs-string cons))

;;; Maps a character to its Unicode combining class, a small integer.

(defvar *combining-class-table* (make-hash-table :test 'equal))

;;; Maps a character to its canonical decomposition, a not necessarily
;;; normal and potentially combining CCS-OR-STRING

(defvar *canonical-decomposition-table* (make-hash-table :test 'equal))

;;; These two map a normal CCS (usually a character) to a list of
;;; CCS-STRING-OR-MAGIC.  The first contains good alternatives, the
;;; second bad fallbacks.

;;; These tables may map a combining character to a non-combining one;
;;; in this case, the non-combining character will be used as a glyph
;;; for the combining one, but the semantics will not be changed.  On
;;; the other hand, they should never map a non-combining CCS to a
;;; combining character.

(defvar *alternatives-table* (make-hash-table :test 'equal))
(defvar *fallbacks-table* (make-hash-table :test 'equal))

;;; Maps a normal CCS to a list of ASCII strings, the preferred glyph
;;; names of the CCS.  This should only contain entries from the AGL,
;;; as the presence of a name in this table inhibits generation of
;;; uniXXXX names.

(defvar *glyph-names* (make-hash-table :test 'equal))

;;; Maps a normal CCS to a list of ASCII strings, the alternate glyph
;;; names.  As this is only applied to named-glyph fonts, use of
;;; alternatives table at the Unicode level should be preferred
;;; whenever possible.

(defvar *alternate-glyph-names* (make-hash-table :test 'equal))

;;; Maps a normal CCS to a precomposed character if possible.  This is
;;; used for generating uniXXXX-style glyph names.

(defvar *ccs-precomposed* (make-hash-table :test 'equal))

;;; Reader macros.  This sets up the following syntax:
;;;
;;;  #Uxxxx: the character with codepoint xxxx;
;;;  #uxxxx: the normal CCS corresponding to #uxxxx.

(defun setup-cedilla-readtable ()
  (setf *readtable* (copy-readtable nil))
  (set-dispatch-macro-character 
   #\# #\U
   #'(lambda (s c n)
       (declare (ignore c n))
       (let ((*read-base* 16))
         (code-char (read s)))))
  (set-dispatch-macro-character 
   #\# #\u
   #'(lambda (s c n)
       (declare (ignore c n))
       (let ((*read-base* 16))
         (normalise-ccs (code-char (read s)))))))
  
;;; Normalisation

(defun ccs-normal-p (ccs)
  "Return true if CCS is in normal form."
  (declare (type ccs-or-string ccs))
  (etypecase ccs
    (character (not (gethash ccs *canonical-decomposition-table*)))
    (vector
     (do* ((l (length ccs)) (i 0 (+ i 1)))
          ((>= i l) t)
       (when (not (ccs-normal-p (aref ccs i)))
         (return nil))))
    (cons
     (do ((l ccs (cdr l)))
         ((atom (cdr l)) 
          (and (ccs-normal-p (car l)) (ccs-normal-p (cdr l))))
       (when (not (ccs-normal-p (car l)))
         (return nil))
       (let ((c0 (gethash (car l) *combining-class-table*))
             (c1 (gethash (cadr l) *combining-class-table*)))
         (unless (or (= 0 c0) (= 0 c1))
           (when (< c0 c1)
             (return nil))))))))

;;; Never mutates its argument.  For bootstrapping reasons, this
;;; should not depend on the values in the canonical decompositions
;;; table being normalised.

;;; We assume that most CCS will already be in normal form, which is
;;; why we perform the check beforehand.

(defun normalise-ccs (ccs)
  "Return a normal-form CCS canonically equivalent to CCS.
Also works on CCS-strings."
  (declare (type ccs-or-string ccs))
  (etypecase ccs
    (vector
     (if (ccs-normal-p ccs) ccs (map 'vector #'normalise-ccs ccs)))
    (character
     (let ((c (gethash ccs *canonical-decomposition-table*)))
       (if c (normalise-ccs c) ccs)))
    (cons
     (if (ccs-normal-p ccs) ccs (ccs-normalise-list (copy-list ccs))))))

;;; This one works in place.  It is pessimised for long CCS on the
;;; assumption that most CCS are short.

(defun ccs-normalise-list (ccs)
  (declare (type cons ccs))
  (let ((nil-ccs (cons nil ccs)))
    (tagbody
     again
       (do ((off-by-one nil-ccs (cdr off-by-one))
            (l (cdr nil-ccs) (cdr l)))
           ((atom l)
            (let ((decomp (gethash l *canonical-decomposition-table*)))
              (when decomp
                (setf (cdr off-by-one) decomp)
                (go again))
              (return-from ccs-normalise-list (cdr nil-ccs))))
         (let ((decomp (gethash (car l) *canonical-decomposition-table*)))
           (when decomp
             (setf (cdr off-by-one) (ccs-concatenate decomp (cdr l)))
             (go again)))
         (unless (atom (cdr l))
           (let ((c0 (gethash (car l) *combining-class-table*))
                 (c1 (gethash (cadr l) *combining-class-table*)))
             ;; both must be combining characters at this point
             (unless (or (= 0 c0) (= 0 c1))
               (when (< c0 c1)
                 (setf (cadr off-by-one) (cadr l)
                       (caddr off-by-one) (car l))
                 (go again)))))))))

(defun ccs-concatenate (ccs-c ccs-b)
  "Applies the combining CCS-C to CCS-B."
  (cond
    ((characterp ccs-c) (cons ccs-c ccs-b))
    (t (cons (car ccs-c) (ccs-concatenate (cdr ccs-c) ccs-b)))))

(defun normalise-ccs-or-magic (ccs)
  (if (and (consp ccs) (symbolp (car ccs)))
      (cons (car ccs) (normalise-ccs-or-magic (cdr ccs)))
      (normalise-ccs ccs)))

;;; Accessors

(defun ccs-combining-class (ccs)
  "Return the combining class of (normal) CCS, or NIL if non-combining."
  (let ((char (if (characterp ccs) ccs (cdr (last ccs)))))
    (values (gethash char *combining-class-table*))))

(defun ccs-alternatives (ccs)
  "Return the alternatives for (normal) CCS."
  (declare (type ccs ccs))
  (values (gethash ccs *alternatives-table*)))

(defun ccs-fallbacks (ccs)
  "Return the fallbacks for (normal) CCS."
  (declare (type ccs ccs))
  (values (gethash ccs *fallbacks-table*)))

(defmacro define-alternatives (&body list)
  `(eval-when (load eval)
    (let ((table *alternatives-table*))
      (flet ((insert-alternative (ccs alt &optional begin)
               (let* ((ccs (normalise-ccs ccs))
                      (new-value (normalise-ccs-or-magic alt))
                      (old-value (gethash ccs table)))
                 (if begin
                     (setf (gethash ccs table) (cons new-value old-value))
                     (setf (gethash ccs table) 
                           (append old-value (list new-value)))))))
        ,@(mapcar #'(lambda (l) `(apply #'insert-alternative ',l))
                  list)))))

(defmacro define-fallbacks (&body list)
  `(eval-when (load eval)
    (let ((table *fallbacks-table*))
      (flet ((insert-fallback (ccs alt &optional begin)
               (let* ((ccs (normalise-ccs ccs))
                      (old-value (gethash ccs table))
                      (new-value (normalise-ccs-or-magic alt)))
                 (if begin
                     (setf (gethash ccs table) (cons new-value old-value))
                     (setf (gethash ccs table) 
                           (append old-value (list new-value)))))))
        ,@(mapcar #'(lambda (l) `(apply #'insert-fallback ',l)) list)))))

(defun dotted-character-p (C)
  "Return true if removing a dot off C is desirable when building composites."
  (member c '(#\i #\j #.(code-char #x0130) #.(code-char #x456))))

(defun dotless-character (C)
  "Return, if possible, the dotless character associated to C."
  (case c
    ((#\i) #.(code-char #x0131))
    ((#\j) nil)
    ((#.(code-char #x0130)) #\I)
    (t nil)))

(defun ccs-glyph-names (ccs)
  "Return, if possible, the preferred glyph names of CCS."
  (or 
   (gethash ccs *glyph-names*)
   (let ((char 
          (if (characterp ccs)
              ccs
              (gethash ccs *ccs-precomposed*))))
     (if char
         (let ((code (char-code char)))
           ;; Can't they make their mind up?
           (list
            (format nil "uni~4,'0X" code)
            (if (<= code #xFFFF)
                (format nil "u~4,'0X" code)
              (format nil "u~X" code))))))))

(defun ccs-dotless-glyph-name (ccs)
  "Return, if possible, the name of the dotless glyph associated to CCS."
  (cond
    ((eql ccs #\i) "dotlessi")
    ((eql ccs #\j) "dotlessj")
    ((eql ccs #.(code-char #x0130)) "I")
    (t nil)))

(defun ccs-alternate-glyph-names (ccs)
  "Return the alternate glyph names for CCS."
  (gethash ccs *alternate-glyph-names*))

(defmacro define-alternate-glyph-names (&body list)
  `(eval-when (load eval)
    (let ((table *alternate-glyph-names*))
      (flet ((insert-glyph-name (ccs name)
               (let* ((ccs (normalise-ccs ccs))
                      (old-value (gethash ccs table)))
                 (setf (gethash ccs table)
                       (append old-value (list name))))))
        ,@(mapcar #'(lambda (l) `(apply #'insert-glyph-name ',l)) list)))))

(defun next-ccs (in)
  "Return the next CCS read from stream IN.
Maps all line terminators to #\Newline and discards sequences of the
form CCS #\Backspace."
  (let ((ccs (read-char in nil in)))
    (cond
      ((eql ccs in) in)
      ((eql ccs #\Newline)
       (let ((next (read-char in nil in)))
         (unless (or (eql next #\Return) (eql next in))
           (unread-char next in)))
       #\Newline)
      ((eql ccs #\Tab) #\Space)
      (t
       (loop 
        (let ((next (read-char in nil in)))
          (when (eql next in)
            (return (normalise-ccs ccs)))
          (when (eql next #\Backspace)
            (return (next-ccs in)))
          (unless (gethash next *combining-class-table*)
            (unread-char next in)
            (return (normalise-ccs ccs)))
          (push next ccs)))))))
