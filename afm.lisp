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

(defvar *afm-buffer*)

(defun read-afm-file (filename &key encoding fixed-encoding resources omit)
  (let ((res
         (mapcar #'identify-resource resources)))
    (when (member nil res)
      (return-from read-afm-file nil))
    (let ((font 
           (if fixed-encoding
               (make-fixed-encoding-ps-font :encoding fixed-encoding 
                                            :resources res)
               (make-named-glyph-ps-font :resources res
                                         :encoding encoding)))
          (*afm-buffer* (make-array 30 
                                    :element-type 'character
                                    :fill-pointer 0 :adjustable t)))
      (with-open-file (in filename :direction :input)
        (read-afm-data in font omit))
      (resolve-composite-glyphs font)
      font)))

(defun read-afm-data (in font omit)
  (let ((token (afm-token in)))
    (unless (equal token "StartFontMetrics")
      (error "Couldn't read header"))
    (afm-skip-eol in)
    (loop
     (let ((token (afm-token in)))
       (cond
         ((eql token :eof) (error "Unexpected EOF"))
         ((equal token "FontName")
          (let ((font-name (afm-string in)))
            (unless (stringp font-name)
              (error "Malformed FontName line"))
            (setf (font-name font) font-name)))
         ((equal token "FontBBox")
          (let ((x0 (afm-number in))
                (y0 (afm-number in))
                (x1 (afm-number in))
                (y1 (afm-number in)))
            (afm-eol in)
            (setf (font-x0 font) x0
                  (font-y0 font) y0
                  (font-x1 font) x1
                  (font-y1 font) y1)))
         ((equal token "StartCharMetrics")
          (afm-number in)
          (afm-eol in)
          (afm-read-char-metrics in font omit))
         ((equal token "StartComposites")
          (afm-number in)
          (afm-eol in)
          (afm-read-composites in font omit))
         ((equal token "EndFontMetrics")
          (return-from read-afm-data font))
         (t (afm-skip-eol in)))))))

(defun afm-read-char-metrics (in font omit)
  (loop
   (let ((token (afm-token in)))
     (cond
       ((eql token :eof) (error "Unexpected EOF"))
       ((equal token "EndCharMetrics") 
        (afm-eol in) 
        (return-from afm-read-char-metrics nil))
       ((equal token "C")
        (multiple-value-bind (glyph index) (afm-read-c-line in)
          (when glyph
            (unless (member (pre-glyph-name glyph) omit :test 'equal)
              (unless (get-glyph font (pre-glyph-name glyph) index)
                (put-glyph font glyph index))))))
       (t (afm-skip-eol in))))))

(defun afm-read-c-line (in)
  (let (c n w x0 y0 x1 y1)
    (setf c (afm-number in))
    (loop
     (tagbody
        (let ((token (afm-token in)))
          (unless (equal token ";")
            (error "Expected semicolon")))
      no-semicolon
        (let ((token (afm-token in)))
          (cond
            ((or (eql token :eof) (eql token :eol))
             (return-from afm-read-c-line
               (values
                (make-font-glyph :name n :width w :x0 x0 :y0 y0 :x1 x1 :y1 y1)
                c)))
            ((equal token ";") (error "Unexpected semicolon"))
            ((equal token "WX") (setf w (afm-number in)))
            ((equal token "N") (setf n (afm-name in)))
            ((equal token "B")
             (setf x0 (afm-number in)
                   y0 (afm-number in)
                   x1 (afm-number in)
                   y1 (afm-number in)))
            (t
             (loop for token = (afm-token in)
                   until (or (eql token :eof) (eql token :eol)
                             (equal token ";")))
             (go no-semicolon))))))))

(defun afm-read-composites (in font omit)
  (loop
   (let ((token (afm-token in)))
     (cond
       ((eql token :eof) (error "Unexpected EOF"))
       ((equal token "EndComposites") 
        (afm-eol in) 
        (return-from afm-read-composites nil))
       ((equal token "CC")
        (multiple-value-bind (glyph index) (afm-read-cc-line in)
          (when glyph
            (unless (member (pre-glyph-name glyph) omit :test 'equal)
              (unless (get-glyph font (pre-glyph-name glyph) index)
                (put-glyph font glyph))))))
       (t (afm-skip-eol in))))))

(defun afm-read-cc-line (in)
  (let (cc pcc)
    (setf cc (afm-name in))
    (afm-number in)
    (loop
     (tagbody
        (let ((token (afm-token in)))
          (unless (equal token ";")
            (error "Expected semicolon")))
        no-semicolon
        (let ((token (afm-token in)))
          (cond
            ((or (eql token :eof) (eql token :eol))
             (return-from afm-read-cc-line
               (make-unresolved-composite-glyph 
                :name cc :components (nreverse pcc))))
            ((equal token "PCC")
             (push (list (afm-name in) 
                         (afm-number in) (afm-number in))
                   pcc))
            ((equal token ";") (error "Unexpected semicolon"))
            (t
             (loop for token = (afm-token in)
                   until (or (eql token :eof) (eql token :eol)
                             (equal token ";")))
             (go no-semicolon))))))))

(defun afm-token (in)
  (let ((afm-buffer *afm-buffer*)
        (char (read-char in nil in)))
    (flet ((stuff ()
             (when (eql char in) (return-from afm-token :eof))
             (when (eql char #\Return)
               (setf char (read-char in nil in))
               (when (eql char in) (return-from afm-token :eof))
               (unless (eql char #\Newline) (unread-char char in)))
             (when (eql char #\Newline) (return-from afm-token :eol))))
      (stuff)
      (loop while (or (eql char #\Space) (eql char #\Tab))
            do (setf char (read-char in nil in)))
      (stuff)
      (setf (fill-pointer afm-buffer) 0)
      (loop
       do (vector-push-extend char afm-buffer)
       do (setf char (read-char in nil in))
       while (not (or (member char '(#\Space #\Tab #\Newline #\Return))
                      (eql char in))))
      (unless (eql char in)
        (unread-char char in)))
    afm-buffer))

(defun afm-number (in)
  (let ((buf (afm-token in)))
    (unless (and (stringp buf) (> (length buf) 0))
      (error "Expected a number"))
    (if (and (eql (aref buf 0) #\<)
             (eql (aref buf (- (length buf) 1)) #\>))
        (let* ((*read-base* 16)
               (n (ignore-errors 
                    (read-from-string buf :start 1 :end (- (length buf) 1)))))
          (unless (realp n) (error "Expected a number"))
          n)
        (let ((n (ignore-errors (read-from-string buf))))
          (unless (realp n) (error "Expected a number"))
          n))))

(defun afm-boolean (in)
  (let ((buf (afm-token in)))
    (unless (stringp buf)
      (error "Expected a Boolean"))
    (not (string-equal buf "false"))))

(defun afm-name (in)
  (let ((buf (afm-token in)))
    (unless (stringp buf)
      (error "Expected a name"))
    (copy-seq buf)))

(defun afm-string (in)
  (let ((afm-buffer *afm-buffer*))
    (let (char)
      (loop do (setf char (read-char in nil in))
            while (or (eql char #\Space) (eql char #\Tab)))
      (when (or (eql char in) (eql char #\Newline))
        (return-from afm-string ""))
      (when (eql char #\Return)
        (setf char (read-char in nil in))
        (unless (or (eql char #\Newline) (eql char in))
          (unread-char char in))
        (return-from afm-string ""))
      (setf (fill-pointer afm-buffer) 0)
      (vector-push-extend char afm-buffer))
    (loop for char = (read-char in nil in)
          while (not (or (eql char in) (eql char #\Newline)))
          do (vector-push-extend char afm-buffer))
    (copy-seq afm-buffer)))

(defun afm-skip-eol (in)
  (let (char)
    (loop do (setf char (read-char in nil in))
          while (not (or (eql char in) 
                         (eql char #\Return) (eql char #\Newline))))
    (when (eql char #\Return)
      (let ((char (read-char in nil in)))
        (unless (or (eql char #\Newline) (eql char in))
          (unread-char char in)))))
  nil)

(defun afm-eol (in)
  (let (char)
    (loop do (setf char (read-char in nil in))
          while (not (or (eql char in) 
                         (eql char #\Return) (eql char #\Newline)))
          unless (or (eql char #\Space) (eql char #\Tab))
          do (error "Unexpected input character ~A, should be eol" char))
    (when (eql char #\Return)
      (let ((char (read-char in nil in)))
        (unless (or (eql char #\Newline) (eql char in))
          (unread-char char in)))))
  nil)

(defun resolve-composite-glyphs (font)
  (when (typep font 'named-glyph-font)
    (maphash
     #'(lambda (name glyph)
         (when (unresolved-composite-glyph-p glyph)
           (let ((width
                  (glyph-width 
                   (gethash (caar (unresolved-composite-glyph-components glyph))
                            (font-glyphs font))))
                 (components
                  (mapcar
                   #'(lambda (c)
                       (make-composite-component
                        :glyph (gethash (car c) (font-glyphs font))
                        :dx (cadr c) :dy (caddr c)))
                   (unresolved-composite-glyph-components glyph))))
             (put-glyph font
                        (make-composite-glyph 
                         :name name :width width
                         :components components)))))
     (font-glyphs font)))
  font)
