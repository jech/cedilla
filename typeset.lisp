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

(defparameter *cedilla-procset*
  (make-string-ps-resource
   :type :procset
   :name "Cedilla"
   :version *cedilla-version*
   :value
   "%!
/S /show load def
/MT /moveto load def
/LT /lineto load def
/F /fill load def
/CT /curveto load def
/CP /closepath load def
/C /concat load def
/SLW /setlinewidth load def
/GSV /gsave load def
/GR /grestore load def
/RF /rectfill where {pop /rectfill load} 
    {{4 2 roll moveto
      1 index 0 rlineto 0 exch rlineto neg 0 rlineto fill} bind}
ifelse def
/RS /rectstroke where {pop /rectstroke load}
    {{4 2 roll moveto
      1 index 0 rlineto 0 exch rlineto neg 0 rlineto closepath stroke} bind}
ifelse def
/RC /rectclip where {pop /rectclip load}
    {{4 2 roll moveto
      1 index 0 rlineto 0 exch rlineto neg 0 rlineto clip} bind} 
ifelse def
/CSV /clipsave where {pop /clipsave load} {/gsave load} ifelse def
/CR /cliprestore where {pop /cliprestore load} {/grestore load} ifelse def
/CS 
{2 index 1 index add 2 index 2 index 2 div add moveto
 2 index 1 index 2 div add 2 index 2 index 2 div add
 2 index 2 div 0 360 arc stroke pop pop pop
} bind def
/RCF {      
  exch dup length 3 add dict begin
  { 1 index /FID ne { def } { pop pop } ifelse } forall
  /Encoding exch def
  currentdict end definefont
} bind def
/UE {
  256 array 0 1 255 { 1 index exch /.notdef put } for
} bind def
/SQR /RS load def
/CRC {4 2 roll moveto
      1 index div 1 exch 
      gsave scale currentpoint 3 2 roll CS grestore} bind def
/DMND {
  4 2 roll moveto 1 index 2 div 0 rmoveto
  1 index 2 div 1 index 2 div rlineto
  1 index 2 div neg 1 index 2 div rlineto
  2 div neg exch 2 div neg exch rlineto closepath stroke
} bind def
/SCRN /SQR load def
/KCP /SQR load def
"))

(defvar *paper-size*)
(defvar *size*)
(defvar *header*)
(defvar *header-baseline*)
(defvar *footer*)
(defvar *footer-baseline*)
(defvar *millisize*)
(defvar *baselineskip*)
(defvar *page-number*)
(defvar *current-x*)
(defvar *current-y*)
(defvar *current-line-width*)
(defvar *typesetter-x*)
(defvar *typesetter-y*)
(defvar *typesetter-line-width*)
(defvar *current-instance*)
(defvar *current-string*)
(defvar *y-top*)
(defvar *y-bot*)
(defvar *x-left*)

(defun typeset-file (in out font-list paper
                     &key
                     (size 11)
                     (wrap nil)
                     (header nil) (header-baseline (/ 72 2.54))
                     (footer nil) (footer-baseline (/ 72 2.54))
                     (title nil) (baselineskip size))
  (let* ((*paper-size* paper)
         (*size* size) (*millisize* (/ size 1000.0))
         (*baselineskip* baselineskip)
         (*header* header) (*header-baseline* header-baseline)
         (*footer* footer) (*footer-baseline* footer-baseline)
         (fonts (or 
                 (mapcan
                  #'(lambda (font) 
                      (append
                       (if (font-instances font) (list font) '())
                       (font-transformed-fonts font)))
                  font-list)
                 (error "Empty document")))
         (font-resources (compute-font-resources fonts))
         (*y-top* (- (max 0 (scaled-quantity (font-y1 (car fonts))))
                     *baselineskip*))
         (*x-left* (- (min 0 (scaled-quantity (font-x0 (car fonts))))))
         (*y-bot* (- (min 0 (scaled-quantity (font-y0 (car fonts)))))))
    (output-header out title (cons *cedilla-procset* font-resources))
    (output-prolog out)
    (output-setup out fonts font-resources)
    (let ((*current-x* nil) (*current-y* nil) (*current-line-width* nil)
          (*typesetter-x* nil) (*typesetter-y* nil) 
          (*typesetter-line-width* nil)
          (*current-instance* nil) (*page-number* 1)
          (*current-string* 
           (make-array 200 :fill-pointer 0 :element-type 'character)))
      (open-page out)
      (labels ((tg (glyph)
                (when wrap
                  (let ((w (scaled-glyph-width glyph)))
                    (when (> (+ *current-x* w)
                             (- (paper-size-x1 paper)
                                (paper-size-right-margin paper)))
                      (new-line out))))
                (typeset-glyph glyph out))
               (tccs (ccs)
                (let ((glyph (recall-glyph ccs)))
                  (etypecase glyph
                    (null (let ((glyph (recall-glyph #\?)))
                            (when (glyph-p glyph)
                              (tg glyph))))
                    (glyph (tg glyph))
                    (list (dolist (g glyph) (tg g)))))))
        (loop
         (let ((ccs (next-ccs in)))
           (when (eql ccs in)
             (return nil))
           (cond
            ((eql ccs #\Newline)
             (new-line out))
            ((eql ccs #\Page)
             (let ((n-ccs (next-ccs in)))
               (when (eql n-ccs #\Newline)
                 (setf n-ccs (next-ccs in)))
               (when (eql n-ccs in)
                 (return nil))
               (new-page out)
               (tccs n-ccs)))
            (t (tccs ccs))))))
      (close-page out)
      (output-trailer out))))

(defun compute-font-resources (fonts)
  (let ((resources '()))
    (dolist (f fonts)
      (dolist (r (font-resources f))
        (unless (member r resources :test #'resource-equivalent-p)
          (push r resources))))
    (dolist (f fonts)
      (when (typep f 'ps-font)
        (let ((r (make-required-ps-resource 
                  :type :font :name (font-name f))))
          (unless (member r resources :test #'resource-equivalent-p)
            (push r resources)))))
    resources))

(defun scaled-quantity (q)
  (* q *millisize*))

(defun scaled-glyph-width (glyph)
  (* (glyph-width glyph) *millisize*))

(defun synchronise-position (out)
  (unless (and (eql *current-x* *typesetter-x*)
               (eql *current-y* *typesetter-y*))
    (finish-string out)
    (format out "~A ~A MT~%" *current-x* *current-y*)
    (setf *typesetter-x* *current-x*
          *typesetter-y* *current-y*)))

(defun synchronise-line-width (out)
  (unless (eql *current-line-width* *typesetter-line-width*)
    (finish-string out)
    (format out "~A SLW~%" *current-line-width*)
    (setf *typesetter-line-width* *current-line-width*)))

(defun open-page (out)
  (format out "%%Page: ~A ~A~%" *page-number* *page-number*)
  (format out "%%BeginPageSetup~%")
  (format out "save~%")
  (format out "%%EndPageSetup~%")
  (typeset-header
   *header* (- (paper-size-y1 *paper-size*) *header-baseline*) out)
  (setf *current-x* (+ (paper-size-x0 *paper-size*) 
                       (paper-size-left-margin *paper-size*))
        *current-y* (- (paper-size-y1 *paper-size*)
                       (+ (paper-size-top-margin *paper-size*)
                          *baselineskip* *y-top*))
        *typesetter-x* nil *typesetter-y* nil))

(defun close-page (out)
  (finish-string out)
  (typeset-header
   *footer* (+ (paper-size-y0 *paper-size*) *footer-baseline*) out)
  (format out "restore~%")
  (setf *current-instance* nil 
        *current-x* nil *current-y* nil *typesetter-x* nil *typesetter-y* nil
        *current-line-width* nil)
  (format out "showpage~%")
  (incf *page-number*))

(defun new-line (out)
  (setf *current-x* (+ (paper-size-x0 *paper-size*)
                       (paper-size-left-margin *paper-size*)))
  (decf *current-y* *baselineskip*)
  (when (< *current-y* (+ (paper-size-y0 *paper-size*)
                          (paper-size-left-margin *paper-size*) *y-bot*))
    (new-page out)))

(defun new-page (out)
  (close-page out)
  (open-page out))

(defun output-character (char out)
  ;; DSC specifies a maximum line length of 255
  ;; we may output up to three characters for every character in a string
  ;; we limit ourselves to 75 characters per string
  (synchronise-position out)
  (when (>= (fill-pointer *current-string*) 75)
    (finish-string out))
  (vector-push char *current-string*))

(defun finish-string (out)
  (when (>= (fill-pointer *current-string*) 1)
    (output-string *current-string* out)
    (format out " S~%")
    (setf (fill-pointer *current-string*) 0)))

(defun output-string (string out)
  (format out "(")
  (loop for i from 0 upto (- (length string) 1)
        for c = (char-code (aref string i))
        when (>= c 256) do (error "Cannot output character \\~3,'0O" c)
        do (cond
             ((or (< c 32) (>= c 127))
              (format out "\\~3,'0O" c))
             ((or (= c 40) (= c 41) (= c 92))
              (format out "\\~C" (aref string i)))
             (t
              (format out "~C" (aref string i)))))
  (format out ")")
  (setf (fill-pointer *current-string*) 0))
          
(defun output-header (out title resources)
  (format out "%!PS-Adobe-3.0~%")
  (format out "%%Creator: (Cedilla ~A)~%" *cedilla-version*)
  (when title
    (format out "%%Title: ~A" title))
  (format out "%%BoundingBox: ~A ~A ~A ~A~%"
          (floor (paper-size-x0 *paper-size*))
          (floor (paper-size-y0 *paper-size*))
          (ceiling (paper-size-x1 *paper-size*))
          (ceiling (paper-size-y1 *paper-size*)))
  (format out "%%DocumentData: Clean7Bit~%")
  (format out "%%Pages: (atend)~%")
  (format out "%%PageOrder: Ascend~%")
  (let ((all-resources '()) (needed-resources '()) (supplied-resources '()))
    (dolist (r resources)
      (unless (required-ps-resource-p r)
        (unless (member r all-resources :test #'resource-equivalent-p)
          (push r all-resources)
          (push r supplied-resources))))
    (dolist (r resources)
      (unless (member r all-resources :test #'resource-equivalent-p)
        (push r all-resources)
        (push r needed-resources)))
    (output-font-list "" all-resources out)
    (output-font-list "Needed" needed-resources out)
    (output-font-list "Supplied" supplied-resources out)
    (output-resource-list "" all-resources out)
    (output-resource-list "Needed" needed-resources out)
    (output-resource-list "Supplied" supplied-resources out))
  (format out "%%EndComments~%"))

(defun output-font-list (kind resource-list out)
  (let ((list (mapcan #'(lambda (r)
                          (if (eql (ps-resource-type r) :font) (list r) '()))
                      resource-list)))
    (when list
      (format out "%%Document~AFonts: " kind)
      (let ((length 0))
        (format out "~A" (ps-resource-name (car list)))
        (incf length (length (ps-resource-name (car list))))
        (dolist (r (cdr list))
          (let* ((f (ps-resource-name r)) (l (+ 1 (length f))))
            (when (> (+ length l) 70)
              (format out "~%%%+")
              (setf length 0))
            (format out " ~A" f)
            (incf length l)))
        (format out "~%")))))

(defun output-resource-list (kind resource-list out)
  (let ((list (mapcan #'(lambda (r)
                          (if (eql (ps-resource-type r) :font) '() (list r)))
                      resource-list)))
    (when list
      (format out "%%Document~AResources: " kind)
      (format out "~(~A~) ~A~%" 
              (ps-resource-type (car list)) (ps-resource-name (car list)))
      (dolist (r (cdr list))
        (format out "%%+ ~A ~A~%" (ps-resource-type r) (ps-resource-name r))))))

(defun output-prolog (out)
  (format out "%%BeginProlog~%")
  (download-resource *cedilla-procset* out)
  (format out "%%EndProlog~%"))

(defun output-setup (out fonts resources)
  (when (or fonts resources)
    (format out "%%BeginSetup~%")
    (dolist (r resources)
      (download-resource r out))
    (let ((font-counter 0) (instance-counter 0))
        (dolist (f fonts)
          (setup-font out f *size* (format nil "F~A" font-counter))
          (dolist (i (font-instances f))
            (setup-font-instance out i 
                                 (format nil "I~A" instance-counter))
            (incf instance-counter))
          (incf font-counter)))
    (format out "%%EndSetup~%")))

(defun output-trailer (out)
  (format out "%%Trailer~%")
  (format out "%%Pages: ~A~%" (- *page-number* 1))
  (format out "%%EOF~%"))

(defmethod typeset-glyph ((glyph font-glyph) out)
  (typeset-font-glyph glyph (glyph-instance glyph) (glyph-index glyph) out))

(defmethod typeset-glyph ((glyph composite-glyph) out)
  (dolist (c (composite-glyph-components glyph))
    (let ((*current-x* (+ *current-x* (scaled-quantity (component-dx c))))
          (*current-y* (+ *current-y* (scaled-quantity (component-dy c)))))
      (typeset-glyph (component-glyph c) out)))
  (incf *current-x* (scaled-glyph-width glyph)))

(defun string-width (s)
  (let ((w 0))
    (map nil
         #'(lambda (c)
             (let ((g (recall-glyph (normalise-ccs c))))
               (etypecase g
                 (null nil)
                 (glyph (incf w (scaled-glyph-width g)))
                 (list (mapc #'(lambda (g*) (incf w (scaled-glyph-width g*)))
                             g)))))
         s)
    w))

(defun typeset-string (s out)
  (map nil
       #'(lambda (c)
             (let ((g (recall-glyph (normalise-ccs c))))
               (etypecase g
                 (null nil)
                 (glyph (typeset-glyph g out))
                 (list (dolist (g* g) (typeset-glyph g* out))))))
       s))

(defun typeset-header (header y out)
  (flet ((th (s x)
          (when s
            (let ((*current-x* x)
                  (*current-y* y))
              (typeset-string s out)
              (finish-string out)))))
    (let ((hl (and (car header) (header-string (car header))))
          (hc (and (cadr header) (header-string (cadr header))))
          (hr (and (caddr header) (header-string (caddr header)))))
      (finish-string out)
      (th hl (+ (paper-size-x0 *paper-size*) 
                (paper-size-left-margin *paper-size*)))
      (th hc (/ (+ (paper-size-x0 *paper-size*) 
                   (paper-size-x1 *paper-size*)
                   (- (string-width hc)))
                2.0))
      (th hr (- (paper-size-x1 *paper-size*) 
                (+ (paper-size-right-margin *paper-size*)
                   (string-width hr)))))))

