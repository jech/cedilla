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

(defgeneric get-glyph (font name &optional index)
  (:documentation
   "Retrieve a glyph from FONT.
Depending on the font type, this looks the glyph up by NAME or by INDEX.")
  )

(defgeneric put-glyph (font glyph &optional index)
  (:documentation
   "Insert a glyph into FONT.
Depending on the font type, this inserts the glyph by GLYPH-NAME or by INDEX.")
  )

(defgeneric find-font-glyph (ccs font &optional dotless)
  (:documentation
   "Find a glyph in FONT that is suitable for typesetting CCS.
If DOTLESS is true, a dotless glyph will be requested; it is the callers
responsability to ensure that CCS is usually dotted.")
  )

(defgeneric typeset-glyph (glyph stream)
  (:documentation
   "Typeset GLYPH on the PostScript stream STREAM.
It is the callee's responsibility to call SYNCHRONISE-POSITION if necessary,
and to update *{CURRENT,TYPESETTER}-{X,Y}*.")
  )

(defgeneric typeset-font-glyph (glyph instance index stream)
  (:documentation
   "Typeset the FONT-GLYPH GLYPH associated to INSTANCE on STREAM.
This is called by the TYPESET-GLYPH method on FONT-GLYPH.")
  )

(defgeneric make-font-instance (font)
  (:documentation
   "Make a font instance suitable for use with FONT and insert it into FONT.")
  )

(defgeneric ensure-instance (glyph)
  (:documentation
   "Ensure that GLYPH belongs to an instance.
This will be called for any glyph that we decide is useful.  For composite,
magic etc. glyphs, this should recursively call ENSURE-INSTANCE on the
glyph's descendents.")
  )

(defgeneric make-transformed-glyph (glyph matrix)
  (:documentation
   "Make a linearly transformed version of GLYPH."))


(defgeneric setup-font (stream font size name)
  (:documentation
   "Do any set-up needed before using FONT.
If the font is realised as a PS font, the PS code will refer to it by the
given name.")
  )


(defgeneric setup-font-instance (stream instance name)
  (:documentation
   "Do any set-up needed before using INSTANCE.
If the instance is realised as a PS font, the PS code will refer to it by the
given name.")
  )

(defgeneric select-font-instance (instance stream)
  (:documentation
   "Switch to using the given INSTANCE.
This typically selects the associated PS font for realised instances, and
does nothing for unrealised instances.")
  )

(defgeneric download-resource (resource out)
  (:documentation
   "Download the given PostScript RESOURCE.
DSC bracketing is taken care of in :BEFORE and :AFTER methods.")
  )

