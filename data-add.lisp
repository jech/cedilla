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

;;; Check the beginning of unicode.lisp for the definitions of the
;;; data structures used and for an explanation of the #U and #u
;;; reader macros.

(eval-when (compile eval)
  (setup-cedilla-readtable))

(define-alternatives
  ;; Combining characters; only a few are computed from the UnicodeData file
  (#u300 #u60)
  (#u0302 #u2C6)                        ; "circumflex"
  (#u0302 #u5E)                         ; "asciicircum"
  (#u0304 #\-)
  (#u0305 #u2014)                       ; after "overscore"
  (#u0305 #uAF)
  (#u0305 #\-)
  (#u030B (:double-h . #u301))          ; after "hungarumlaut"
  (#u030C #u2C7)
  (#u030D #\')                          ; vertical line above
  (#u030E #\")                          ; double vertical line above
  (#u030F (:double-h . #u300))          ; double grave accent
  (#u0310 (#u30D . #u306))
  (#u0311 (:mirror-v . #u306))
  (#u0312 #u2018)                       ; turned comma above
  (#u0313 #\,)
  (#u0313 #u1FBF)
  (#u0314 (:mirror-h . #u313))              ; reversed comma above
  (#u0315 #\,)
  (#u0316 #\`)
  (#u0317 #uB4)
  ;; ...
  (#u00AD #\-)
  (#u031F (:small . #\+))
  (#u0320 (:small . #\-))
  (#u0323 #u2D9)
  (#u0324 #uA8)
  (#u0325 #u2DA)
  (#u0326 #\,)
  (#u0329 #\')                          ; vertical line below
  (#u032C #u2C7)
  (#u032D #u2C6)
  (#u032E #u2D8)
  (#u032F #u311)
  (#u0330 #u2DC)
  (#u0331 #uAF)
  (#u0331 #\-)
  (#u0332 #u2014)
  (#u0332 #uAF)
  (#u0332 #\-)
  (#u0333 (:double-v . #u305))          ; double low line below
  (#u0334 #u2DC)
  (#u0335 #\-)
  (#u0336 #u2014)
  (#u0336 #uAF)
  (#u0337 #\/)
  (#u0338 #u2044)
  (#u0338 #\/)
  (#u033D (:small . #uD7))
  (#u033E (:rotate . #\~))
  (#u033F (:double-v . #u305))          ; double overline
  (#u0342 #u2DC)                        ; perispomeni
  (#u0345 (:small . #u3B9))
  (#u0347 (:small . #\=))
  (#u0348 #\")                          ; double veritical line below
  (#u0360 (:double-v . #u303))
  (#u0361 (:double-v . #u311))
  (#u20D2 #\|)
  (#u20D3 (:small . #\|))
  (#u20D8 #u2DA)
  (#u20D6 #u2190)
  (#u20D7 #u2192)
  (#u20DB (:small . #u2234))
  (#u20DC (:small . "::"))
  (#u1FFE #u2018)                       ; dasia
  ;; random other stuff
  (#u110 #uD0)                          ; D-bar -> Eth
  ;; Don't include this as it would cause the dot of a dotted i to be
  ;; removed when composing.
  ;;  ((#u307 . #\i) #\i)
  (#u200C #u200B)                       ; zwj
  (#u200D #u200B)                       ; zwnj 
  (#u2218 #u25CB)
  ;; magic
  (#u110 (:bar . #\D))
  (#u111 (:bar . #\d))
  (#u1E4 (:bar . #\G))
  (#u1E5 (:bar . #\g))
  (#u126 (:bar . #\H))
  (#u127 (:bar . #\h))
  (#u197 (:bar . #\I))
  (#u268 (:bar . #\i))
  (#u166 (:bar . #\T))
  (#u167 (:bar . #\t))
  (#u1B5 (:bar . #\Z))
  (#u1B6 (:bar . #\z))
  (#u2127 (:mirror-v . #u2126))
  (#u2129 (:turn . #u3B9))
  (#u2132 (:turn . #\F))
  (#u213A (:rotate . #\Q))
  (#u2295 (:small-circle . #\+))
  (#u2296 (:small-circle . #\-))
  (#u2297 (:small-circle . #uD7))
  (#u2298 (:small-circle . #\/))
  (#u2299 (:small-circle . #uB7))
  (#u229A (:small-circle . #u2218))
  (#u229B (:small-circle . #\*))
  (#u229C (:small-circle . #\=))
  (#u229D (:small-circle . #\-))
  (#u229E (:square . #\+))
  (#u229F (:square . #\-))
  (#u22A0 (:square . #uD7))
  (#u22A1 (:square . #uB7))
)

(define-fallbacks
  ;; Combining characters
  (#u20D0 #u20D6)
  (#u20D1 #u20D7)
  ;; Non-combining characters
  (#u018F (:stretch . #u0259))
  (#u01DD (:turn . #\e))
  (#u0258 (:mirror-h . #\e))
  (#u0259 (:turn . #\e))
  ;; Modifier letters
  (#u02B4 (:super . (:turn . #\r)))
  (#u02B6 (:super . (:mirror-v . #\R)))
  (#u02B9 #\')
  (#u02BA #.(coerce '(#u02B9 #u02B9) 'vector))
  (#u02BB (#u312 . #\Space))
  (#u02BC #\')
  (#u02BD (#u314 . #\Space))
  (#u02C6 #\^)
  (#u02C7 (#u030C . #\Space))
  (#u02C8 #\')
  (#u02C9 (#u304 . #\Space))
  (#u02CA (#u301 . #\Space))
  (#u02CB (#u300 . #\Space))
  (#u02CC (#u329 . #\Space))
  (#u02CD (#u331 . #\Space))
  (#u02CE (#u316 . #\Space))
  (#u02CF (#u317 . #\Space))
  (#u02EE #.(coerce '(#u2BC #u2BC) 'vector))
  ;; Punctuation
  (#u2010 #\-)                          ; hyphen
  (#u2011 #\-)                          ; non-breaking hyphen
  (#u2012 #\-)                          ; figure dash
  (#u2013 #\-)                          ; en dash
  (#u2014 #\-)                          ; em dash
  (#u2015 #u2014)                       ; horizontal bar
  (#u2016 (:double-h . #\|))
  (#u201A #\,)
  (#u201B #u2018)                       ; high-reversed-9 q. mark
  (#u201E #.(coerce '(#u201A #u201A) 'string))
  (#u201F #u201C)                       ; double high-reversed-9 q. mark
  (#u2032 #\')
  (#u2035 #u2018)
  (#u2043 #\-)
  (#u2044 #\/)
  (#u212E #\e)
  ;; Mathematical operators
  (#u220A (:small . #u2208))
  (#u220B (:mirror-h . #u2208))
  (#u220D (:small . #u220B))
  (#u2210 (:mirror-v . #u220F))
  (#u2212 #\-)
  (#u2223 #\|)
  (#u2225 #u2016)
  (#u2213 (:mirror-v . #u00B1))
  (#u2214 (#u0307 . #\+))
  (#u2216 #\\)
  (#u221B #.(coerce '(#u00B3 #u221A) 'vector))
  (#u221C #.(coerce '(#u2074 #u221A) 'vector))
  (#u2225 #u2016)
  (#u2235 (:mirror-v . #u2234))
  (#u2236 #\:)
  (#u2237 (:double-h . #u2236))
  (#u2238 (#u0307 . #\-))
  (#u223C #\~)
  (#u223D (:mirror-h . #u223C))
  (#u2240 (:rotate . #\~))
  (#u2242 (#U0304 . #\~))
  (#u2243 (#U0331 . #\~))
  (#u2250 (#u0307 . #\=))
  (#u2266 (#u0347 . #\<))
  (#u2267 (#u0347 . #\>))
  (#u226A (:double-h . #\<))
  (#u226B (:double-h . #\>))
  (#u22C4 #u25C7)
  (#u22C6 #u2217)
  )

(define-alternate-glyph-names
  ;; Alternatives that don't go through Unicode
  (#u0305 "overscore")
  (#u0306 "cyrbreve")
  (#u0307 "dot")
  (#u030F "dblgrave")
  (#u0311 "invbreve")     ; ???
  ;; ...
  (#u0332 "overscore")    ; low line below
  (#u0336 "overscore")
  (#u03BC "mu")
  (#u0394 "Delta")
)
