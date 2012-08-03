(in-package "CEDILLA")

;;; For compatibility with legacy encodings, many Latin fonts contain
;;; a number of Greek glyphs (almost always mu, but sometimes also
;;; others).  In order to get all Greek glyphs from the same font, we
;;; explicitly :OMIT them.

(eval-when (load compile eval)
(defparameter *greek-glyph-names*
  '("Alpha" "Beta" "Chi" "Delta" "Epsilon" "Phi" "Gamma" "Eta" "Iota"
    "theta1" "Kappa" "Lambda" "Mu" "Nu" "Omicron" "Pi" "Theta" "Rho"
    "Sigma" "Tau" "Upsilon" "sigma1" "Omega" "Xi" "Psi" "Zeta" "alpha"
    "beta" "chi" "delta" "epsilon" "phi" "gamma" "eta" "iota" "phi1"
    "kappa" "lambda" "mu" "nu" "omicron" "pi" "theta" "rho" "sigma" "tau"
    "upsilon" "omega1" "omega" "xi" "psi" "zeta" "Upsilon1"))
)

;;; Where we search for AFM, PFA and PFB files.
(defparameter *resources-path*
  '("/usr/share/texlive/texmf-dist/fonts/afm/adobe/courier"
    "/usr/share/texlive/texmf-dist/fonts/afm/adobe/times"
    "/usr/share/texlive/texmf-dist/fonts/afm/adobe/helvetic"
    "/usr/share/texlive/texmf-dist/fonts/afm/adobe/symbol"
    "/usr/share/texlive/texmf-dist/fonts/afm/adobe/zapfding"
    "/usr/share/texlive/texmf-dist/fonts/afm/adobe/utopia"
    "/usr/share/texlive/texmf-dist/fonts/type1/adobe/utopia"
    "/usr/share/texlive/texmf-dist/fonts/afm/public/omega"
    "/usr/share/texlive/texmf-dist/fonts/type1/public/omega"
    "/usr/share/fonts/type1/gsfonts"))


;;; Font sets using standard PS fonts.

(define-fontset "courier"
  `((:afm "pcrr8a.afm"
     :omit ,*greek-glyph-names*)
    (:built-in :width 600 :cap-height 562 :x-height 426)
    (:afm "psyr.afm")
    (:afm "pzdr.afm"
     :encoding ,#'zapf-dingbats-encoding)))

(define-fontset "times"
  `((:afm "ptmr8a.afm"
     :omit ,*greek-glyph-names*)
    (:built-in :width 667 :figure-width 500 :cap-height 662 :x-height 450)
    (:afm "psyr.afm")
    (:afm "pzdr.afm" :encoding ,#'zapf-dingbats-encoding)))

(define-fontset "helvetica"
  `((:afm "phvr8a.afm"
     :omit ,*greek-glyph-names*)
    (:built-in :width 722 :figure-width 556 :cap-height 718 :x-height 523)
    (:afm "psyr.afm")
    (:afm "pzdr.afm" :encoding ,#'zapf-dingbats-encoding)))

;;; Just like Courier, but with narrower spaces.

(define-fontset "courier-space"
  `((:space :width 300)
    (:afm "pcrr8a.afm"
     :omit ,*greek-glyph-names*)
    (:built-in :width 600 :cap-height 562 :x-height 426)
    (:afm "psyr.afm")
    (:afm "pzdr.afm" :encoding ,#'zapf-dingbats-encoding)))

;;; Random free fonts.

;;; An Adobe font that found its way into X11R5.

(define-fontset "utopia"
  `((:afm "putr8a.afm"
     :resources ("putr8a.pfb")
     :omit ,*greek-glyph-names*)
    (:built-in :width 684 :figure-width 530 :cap-height 692 :x-height 509)
    (:afm "psyr.afm")
    (:afm "pzdr.afm" :encoding ,#'zapf-dingbats-encoding)))

;;; The Omega Serif fonts, a set of international fonts provided with
;;; Omega, Yannis Haralambous' international version of TeX.

;;; As these fonts are designed for use with Omega, they do not
;;; include either a space or any precombined characters.  In
;;; addition, they use a selection of non-standard glyph names, which
;;; we compensate for.

(eval-when (load compile eval)
(defparameter *omega-serif-fonts*
  `(
    ;; Omega Serif Common
    (:afm "omseco.afm"
     :resources ("omseco.pfb")
     :encoding ,#'omega-encoding)
    ;; Omega Serif Latin
    (:afm "omsela.afm"
     :resources ("omsela.pfb")
     :encoding ,#'omega-encoding)
    ;; Omega Serif Greek
    (:afm "omsegr.afm"
     :resources ("omsegr.pfb")
     :encoding ,#'omega-encoding)
    ;; Omega Serif Cyrillic and Cyrillic Extended
    (:afm "omsecy.afm"
     :resources ("omsecy.pfb")
     :encoding ,#'omega-encoding)
    (:built-in :width 667 :figure-width 500 :cap-height 662 :x-height 450)
    (:afm "psyr.afm")
    (:afm "pzdr.afm" :encoding ,#'zapf-dingbats-encoding)))
)

;;; One way of making the Omega Serif fonts usable is to simply
;;; provide them with a space glyph.
(define-fontset "omega-serif"
  `((:space :width 250)
    ,@*omega-serif-fonts*))

;;; The alternative is to use them with the Times-Roman font, with
;;; which they blend well
(define-fontset "times-omega-serif"
  `((:afm "ptmr8a.afm"
     :omit ,*greek-glyph-names*)
    ,@*omega-serif-fonts*))

;;; The URW++ fonts included with Ghostscript.  Older versions cover
;;; the full PS level 3 character set, which covers most European
;;; languages.  Recent add Cyrillic glyphs.

(define-fontset "urw-gothic"
  `((:afm "a010013l.afm"
     :resources ("a010013l.pfb")
     :omit ,*greek-glyph-names*)
    (:built-in :width 813 :figure-width 554 :cap-height 739 :x-height 547)
    (:afm "s050000l.afm"
     :resources ("s050000l.pfb"))
    (:afm "pzdr.afm"
     :encoding ,#'zapf-dingbats-encoding)))
    
(define-fontset "bookman"
  `((:afm "b018012l.afm"
     :resources ("b018012l.pfb")
     :omit ,*greek-glyph-names*)
    (:built-in :width 740 :figure-width 620 :cap-height 681 :x-height 485)
    (:afm "s050000l.afm"
     :resources ("s050000l.pfb"))
    (:afm "pzdr.afm"
     :encoding ,#'zapf-dingbats-encoding)))
    
(define-fontset "century-schoolbook"
  `((:afm "c059013l.afm"
     :resources ("c059013l.pfb")
     :omit ,*greek-glyph-names*)
    (:built-in :width 722 :figure-width 556 :cap-height 722 :x-height 466)
    (:afm "s050000l.afm"
     :resources ("s050000l.pfb"))
    (:afm "pzdr.afm"
     :encoding ,#'zapf-dingbats-encoding)))

(define-fontset "nimbus-sans"
  `((:afm "n019003l.afm"
     :resources ("n019003l.pfb")
     :omit ,*greek-glyph-names*)
    (:built-in :width 667 :figure-width 556 :cap-height 729 :x-height 524)
    (:afm "s050000l.afm"
     :resources ("s050000l.pfb"))
    (:afm "pzdr.afm"
     :encoding ,#'zapf-dingbats-encoding)))

(define-fontset "nimbus-roman"
  `((:afm "n021003l.afm"
     :resources ("n021003l.pfb")
     :omit ,*greek-glyph-names*)
    (:built-in :width 667 :figure-width 500 :cap-height 662 :x-height 450)
    (:afm "s050000l.afm"
     :resources ("s050000l.pfb"))
    (:afm "pzdr.afm"
     :encoding ,#'zapf-dingbats-encoding)))

(define-fontset "nimbus-mono"
  `((:afm "n022003l.afm"
     :resources ("n022003l.pfb")
     :omit ,*greek-glyph-names*)
    (:built-in :width 600 :figure-width 600 :cap-height 563 :x-height 417)
    (:afm "s050000l.afm"
     :resources ("s050000l.pfb"))
    (:afm "pzdr.afm"
     :encoding ,#'zapf-dingbats-encoding)))

(define-fontset "palladio"
  `((:afm "p052003l.afm"
     :resources ("p052003l.pfb")
     :omit ,*greek-glyph-names*)
    (:built-in :width 709 :figure-width 500 :cap-height 692 :x-height 469)
    (:afm "s050000l.afm"
     :resources ("s050000l.pfb"))
    (:afm "pzdr.afm"
     :encoding ,#'zapf-dingbats-encoding)))

;;; Cedilla really should compensate for the font's ItalicAngle when
;;; positioning diacritics.

(define-fontset "chancery"
  `((:afm "z003034l.afm"
     :resources ("z003034l.pfb")
     :omit ,*greek-glyph-names*)
    (:built-in :width 520 :figure-width 440 :cap-height 678 :x-height 411)
    (:afm "s050000l.afm")
    (:afm "pzdr.afm"
     :encoding ,#'zapf-dingbats-encoding)))

;;; Paper sizes.  See macros.lisp for additional keyword arguments.

(define-paper-size "A4"
  (cm 21) (cm 29.7))

(define-paper-size "Letter"
  (in 8.5) (in 11))
