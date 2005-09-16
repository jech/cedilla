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

;;; Font sets using standard PS fonts.

(define-fontset "courier"
  `((:afm "/usr/share/texmf/fonts/afm/adobe/courier/pcrr8a.afm"
     :omit ,*greek-glyph-names*)
    (:built-in :width 600 :cap-height 562 :x-height 426)
    (:afm "/usr/share/texmf/fonts/afm/adobe/symbol/psyr.afm")
    (:afm "/usr/share/texmf/fonts/afm/adobe/zapfding/pzdr.afm"
     :encoding ,#'zapf-dingbats-encoding)))

(define-fontset "times"
  `((:afm "/usr/share/texmf/fonts/afm/adobe/times/ptmr8a.afm"
     :omit ,*greek-glyph-names*)
    (:built-in :width 667 :figure-width 500 :cap-height 662 :x-height 450)
    (:afm "/usr/share/texmf/fonts/afm/adobe/symbol/psyr.afm")
    (:afm "/usr/share/texmf/fonts/afm/adobe/zapfding/pzdr.afm"
     :encoding ,#'zapf-dingbats-encoding)))

(define-fontset "helvetica"
  `((:afm "/usr/share/texmf/fonts/afm/adobe/helvetic/phvr8a.afm"
     :omit ,*greek-glyph-names*)
    (:built-in :width 722 :figure-width 556 :cap-height 718 :x-height 523)
    (:afm "/usr/share/texmf/fonts/afm/adobe/symbol/psyr.afm")
    (:afm "/usr/share/texmf/fonts/afm/adobe/zapfding/pzdr.afm"
     :encoding ,#'zapf-dingbats-encoding)))

;;; Just like Courier, but with narrower spaces.

(define-fontset "courier-space"
  `((:space :width 300)
    (:afm "/usr/share/texmf/fonts/afm/adobe/courier/pcrr8a.afm"
     :omit ,*greek-glyph-names*)
    (:built-in :width 600 :cap-height 562 :x-height 426)
    (:afm "/usr/share/texmf/fonts/afm/adobe/symbol/psyr.afm")
    (:afm "/usr/share/texmf/fonts/afm/adobe/zapfding/pzdr.afm"
     :encoding ,#'zapf-dingbats-encoding)))

;;; Random free fonts.

;;; This version of Courier covers all of Latin-2 out of the box.  It
;;; also contains a number of line- and box-drawing glyphs for
;;; compatibility with the IBM-PC character set.
   
(define-fontset "ibm-courier"
  `((:afm "/usr/X11R6/lib/X11/fonts/Type1/cour.afm"
     :resources ("/usr/X11R6/lib/X11/fonts/Type1/cour.pfa")
     :omit ,*greek-glyph-names*)
    (:built-in :width 600 :cap-height 562 :x-height 426)
    (:afm "/usr/share/texmf/fonts/afm/adobe/symbol/psyr.afm")
    (:afm "/usr/share/texmf/fonts/afm/adobe/zapfding/pzdr.afm"
     :encoding ,#'zapf-dingbats-encoding)))

;;; An Adobe font that found its way into X11R5.

(define-fontset "utopia"
  `((:afm "/usr/X11R6/lib/X11/fonts/Type1/UTRG____.afm"
     :resources ("/usr/X11R6/lib/X11/fonts/Type1/UTRG____.pfa")
     :omit ,*greek-glyph-names*)
    (:built-in :width 684 :figure-width 530 :cap-height 692 :x-height 509)
    (:afm "/usr/share/texmf/fonts/afm/adobe/symbol/psyr.afm")
    (:afm "/usr/share/texmf/fonts/afm/adobe/zapfding/pzdr.afm"
     :encoding ,#'zapf-dingbats-encoding)))

;;; The Luxi fonts, included with XFree86 4.2.0.

(define-fontset "luxi-mono"
  `((:afm "/usr/X11R6/lib/X11/fonts/Type1/l047013t.afm"
     :resources ("/usr/X11R6/lib/X11/fonts/Type1/l047013t.pfa")
     :omit ,*greek-glyph-names*)
    (:built-in :width 600 :cap-height 692 :x-height 509)
    (:afm "/usr/share/texmf/fonts/afm/adobe/symbol/psyr.afm")
    (:afm "/usr/share/texmf/fonts/afm/adobe/zapfding/pzdr.afm"
     :encoding ,#'zapf-dingbats-encoding)))

(define-fontset "luxi-sans"
  `((:afm "/usr/X11R6/lib/X11/fonts/Type1/l048013t.afm"
     :resources ("/usr/X11R6/lib/X11/fonts/Type1/l048013t.pfa")
     :omit ,*greek-glyph-names*)
    (:built-in :width 722 :figure-width 556 :cap-height 723 :x-height 530)
    (:afm "/usr/share/texmf/fonts/afm/adobe/symbol/psyr.afm")
    (:afm "/usr/share/texmf/fonts/afm/adobe/zapfding/pzdr.afm"
     :encoding ,#'zapf-dingbats-encoding)))

(define-fontset "luxi-serif"
  `((:afm "/usr/X11R6/lib/X11/fonts/Type1/l049013t.afm"
     :resources ("/usr/X11R6/lib/X11/fonts/Type1/l049013t.pfa")
     :omit ,*greek-glyph-names*)
    (:built-in :width 667 :figure-width 500 :cap-height 723 :x-height 530)
    (:afm "/usr/share/texmf/fonts/afm/adobe/symbol/psyr.afm")
    (:afm "/usr/share/texmf/fonts/afm/adobe/zapfding/pzdr.afm"
     :encoding ,#'zapf-dingbats-encoding)))

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
    (:afm "/usr/share/texmf/fonts/afm/public/omega/omseco.afm"
     :resources ("/usr/share/texmf/fonts/type1/public/omega/omseco.pfb")
     :encoding ,#'omega-encoding)
    ;; Omega Serif Latin
    (:afm "/usr/share/texmf/fonts/afm/public/omega/omsela.afm"
     :resources ("/usr/share/texmf/fonts/type1/public/omega/omsela.pfb")
     :encoding ,#'omega-encoding)
    ;; Omega Serif Greek
    (:afm "/usr/share/texmf/fonts/afm/public/omega/omsegr.afm"
     :resources ("/usr/share/texmf/fonts/type1/public/omega/omsegr.pfb")
     :encoding ,#'omega-encoding)
    ;; Omega Serif Cyrillic and Cyrillic Extended
    (:afm "/usr/share/texmf/fonts/afm/public/omega/omsecy.afm"
     :resources ("/usr/share/texmf/fonts/type1/public/omega/omsecy.pfb")
     :encoding ,#'omega-encoding)
    #+ignore
    (:afm "/usr/share/texmf/fonts/afm/public/omega/omsecx.afm"
     :resources ("/usr/share/texmf/fonts/type1/public/omega/omsecx.pfb")
     :encoding ,#'omega-encoding)
    ;; Omega Serif Armenian
    #+ignore
    (:afm "/usr/share/texmf/fonts/afm/public/omega/omseha.afm"
     :resources ("/usr/share/texmf/fonts/type1/public/omega/omseha.pfb")
     :encoding ,#'omega-encoding)
    ;; Omega Serif Hebrew
    #+ignore
    (:afm "/usr/share/texmf/fonts/afm/public/omega/omsehe.afm"
     :resources ("/usr/share/texmf/fonts/type1/public/omega/omsehe.pfb")
     :encoding ,#'omega-encoding)
    ;; Omega Serif Tifinagh
    #+ignore
    (:afm "/usr/share/texmf/fonts/afm/public/omega/omseti.afm"
     :resources ("/usr/share/texmf/fonts/type1/public/omega/omseti.pfb")
     :encoding ,#'omega-encoding)
    ;; Omega Serif IPA
    #+ignore
    (:afm "/usr/share/texmf/fonts/afm/public/omega/omseip.afm"
     :resources ("/usr/share/texmf/fonts/type1/public/omega/omseip.pfb")
     :encoding ,#'omega-encoding)
    (:built-in :width 667 :figure-width 500 :cap-height 662 :x-height 450)))
)

;;; One way of making the Omega Serif fonts usable is to simply
;;; provide them with a space glyph.
(define-fontset "omega-serif"
  `((:space :width 250)
    ,@*omega-serif-fonts*))

;;; The alternative is to use them with the Times-Roman font, with
;;; which they blend well
(define-fontset "times-omega-serif"
  `((:afm "/usr/share/texmf/fonts/afm/adobe/times/ptmr8a.afm"
     :omit ,*greek-glyph-names*)
    ,@*omega-serif-fonts*))

;;; Antykwa Poltawskiego.

(define-fontset "antp"
  `((:afm "/usr/share/texmf/fonts/afm/public/antp/antpr.afm"
     :omit ,*greek-glyph-names*
     :resources ("/usr/share/texmf/fonts/type1/public/antp/antpr.pfb"))
    (:built-in :width 656 :figure-width 480 :cap-height 700 :x-height 440)
    (:afm "/usr/share/texmf/fonts/afm/adobe/symbol/psyr.afm")
    (:afm "/usr/share/texmf/fonts/afm/adobe/zapfding/pzdr.afm"
     :encoding ,#'zapf-dingbats-encoding)))

;;; Antykwa Torunska.

(define-fontset "antt"
  `((:afm "/usr/share/texmf/fonts/afm/public/antt/anttr.afm"
     :omit ,*greek-glyph-names*
     :resources ("/usr/share/texmf/fonts/type1/public/antt/anttr.pfb"))
    (:built-in :width 763 :figure-width 520 :cap-height 714 :x-height 504)
    (:afm "/usr/share/texmf/fonts/afm/adobe/symbol/psyr.afm")
    (:afm "/usr/share/texmf/fonts/afm/adobe/zapfding/pzdr.afm"
     :encoding ,#'zapf-dingbats-encoding)))

;;; The URW++ fonts included with Ghostscript.  The versions included
;;; with GS 6.0 cover the full PS level 3 character set, which
;;; covers most European languages.

(define-fontset "gothicl"
  `((:afm "/usr/share/fonts/type1/gsfonts/a010013l.afm"
     :resources ("/usr/share/fonts/type1/gsfonts/a010013l.pfb")
     :omit ,*greek-glyph-names*)
    (:built-in :width 813 :figure-width 554 :cap-height 739 :x-height 547)
    (:afm "/usr/share/texmf/fonts/afm/adobe/symbol/psyr.afm")
    (:afm "/usr/share/texmf/fonts/afm/adobe/zapfding/pzdr.afm"
     :encoding ,#'zapf-dingbats-encoding)))
    
(define-fontset "bookman"
  `((:afm "/usr/share/fonts/type1/gsfonts/b018012l.afm"
     :resources ("/usr/share/fonts/type1/gsfonts/b018012l.pfb")
     :omit ,*greek-glyph-names*)
    (:built-in :width 740 :figure-width 620 :cap-height 681 :x-height 485)
    (:afm "/usr/share/texmf/fonts/afm/adobe/symbol/psyr.afm")
    (:afm "/usr/share/texmf/fonts/afm/adobe/zapfding/pzdr.afm"
     :encoding ,#'zapf-dingbats-encoding)))
    
(define-fontset "century-schoolbook"
  `((:afm "/usr/share/fonts/type1/gsfonts/c059013l.afm"
     :resources ("/usr/share/fonts/type1/gsfonts/c059013l.pfb")
     :omit ,*greek-glyph-names*)
    (:built-in :width 722 :figure-width 556 :cap-height 722 :x-height 466)
    (:afm "/usr/share/texmf/fonts/afm/adobe/symbol/psyr.afm")
    (:afm "/usr/share/texmf/fonts/afm/adobe/zapfding/pzdr.afm"
     :encoding ,#'zapf-dingbats-encoding)))
    
(define-fontset "palladio"
  `((:afm "/usr/share/fonts/type1/gsfonts/p052003l.afm"
     :resources ("/usr/share/fonts/type1/gsfonts/p052003l.pfb")
     :omit ,*greek-glyph-names*)
    (:built-in :width 709 :figure-width 500 :cap-height 692 :x-height 469)
    (:afm "/usr/share/texmf/fonts/afm/adobe/symbol/psyr.afm")
    (:afm "/usr/share/texmf/fonts/afm/adobe/zapfding/pzdr.afm"
     :encoding ,#'zapf-dingbats-encoding)))

;;; Cedilla really should compensate for the font's ItalicAngle when
;;; positioning diacritics.

(define-fontset "chancery"
  `((:afm "/usr/share/fonts/type1/gsfonts/z003034l.afm"
     :resources ("/usr/share/fonts/type1/gsfonts/z003034l.pfb")
     :omit ,*greek-glyph-names*)
    (:built-in :width 520 :figure-width 440 :cap-height 678 :x-height 411)
    (:afm "/usr/share/texmf/fonts/afm/adobe/symbol/psyr.afm")
    (:afm "/usr/share/texmf/fonts/afm/adobe/zapfding/pzdr.afm"
     :encoding ,#'zapf-dingbats-encoding)))

;;; This is a Cyrillic font that uses standard glyph names -- no
;;; hackery needed.  It also covers all of the AdobeStandard encoding,
;;; so there's no need to provide Courier as a fallback.

;;; For some strange reason it contains a Yi that claims to be
;;; uni0259 (schwa), so we omit that.

(define-fontset "cyrillic-courier"
  `((:afm "/usr/X11R6/lib/X11/fonts/Type1/n022003d.afm"
     :resources ("/usr/X11R6/lib/X11/fonts/Type1/n022003d.pfb")
     :omit ("uni0259" ,@*greek-glyph-names*))
    (:built-in :width 600 :cap-height 562 :x-height 426)
    (:afm "/usr/share/texmf/fonts/afm/adobe/symbol/psyr.afm")
    (:afm "/usr/share/texmf/fonts/afm/adobe/zapfding/pzdr.afm"
     :encoding ,#'zapf-dingbats-encoding)))

;;; Paper sizes.  See macros.lisp for additional keyword arguments.

(define-paper-size "A4"
  (cm 21) (cm 29.7))

(define-paper-size "Letter"
  (in 8.5) (in 11))
