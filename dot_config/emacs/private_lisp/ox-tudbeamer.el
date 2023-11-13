;;; ox-tudbeamer.el --- Cutsom Beamer LaTeX Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This library implements a LaTeX beamer tud back-end and export,
;; derived from the LaTeX one.

;;; Code:
(require 'cl-lib)
(require 'org)
(require 'ox-latex)
(require 'ox-beamer)


(add-to-list 'org-latex-classes
	     '("tudscrartcl" "\\documentclass[]{tudscrartcl}"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(add-to-list 'org-latex-classes
	           '("tudbeamer" "\\documentclass[presentation,t,aspectratio=169]{beamer}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]
\\usepackage[utf8]{inputenc}
\\usepackage[normalem]{ulem}
\\usepackage[TS1,T1]{fontenc}
\\usepackage{graphicx}
\\usepackage{grffile}
\\usepackage{longtable}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{amssymb}
\\usepackage{capt-of}
\\usepackage{hyperref}
\\usepackage{booktabs}
\\usepackage{amsmath}
\\usepackage{pdfcomment}
\\usepackage[AUTO]{babel}
\\usepackage{tudscrfonts}
\\usetheme{tud}
"
	             ("\\section{%s}" . "\\section*{%s}")
	             ("\\subsection{%s}" . "\\subsection*{%s}")
	             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))



;;; User-Configurable Variables

(defgroup org-export-tudscrartcl nil
  "Options specific for using the tudscrartcl class in LaTeX export."
  :tag "Org tudscrartcl"
  :group 'org-export
  :version "25.3")

(defgroup org-export-tudbeamer nil
  "Options specific for using the tudbeamer class in LaTeX export."
  :tag "Org tudbeamer"
  :group 'org-export
  :version "25.3")

(org-export-define-derived-backend 'tudscrartcl 'latex
  :options-alist
  '((:latex-class "LATEX_CLASS" nil "tudscrartcl" t)
    (:institute "INSTITUTE" nil nil pase)
    (:faculty "FACULTY" nil nil parse))
  :menu-entry '(?l "Export to LaTeX"
                   ((?T "As PDF file and open (tudscrartcl)"
                        (lambda (a s v b)
                          (let ((outfile (org-export-output-file-name ".tex" s)))
                            (if a
                                (org-export-to-file 'tudscrartcl outfile
                                  t s v b nil #'org-latex-compile)
                              (org-open-file (org-export-to-file 'tudscrartcl outfile
                                               a s v b nil #'org-latex-compile))))))))
  :translate-alist '((template . org-tudscrartcl-template)))

;;; Define Back-End
(org-export-define-derived-backend 'tudbeamer 'beamer
  :options-alist
  '((:latex-class "LATEX_CLASS" nil "tudbeamer" t)
    (:pdfcomment-export "PDFCOMMENT_EXPORT" nil "nil" t)
    (:institute "INSTITUTE" nil nil parse))
  :menu-entry '(?l "Export to LaTeX"
                  ((?t "TUD Beamer (open)"
                        (lambda (a s v b)
                          (let ((outfile (org-export-output-file-name ".tex" s)))
                            (if a
                                (org-export-to-file 'tudbeamer outfile
                                  t s v b nil #'org-latex-compile)
                              (org-open-file (org-export-to-file 'tudbeamer outfile
                                               a s v b nil #'org-latex-compile))))))))
  :translate-alist '((template . org-tudbeamer-template)
                     (keyword . org-tudbeamer-keyword)))


(defun org-tudbeamer-keyword (keyword _contents info)
 "Übersetze das spezielle PDFCOMMENT-Keyword in Org zu \pdfcomment in LaTeX, 
behandle andere Keywords standardmäßig, basierend auf der Option PDFCOMMENT_EXPORT."
  (if (and (string= "PDFCOMMENT" (org-element-property :key keyword))
           (string= "t" (plist-get info :pdfcomment-export)))
      (format "\\pdfcomment{%s}" (org-element-property :value keyword))
    ;; Verwende die Standardbehandlung des beamer Backends für alle anderen Keywords
    (org-export-with-backend 'beamer keyword _contents info)))
;;;; Template
;;
;; Template used is similar to the one used in `latex' back-end,
;; excepted for the table of contents and tudbeamer themes.

(defun org-tudbeamer-template (contents info)
  "Return complete document string after Beamer conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((title (org-export-data (plist-get info :title) info))
	(subtitle (org-export-data (plist-get info :subtitle) info)))
    (concat
     ;; Time-stamp.
     (and (plist-get info :time-stamp-file)
	  (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
     ;; LaTeX compiler
     (org-latex--insert-compiler info)
     ;; Document class and packages.
     (org-latex-make-preamble info)
     ;; Insert themes.
     (let ((format-theme
	    (lambda (prop command)
	      (let ((theme (plist-get info prop)))
		(when theme
		  (concat command
			  (if (not (string-match "\\[.*\\]" theme))
			      (format "{%s}\n" theme)
			    (format "%s{%s}\n"
				    (match-string 0 theme)
				    (org-trim
				     (replace-match "" nil nil theme))))))))))
       (mapconcat (lambda (args) (apply format-theme args))
		  '((:beamer-theme "\\usetheme")
		    (:beamer-color-theme "\\usecolortheme")
		    (:beamer-font-theme "\\usefonttheme")
		    (:beamer-inner-theme "\\useinnertheme")
		    (:beamer-outer-theme "\\useoutertheme"))
		  ""))
     ;; Possibly limit depth for headline numbering.
     (let ((sec-num (plist-get info :section-numbers)))
       (when (integerp sec-num)
	 (format "\\setcounter{secnumdepth}{%d}\n" sec-num)))
     ;; Author.
     (let ((author (and (plist-get info :with-author)
			(let ((auth (plist-get info :author)))
			  (and auth (org-export-data auth info)))))
	   (email (and (plist-get info :with-email)
		       (org-export-data (plist-get info :email) info))))
       (cond ((and author email (not (string= "" email)))
	      (format "\\author{%s\\thanks{%s}}\n" author email))
	     ((or author email) (format "\\author{%s}\n" (or author email)))))
     ;; Date.
     (let ((date (and (plist-get info :with-date) (org-export-get-date info))))
       (format "\\date{%s}\n" (org-export-data date info)))
     ;; Institute.
     (let ((institute (plist-get info :institute)))
       (format "\\institut{%s}\n" (org-export-data institute info)))
     ;; Title
     (format "\\title{%s}\n" title)
     (when (org-string-nw-p subtitle)
       (concat (format (plist-get info :beamer-subtitle-format) subtitle) "\n"))
     ;; Beamer-header
     (let ((beamer-header (plist-get info :beamer-header)))
       (when beamer-header
	 (format "%s\n" (plist-get info :beamer-header))))
     ;; 9. Hyperref options.
     (let ((template (plist-get info :latex-hyperref-template)))
       (and (stringp template)
	    (format-spec template (org-latex--format-spec info))))
     ;; engrave-faces-latex preamble
     (when (and (eq org-latex-src-block-backend 'engraved)
                (org-element-map (plist-get info :parse-tree)
                    '(src-block inline-src-block) #'identity
                    info t))
       (org-latex-generate-engraved-preamble info))
     ;; Document start.
     "\\begin{document}\n\n"
     ;; Title command.
     (org-element-normalize-string
      (cond ((not (plist-get info :with-title)) nil)
	    ((string= "" title) nil)
	    ((not (stringp org-latex-title-command)) nil)
	    ((string-match "\\(?:[^%]\\|^\\)%s"
			   org-latex-title-command)
	     (format org-latex-title-command title))
	    (t org-latex-title-command)))
     ;; Table of contents.
     (let ((depth (plist-get info :with-toc)))
       (when depth
	 (concat
	  (format "\\begin{frame}%s{%s}\n"
		  (org-beamer--normalize-argument
		   (plist-get info :beamer-outline-frame-options) 'option)
		  (plist-get info :beamer-outline-frame-title))
	  (when (wholenump depth)
	    (format "\\setcounter{tocdepth}{%d}\n" depth))
	  "\\tableofcontents\n"
	  "\\end{frame}\n\n")))
     ;; Document's body.
     contents
     ;; Creator.
     (if (plist-get info :with-creator)
	 (concat (plist-get info :creator) "\n")
       "")
     ;; Document end.
     "\\end{document}")))

(defun org-tudscrartcl-template (contents info)
  "Return complete document string after LaTeX conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((title (org-export-data (plist-get info :title) info))
	(spec (org-latex--format-spec info)))
    (concat
     ;; Time-stamp.
     (and (plist-get info :time-stamp-file)
	  (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
     ;; LaTeX compiler.
     (org-latex--insert-compiler info)
     ;; Document class and packages.
     (org-latex-make-preamble info)
     ;; Possibly limit depth for headline numbering.
     (let ((sec-num (plist-get info :section-numbers)))
       (when (integerp sec-num)
	 (format "\\setcounter{secnumdepth}{%d}\n" sec-num)))
     ;; Author.
     (let ((author (and (plist-get info :with-author)
			(let ((auth (plist-get info :author)))
			  (and auth (org-export-data auth info)))))
	   (email (and (plist-get info :with-email)
		       (org-export-data (plist-get info :email) info))))
       (cond ((and author email (not (string= "" email)))
	      (format "\\author{%s\\thanks{%s}}\n" author email))
	     ((or author email) (format "\\author{%s}\n" (or author email)))))
     ;; Date.
     ;; LaTeX displays today's date by default. One can override this by
     ;; inserting \date{} for no date, or \date{string} with any other
     ;; string to be displayed as the date.
     (let ((date (and (plist-get info :with-date) (org-export-get-date info))))
       (format "\\date{%s}\n" (org-export-data date info)))
     ;; Title and subtitle.
     (let* ((subtitle (plist-get info :subtitle))
	    (formatted-subtitle
	     (when subtitle
	       (format (plist-get info :latex-subtitle-format)
		       (org-export-data subtitle info))))
	    (separate (plist-get info :latex-subtitle-separate)))
       (concat
	(format "\\title{%s%s}\n" title
		(if separate "" (or formatted-subtitle "")))
	(when (and separate subtitle)
	  (concat formatted-subtitle "\n"))))
     ;; Hyperref options.
     (let ((template (plist-get info :latex-hyperref-template)))
       (and (stringp template)
            (format-spec template spec)))
     ;; engrave-faces-latex preamble
     (when (and (eq org-latex-src-block-backend 'engraved)
                (org-element-map (plist-get info :parse-tree)
                    '(src-block inline-src-block) #'identity
                    info t))
       (org-latex-generate-engraved-preamble info))
     ;; Document start.
     "\\begin{document}\n\n"
     ;; Title command.
     (let* ((title-command (plist-get info :latex-title-command))
            (command (and (stringp title-command)
                          (format-spec title-command spec))))
       (org-element-normalize-string
	(cond ((not (plist-get info :with-title)) nil)
	      ((string= "" title) nil)
	      ((not (stringp command)) nil)
	      ((string-match "\\(?:[^%]\\|^\\)%s" command)
	       (format command title))
	      (t command))))
     ;; Institute.
     (let ((institute (plist-get info :institute)))
       (format "\\institute{%s}\n" (org-export-data institute info)))
     ;; Faculty.
     (let ((faculty (plist-get info :faculty)))
       (format "\\faculty{%s}\n" (org-export-data faculty info)))
     ;; Table of contents.
     (let ((depth (plist-get info :with-toc)))
       (when depth
	 (concat (when (integerp depth)
		   (format "\\setcounter{tocdepth}{%d}\n" depth))
		 (plist-get info :latex-toc-command))))
     ;; Document's body.
     contents
     ;; Creator.
     (and (plist-get info :with-creator)
	  (concat (plist-get info :creator) "\n"))
     ;; Document end.
     "\\end{document}")))

(provide 'ox-tudbeamer)
;;; ox-tudbeamer.el ends here
