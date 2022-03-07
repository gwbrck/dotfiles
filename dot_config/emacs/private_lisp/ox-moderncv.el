;;; ox-moderncv.el --- Cutsom CV LaTeX moderncv and html Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Based on https://gitlab.com/Titan-C/org-cv
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This library implements a LaTeX moderncv back-end and export to HTML,
;; derived from the LaTeX one.

;;; Code:
(require 'cl-lib)
(require 'org)
(require 'ox-latex)


(add-to-list 'org-latex-classes
	     '("moderncv" "\\documentclass[a4paper, 11pt, sans]{moderncv}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]
\\usepackage[AUTO]{inputenc}
\\usepackage[normalem]{ulem}
\\usepackage{csquotes}
\\usepackage{ragged2e}
\\usepackage[scale=0.7]{geometry} % Reduce document margins
\\recomputelengths
\\usepackage[notquote]{hanging}
\\usepackage[AUTO]{babel}"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))



;;; User-Configurable Variables

(defgroup org-export-cv nil
  "Options specific for using the moderncv class in LaTeX export."
  :tag "Org moderncv"
  :group 'org-export
  :version "25.3")

;;; Define Back-End
(org-export-define-derived-backend 'moderncv 'latex
  :options-alist
  '((:latex-class "LATEX_CLASS" nil "moderncv" t)
    (:cvstyle "CVSTYLE" nil "classic" t)
    (:cvcolor "CVCOLOR" nil nil t)
    (:mobile "MOBILE" nil nil parse)
    (:homepage "HOMEPAGE" nil nil parse)
    (:address "ADDRESS" nil nil newline)
    (:photo "PHOTO" nil nil parse)
    (:gitlab "GITLAB" nil nil parse)
    (:github "GITHUB" nil nil parse)
    (:linkedin "LINKEDIN" nil nil parse)
    (:birthdate "BIRTHDATE" nil nil parse)
    (:with-email nil "email" t t)
    )
  :menu-entry '(?r "Export Resumee"
                   ((?o "As PDF file and open"
                        (lambda (a s v b)
                          (let ((outfile (org-export-output-file-name ".tex" s)))
                            (if (org-export-to-file 'moderncv outfile
                                  a s v b nil #'org-latex-compile)
                                (find-file (org-export-output-file-name ".pdf" s))))))))

  :translate-alist '((template . org-moderncv-template)
                     (headline . org-moderncv-headline)))


;;;; Template
;;
;; Template used is similar to the one used in `latex' back-end,
;; excepted for the table of contents and moderncv themes.

(defun org-moderncv-template (contents info)
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
     ;; cvstyle
     (let ((cvstyle (org-export-data (plist-get info :cvstyle) info)))
       (when cvstyle (format "\\moderncvstyle{%s}\n" cvstyle)))
     ;; cvcolor
     (let ((cvcolor (org-export-data (plist-get info :cvcolor) info)))
       (when (not (string-empty-p cvcolor)) (format "\\moderncvcolor{%s}\n" cvcolor)))
     ;; Possibly limit depth for headline numbering.
     (let ((sec-num (plist-get info :section-numbers)))
       (when (integerp sec-num)
         (format "\\setcounter{secnumdepth}{%d}\n" sec-num)))
     ;; Author.
     (let ((author (and (plist-get info :with-author)
                        (let ((auth (plist-get info :author)))
                          (and auth (org-export-data auth info))))))
       (format "\\name{%s}{}\n" author))
     ;; birthdate
     (let ((birthdate (org-export-data (plist-get info :birthdate) info)))
       (when (org-string-nw-p birthdate)
         (format "\\born{%s}\n" birthdate)))
     ;; photo
     (let ((photo (org-export-data (plist-get info :photo) info)))
       (when (org-string-nw-p photo)
         (format "\\photo[70pt][0.4pt]{%s}\n" photo)))
     ;; email
     (let ((email (and (plist-get info :with-email)
                       (org-export-data (plist-get info :email) info))))
       (when (org-string-nw-p email)
         (format "\\email{%s}\n" email)))
     ;; phone
     (let ((mobile (org-export-data (plist-get info :mobile) info)))
       (when (org-string-nw-p mobile)
         (format "\\phone[mobile]{%s}\n" mobile)))
     ;; homepage
     (let ((homepage (org-export-data (plist-get info :homepage) info)))
       (when (org-string-nw-p homepage)
         (format "\\homepage{%s}\n" homepage)))
     ;; address
     (let ((address (org-export-data (plist-get info :address) info)))
       (when (org-string-nw-p address)
         (format "\\address%s\n" (mapconcat (lambda (line)
                                              (format "{%s}" line))
                                            (split-string address "\n") ""))))
     (mapconcat (lambda (social-network)
                  (let ((network (org-export-data
                                  (plist-get info (car social-network)) info)))
                    (when (org-string-nw-p network)
                      (format "\\social[%s]{%s}\n"
                              (nth 1 social-network) network))))
                '((:github "github")
                  (:gitlab "gitlab")
                  (:linkedin "linkedin"))
                "")

     ;; Date.
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
     ;;(let ((template (plist-get info :latex-hyperref-template)))
     ;;  (and (stringp template)
     ;;       (format-spec template spec)))
     ;; Document start.
     "\\begin{document}\n\n"
     ;; Title command.
     (when (not (string-match "makelettertitle" contents))
       "\\makecvtitle\n")
     ;; Document's body.
     contents
     ;; Creator.
     (and (plist-get info :with-creator)
          (concat (plist-get info :creator) "\n"))
     ;; Document end.
     "\\end{document}")))

(defun org-moderncv--format-cventry (headline contents info)
  "Format HEADLINE as as cventry.
CONTENTS holds the contents of the headline.  INFO is a plist used
as a communication channel."
  (let* ((title (org-export-data (org-element-property :title headline) info))
         (from (or (org-element-property :FROM headline)
                   (error "No FROM property provided for cventry %s" title)))
         (to (org-element-property :TO headline))
         (position (or (org-element-property :POSITION headline) ""))
         (institute (or (org-element-property :INSTITUTE headline) ""))
         (location (or (org-element-property :LOCATION headline) "")))
    (format "\\cventry{%s%s}{%s}{%s}{%s}{%s}{%s}\n"
            from
            (if to (concat " -- " to) "")
            title
            (s-trim (org-export-string-as position 'latex t))
            (s-trim (org-export-string-as institute 'latex t))
            (s-trim (org-export-string-as location 'latex t))
            (s-trim contents))))

(defun org-moderncv--format-cvitem (headline contents info)
  "Format HEADLINE as as cventry.
CONTENTS holds the contents of the headline.  INFO is a plist used
as a communication channel."
  (let* ((title (org-export-data (org-element-property :title headline) info))
         (omit (org-element-property :OMIT_HEADLINE headline)))
    (if omit
        (format "\\cvitem{}{%s}\n" (s-trim contents))
      (format "\\cvitem{%s}{%s}\n" title (s-trim contents)))))

(defun org-moderncv--format-coverletter (headline contents info)
  "Format HEADLINE as as cventry.
CONTENTS holds the contents of the headline.  INFO is a plist used
as a communication channel."
  (let ((recipient (org-element-property :RECIPIENT headline))
        (recipient_adress (org-element-property :RECIPIENT_ADRESS headline))
        (opening (org-element-property :OPENING headline))
        (closing (org-element-property :CLOSING headline)))
    (concat
     (when (or recipient recipient_adress)
       (format "\\recipient{%s}{%s}\n"
               (or recipient "")
               (or recipient_adress "")))
     (when opening
       (format "\\opening{%s}\n" opening))
     (when closing
       (format "\\closing{%s}\n" closing))
     "\\makelettertitle\\justifying\n"
     contents
     "\n\n\\makeletterclosing\n\\newpage\n\n\\makecvtitle\n")))


;;;; Headline
(defun org-moderncv-headline (headline contents info)
  "Transcode HEADLINE element into moderncv code.
CONTENTS is the contents of the headline.  INFO is a plist used
as a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let ((environment (let ((env (org-element-property :CV_ENV headline)))
                         (or (org-string-nw-p env) "block"))))
      (cond
       ;; is a cv entry
       ((equal environment "cventry")
        (org-moderncv--format-cventry headline contents info))
       ((equal environment "cvitem")
        (org-moderncv--format-cvitem headline contents info))
       ((equal (org-element-property :raw-value headline) "Anschreiben")
        (org-moderncv--format-coverletter headline contents info))
       ((org-export-with-backend 'latex headline contents info))))))

(provide 'ox-moderncv)
;;; ox-moderncv.el ends here
