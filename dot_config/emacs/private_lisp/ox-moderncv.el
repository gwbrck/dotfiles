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
(require 'ox-html)


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
    (:with-email nil "email" t t))
  :menu-entry '(?r "Export Resumee"
                   ((?p "As PDF file and open"
                        (lambda (a s v b)
                          (let ((outfile (org-export-output-file-name ".tex" s)))
                            (if a
                                (org-export-to-file 'moderncv outfile
                                  t s v b nil #'org-latex-compile)
                              (org-open-file (org-export-to-file 'moderncv outfile
                                               a s v b nil #'org-latex-compile))))))))
  :translate-alist '((template . org-moderncv-template)
                     (headline . org-moderncv-headline)))

(org-export-define-derived-backend 'htmlcv 'html
  :options-alist
  '((:mobile "MOBILE" nil nil parse)
    (:homepage "HOMEPAGE" nil nil parse)
    (:address "ADDRESS" nil nil newline)
    (:photo "PHOTO" nil nil parse)
    (:gitlab "GITLAB" nil nil parse)
    (:github "GITHUB" nil nil parse)
    (:linkedin "LINKEDIN" nil nil parse)
    (:birthdate "BIRTHDATE" nil nil parse)
    (:with-email nil "email" t t))
  :menu-entry '(?r "Export Resumee"
                   ((?h "As HTML file and open"
                        (lambda (a s v b)
                          (let ((outfile (org-export-output-file-name ".html" s)))
                            (if a
                                (org-export-to-file 'htmlcv outfile t s v b nil)
                              (org-open-file (org-export-to-file 'htmlcv outfile
                                  a s v b nil))))))))
  :translate-alist '((template . org-htmlcv-template)
                     (headline . org-htmlcv-headline)))

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

;; TODO implement (not org-html-inner-template) org-html-template org-html-headline

(defun org-htmlcv-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   (when (and (not (org-html-html5-p info)) (org-html-xhtml-p info))
     (let* ((xml-declaration (plist-get info :html-xml-declaration))
	    (decl (or (and (stringp xml-declaration) xml-declaration)
		      (cdr (assoc (plist-get info :html-extension)
				  xml-declaration))
		      (cdr (assoc "html" xml-declaration))
		      "")))
       (when (not (or (not decl) (string= "" decl)))
	 (format "%s\n"
		 (format decl
			 (or (and org-html-coding-system
				  (fboundp 'coding-system-get)
				  (coding-system-get org-html-coding-system 'mime-charset))
			     "iso-8859-1"))))))
   (org-html-doctype info)
   "\n"
   (concat "<html"
	   (cond ((org-html-xhtml-p info)
		  (format
		   " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
		   (plist-get info :language) (plist-get info :language)))
		 ((org-html-html5-p info)
		  (format " lang=\"%s\"" (plist-get info :language))))
	   ">\n")
   "<head>\n"
   (org-html--build-meta-info info)
   (org-html--build-head info)
   (org-html--build-mathjax-config info)
   "</head>\n"
   "<body>\n"
   (let ((link-up (org-trim (plist-get info :html-link-up)))
	 (link-home (org-trim (plist-get info :html-link-home))))
     (unless (and (string= link-up "") (string= link-home ""))
       (format (plist-get info :html-home/up-format)
	       (or link-up link-home)
	       (or link-home link-up))))
   ;; Preamble.
   (org-html--build-pre/postamble 'preamble info)
   ;; Document contents.
   (let ((div (assq 'content (plist-get info :html-divs))))
     (format "<%s id=\"%s\" class=\"%s\">\n"
             (nth 1 div)
             (nth 2 div)
             (plist-get info :html-content-class)))
   ;; Document title.
   (when (plist-get info :with-title)
     (let ((title (and (plist-get info :with-title)
		       (plist-get info :title)))
	   (subtitle (plist-get info :subtitle))
	   (html5-fancy (org-html--html5-fancy-p info)))
       (when title
	 (format
	  (if html5-fancy
	      "<header>\n<h1 class=\"title\">%s</h1>\n%s</header>"
	    "<h1 class=\"title\">%s%s</h1>\n")
	  (org-export-data title info)
	  (if subtitle
	      (format
	       (if html5-fancy
		   "<p class=\"subtitle\" role=\"doc-subtitle\">%s</p>\n"
		 (concat "\n" (org-html-close-tag "br" nil info) "\n"
			 "<span class=\"subtitle\">%s</span>\n"))
	       (org-export-data subtitle info))
	    "")))))
   contents
   (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
   ;; Postamble.
   (org-html--build-pre/postamble 'postamble info)
   ;; Possibly use the Klipse library live code blocks.
   (when (plist-get info :html-klipsify-src)
     (concat "<script>" (plist-get info :html-klipse-selection-script)
	     "</script><script src=\""
	     org-html-klipse-js
	     "\"></script><link rel=\"stylesheet\" type=\"text/css\" href=\""
	     org-html-klipse-css "\"/>"))
   ;; Closing document.
   "</body>\n</html>"))

(defun org-htmlcv-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((numberedp (org-export-numbered-headline-p headline info))
           (numbers (org-export-get-headline-number headline info))
           (level (+ (org-export-get-relative-level headline info)
                     (1- (plist-get info :html-toplevel-hlevel))))
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword headline)))
                        (and todo (org-export-data todo info)))))
           (todo-type (and todo (org-element-property :todo-type headline)))
           (priority (and (plist-get info :with-priority)
                          (org-element-property :priority headline)))
           (text (org-export-data (org-element-property :title headline) info))
           (tags (and (plist-get info :with-tags)
                      (org-export-get-tags headline info)))
           (full-text (funcall (plist-get info :html-format-headline-function)
                               todo todo-type priority text tags info))
           (contents (or contents ""))
	   (id (org-html--reference headline info))
	   (formatted-text
	    (if (plist-get info :html-self-link-headlines)
		(format "<a href=\"#%s\">%s</a>" id full-text)
	      full-text)))
      (if (org-export-low-level-p headline info)
          ;; This is a deep sub-tree: export it as a list item.
          (let* ((html-type (if numberedp "ol" "ul")))
	    (concat
	     (and (org-export-first-sibling-p headline info)
		  (apply #'format "<%s class=\"org-%s\">\n"
			 (make-list 2 html-type)))
	     (org-html-format-list-item
	      contents (if numberedp 'ordered 'unordered)
	      nil info nil
	      (concat (org-html--anchor id nil nil info) formatted-text)) "\n"
	     (and (org-export-last-sibling-p headline info)
		  (format "</%s>\n" html-type))))
	;; Standard headline.  Export it as a section.
        (let ((extra-class
	       (org-element-property :HTML_CONTAINER_CLASS headline))
	      (headline-class
	       (org-element-property :HTML_HEADLINE_CLASS headline))
              (first-content (car (org-element-contents headline))))
          (format "<%s id=\"%s\" class=\"%s\">%s%s</%s>\n"
                  (org-html--container headline info)
                  (format "outline-container-%s" id)
                  (concat (format "outline-%d" level)
                          (and extra-class " ")
                          extra-class)
                  (format "\n<h%d id=\"%s\"%s>%s</h%d>\n"
                          level
                          id
			  (if (not headline-class) ""
			    (format " class=\"%s\"" headline-class))
                          (concat
                           (and numberedp
                                (format
                                 "<span class=\"section-number-%d\">%s</span> "
                                 level
                                 (concat (mapconcat #'number-to-string numbers ".") ".")))
                           formatted-text)
                          level)
                  ;; When there is no section, pretend there is an
                  ;; empty one to get the correct <div
                  ;; class="outline-...> which is needed by
                  ;; `org-info.js'.
                  (if (eq (org-element-type first-content) 'section) contents
                    (concat (org-html-section first-content "" info) contents))
                  (org-html--container headline info)))))))

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
