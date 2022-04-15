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
    ;;(:subtitle "AUTHOR" nil nil parse)
    (:birthdate "BIRTHDATE" nil nil parse)
    (:with-email nil "email" t t)
    (:section-numbers nil nil nil t)
    (:html-html5-fancy nil nil t t)
    (:html-head "HTML_HEAD" nil
                "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />\n" t)
    (:html-head-include-default-style nil nil nil org-html-head-include-default-style)
    (:html-doctype nil nil "html5" t)
    (:with-toc nil nil 1 t))
  :menu-entry '(?r "Export Resumee"
                   ((?h "As HTML file and open"
                        (lambda (a s v b)
                          (let ((outfile (org-export-output-file-name ".html" s)))
                            (if a
                                (org-export-to-file 'htmlcv outfile t s v b nil)
                              (org-open-file (org-export-to-file 'htmlcv outfile
                                  a s v b nil))))))))
  :translate-alist '((template . org-htmlcv-template)
                     (headline . org-htmlcv-headline)
                     (inner-template . org-htmlcv-inner-template)))

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

(defun org-htmlcv--div-class (class string &optional info prefix postfix)
  "Div CLASS with STRING.  When INFO is passed, STRING will exportet with backend.
POSTFIX and PREFIX are strings wich get never exportet via backend."
  (concat
   (format "<div class=\"%s\">" class)
   (when prefix prefix)
   (if info (org-export-data string info) string)
   (when postfix postfix)
   "</div>\n"))

(defun org-htmlcv-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth (org-html-toc depth info)))
   ;; Profile
   "<section class=\"profile\">\n"
   ;; photo <img src="pic_trulli.jpg" alt="Italian Trulli">
   (let ((photo (org-export-data (plist-get info :photo) info)))
     (when (org-string-nw-p photo)
       (format "<div id=\"pic\">%s</div>"
               (org-htmlcv--div-class "profilepic-wrapper" photo nil
                                      "<div class=\"profilepic\"><img src=\""
                                      "\" alt=\"profile picture\"></div>"))))

   (let ((author (org-export-data (plist-get info :author) info)))
     (when (org-string-nw-p author)
       (format "<h1>%s</h1>"
               (org-export-data author info))))
   "<ul>\n"
   (let ((birthdate (org-export-data (plist-get info :birthdate) info)))
     (when (org-string-nw-p birthdate)
       (format "<li>&#10033; %s</li>\n" birthdate)))
   ;; email
   (let ((email (and (plist-get info :with-email)
                     (org-export-data (plist-get info :email) info))))
     (when (org-string-nw-p email)
       (format "<li><a href=\"mailto:%s\"><i class=\"fa-solid fa-envelope\"></i> %s</a></li>\n" email email)))
   ;; phone
   (let ((mobile (org-export-data (plist-get info :mobile) info)))
     (when (org-string-nw-p mobile)
       (format "<li><i class=\"fa-solid fa-mobile-screen\"></i> %s</li>\n" mobile)))
   ;;github
   (let ((gh (org-export-data (plist-get info :github) info)))
     (when (org-string-nw-p gh)
       (format
        "<li><a href=\"https://github.com/%s/\"><i class=\"fa-brands fa-github\"></i> %s</a></li>\n"
        gh gh)))
   "</ul>\n"
   (format
    "<a id=\"btn\" href=./%s.pdf download=\"CV_%s.pdf\"= ><i class=\"fa-solid fa-file-pdf\"></i> Download</a>"
    (s-trim (replace-regexp-in-string "^.+\\/" "" (org-export-data (plist-get info :homepage) info)))
    (s-trim (replace-regexp-in-string " " "_" (org-export-data (plist-get info :author) info))))
   "</section>\n"
   ;; Document contents.
   "<section class=\"main-content\">\n"
   contents
   "</section>\n"
   ;; Footnotes section.
   (org-html-footnote-section info)))

(defun org-htmlcv-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((numberedp (org-export-numbered-headline-p headline info))
           (numbers (org-export-get-headline-number headline info))
           (level (+ (org-export-get-relative-level headline info)
                     (1- (plist-get info :html-toplevel-hlevel))))
           ;;(recipient (org-element-property :RECIPIENT headline))
           ;;(recipient_adress (org-element-property :RECIPIENT_ADRESS headline))
           ;;(opening (org-element-property :OPENING headline))
           ;;(closing (org-element-property :CLOSING headline))
           (omit (org-element-property :OMIT_HEADLINE headline))
           (from (or (org-element-property :FROM headline) ""))
           (to (org-element-property :TO headline))
           (time (if to (format "%s &ndash; %s" from to) from))
           (position (or (org-element-property :POSITION headline) ""))
           (institute (or (org-element-property :INSTITUTE headline) ""))
           (location (or (org-element-property :LOCATION headline) ""))
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
           (environment (let ((env (org-element-property :CV_ENV headline)))
                          (or (org-string-nw-p env) "block")))
           (contents (cond
                      ((equal contents nil) "")
                      ((equal (org-element-property :raw-value headline) "Motivation")
                       (let* ((match-s
                               (car
                                (s-match
                                 "^[[:word:]]+"
                                 (replace-regexp-in-string "<[^>]+>" "" contents)
                                 0)))
                              (match (string-match match-s contents)))
                         (concat
                          (substring contents 0 match)
                          (upcase (substring contents match (+ match 1)))
                          (substring contents (+ match 1)))))
                      (t contents)))
	   (id (org-html--reference headline info))
	   (formatted-text
	    (if (plist-get info :html-self-link-headlines)
		(format "<a href=\"#%s\">%s</a>" id full-text)
	      full-text)))
        ;; This is a deep sub-tree: export it as a list item.
      (cond
       ((or (equal environment "cventry")
            (equal environment "cvitem"))
        (concat
         (format "<div class=%s-wrapper>\n<div class=title>%s</div>\n"
                 environment
                 (if omit "" full-text))
         (org-htmlcv--div-class "time" time)
         (org-htmlcv--div-class "position" position info)
         (org-htmlcv--div-class "location" location info)
         (org-htmlcv--div-class "institute" institute info)
         (org-htmlcv--div-class "contents" contents)
         "</div>\n"))
       ((equal (org-element-property :raw-value headline) "Motivation")
        (let ((extra-class
	       (org-element-property :HTML_CONTAINER_CLASS headline))
	      (headline-class
	       (org-element-property :HTML_HEADLINE_CLASS headline)))
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
                  (concat
                   "<div class=\"motivation\">\n"
                   "<input id=\"collapsible\" class=\"toggle\" type=\"checkbox\">\n"
                   "<div class=\"collapsible-content\">\n"
                   "<div class=\"content-inner\">\n"
                   contents
                   "</div><label for=\"collapsible\" id=\"lbl-toggle\"></label>\n"
                   "</div></div>")
                  (org-html--container headline info))))
       ((org-export-low-level-p headline info)
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
		 (format "</%s>\n" html-type)))))
       ;; Standard headline.  Export it as a section.
       (t
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
                  (org-html--container headline info))))))))


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
   "<script>\n"
   "function resize() {
     if (window.innerWidth > 850) { 
         var height = document.getElementById(\"table-of-contents\").offsetHeight;
         var anav = document.querySelectorAll(\"nav ul li a\");
         for (i = 0; i < anav.length; i++) {
             anav[i].onclick = function (info) {
                 var trgt = document.getElementById(\"outline-container-\" + info.target.hash.substring(1));
                 trgt.scrollIntoView({
                     block: 'start',
                     behavior: 'smooth',
                 });
                 return false;
             }
         }
         
         var section = document.querySelectorAll(\".main-content h2\");
         for (i = 0; i < section.length; i++) {
             section[i].style.top = height + \"px\";
             section[i].style.scrollMargin = height + \"px\"; 
         }
         
         var outlineclass = document.getElementsByClassName(\"outline-2\");
         for (i = 0; i < outlineclass.length; i++) {
             outlineclass[i].style.scrollMargin = height + \"px\"; 
         }
         
         document.getElementsByClassName(\"profile\")[0].style.top = height + \"px\";
         document.getElementsByClassName(\"profile\")[0].style.height = (window.innerHeight - height) + \"px\";
     }
 }
 window.onresize = resize;
 resize();\n</script>\n"
   "</body>\n</html>"))

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
     (downcase (substring contents 0 1))
     (substring contents 1)
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
       ((equal (org-element-property :raw-value headline) "Motivation")
        (org-moderncv--format-coverletter headline contents info))
       ((org-export-with-backend 'latex headline contents info))))))

(provide 'ox-moderncv)
;;; ox-moderncv.el ends here
