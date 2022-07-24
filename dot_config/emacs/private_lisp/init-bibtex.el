;;; package --- Summary:
;;; Commentary:
;;; Code:

(require 'bibtex)
(require 'url-http)
(require 'xml)
(require 'oc)

(defvar main-bib-file nil)

(setq bibtex-dialect 'biblatex)

(setq bibtex-entry-format '(opts-or-alts
                            numerical-fields
                            whitespace
                            realign
                            last-comma
                            delimiters
                            unify-case
                            sort-fields
                            delimiters
                            required-fields))

(setq
 bibtex-autokey-name-year-separator "_"
 bibtex-autokey-year-title-separator "_"
 bibtex-autokey-titlewords 1
 bibtex-autokey-year-length 4
 bibtex-autokey-edit-before-use nil
 bibtex-autokey-additional-names 1
 bibtex-autokey-titleword-ignore '("A" "An" "On" "The" "Eine" "Ein" "Der" "Die" "Das")
 bibtex-autokey-titlewords-stretch 0
 bibtex-autokey-titleword-length 5
 bibtex-autokey-name-separator "-"
 bibtex-autokey-names 2
 bibtex-autokey-additional-names "-ea"
 bibtex-comma-after-last-field t)

(defun bibtex-clean-entry--preclean (&optional _key _called_by_reformat)
  (let ((case-fold-search t)
        (bounds)
        (start (bibtex-beginning-of-entry))
        (doi (bibtex-autokey-get-field "doi"))
        (url (bibtex-autokey-get-field "url"))
        (_ (or (looking-at bibtex-any-entry-maybe-empty-head)
	       (user-error "Not inside a BibTeX entry")))
        (entry-type (bibtex-type-in-head)))
    (save-excursion
      (save-restriction
        (bibtex-narrow-to-entry)
        (goto-char (point-min))
        (while (re-search-forward "[‑–−–—‒]" nil t)
          (replace-match "-"))
        (bibtex-beginning-first-field)
        (when (setq bounds (bibtex-search-forward-field "journal"))
          (goto-char (caar bounds))
          (while (re-search-forward "journal[\s]*=" (nth 3 bounds) t)
            (replace-match "journaltitle ="))
          (bibtex-beginning-first-field))
        (when (setq bounds (bibtex-search-forward-field "abstract"))
          (kill-region (caar bounds) (nth 3 bounds))
          (bibtex-beginning-first-field))
        (when (and (eq doi "")
                   (string-match "doi.org" url))
          (setq doi url)
          (bibtex-make-field "doi")
          (bibtex-beginning-first-field))
        (when (or (string-match "^http://dx.doi.org/" doi)
                  (string-match "^http://doi.org/" doi)
                  (string-match "^doi:" doi)
	          (string-match "^https://doi.org/" doi))
          (setq doi (replace-match "" nil nil doi))
          (setq bounds (bibtex-search-forward-field "doi"))
          (kill-region (caar bounds) (nth 3 bounds))
          (bibtex-make-field "doi")
          (backward-char)
          (insert doi)
          (bibtex-beginning-first-field))
        (when (and (not (eq doi ""))
                   (string-match "doi.org" url))
          (when (setq bounds (bibtex-search-forward-field "url"))
            (kill-region (caar bounds) (nth 3 bounds))
            (bibtex-beginning-first-field)))
        (when (setq bounds (bibtex-search-forward-field "abstract"))
          (kill-region (caar bounds) (nth 3 bounds))
          (bibtex-beginning-first-field))
        (when (and (setq bounds (bibtex-search-forward-field "pages"))
                   (string-match "[0-9]-[0-9]" (bibtex-text-in-field-bounds bounds)))
          (goto-char (caar bounds))
          (while (search-forward "-" (nth 3 bounds) t)
            (replace-match "--"))
          (bibtex-beginning-first-field))
        (goto-char (point-min))
        (re-search-forward (if (eq entry-type 'string)
                               bibtex-string-maybe-empty-head
                             bibtex-entry-maybe-empty-head))
        (if (match-beginning bibtex-key-in-head)
            (delete-region (match-beginning bibtex-key-in-head)
                           (match-end bibtex-key-in-head)))))))

(advice-add 'bibtex-clean-entry :before #'bibtex-clean-entry--preclean)

;; (defun doi2csljson (doi)
;;   "Retrieve csl-json information for DOI using crossref API."
;;   (interactive "MDOI: ")
;;   (let ((url-mime-accept-string "application/vnd.citationstyles.csl+json")
;;         (buff (current-buffer)))
;;     (with-current-buffer
;;         (url-retrieve-synchronously (format "https://doi.org/%s" doi))
;;       (goto-char (point-min))
;;       (re-search-forward "^$")
;;       (delete-region (point) (point-min))
;;       (doi2csljson--insert (buffer-string) buff))))
;;
;;
;; (defun doi2csljson--insert (json buff)
;;   "Encode raw JSON and inseer it in BUFF."
;;   (let ((js (json-parse-string json :object-type 'alist)))
;;     (with-current-buffer buff
;;       (json-insert js))))

(defun doi-to-bibtex (doi)
  "Retrieve csl-json information for DOI using crossref API."
  (interactive "MDOI: ")
  (let ((url-mime-accept-string "text/bibliography;style=bibtex")
        (buff (current-buffer)))
    (with-current-buffer
        (url-retrieve-synchronously (format "https://doi.org/%s" doi))
      (bibtex-entry-insert-buffer
       (decode-coding-string
        (buffer-substring (string-match "@" (buffer-string)) (point)) 'utf-8)
       buff))))

(defun bibtex-entry-insert-buffer (bibtex buff)
  "Add BIBTEX and insert it in BUFF or prompted buffers."
  (let ((key)
        (target-buffs
         (with-current-buffer
             buff
           (org-cite-list-bibliography-files))))
    (with-current-buffer buff
      (when (eq major-mode 'bibtex-mode)
        (add-to-list 'target-buffs (buffer-file-name))))
    (if (eq (length target-buffs) 1)
        (find-file (car target-buffs))
      (find-file (completing-read "Target bib: " target-buffs nil t "main")))
    (with-current-buffer (current-buffer)
      (goto-char (point-max))
      (insert "\n")
      (goto-char (point-max))
      (insert bibtex)
      (bibtex-clean-entry)
      (setq key (bibtex-key-in-head))
      (bibtex-sort-buffer)
      (bibtex-search-entry key))))

(defun arxiv-id-to-bibtex (arxiv-id)
  "Retrieve (raw) bibtex information for ARXIV-ID item using arxiv API."
  (interactive "Marxiv-id: ")
  (let* ((buff (current-buffer))
         (arxiv-code
          (with-current-buffer
              (url-retrieve-synchronously (format "http://adsabs.harvard.edu/cgi-bin/bib_query?arXiv:%s" arxiv-id))
            (search-forward-regexp "<link rel=\"canonical\" href=\"http://ui.adsabs.harvard.edu/abs/\\(.*\\)/abstract\"/>")
            (match-string 1)))
         (bibtex (when arxiv-code
                   (with-current-buffer
                       (url-retrieve-synchronously (format "https://ui.adsabs.harvard.edu/abs/%s/exportcitation" arxiv-code))
                     (when (re-search-forward
	                    "<textarea.*>\\(.*\\(?:\n.*\\)*?\\(?:\n\\s-*\n\\|\\'\\)\\)</textarea>"
	                    nil t)
                       (xml-substitute-special (match-string 1)))))))
    (bibtex-entry-insert-buffer bibtex buff)))

(defun isbn-to-bibtex (isbn)
  "Retrieve biblatex information for ISBN using crossref API."
  (interactive "MISBN: ")
  (let ((buff (current-buffer)))
    (with-current-buffer
        (url-retrieve-synchronously
         (format
          "https://openlibrary.org/api/books?bibkeys=ISBN:%s&jscmd=data&format=json"
          isbn))
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point) (point-min))
      (isbn-to-bibtex--parse (buffer-string) buff))))

(defun isbn-to-bibtex--parse (json buff)
  "Encode raw JSON and inseer it in BUFF."
  (let* ((o (car (json-parse-string json :object-type 'alist)))
         (title (cdr (assoc 'title o)))
         (subtitle (cdr (assoc 'subtitle o)))
         (date (cdr (assoc 'publish_date o)))
         (nop (cdr (assoc 'number_of_pages o)))
         (p (cdr (assoc 'pagination o)))
         (key)
         (isbn10
          (mapconcat 'identity
                     (cdr (assoc 'isbn_10 (cdr (assoc 'identifiers o))))
                     " "))
         (isbn13
          (mapconcat 'identity
                     (cdr (assoc 'isbn_13 (cdr (assoc 'identifiers o))))
                     " "))
         (authors
          (mapconcat
           (lambda (x)
             (cdr (assoc 'name x)))
           (cdr (assoc 'authors o)) " and "))
         (publishers
          (mapconcat
           (lambda (x)
             (cdr (assoc 'name x)))
           (cdr (assoc 'publishers o)) " and "))
         (locations
          (mapconcat
           (lambda (x)
             (cdr (assoc 'name x)))
           (cdr (assoc 'publish_places o)) " and ")))
    (with-temp-buffer
      (bibtex-mode)
      (goto-char (point-max))
      (insert "\n\n@Book{,\n\n}")
      (bibtex-beginning-first-field)
      (when date
        (bibtex-make-field "year")
        (backward-char)
        (insert
         (let ((year date))
           (when (string-match "[0-9][0-9][0-9][0-9]" year)
             (match-string 0 year))))
        (bibtex-beginning-first-field))
      (when authors
        (bibtex-make-field "author")
        (backward-char)
        (insert authors)
        (bibtex-beginning-first-field))
      (when title
        (bibtex-make-field "title")
        (backward-char)
        (insert title)
        (bibtex-beginning-first-field))
      (when (and locations
                 (not (string-empty-p locations)))
        (bibtex-make-field "location")
        (backward-char)
        (insert locations)
        (bibtex-beginning-first-field))
      (when publishers
        (bibtex-make-field "publisher")
        (backward-char)
        (insert publishers)
        (bibtex-beginning-first-field))
      (when (and subtitle
                 (not (string-empty-p subtitle)))
        (bibtex-make-field "subtitle")
        (backward-char)
        (insert subtitle)
        (bibtex-beginning-first-field))
      (when (or nop p)
        (bibtex-make-field "pages")
        (backward-char)
        (insert
         (if nop
             (number-to-string nop)
           (string-match "[0-9]+" p)
           (match-string 0 p)))
        (bibtex-beginning-first-field))
      (when (or
             (not (string-empty-p isbn10))
             (not (string-empty-p isbn13)))
        (bibtex-make-field "isbn")
        (backward-char)
        (insert
         (if (not (string-empty-p isbn13))
             isbn13
           isbn10))
        (bibtex-beginning-first-field))
      (goto-char (point-max))
      (insert "\n")
      (bibtex-entry-insert-buffer (buffer-string) buff))))


(provide 'init-bibtex)
;;; init-bibtex.el ends here
