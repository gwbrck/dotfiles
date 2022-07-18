;;; package --- Summary:
;;; Commentary:
;;; Code:

(require 'bibtex)

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

(add-to-list 'bibtex-autokey-name-change-strings '("ß" . "ss"))
(add-to-list 'bibtex-autokey-name-change-strings '("å" . "a"))
(add-to-list 'bibtex-autokey-name-change-strings '("Å" . "A"))
(add-to-list 'bibtex-autokey-name-change-strings '("ö" . "oe"))
(add-to-list 'bibtex-autokey-name-change-strings '("Ö" . "Oe"))
(add-to-list 'bibtex-autokey-name-change-strings '("ä" . "ae"))
(add-to-list 'bibtex-autokey-name-change-strings '("Ä" . "Ae"))
(add-to-list 'bibtex-autokey-name-change-strings '("Ü" . "Ue"))
(add-to-list 'bibtex-autokey-name-change-strings '("ü" . "ue"))
(add-to-list 'bibtex-autokey-titleword-change-strings '("ß" . "ss"))
(add-to-list 'bibtex-autokey-titleword-change-strings '("å" . "a"))
(add-to-list 'bibtex-autokey-titleword-change-strings '("Å" . "A"))
(add-to-list 'bibtex-autokey-titleword-change-strings '("ö" . "oe"))
(add-to-list 'bibtex-autokey-titleword-change-strings '("Ö" . "Oe"))
(add-to-list 'bibtex-autokey-titleword-change-strings '("ä" . "ae"))
(add-to-list 'bibtex-autokey-titleword-change-strings '("Ä" . "Ae"))
(add-to-list 'bibtex-autokey-titleword-change-strings '("Ü" . "Ue"))
(add-to-list 'bibtex-autokey-titleword-change-strings '("ü" . "ue"))


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


(provide 'init-bibtex)
;;; init-bibtex.el ends here
