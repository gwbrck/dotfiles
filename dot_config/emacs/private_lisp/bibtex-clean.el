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

(defun gwbrck/bibtex-dashes ()
  (let (bounds)
    (when (looking-at bibtex-entry-maybe-empty-head)
      (goto-char (match-end 0))
      (while (setq bounds (bibtex-parse-field))
        (goto-char (bibtex-start-of-field bounds))
        (if (and (member (bibtex-name-in-field bounds) '("pages" "Pages"))
                 (string-match "[0-9]-[0-9]" (bibtex-text-in-field-bounds bounds)))
            (save-restriction
              (narrow-to-region (caar bounds) (nth 3 bounds))
              (goto-char (point-min))
              (while (search-forward "-" nil t)
                (replace-match "--")))
          (goto-char (bibtex-end-of-field bounds)))))))

(defun gwbrck/bibtex-journal ()
  (let (bounds)
    (when (looking-at bibtex-entry-maybe-empty-head)
      (goto-char (match-end 0))
      (while (setq bounds (bibtex-parse-field))
        (goto-char (bibtex-start-of-field bounds))
        (if (member (bibtex-name-in-field bounds) '("journal" "Journal"))
            (save-restriction
              (narrow-to-region (caar bounds) (nth 3 bounds))
              (goto-char (point-min))
              (while (re-search-forward "^[\t ]*journal" nil t)
                (replace-match "journaltitle")))
          (goto-char (bibtex-end-of-field bounds)))))))

(defun gwbrck/bibtex-abstract  ()
  (let (bounds)
    (when (looking-at bibtex-entry-maybe-empty-head)
      (goto-char (match-end 0))
      (while (setq bounds (bibtex-parse-field))
        (goto-char (bibtex-start-of-field bounds))
        (if (member (bibtex-name-in-field bounds) '("abstract" "Abstract"))
            (kill-region (caar bounds) (nth 3 bounds))
          (goto-char (bibtex-end-of-field bounds)))))))

(defun gwbrck/bibtex-clean-entry (&optional x)
  (interactive)
  (save-excursion
    (save-restriction
      (bibtex-narrow-to-entry)
      ;;(bibtex-mark-entry)
      ;;(ucs-normalize-NFC-region)
      (bibtex-beginning-of-entry)
      (gwbrck/bibtex-journal)
      (bibtex-beginning-of-entry)
      (gwbrck/bibtex-dashes)
      (bibtex-beginning-of-entry)
      (gwbrck/bibtex-abstract)))
  (bibtex-clean-entry 2))


(define-minor-mode bibtex-clean-mode
  "A minor mode that keeps bilatex-files as I like."
  :lighter "bibtex-clean-mode")

(define-key bibtex-mode-map [remap bibtex-clean-entry] 'gwbrck/bibtex-clean-entry)

(provide 'bibtex-clean-mode)
;;; bibtex-clean.el ends here
