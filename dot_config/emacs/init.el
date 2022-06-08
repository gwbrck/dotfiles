;;; init.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Package --- Summary
;;; Code:
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package custom
  :no-require t
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package emacs
  :init
  (setq gwbrck/roam "~/Documents/myBib/roam/")
  (setq org-directory gwbrck/roam)
  (setq default-directory "~/"))

(use-package emacs
  :when (equal system-type 'darwin)
  :init
  (setq auth-sources '(macos-keychain-internet macos-keychain-generic "~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))
  (setq mac-command-modifier      'super
        ns-command-modifier       'super
        mac-option-modifier       'meta
        ns-option-modifier        'meta
        mac-right-option-modifier 'none
        ns-right-option-modifier  'none)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8))

(use-package emacs
  :init
  (setq native-comp-async-report-warnings-errors nil)
  (setq inhibit-startup-message t)
  (setq frame-resize-pixelwise t)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode 10)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq visible-bell t)
  (column-number-mode)
  (global-display-line-numbers-mode t)
  ;; Disable line numbers for some modes
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  vterm-mode-hook
                  pdf-view-mode-hook
                  treemacs-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))
  (setq-default indent-tabs-mode nil)
  (setq tab-always-indent 'complete))

(use-package pixel-scroll
  :when (>= emacs-major-version 29)
  :demand t
  :init
  (pixel-scroll-precision-mode))
  
(use-package files
  :no-require t
  :config
  (setq insert-directory-program "gls")
  (setq backup-directory-alist '(("." . "~/.config/emacs/backup")))
  (setq backup-by-copying t)
  (setq version-control t)
  (setq delete-old-versions t)
  (setq kept-new-versions 20)
  (setq kept-old-versions 5))

(use-package ns-auto-titlebar
  :straight t
  :when (equal system-type 'darwin)
  :init (ns-auto-titlebar-mode +1))

(use-package evil
  :straight t
  :demand t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  ;;(setq evil-want-minibuffer t)
  :config
  (evil-mode 1))

(use-package general
  :straight t
  :after evil
  :config
  (general-unbind 'motion "SPC")
  (general-create-definer leader-key-def-mapless
    :states '(normal insert visual emacs motion)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-create-definer leader-key-def
    :states '(normal insert visual emacs motion)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(use-package evil-collection
  :after evil
  :straight t
  :config
  (evil-collection-init)
  (leader-key-def
    "SPC" '(execute-extended-command :wk "M-x")
    "x" '(:keymap ctl-x-map :wk "ctl-x-map")
    "h" '(:keymap help-map :wk "help")
    "b" '(:ignore t :wk "buffers")
    "bn" '(next-buffer :wk "next Buffer")
    "bv" '(previous-buffer :wk "preVious Buffer")
    "bk" '(:ignore t :which-key "kill buffer")
    "bks" '(kill-some-buffers :wk "kill some buffers")
    "bkk" '(kill-this-buffer :wk "kill this buffer")
    "w" '(evil-window-map :which-key "windows")
    "s" '(flyspell-mode :wk "spell")))

(use-package evil-org
  :straight t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org)
  (require 'evil-org-agenda)
  (evil-org-set-key-theme '(navigation todo insert textobjects additional))
  (evil-org-agenda-set-keys))

(use-package evil-snipe
  :straight t
  :after evil-collection
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
  (setq evil-snipe-scope 'buffer))

(use-package evil-surround
  :straight t
  :after evil-collection
  :config
  (global-evil-surround-mode 1))

(use-package pulse
  :custom (pulse-flag t)
  :straight (:type built-in)
  :config
  (defun gwbrck/evil-yank-advice (orig-fn beg end &rest args)
    (pulse-momentary-highlight-region beg end 'highlight)
    (apply orig-fn beg end args))
  (advice-add 'evil-yank :around #'gwbrck/evil-yank-advice))

(use-package elec-pair
  :demand t
  :init
  (electric-pair-mode))

(use-package all-the-icons
  :straight t)

(use-package minions
  :straight t
  :config
  (setq minions-mode-line-lighter "[↑]")
  :init (minions-mode 1))

(use-package which-key
  :straight t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1))

(use-package org
  :straight t
  :demand t
  :hook ((org-mode . gwbrck/org-mode-setup))
  :init
  (defun gwbrck/org-mode-setup ()
    (org-indent-mode)
    (variable-pitch-mode 1)
    (gwbrck/set-font-faces)
    (visual-line-mode 1))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (emacs-lisp . t)))
  (setq org-ellipsis " ▾")
  (defun gwbrck/org-mode-visual-fill ()
    (setq visual-fill-column-width 110
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))
  :config
  (add-to-list 'org-tags-exclude-from-inheritance "project")
  (setq org-default-notes-file (concat gwbrck/roam "main.org"))
  (advice-add 'org-refile :after 'org-save-all-org-buffers))

(use-package org-agenda
  :after org
  ;; agenda files are defined by vulpea functions (orgroam)
  ;;(setq org-agenda-files (directory-files-recursively gwbrck/roam "\\.org$"))
  :custom
  (org-agenda-start-with-log-mode t)
  (org-log-done 'note)
  (org-log-into-drawer t)
  :general
  ("SPC" nil :keymaps 'org-agenda-mode-map)
  ("SPC" nil :keymaps 'org-agenda-mode-map :states 'motion)
  (leader-key-def
    "a" '(org-agenda :wk "org-agenda")))

(use-package org-tempo
  :after org
  :init
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("R" . "src R")))

(use-package visual-fill-column
  :straight t
  :after org
  :hook (org-mode . gwbrck/org-mode-visual-fill))

(use-package org-bullets
  :straight t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-contrib
  :after org
  :straight t
  :init
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

(use-package org-make-toc
  :after org
  :straight t
  :hook (org-mode . org-make-toc-mode))

(use-package faces
  :config
  (defun gwbrck/set-font-faces ()
    (set-face-attribute 'default nil :font "Fira Code" :height 120)
    (set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 1.0)
    (set-face-attribute 'variable-pitch nil :font "Cantarell" :weight 'regular :height 1.0)
    (dolist (face '((org-level-1 . 1.2)
                    (org-level-2 . 1.1)
                    (org-level-3 . 1.1)
                    (org-level-4 . 1.0)
                    (org-level-5 . 1.0)
                    (org-level-6 . 1.0)
                    (org-level-7 . 1.0)
                    (org-level-8 . 1.0)))
      (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))
    (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil   :inherit 'fixed-pitch)
    (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-hide nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-block-begin-line nil :height 0.8))
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (gwbrck/set-font-faces))))
  (gwbrck/set-font-faces))

(use-package org-modern
  :straight t
  :config
  (global-org-modern-mode))

(use-package calendar
  :custom
  (calendar-set-date-style 'iso)
  :general
  ("SPC" nil :keymaps 'calendar-mode-map)
  ("SPC" nil :states 'normal :keymaps 'calendar-mode-map))

(use-package doom-themes
  :straight t
  :custom
  (doom-themes-padded-modeline 5))

(use-package modus-themes
  :straight t
  :init
  (setq modus-themes-completions
        (quote ((matches . (background))
                (selection . (extrabold intense accented))
                (popup . (extrabold intense accented)))))
  (setq modus-themes-org-blocks 'gray-background)
  (setq modus-themes-mode-line '(borderless 6)))

(use-package emacs
  :when (equal system-type 'darwin)
  :init
  (defun gwbrck/apply-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme 'doom-one-light t))
      ('dark (load-theme 'doom-vibrant t)))
    (gwbrck/set-font-faces))
  (add-hook 'ns-system-appearance-change-functions #'gwbrck/apply-theme)
  (when (equal system-type 'darwin)
    (defun ns-raise-emacs ()
      "Raise Emacs."
      (ns-do-applescript "tell application \"Emacs\" to activate"))
    (defun ns-raise-emacs-with-frame (frame)
      "Raise Emacs and select the provided frame."
      (with-selected-frame frame
        (when (display-graphic-p)
          (ns-raise-emacs))))
    (add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame)
    (when (display-graphic-p)
      (ns-raise-emacs))))

(use-package vertico
  :straight t
  :init
  (vertico-mode)
  (setq vertico-resize t)
  (setq vertico-cycle t))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :straight t
  :init
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)
  (setq enable-recursive-minibuffers t))

(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

(use-package consult
  :straight t
  :general
  ("C-s" 'consult-line)
  (leader-key-def "bs" 'consult-buffer)
  (:keymaps 'org-mode-map
            :states 'normal
            "/" 'consult-outline)
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

(use-package embark
  :straight t
  :general
  (leader-key-def
    "e" '(embark-act :wk "embark")
    "E" '(embark-dwim :wk "embark"))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
	(which-key--show-keymap
	 (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
	 (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
	 nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))
  (setq embark-indicators
	'(embark-which-key-indicator
	  embark-highlight-indicator
	  embark-isearch-highlight-indicator))
  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))
  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)
  (delete '(kill-buffer embark--confirm) embark-pre-action-hooks))

(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-echo-documentation t) ;; Do not show documentation in the echo area
  (corfu-scroll-margin 5)        ;; Use scroll margin
  (corfu-preview-current t)
  (corfu-preselect-first nil)
  :general
  (:keymaps 'corfu-map
        "TAB"  'corfu-next
        [tab] 'corfu-next
        "S-TAB" 'corfu-previous
        [backtab] 'corfu-previous
        "SPC" 'corfu-insert-separator)
  :init
  (global-corfu-mode))

(use-package corfu-doc
  :straight t
  :hook (corfu-mode . corfu-doc-mode))

(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :straight t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package helpful
  :straight t
  :general
  ([remap describe-function] 'helpful-function
   [remap describe-symbol] 'helpful-symbol
   [remap describe-variable] 'helpful-variable
   [remap describe-command] 'helpful-command
   [remap describe-key] 'helpful-key))

(use-package elisp-demos
  :straight t
  :after helpful
  :init
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package flycheck
  :straight t
  :init
  (global-flycheck-mode))


(use-package bibtex-clean
  :after bibtex
  :hook ((bibtex-mode . bibtex-clean-mode)
         (biblio-mode . bibtex-clean-mode))
  :load-path "lisp/")

(use-package biblio
  :straight t
  :after bibtex-clean
  :custom
  (biblio-cleanup-bibtex-function 'gwbrck/bibtex-clean-entry))

(use-package citar
  :straight t
  :config
  (setq citar-bibliography '("~/Documents/myBib/main.bib"))
  (setq citar-library-paths '("~/Documents/myBib/pdfs"))
  (setq citar-notes-paths '("~/Documents/myBib/roam/Konspekte"))
  (setq citar-symbols
	`((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
          (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
          (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
  (setq citar-symbol-separator "  ")
  (setq citar-file-note-org-include '(org-id org-roam-ref)))

(use-package oc
  :no-require
  :custom
  (org-cite-global-bibliography '("~/Documents/myBib/main.bib"))
  (org-cite-csl-styles-dir "~/.config/csl/styles")
  (org-cite-csl-locales-dir "~/.config/csl/locales")
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (org-cite-export-processors '((t csl "apa.csl")))
  :general
  (:keymaps 'org-mode-map
            "C-c b" 'org-cite-insert))

(use-package org-roam
  :straight t
  :after org
  :init
  (setq org-roam-v2-ack t)
  :config
  (setq org-roam-directory gwbrck/roam)
  (setq org-roam-capture-templates 
        '(("d" "default" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
          ("p" "projekte" plain "%?"
           :if-new (file+head "Projekte/${slug}.org"
                              "#+title: ${title}\n#+created: %(format-time-string \"[%Y-%m-%d %H:%M]\")\n\n")
           :unnarrowed t)))
  (org-roam-setup))

(use-package vulpea-org-roam
  :after org-roam
  :load-path "lisp/")

(use-package ox
  :after org
  :init
  (setq org-export-default-language "de-de")
  (add-to-list 'org-export-smart-quotes-alist 
               '("de-de"
                 (primary-opening   :utf-8 "\"" :html "&ldquo;" :latex "\\enquote{"  :texinfo "``")
                 (primary-closing   :utf-8 "\"" :html "&rdquo;" :latex "}"           :texinfo "''")
                 (secondary-opening :utf-8 "'" :html "&lsquo;" :latex "\\enquote*{" :texinfo "`")
                 (secondary-closing :utf-8 "'" :html "&rsquo;" :latex "}"           :texinfo "'")
                 (apostrophe        :utf-8 "’" :html "&rsquo;")))
  (setq org-export-with-smart-quotes t))

(use-package ox-latex
  :after org
  :init
  (setq org-latex-listings 'minted)
  (setq org-latex-minted-options
        '(("bgcolor" "WhiteSmoke")))
  (setq org-latex-tables-booktabs t)
  (setq org-latex-caption-above t)
  (setq org-latex-pdf-process '("latexmk -shell-escape -bibtex -pdf %f"))
  (setq org-latex-default-class "article")
  ;;(add-to-list 'org-latex-packages-alist '"\\addbibresource{~/Documents/myBib/main.bib}")
  (add-to-list 'org-latex-packages-alist '("" "tabularx"))
  (add-to-list 'org-latex-packages-alist '("" "booktabs"))
  (add-to-list 'org-latex-packages-alist '("tableposition=top" "caption" t nil))
  ;;(add-to-list 'org-latex-packages-alist '("shorthands=off, ngerman" "babel"))
  (add-to-list 'org-latex-packages-alist '("shorthands=off, AUTO" "babel" t))
  (add-to-list 'org-latex-packages-alist '("" "csquotes"))
  ;;(add-to-list 'org-latex-packages-alist '("style=apa, backend=biber, natbib=true" "biblatex"))
  (add-to-list 'org-latex-packages-alist '("" "minted")) 
  (add-to-list 'org-latex-packages-alist '("" "setspace")) 
  (add-to-list 'org-latex-packages-alist '("" "xspace")) 
  (add-to-list 'org-latex-packages-alist '("" "pdflscape")) 
  (add-to-list 'org-latex-packages-alist '("" "longtable")) 
  (add-to-list 'org-latex-packages-alist '("" "array")) 
  (add-to-list 'org-latex-packages-alist '("" "multirow")) 
  (add-to-list 'org-latex-packages-alist '("" "float"))
  (add-to-list 'org-latex-packages-alist '("" "subfig"))
  (add-to-list 'org-latex-packages-alist '("" "colortbl"))
  (add-to-list 'org-latex-packages-alist '("" "tabu"))
  (add-to-list 'org-latex-packages-alist '("" "threeparttable"))
  (add-to-list 'org-latex-packages-alist '("" "threeparttablex"))
  (add-to-list 'org-latex-packages-alist '("" "makecell"))
  (add-to-list 'org-latex-packages-alist '("svgnames" "xcolor"))
  (add-to-list 'org-latex-packages-alist '("notquote" "hanging"))
  (add-to-list 'org-latex-packages-alist '("inline" "enumitem")))

(use-package ox-moderncv
  :load-path "lisp/")
  
(use-package flyspell
  :hook ((flyspell-mode . flyspell-buffer)
         (flyspell-mode . synosaurus-mode))
  :after evil
  :config
  (assoc-delete-all 'flyspell-mode minor-mode-map-alist)
  (setq flyspell-mode-map (make-sparse-keymap))
  (add-to-list 'minor-mode-map-alist `(flyspell-mode . ,flyspell-mode-map))
  (cond ((executable-find "enchant-2")  (progn
                                          (setq-default ispell-program-name "enchant-2")
                                          (setq ispell-dictionary   "de_DE")))
        ((executable-find "hunspell")   (progn
                                          (setq-default ispell-program-name "hunspell")
                                          (setq ispell-really-hunspell t)
                                          (setq ispell-dictionary   "de_DE")))
        ((executable-find "aspell")     (progn
                                          (setq-default ispell-program-name "aspell")
                                          (setq ispell-dictionary   "de_DE"))))
  :general
  (leader-key-def-mapless
    :keymaps 'flyspell-mode-map
    "s" '(:ignore t :wk "spell")
    "sn" '(flyspell-goto-next-error :wk "next error")
    "sv" '(evil-prev-flyspell-error :wk "preVious error")))

(use-package flyspell-correct
  :straight t
  :after flyspell
  :general
  (leader-key-def-mapless
    :keymaps 'flyspell-mode-map
    "ss" 'flyspell-correct-wrapper))

(use-package synosaurus
  :straight t
  :after flyspell
  :init
  (setq synosaurus-backend 'synosaurus-backend-openthesaurus)
  (setq synosaurus-choose-method 'default)
  :config
  (assoc-delete-all 'synosaurus-mode minor-mode-map-alist)
  (setq synosaurus-mode-map (make-sparse-keymap))
  (add-to-list 'minor-mode-map-alist `(synosaurus-mode . ,synosaurus-mode-map))
  :general
  (leader-key-def-mapless
    :keymaps 'flyspell-mode-map
    "so" '(:ignore t :wk "openthesaurus")
    "si" '(synosaurus-choose-and-insert :wk "synonym insert")
    "sc" '(synosaurus-choose-and-replace :wk "caw with synonym")))

(use-package tree-sitter
  :straight t
  :init
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :straight t)

(use-package lsp-mode
  :straight t
  :commands (lsp)
  :custom
  (lsp-completion-provider nil)
  (lsp-keep-workspace-alive nil)
  (lsp-keymap-prefix "C-c l")
  :init
  (defun gwbrck/start-pylsp ()
    "Function to start python lsp"
    (when (pipenv-project?)
      (pipenv-mode)
      (pipenv-activate)
      (unless (pipenv-executable-find "pylsp")
        (pipenv--force-wait (pipenv-install "--dev python-lsp-server[all]")))
      (setq-local lsp-pylsp-plugins-jedi-environment pyvenv-virtual-env)
      (setq-local lsp-pylsp-server-command (pipenv-executable-find "pylsp"))
      (let ((root (lsp-workspace-root)))
        (if (boundp 'gwbrck/py-projd)
            (add-to-list 'gwbrck/py-projd root)
        (setq gwbrck/py-projd (list root))))
      (add-hook
       'lsp-after-uninitialized-functions
       (lambda (ws)
         (when (member (lsp--workspace-root ws) gwbrck/py-projd)
           (pipenv-deactivate))))
      (lsp)))
  (defun gwbrck/corfu-lsp ()
     "Using orderless instead of default-lsp"
     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
           '(orderless)))
  :hook
  (lsp-completion-mode . gwbrck/corfu-lsp)
  (c-mode . lsp)
  (c++-mode . lsp)
  (ess-r-mode . lsp)
  (python-mode . gwbrck/start-pylsp)
  (typescript-mode . lsp)
  (json-mode . lsp)
  (java-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-include-signature t))

(use-package consult-lsp
  :straight t)

(use-package lsp-java
  :straight t
  :after dap-mode lsp-mode
  :config
  (require 'dap-java))

(use-package typescript-mode
  :straight t
  :mode "\\.\\(ts\\|tsx\\)\\'"
  :config
  (setq typescript-indent-level 2))

(use-package npm-mode
  :hook
  (typescript-mode . npm-mode)
  (javascript-mode . npm-mode)
  :straight t)

(use-package yaml-mode
  :mode "\\.yml\\'"
  :straight t)

(use-package json-mode
  :straight t
  :mode "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'")

(use-package pipenv
  :straight t
  :custom
  (pipenv-with-projectile nil)
  (pipenv-with-flycheck nil))

(use-package yasnippet
  :straight t
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :straight t
  :ensure t)

(use-package dap-mode
  :straight t
  :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package magit
  :straight t
  :commands (magit-add-section-hook)
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append)
  :general
  (:keymaps 'magit-mode-map "SPC" nil))

(use-package project
  :general
  (leader-key-def
    "p" '(:keymap project-prefix-map :wk "projects")
    "bkp" '(project-kill-buffers :wk "kill project")))

(use-package treemacs
  :straight t
  :defer t
  :init
  (setq treemacs-follow-after-init t
        treemacs-is-never-other-window t)
  :config
  ;; Don't follow the cursor
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

(use-package treemacs-all-the-icons
  :straight t
  :after (treemacs)
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package treemacs-evil
  :straight t
  :ensure t
  :after (evil treemacs))

(use-package treemacs-magit
  :straight t
  :ensure t
  :after (treemacs magit))

(use-package lsp-treemacs
  :straight t
  :after (treemacs lsp)
  :init
  (lsp-treemacs-sync-mode 1)
  (setq lsp-treemacs-theme "all-the-icons"))

(use-package evil-nerd-commenter
  :straight t
  :general
  ("M-/" 'evilnc-comment-or-uncomment-lines))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ess
  :straight t
  :config
  (setq ess-use-eldoc nil)
  (setq ess-use-flymake nil))

(use-package fish-mode
  :straight t)

(use-package term
  :config
  (setq explicit-shell-file-name "/usr/local/bin/fish")
  (setq explicit-fish-args '("-l"))
  (setq term-prompt-regexp "^∃[0-9]*❯ \\|❯ "))

(use-package eterm-256color
  :straight t
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :straight t
  :commands vterm
  :general
  ("C-s-'" 'vterm)
  :config
  (setq vterm-shell "/usr/local/bin/fish --login")
  (setq term-prompt-regexp "^∃[0-9]*❯ \\|❯ ")
  (setq vterm-max-scrollback 10000))

(use-package dired
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :general
  (:states 'normal :keymaps 'dired-mode-map
    "SPC" nil))

(use-package dired-single
  :straight t)

(use-package all-the-icons-dired
  :straight t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package openwith
  :straight t
  :demand t
  :custom
  (openwith-associations '(("\\.pdf\\'" "open" (file))))
  :init
  (openwith-mode t))

(use-package dired-hide-dotfiles
  :straight t 
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package bitwarden-min
  :load-path "lisp/")

(use-package chezmoi
  :straight t
  :general
  (leader-key-def
    "c" '(:ignore t :wk "chezmoi")
    "cf" 'chezmoi-find
    "cd" 'chezmoi-diff
    "cm" 'chezmoi-magit-status
    "cw" 'chezmoi-write))

(use-package ansible
  :straight t
  :hook (yaml-mode . ansible)
  :custom
  (ansible-vault-password-file nil))

(use-package server
  :unless (daemonp)
  :custom
  (server-socket-dir "~/.cache/emacsserver")
  (server-name "emacsen")
  :config
  (unless (server-running-p)
  (server-start)))

;;; init.el ends here
