;;; init.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Package --- Summary
;;; Code:
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
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
  (setq gwbrck/roam "~/Nextcloud/roam/")
  (setq org-directory gwbrck/roam)
  (setq default-directory "~/"))

(use-package emacs
  :when (equal system-type 'darwin)
  :init
  (setq mac-command-modifier      'super
        ns-command-modifier       'super
        mac-option-modifier       'meta
        ns-option-modifier        'meta
        mac-right-option-modifier 'none
        ns-right-option-modifier  'none)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8))

(use-package epg
  :config
  (defun pinentry-loopback-after-init ()
    (unless (eq epg-pinentry-mode 'loopback)
      (setq epg-pinentry-mode 'loopback)))
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook 'pinentry-loopback-after-init)
    (add-hook 'after-init-hook 'pinentry-loopback-after-init)))

(use-package auth-source
  :custom
  (auth-sources '(password-store "~/.authinfo.gpg")))

(use-package password-store
  ;;also comes with brew installation of pass
  :straight t)

(use-package password-store-otp
  :straight t)

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
  (setq evil-cross-lines t)
  (setq evil-respect-visual-line-mode t)
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
  (general-create-definer leader-key-def-minor
    :states '(normal insert visual emacs motion)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-create-definer leader-key-def
    :keymaps '(normal insert visual emacs motion)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-create-definer leader-key-def-c-map
    :states '(normal insert visual emacs motion)
    :prefix "SPC c"
    :prefix-map 'leader-c-map
    :global-prefix "C-SPC c"))

(use-package evil-collection
  :after evil
  :straight t
  :config
  (global-set-key (kbd "s-<backspace>") #'delete-indentation)
  (evil-collection-init)
  (leader-key-def
    "f" '(:ignore t :wk "find")
    "ff" '(find-file :wk "file")
    "d" '(:ignore t :wk "dired")
    "dd" #'dired
    "c" '(:ignore t :wk "mode-map")
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

(use-package view
  :general
  ("SPC" nil :keymaps 'view-mode-map)
  ("SPC" nil :keymaps 'view-mode-map :states 'normal))

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
  :custom
  (org-default-notes-file (concat gwbrck/roam "INBOX.org"))
  (org-clock-clocktable-default-properties '(:maxlevel 4 :scope agenda))
  (org-archive-location (concat gwbrck/roam "archiv.org::* From %s"))
  (org-archive-subtree-save-file-p t)
  (org-startup-folded t)
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
  (add-to-list 'org-tags-exclude-from-inheritance "tasks")
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (advice-add 'org-archive-subtree :after 'org-save-all-org-buffers)
  (setq org-tag-alist
        '((:endgroup)
          ("Konspekt" . ?K)
          ("Exzerpt" . ?E)
          ("fun" . ?c)
          ("work" . ?w)
          ("personal" . ?p))))

(use-package org-agenda
  :after org
  ;; agenda files are defined by vulpea functions (orgroam)
  ;;(setq org-agenda-files (directory-files-recursively gwbrck/roam "\\.org$"))
  :custom
  (org-agenda-columns-add-appointments-to-effort-sum t)
  (org-agenda-start-with-log-mode t)
  (org-log-done 'note)
  (org-log-into-drawer t)
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  :general
  ("SPC" nil :keymaps 'org-agenda-mode-map)
  ("SPC" nil :keymaps 'org-agenda-mode-map :states 'motion)
  (leader-key-def
    "a" '(org-agenda :wk "agenda"))
  :config
  (setq org-agenda-custom-commands
        '(("f" "fun"
           ((tags-todo "fun" ((org-agenda-overriding-header "FUN TODOs")))))
          ("A" "all dashboard"
           ((agenda "" ((org-agenda-span 1)
                        (org-agenda-archives-mode t)
                        (org-agenda-clockreport-mode t)))
            (alltodo "" ((org-agenda-overriding-header "Non schedueled TODOs")))))
          ("W" "all week dashboard"
           ((agenda "" ((org-agenda-span 7)
                        (org-agenda-archives-mode t)
                        (org-agenda-clockreport-mode t)
                        (org-agenda-clockreport-parameter-plist '(:link t :maxlever 4 :block thisweek))))
            (alltodo "" ((org-agenda-overriding-header "Non schedueled TODOs")))))
          ("w" "default week dashboard"
           ((agenda "" ((org-agenda-span 7)
                        (org-agenda-tag-filter-preset '("-fun"))))
            (tags-todo "-fun" ((org-agenda-overriding-header "TODOs")))))
          ("a" "default dashboard"
           ((agenda "" ((org-agenda-span 1)
                        (org-agenda-tag-filter-preset '("-fun"))))
            (tags-todo "-fun" ((org-agenda-overriding-header "TODOs"))))))))

(use-package ox-icalendar
  :after org
  :custom
  (org-icalendar-use-scheduled '(event-if-todo-not-done event-if-not-todo))
  (org-icalendar-use-deadline '(event-if-todo-not-done event-if-not-todo)))

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
  (leader-key-def
    "bs" 'consult-buffer
    "fF" 'consult-ripgrep)
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
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-," . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings))
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
  ;;(defun safe-global-corfu-mode ()
  ;;  (when (display-graphic-p)
  ;;    (global-corfu-mode)))
  ;;(if (daemonp)
  ;;    (add-hook 'server-after-make-frame-hook #'safe-global-corfu-mode)
  ;;  (safe-global-corfu-mode)))

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

(use-package help
  :general
  ("SPC" nil :keymaps 'help-mode-map)
  ("SPC" nil :keymaps 'help-mode-map :states 'normal))

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

(use-package init-bibtex
  :load-path "lisp/"
  :demand t
  :config
  (setq main-bib-file (concat gwbrck/roam "../main.bib")))

(use-package citar
  :straight t
  :demand t
  :after init-bibtex
  :config
  (add-to-list 'citar-bibliography main-bib-file)
  (add-to-list 'citar-library-paths (concat gwbrck/roam "pdfs"))
  (setq citar-symbols
	`((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
          (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
          (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
  (setq citar-symbol-separator "  ")
  :general
  (leader-key-def
    "fb" 'citar-open))

(use-package pdf-drop-mode
  :straight (pdf-drop-mode :type git :host github :repo "rougier/pdf-drop-mode")
  :custom
  (pdf-drop-search-methods '(doi/metadata
                             doi/content
                             arxiv/content))
  (pdf-drop-search-hook #'pdf-drop-pdf-process)
  :config
  (defun pdf-drop-pdf-process (file file-id)
    (when file-id
      (cond ((eq 'doi (car file-id))
             (doi-to-bibtex (cdr file-id)))
            ((eq 'arxiv (car file-id))
             (arxiv-id-to-bibtex (cdr file-id))))
      (let* ((key (with-current-buffer (current-buffer) (bibtex-key-in-head)))
             (directory (if (cdr citar-library-paths)
                            (completing-read "Directory: " citar-library-paths)
                          (car citar-library-paths)))
             (file-path (expand-file-name key directory))
             (extension (file-name-extension file)))
        (copy-file file
                   (concat file-path "." extension) 1))))
      (pdf-drop-mode))

(use-package citar-org-roam
  :after citar org-roam
  :straight (citar-org-roam :type git :host github :repo "emacs-citar/citar-org-roam")
  :custom
  (citar-org-roam-subdir "annotaions")
  (citar-org-roam-note-title-template "${author editor} (${year}) :: ${title}")
  :config
  (citar-org-roam-mode))

(use-package citar-embark
  :straight t
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(use-package oc
  :no-require
  :config
  (add-to-list 'org-cite-global-bibliography main-bib-file)
  :custom
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
  :demand t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory gwbrck/roam)
  (org-roam-completion-everywhere t)
  :config
  (setq org-roam-db-node-include-function
        (lambda ()
          (not (member "ARCHIVE" (org-get-tags)))))
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  (setq org-roam-node-display-template
        (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (defun org-roam-capture-agenda ()
    (interactive)
    (org-roam-capture- :node (org-roam-node-read
                              nil
                              (lambda (node)
                                (member "tasks" (org-roam-node-tags node))))
                       :templates '(("p" "tasks tag" plain "* TODO %?\nSCHEDULED: %t"
                                     :target (file+head+olp "${slug}-%<%Y%m%d%H%M%S>.org"
                                                            "#+title: ${title}\n"
                                                            ("Tasks"))))))
  (defun org-agenda-capture (&optional with-time)
    "Call `org-capture' with the date at point.
With a `C-1' prefix, use the HH:MM value at point (if any) or the
current HH:MM time."
    (interactive "P")
    (if (not (eq major-mode 'org-agenda-mode))
        (user-error "You cannot do this outside of agenda buffers")
      (let ((org-overriding-default-time
	     (org-get-cursor-date (equal with-time 1))))
        (call-interactively 'org-roam-capture-agenda))))

  (setq org-roam-capture-templates 
        '(("m" "main" plain "%?"
           :target
           (file+head "main/${slug}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("a" "annotation" plain "%?"
           :target
           (file+head "annotations/${title}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("p" "project" plain "%?"
           :target
           (file+head "projects/${title}.org" "#+title: ${title}")
           :immediate-finish t
           :unnarrowed t)
          ("t" "INBOX Task" plain "* TODO ${slug} %?\n"
           :target
           (file+head+olp "INBOX.org" "#+title: INBOX\n" ("Tasks"))
           :unnarrowed t)
          ("n" "INBOX Note" plain "* %?\n"
           :target (node "INBOX")
           :immediate-finish t
           :unnarroed t)))
  (advice-add 'org-roam-refile :after 'org-save-all-org-buffers)
  (org-roam-setup)
  :general
  (leader-key-def
    "fn" 'org-roam-node-find
    "o" 'org-roam-capture))

(use-package org-caldav
  :straight t
  :after org-roam
  :config
  (advice-add 'org-caldav-sync :after #'org-save-all-org-buffers)
  :general
  ("SPC" nil :keymaps 'org-caldav-sync-results-mode-map)
  ("SPC" nil :states 'normal :keymaps 'org-caldav-sync-results-mode-map))

(use-package vulpea-org-roam-caldav
  :after org-caldav
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
         (flyspell-mode . synosaurus-mode)
         (flyspell-mode . evil-normalize-keymaps))
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
  (leader-key-def-minor
    :keymaps 'flyspell-mode-map
    "s" nil
    "s" '(:ignore t :wk "spell")
    "sn" '(flyspell-goto-next-error :wk "next error")
    "sv" '(evil-prev-flyspell-error :wk "preVious error")))

(use-package flyspell-correct
  :straight t
  :after flyspell
  :general
  (leader-key-def-minor
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
  (leader-key-def-minor
    :keymaps 'flyspell-mode-map
    "so" '(:ignore t :wk "openthesaurus")
    "si" '(synosaurus-choose-and-insert :wk "synonym insert")
    "sc" '(synosaurus-choose-and-replace :wk "caw with synonym")))

(use-package tree-sitter
  :straight t
  :init
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  :config
  (setf (alist-get 'typescript-tsx-mode tree-sitter-major-mode-language-alist) 'tsx))

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
  :mode (rx ".ts" string-end)
  :custom
  (typescript-indent-level 2)
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "typescript-tsx")
  (add-to-list 'auto-mode-alist (cons (rx ".tsx" string-end) #'typescript-tsx-mode)))

(use-package npm-mode
  :straight t
  :config
  (add-to-list 'which-key-replacement-alist '((nil . "npm-mode-") . (nil . "")))
  (add-to-list 'which-key-replacement-alist '((nil . "npm-mode-npm-") . (nil . "")))
  :general
  (leader-key-def-c-map
    "n" '(:keymap npm-mode-command-keymap :package npm-mode :wk "npm")))

(use-package yaml-mode
  :mode "\\.yml\\'"
  :straight t)

(use-package json-mode
  :straight t
  :mode "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'")

(use-package format-all
  :straight t)

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
  (:keymaps 'magit-diff-mode-map "SPC" nil :states 'normal)
  (:keymaps 'magit-diff-mode-map "SPC" nil)
  (:keymaps 'magit-mode-map "SPC" nil)
  (:keymaps 'magit-mode-map "SPC" nil :states 'normal))

(use-package project
  :general
  (leader-key-def
    "p" '(:keymap project-prefix-map :wk "projects")
    "fp" '(project-find-file :wk "project find")
    "dp" '(project-dired :wk "project dired")
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

(use-package compile
  :custom
  (compilation-scroll-output t)
  :config
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

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

(use-package chezmoi
  :straight t
  :general
  (leader-key-def
    "fd" '(chezmoi-find :wk "dotfiles")
    "xcm" '(chezmoi-magit-status :wk "chezmoi magit")
    "xcd" 'chezmoi-diff
    "xcw" 'chezmoi-write))

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
