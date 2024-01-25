;;; init.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Package --- Summary
;;; Code:

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(use-package custom
  :no-require t
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(when (version< emacs-version "30.0")
  (unless (package-installed-p 'vc-use-package)
    (package-vc-install "https://github.com/slotThe/vc-use-package"))
  (require 'vc-use-package))

(use-package emacs
  :init
  (setq gwbrck/bib "~/Documents/Bib")
  (setq org-directory (concat gwbrck/bib "/org/"))
  (setq default-directory "~/"))

(use-package emacs
  :when (equal system-type 'darwin)
  :init
  (setq mac-command-modifier      'control
        ns-command-modifier       'control
        mac-control-modifier      'meta
        ns-control-modifier       'meta
        mac-option-modifier       'super
        ns-option-modifier        'super
        mac-right-option-modifier 'none
        ns-right-option-modifier  'none)
  (global-set-key (kbd "M-<backspace>") #'delete-indentation)
  (global-set-key (kbd "s-<backspace>") #'backward-kill-word)
  (global-set-key (kbd "C-<backspace>") #'kill-line)
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
  :ensure t)

(use-package password-store-otp
  :ensure t)

(use-package emacs
  :init
  (setq native-comp-async-report-warnings-errors nil)
  (setq inhibit-startup-message t)
  (setq frame-resize-pixelwise t)
  (add-to-list 'default-frame-alist '(height . 60))
  (add-to-list 'default-frame-alist '(width . 120))
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (when (eq system-type 'gnu/linux)
    (menu-bar-mode -1))
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
  (setq-default tab-width 4)
  (setq tab-always-indent 'complete))

(use-package pixel-scroll
  :when (>= emacs-major-version 29)
  :demand t
  :init
  (pixel-scroll-precision-mode))

(use-package olivetti
  :ensure t)

(use-package files
  :no-require t
  :config
  (if (eq system-type 'gnu/linux)
      (setq insert-directory-program "ls")
    (setq insert-directory-program "gls"))
  (setq backup-directory-alist '(("." . "~/.config/emacs/backup")))
  (setq backup-by-copying t)
  (setq version-control t)
  (setq delete-old-versions t)
  (setq kept-new-versions 20)
  (setq kept-old-versions 5))

(use-package ns-auto-titlebar
  :ensure t
  :when (equal system-type 'darwin)
  :init (ns-auto-titlebar-mode +1))

(use-package evil
  :ensure t
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
  :ensure t
  :after evil
  :config
  (general-unbind 'motion "SPC")
  (general-create-definer leader-key-def
    :keymaps '(normal insert visual emacs motion)
    :prefix "SPC"
    :prefix-command 'leader-command
    :prefix-map 'leader-map
    :global-prefix "C-SPC")
  (general-create-definer leader-key-def-c-map
    :states '(normal insert visual emacs motion)
    :prefix "SPC c"
    :prefix-map 'leader-c-map
    :global-prefix "C-SPC c"))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init)
  (leader-key-def
    "s" '(:ignore t :wk "spell")
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
    "w" '(evil-window-map :which-key "windows")))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org)
  (require 'evil-org-agenda)
  (evil-org-set-key-theme '(navigation todo insert textobjects additional))
  (evil-org-agenda-set-keys))

(use-package evil-snipe
  :ensure t
  :after evil-collection
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
  (setq evil-snipe-scope 'buffer))

(use-package evil-surround
  :ensure t
  :after evil-collection
  :config
  (global-evil-surround-mode 1))

(use-package view
  :general
  ("SPC" nil :keymaps 'view-mode-map)
  ("SPC" nil :keymaps 'view-mode-map :states 'normal))

(use-package pulse
  :custom (pulse-flag t)
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
  :ensure t)

(use-package minions
  :ensure t
  :config
  (setq minions-mode-line-lighter "[↑]")
  :init (minions-mode 1))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1))

(use-package restclient
  :ensure t)

(use-package ob-restclient
  :ensure t
  :after org)

(use-package mermaid-ts-mode
  :ensure t)

(use-package ob-mermaid
  :ensure t)

(use-package org
  :ensure t
  :demand t
  :hook ((org-mode . gwbrck/set-font-faces)
         (org-mode . visual-line-mode)
         (org-mode . variable-pitch-mode))
  :custom
  (org-default-notes-file (concat org-directory "main.org"))
  (org-startup-folded t)
  (org-startup-indented t)
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (python . t)
     (dot . t)
     (mermaid . t)
     (shell . t)
     (emacs-lisp . t)
     (restclient . t)))
  (setq org-ellipsis " ▾"))

(use-package oc-csl-activate
  :vc (oc-csl-activate
       :url "https://github.com/andras-simonyi/org-cite-csl-activate.git"
       :rev :newest
       :branch "main")
  :ensure t
  :custom (org-cite-csl-activate-use-citar-cache t)
  :config
  (add-hook 'org-mode-hook (lambda () (cursor-sensor-mode 1)) 98)
  (add-hook 'org-mode-hook (lambda () (org-cite-csl-activate-render-all)) 99))

(use-package org-agenda
  :after org
  :custom
  ;; agenda files are defined by vulpea functions (orgroam)
  (org-agenda-files `(,(concat org-directory "main.org")))
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
    "a" '(org-agenda :wk "agenda")))

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
  (add-to-list 'org-structure-template-alist '("get" . "src restclient"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("R" . "src R")))

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-contrib
  :after org
  :ensure t
  :init
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

(use-package org-make-toc
  :after org
  :ensure t
  :hook (org-mode . org-make-toc-mode))

(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t
        modus-themes-org-blocks 'gray-background)
  (setq modus-themes-common-palette-overrides
      '((fg-completion-match-0 blue)
        (fg-completion-match-1 magenta-warmer)
        (fg-completion-match-2 cyan)
        (fg-completion-match-3 red)
        (bg-completion-match-0 bg-blue-nuanced)
        (bg-completion-match-1 bg-magenta-nuanced)
        (bg-completion-match-2 bg-cyan-nuanced)
        (bg-completion-match-3 bg-red-nuanced))))

(use-package faces
  :config
  (defun gwbrck/set-font-faces (&optional frame)
    (interactive)
    (select-frame (or frame (selected-frame)))
    (when (display-graphic-p)
      (set-face-attribute 'default nil :font "Fira Code" :height 120)
      (set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 1.0)
      (set-face-attribute 'variable-pitch nil :font "Cantarell" :weight 'regular :height 1.0)
      (dolist (face '((org-level-1 . 1.3)
                      (org-level-2 . 1.2)
                      (org-level-3 . 1.1)
                      (org-level-4 . 1.0)
                      (org-level-5 . 1.0)
                      (org-level-6 . 1.0)
                      (org-level-7 . 1.0)
                      (org-level-8 . 1.0)))
        (set-face-attribute (car face) nil :height (cdr face)))
      (set-face-attribute 'org-document-title nil :height 1.6 :inherit 'default)
      (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
      (set-face-attribute 'org-code nil   :inherit 'fixed-pitch)
      (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
      (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
      (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
      (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
      (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
      (set-face-attribute 'org-hide nil :inherit 'fixed-pitch)
      (set-face-attribute 'org-block-begin-line nil :height 0.8)
      (dolist (face '(window-divider
                      window-divider-first-pixel
                      window-divider-last-pixel))
        (face-spec-reset-face face)
        (set-face-foreground face (face-attribute 'default :background)))
      (set-face-background 'fringe (face-attribute 'default :background))
      (set-face-background 'line-number (face-attribute 'default :background))
      (set-face-attribute 'mode-line nil
                          :box `(:line-width 5 :color ,(face-attribute 'mode-line :background nil)))
      (set-face-attribute 'mode-line-inactive nil
                          :box `(:line-width 5 :color ,(face-attribute 'mode-line-inactive :background nil)))
      (set-face-background 'fringe (face-attribute 'default :background))))
  (add-hook 'after-make-frame-functions 'gwbrck/set-font-faces)
  (unless (daemonp) (gwbrck/set-font-faces))
  (defun gwbrck/apply-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme 'modus-operandi-tinted t))
      ('dark (load-theme 'modus-vivendi-tinted t)))
    (gwbrck/set-font-faces)))

(use-package calendar
  :custom
  (calendar-set-date-style 'iso)
  :general
  ("SPC" nil :keymaps 'calendar-mode-map)
  ("SPC" nil :states 'normal :keymaps 'calendar-mode-map))

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-padded-modeline 5))

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1))

(use-package emacs
  :when (equal system-type 'darwin)
  :config
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

(use-package emacs
  :when (equal system-type 'gnu/linux)
  :config
  (defun theme--handle-dbus-event (a setting values)
    "Handler for FreeDesktop theme changes."
    (let ((scheme (car values)))
      (cond
       ((eq 1 scheme)
        (gwbrck/apply-theme 'dark))
       ((eq 2 scheme)
        (gwbrck/apply-theme 'light))
       (t (message "I don't know how to handle scheme: %s" scheme)))))
  (when (message (getenv "DBUS_SESSION_BUS_ADDRESS"))
    (require 'dbus)
    (theme--handle-dbus-event
     nil
     nil
     (dbus-call-method :session
                       "org.freedesktop.impl.portal.desktop.darkman"
                       "/org/freedesktop/portal/desktop"
                       "org.freedesktop.impl.portal.Settings"
                       "Read"
                       "org.freedesktop.appearance" "color-scheme"))
    (dbus-register-signal :session
                          "org.freedesktop.impl.portal.desktop.darkman"
                          "/org/freedesktop/portal/desktop"
                          "org.freedesktop.impl.portal.Settings"
                          "SettingChanged"
                          #'theme--handle-dbus-event)))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (setq vertico-resize t)
  (setq vertico-cycle t))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :ensure t
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
  :ensure t
  :init
  (marginalia-mode))

(use-package consult
  :ensure t
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
  :ensure t
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
  :ensure t
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-echo-documentation t)
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
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package help
  :general
  ("SPC" nil :keymaps 'help-mode-map)
  ("SPC" nil :keymaps 'help-mode-map :states 'normal))

(use-package helpful
  :ensure t
  :general
  ([remap describe-function] 'helpful-function
   [remap describe-symbol] 'helpful-symbol
   [remap describe-variable] 'helpful-variable
   [remap describe-command] 'helpful-command
   [remap describe-key] 'helpful-key))

(use-package elisp-demos
  :ensure t
  :after helpful
  :init
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package init-bibtex
  :load-path "lisp/"
  :demand t
  :config
  (setq main-bib-file (concat gwbrck/bib "/main.bib")))

(use-package zotra
  :ensure t
  :custom
  (zotra-backend 'zotra-server)
  (zotra-local-server-directory "~/.config/zotra-server/")
  (zotra-default-entry-format "biblatex")
  (zotra-download-attachment-default-directory "~/Desktop/")
  :config
  (defun zotra-download-attachment-for-current-entry ()
    (interactive)
    (save-excursion
      (bibtex-beginning-of-entry)
      (let* ((entry (bibtex-parse-entry t))
             (key (cdr (assoc "=key=" entry)))
             (url (or (cdr (assoc "url" entry))
                      (cdr (assoc "doi" entry))))
             (filename (concat key ".pdf")))
        (when (and entry
                   (y-or-n-p (concat "Try download for " key)))
          (let* ((newfile (zotra-download-attachment
                            url (concat gwbrck/bib "/pdfs/") filename)))
            (when newfile
              (bibtex-make-field (list "File" nil newfile) t)))))))
  (add-hook 'zotra-after-get-bibtex-entry-hook 'zotra-download-attachment-for-current-entry 99))

(use-package citar
  :ensure t
  :demand t
  :after init-bibtex
  :config
  (add-to-list 'citar-bibliography main-bib-file)
  (add-to-list 'citar-library-paths (concat gwbrck/bib "/pdfs"))
  (add-to-list 'citar-notes-paths (file-truename (concat org-directory "/roam/annotations")))
  (setq citar-indicator-files-icons
    (citar-indicator-create
     :symbol (all-the-icons-faicon
              "file-o"
              :face 'all-the-icons-green
              :v-adjust -0.1)
     :function #'citar-has-files
     :padding "  "
     :tag "has:files"))
  (setq citar-indicator-links-icons
    (citar-indicator-create
     :symbol (all-the-icons-octicon
              "link"
              :face 'all-the-icons-orange
              :v-adjust 0.01)
     :function #'citar-has-links
     :padding "  "
     :tag "has:links"))
  (setq citar-indicator-notes-icons
    (citar-indicator-create
     :symbol (all-the-icons-material
              "speaker_notes"
              :face 'all-the-icons-blue
              :v-adjust -0.3)
     :function #'citar-has-notes
     :padding "  "
     :tag "has:notes"))
  (setq citar-indicators
        (list citar-indicator-files-icons
              citar-indicator-links-icons
              citar-indicator-notes-icons))
  :general
  (leader-key-def
    "fb" 'citar-open)
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

(use-package zotra-pdf-drop-mode
  :vc (zotra-pdf-drop-mode
       :url "https://github.com/gwbrck/zotra-pdf-drop-mode.git"
       :rev :newest
       :branch "main")
  :after citar
  :custom
  (zotra-pdf-drop-on-new-entry-hook #'zotra-pdf-drop-process)
  :config
  (defun zotra-pdf-drop-process (file file-id)
    (setq zotra-last-processed-pdf file)
    (zotra-add-entry-from-search file-id))
  (zotra-pdf-drop-mode))

(use-package citar-org-roam
  :after citar org-roam
  :ensure t
  :custom
  (citar-org-roam-subdir "annotations")
  (citar-org-roam-note-title-template "${author editor} (${year}): ${title}")
  :config
  (citar-org-roam-mode))

(use-package citar-embark
  :ensure t
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
  (org-cite-activate-processor 'csl-activate)
  (org-cite-export-processors '((t csl "apa.csl")))
  :general
  (:keymaps 'org-mode-map
            "C-c b" 'org-cite-insert))

(use-package org-roam
  :ensure t
  :after org
  :custom
  (org-roam-completion-everywhere t)
  (org-roam-directory (file-truename (concat org-directory "/roam")))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

(use-package org-roam-ui
  :ensure t)

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
  (setq org-export-with-smart-quotes t)
  (setq org-html-style
        "<style type=\"text/css\">
       body { font-family: sans-serif; }
       </style>"))

(use-package htmlize
  :ensure t)

(use-package ox-latex
  :after org
  :init
  (setq org-latex-listings 'minted)
  (setq org-latex-minted-options
        '(("bgcolor" "WhiteSmoke")
          ("breaklines" "true")))
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

(use-package ox-tudbeamer
  :load-path "lisp/")

(use-package ox-pandoc
  :ensure t)

(use-package consult-notes
  :ensure t)

(use-package consult-org-roam
   :ensure t
   :after org-roam
   :init
   (require 'consult-org-roam)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   ;; Use `ripgrep' for searching with `consult-org-roam-search'
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?r)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers t)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize
    consult-org-roam-forward-links
    :preview-key (kbd "M-."))
   :bind
   ;; Define some convenient keybindings as an addition
   ("C-c n e" . consult-org-roam-file-find)
   ("C-c n b" . consult-org-roam-backlinks)
   ("C-c n l" . consult-org-roam-forward-links)
   ("C-c n r" . consult-org-roam-search))

;; (use-package flyspell
;;   :hook ((flyspell-mode . flyspell-buffer)
;;          (flyspell-mode . synosaurus-mode)
;;          (flyspell-mode . evil-normalize-keymaps))
;;   :after evil
;;   :config
;;   (assoc-delete-all 'flyspell-mode minor-mode-map-alist)
;;   (setq flyspell-mode-map (make-sparse-keymap))
;;   (add-to-list 'minor-mode-map-alist `(flyspell-mode . ,flyspell-mode-map))
;;   (cond ((executable-find "enchant-2")  (progn
;;                                           (setq-default ispell-program-name "enchant-2")
;;                                           (setq ispell-dictionary   "de_DE")))
;;         ((executable-find "hunspell")   (progn
;;                                           (setq-default ispell-program-name "hunspell")
;;                                           (setq ispell-really-hunspell t)
;;                                           (setq ispell-dictionary   "de_DE")))
;;         ((executable-find "aspell")     (progn
;;                                           (setq-default ispell-program-name "aspell")
;;                                           (setq ispell-dictionary   "de_DE"))))
;;   :general
;;   (leader-key-def-minor
;;     :keymaps 'flyspell-mode-map
;;     "s" nil
;;     "s" '(:ignore t :wk "spell")
;;     "sn" '(flyspell-goto-next-error :wk "next error")
;;     "sv" '(evil-prev-flyspell-error :wk "preVious error")))
;;
;; (use-package flyspell-correct
;;   :ensure t
;;   :after flyspell
;;   :general
;;   (leader-key-def-minor
;;     :keymaps 'flyspell-mode-map
;;     "ss" 'flyspell-correct-wrapper))
;;

(use-package languagetool
  :ensure t
  :defer t
  :init
  (setq languagetool-server-url "https://api.languagetoolplus.com"
        languagetool-server-port 443)
  (setq languagetool-api-key (password-store-get "dev/languagetool")
        languagetool-username (password-store-get-field "dev/languagetool" "Username"))
  (setq languagetool-server-mode-map (make-sparse-keymap))
  :commands (languagetool-correct-at-point
             languagetool-correct-buffer
             languagetool-set-language
             languagetool-server-mode)
  :general
  (leader-key-def
    "ss" '(languagetool-server-mode :wk "server mode")
    "sb" '(languagetool-correct-buffer :wk "correct buffer")
    "sp" '(languagetool-correct-at-point :wk "correct at point")
    "sp" '(languagetool-set-language :wk "correct at point")))

(use-package gptel
  :ensure t
  :custom
  (gptel-api-key
    (lambda ()
      (password-store-get "dev/open-ai"))))

(use-package synosaurus
  :ensure t
  :init
  (setq synosaurus-backend 'synosaurus-backend-openthesaurus)
  (setq synosaurus-choose-method 'default)
  :config
  (assoc-delete-all 'synosaurus-mode minor-mode-map-alist)
  (setq synosaurus-mode-map (make-sparse-keymap))
  (add-to-list 'minor-mode-map-alist `(synosaurus-mode . ,synosaurus-mode-map))
  :general
  (leader-key-def
    :keymaps 'languagetool-server-mode-map
    "si" '(synosaurus-choose-and-insert :wk "synonym insert")
    "sc" '(synosaurus-choose-and-replace :wk "caw with synonym")))

(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-display-truncation-message nil))

(use-package eldoc-box
  :ensure t)

(use-package treesit-auto
  :ensure t
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(use-package eglot
  :config
  (defun eglot-register-format-on-save ()
    "Register a buffer-local 'before-save' hook for formatting with EGLOT."
    (add-hook 'before-save-hook 'eglot-format-buffer-on-save nil t))
  (defun eglot-format-buffer-on-save ()
    "Format buffer using EGLOT before saving, if EGLOT server is active."
    (when (bound-and-true-p eglot--managed-mode)
      (eglot-format-buffer)))
  (add-hook 'eglot-managed-mode-hook #'eglot-register-format-on-save)
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)
  (add-to-list 'eglot-server-programs
               '(json-mode . ("vscode-json-language-server"
                              "--stdio" :initializationOptions (:provideFormatter t))))
  :custom
  (eglot-autoshutdown t)
  (eldoc-echo-area-prefer-doc-buffer t)
  :hook
  (c-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  (ess-r-mode . eglot-ensure)
  (tsx-ts-mode . eglot-ensure)
  (typescript-mode . eglot-ensure)
  (json-mode . eglot-ensure)
  (java-mode . eglot-ensure))

(use-package consult-eglot
  :ensure t)

(use-package pet
  :ensure-system-package (dasel sqlite3)
  :ensure t
  :after eglot
  :config
  (add-hook 'python-base-mode-hook
            (lambda ()
              (setq-local python-shell-interpreter (pet-executable-find "python")
                          python-shell-virtualenv-root (pet-virtualenv-root))
              (when (pet-virtualenv-root)
                (unless (pet-executable-find "pylsp")
                  (message "Install pylsp...")
                  (shell-command (concat (file-name-as-directory (pet-virtualenv-root))
                                         "bin/pip install "
                                         "python-lsp-server"
                                         "[flake8, "
                                         "mccabe, "
                                         "yapf, "
                                         "pycodestyle, "
                                         "pydocstyle, "
                                         "pyflakes, "
                                         "pylint, "
                                         "rope, "
                                         "whatthepatch]"))))
              (pet-eglot-setup)
              (eglot-ensure)
              ;;(setq-local python-pytest-executable (pet-executable-find "pytest"))
              ) -10 ))

(use-package python
  :config
  (setq python-indent-offset 4)
  (defun python-custom-settings ()
    (setq-local tab-width 4))
  (add-hook 'python-base-mode-hook 'python-custom-settings))

(use-package typescript-mode
  :ensure t
  :custom
  (typescript-indent-level 2))

(use-package npm-mode
  :ensure t
  :config
  (add-to-list 'which-key-replacement-alist '((nil . "npm-mode-") . (nil . "")))
  (add-to-list 'which-key-replacement-alist '((nil . "npm-mode-npm-") . (nil . "")))
  :general
  (leader-key-def-c-map
    "n" '(:keymap npm-mode-command-keymap :package npm-mode :wk "npm")))

(use-package yaml-mode
  :mode "\\.yml\\'"
  :ensure t)

(use-package json-mode
  :ensure t
  :mode "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'")

(use-package lua-mode
  :ensure t)

(use-package format-all
  :ensure t)

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :ensure t)

(use-package magit
  :ensure t
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

(use-package with-editor
  :when (equal system-type 'darwin)
  :config
  (setq with-editor-emacsclient-executable (executable-find "emacsclient")))

(use-package info
  :general
  (:keymaps 'Info-mode-map "SPC" nil :states 'normal)
  (:keymaps 'Info-mode-map "SPC" nil))

(use-package project
  :general
  (leader-key-def
    "p" '(:keymap project-prefix-map :wk "projects")
    "fp" '(project-find-file :wk "project find")
    "dp" '(project-dired :wk "project dired")
    "bkp" '(project-kill-buffers :wk "kill project"))
  :config
  (unless (cl-member-if (lambda (s) (string-match "/Code/" s)) (project-known-project-roots))
    (project-remember-projects-under "~/Code/" t))
  (add-to-list 'project-find-functions 'ess-r-project t))

(use-package evil-nerd-commenter
  :ensure t
  :general
  ("M-/" 'evilnc-comment-or-uncomment-lines))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ess
  :ensure t
  :config
  (defun ess-r-mode-tab-width ()
    (setq-local tab-width 2))
  (add-hook 'ess-r-mode-hook 'ess-r-mode-tab-width)
  (setq ess-use-eldoc nil)
  (setq ess-use-flymake nil))

(use-package quarto-mode
  :ensure t
  :config
  (defun quarto-preview ()
    "Overwritten quarto function"
    (interactive)
    (when quarto-mode--preview-process
    (delete-process quarto-mode--preview-process))
    (when (get-buffer "*quarto-preview*")
      (kill-buffer "*quarto-preview*"))

    (let* ((project-directory (quarto-mode--buffer-in-quarto-project-p))
	       (browser-path (cond
			              (project-directory
			               (file-relative-name buffer-file-name project-directory))
			              (t "")))
	       (process
	        (let ((process-environment (cons (concat "QUARTO_RENDER_TOKEN="
						                             quarto-mode--quarto-preview-uuid)
					                         process-environment)))
	          (make-process :name (format "quarto-preview-%s" buffer-file-name)
			                :buffer "*quarto-preview*"
			                :command (list quarto-command
					                       "preview"
					                       buffer-file-name)))))
      (setq quarto-mode--preview-process process)
      (with-current-buffer (process-buffer process)
        (shell-mode)
        (set-process-filter process 'quarto-mode--process-filter)))))

(use-package fish-mode
  :ensure t)

(use-package term
  :config
  (if (eq system-type 'gnu/linux)
      (setq explicit-shell-file-name "/bin/fish")
    (setq explicit-shell-file-name "/opt/homebrew/bin/fish"))
  (setq explicit-fish-args '("-l"))
  (setq term-prompt-regexp "^∃[0-9]*❯ \\|❯ "))

(use-package eterm-256color
  :ensure t
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :ensure t
  :commands vterm
  :general
  ("M-'" 'vterm)
  :config
  (if (eq system-type 'gnu/linux)
      (setq vterm-shell "/bin/fish")
    (setq vterm-shell "/opt/homebrew/bin/fish --login"))
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
  :config
  (evil-define-key 'normal dired-mode-map (kbd "SPC") nil))

(use-package ediff
  :custom
  (ediff-keep-variants t)
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  :config
  (defun edt-unbind-space ()
    (define-key ediff-mode-map (kbd "<SPC>") nil))
  (defun edt-meta-unbind-space ()
    (define-key ediff-meta-buffer-map (kbd "<SPC>") 'leader-command))
  (add-hook 'ediff-keymap-setup-hook 'edt-unbind-space t)
  (add-hook 'ediff-meta-buffer-keymap-setup-hook 'edt-meta-unbind-space t))

(use-package diff-mode
  :config
  (evil-define-key 'normal diff-mode-map (kbd "SPC") nil))

(use-package elfeed-protocol
  :ensure t
  :custom
  (elfeed-protocol-enabled-protocols '(fever))
  (elfeed-protocol-fever-update-unread-only nil)
  :config
  (setq elfeed-protocol-feeds `((,(password-store-get-field "dev/freshrss" "elfeed")
                           :api-url ,(password-store-get-field "dev/freshrss" "api_url")
                           :password ,(password-store-get-field "dev/freshrss" "api_password"))))
  (evil-define-key 'normal elfeed-show-mode-map (kbd "SPC") nil)
  (evil-define-key 'normal elfeed-search-mode-map (kbd "SPC") nil)
  (elfeed-protocol-enable))

(use-package dired-single
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :ensure t
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package chezmoi
  :ensure t
  :general
  (leader-key-def
    "fd" '(chezmoi-find :wk "dotfiles")
    "xcm" '(chezmoi-magit-status :wk "chezmoi magit")
    "xcd" 'chezmoi-diff
    "xcw" 'chezmoi-write))

(use-package ansible
  :ensure t
  :hook (yaml-mode . ansible)
  :custom
  (ansible-vault-password-file nil))

(use-package pdf-tools
  :ensure t
  :init
  ;;Prevent cursor of beeing visible
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (set (make-local-variable 'evil-normal-state-cursor) (list nil))))
  :config
  (add-to-list 'pdf-tools-enabled-modes 'pdf-view-auto-slice-minor-mode)
  (add-to-list 'pdf-tools-enabled-modes 'pdf-view-themed-minor-mode)
  (pdf-tools-install :no-query)
  :general
  ("SPC" nil :keymaps 'pdf-view-mode-map)
  ("SPC" nil :keymaps 'pdf-view-mode-map :states 'normal))

(use-package nov;;.el
  :ensure t)

(use-package reveal-in-osx-finder
  :ensure t)

(use-package server
  :unless (daemonp)
  :config
  (unless (server-running-p)
  (server-start)))

;;; init.el ends here
