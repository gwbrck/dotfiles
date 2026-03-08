(use-package ns-auto-titlebar
  :ensure t
  :when (equal system-type 'darwin)
  :init (ns-auto-titlebar-mode +1))

(use-package nerd-icons
  :ensure t
  :config
  (let* ((icon '(:eval (nerd-icons-icon-for-buffer)))
         (n 4)
         (before (cl-subseq mode-line-format 0 n))
         (after (cl-subseq mode-line-format n)))
    (setq-default mode-line-format (append before (list icon " ") after))))

(use-package minions
  :ensure t
  :config
  (setq minions-mode-line-lighter "[↑]")
  :init (minions-mode 1))

(use-package which-key
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.1))

(use-package emacs
  :config
  (require-theme 'modus-themes) ; `require-theme' is ONLY for the built-in Modus themes

  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t
        modus-themes-headings
            '((1 . (variable-pitch 1.8))
              (2 . (semibold 1.5))
              (t . t))
        modus-themes-variable-pitch-ui t))


(use-package auto-dark
  :ensure t
  :custom
  (auto-dark-themes '((modus-vivendi) (modus-operandi)))
  (auto-dark-allow-osascript t)
  :init (auto-dark-mode))

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1))

(use-package spacious-padding
  :ensure t
  :init
  (spacious-padding-mode 1))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-sort-function 'vertico-sort-history-alpha))

;; Persist history over Emacs restarts.

(use-package savehist
  :ensure t
  :init
  (savehist-mode))

;; Search for partial matches in any order

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles partial-completion)))))

;; Enable richer annotations using the Marginalia package

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package helpful
  :ensure t
  :bind
  (("C-h f" . helpful-function)
   ("C-h x" . helpful-command)
   ("C-h k" . helpful-key)
   ("C-h v" . helpful-variable)))

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

(use-package text-mode
  :hook
  (text-mode . visual-line-mode)
  :init
  (delete-selection-mode t)
  :custom
  (sentence-end-double-space nil)
  (scroll-error-top-bottom t)
  (save-interprogram-paste-before-kill t))

(use-package olivetti
  :ensure t)

(use-package quarto-mode
  :ensure t)

(use-package mixed-pitch
  :ensure t
  :config
  (dolist (face '(markdown-code-face
                  markdown-pre-face
                  markdown-table-face
                  markdown-markup-face
                  markdown-inline-code-face
                  markdown-metadata-key-face
                  markdown-metadata-value-face
                  markdown-language-keyword-face
                  markdown-list-face
                  markdown-header-delimiter-face
                  markdown-header-rule-face
                  markdown-url-face))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face)))

(use-package dired
  :commands (dired dired-jump)
  :custom 
  (dired-listing-switches "-agho --group-directories-first")
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (evil-define-key 'normal dired-mode-map (kbd "SPC") nil))

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package dired-hide-dotfiles
  :ensure t
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))


(use-package chezmoi
  :ensure t)

(use-package server
  :unless (daemonp)
  :config
  (unless (server-running-p)
  (server-start)))
