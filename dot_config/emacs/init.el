(use-package ns-auto-titlebar
  :ensure t
  :when (equal system-type 'darwin)
  :init (ns-auto-titlebar-mode +1))

(use-package all-the-icons
  :ensure t
  :config
  (let* ((icon '(:eval (all-the-icons-icon-for-mode major-mode)))
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
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1))

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



