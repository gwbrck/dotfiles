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

(use-package markdown-mode
  :ensure t
  :hook
  (markdown-mode . my-markdown-fixed-and-variable)
  :config
  (defun my-markdown-fixed-and-variable ()
    "Use variable-pitch for prose, fixed-pitch for code/structural parts in markdown."
    (variable-pitch-mode 1)
    ;; Basis-Fonts setzen (optional)
    (set (make-local-variable 'face-remapping-alist)
         `((default variable-pitch)
           (fixed-pitch fixed-pitch)
           (fixed-pitch-serif fixed-pitch-serif)))
    (set-face-attribute 'markdown-code-face nil :inherit 'fixed-pitch)
    (set-face-attribute 'markdown-pre-face  nil :inherit 'fixed-pitch)
    (set-face-attribute 'markdown-table-face nil :inherit 'fixed-pitch)
    (set-face-attribute 'markdown-markup-face nil :inherit 'fixed-pitch)
    (set-face-attribute 'markdown-inline-code-face nil :inherit 'fixed-pitch)
    (set-face-attribute 'markdown-metadata-key-face nil :inherit 'fixed-pitch)
    (set-face-attribute 'markdown-metadata-value-face nil :inherit 'fixed-pitch)
    (set-face-attribute 'markdown-language-keyword-face nil :inherit 'fixed-pitch)
    (set-face-attribute 'markdown-list-face nil :inherit 'fixed-pitch)
    (set-face-attribute 'markdown-code-face nil :inherit 'fixed-pitch)
    (set-face-attribute 'markdown-pre-face nil :inherit 'fixed-pitch)
    (set-face-attribute 'markdown-header-delimiter-face nil :inherit 'fixed-pitch)
    (set-face-attribute 'markdown-header-rule-face nil :inherit 'fixed-pitch)
    (set-face-attribute 'markdown-url-face nil :inherit 'fixed-pitch)
    (set-face-attribute 'markdown-header-face-1 nil
                        :inherit 'variable-pitch :weight 'bold :height 180)
    (set-face-attribute 'markdown-header-face-2 nil
                        :inherit 'variable-pitch :weight 'bold :height 160)
    (set-face-attribute 'markdown-header-face-3 nil
                        :inherit 'variable-pitch :weight 'bold :height 145)
    (set-face-attribute 'markdown-header-face-4 nil
                        :inherit 'variable-pitch :weight 'semi-bold :height 130)
    (set-face-attribute 'markdown-header-face-5 nil
                        :inherit 'variable-pitch :weight 'semi-bold :height 120)
    (set-face-attribute 'markdown-header-face-6 nil
                        :inherit 'variable-pitch :weight 'semi-bold :height 110)

    ;; Fallback: Code-Block-Overlays korrekt setzen
    (setq-local line-spacing 0.1)))

(use-package quarto-mode
  :ensure t)



