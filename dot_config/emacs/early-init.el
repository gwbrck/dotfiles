;;; early-init.el --- early init -*- lexical-binding: t; -*-

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(when (featurep 'native-compile)
  (when (boundp 'native-comp-async-report-warnings-errors)
    (setq native-comp-async-report-warnings-errors 'silent)))
(when (boundp 'comp-async-report-warnings-errors)
  (setq comp-async-report-warnings-errors 'silent))

;;;; UI Configuration

;; The `default-frame-alist` is the canonical way to set initial frame properties.
;; This single setting handles the initial frame and all subsequent frames.
(let ((font-spec (if (eq system-type 'darwin)
                     "JuliaMono-13"      ;; Font and size for macOS
                   "DejaVu Sans Mono-11"))) ;; Font and size for Linux
  (add-to-list 'default-frame-alist `(font . ,font-spec))
  (add-to-list 'default-frame-alist '(menu-bar-lines . 0))
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
  (add-to-list 'default-frame-alist '(height . 80))
  (add-to-list 'default-frame-alist '(width . 140)))


;; Pixel-based resizing and scrolling for smoother experience
(add-hook 'emacs-startup-hook 'pixel-scroll-precision-mode)

(setq frame-resize-pixelwise t
      window-resize-pixelwise t
      frame-inhibit-implied-resize t
      initial-scratch-message ";; Hi! ✨"
      inhibit-startup-screen t
      visible-bell t
      use-short-answers t)

;;;; Package System

(setq package-enable-at-startup t
      package-quickstart nil
      package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/"))
      package-archive-priorities '(("melpa" . 3) ("gnu-elpa" . 2) ("nongnu" . 1)))

;;;; Custom File
;; Set the custom-file location early.
(setq custom-file (locate-user-emacs-file "custom.el"))

(setq default-directory "~/")

;; Backups
(let ((backup-dir (expand-file-name "~/.cache/emacs/backup/")))
  (make-directory backup-dir t)
  (setq backup-directory-alist `(("." . ,backup-dir))
        backup-by-copying t
        version-control t
        delete-old-versions t
        kept-new-versions 20
        kept-old-versions 5))
