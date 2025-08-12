
;;; early-init.el --- early init -*- lexical-binding: t; -*-

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq frame-inhibit-implied-resize t)

(let ((is-mac (eq system-type 'darwin)))
  (setf (alist-get 'menu-bar-lines       default-frame-alist) (if is-mac 1 0)
        (alist-get 'tool-bar-lines       default-frame-alist) 0
        (alist-get 'vertical-scroll-bars default-frame-alist) nil
        (alist-get 'height               default-frame-alist) 60
        (alist-get 'width                default-frame-alist) 120)
  (setq use-dialog-box nil
        use-file-dialog nil
        ring-bell-function #'ignore
        inhibit-startup-screen t
        inhibit-startup-message t
        inhibit-startup-buffer-menu t
        initial-scratch-message ";; Hi! ❇")
  (unless is-mac (when (fboundp 'menu-bar-mode) (menu-bar-mode -1)))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (when (fboundp 'tooltip-mode)   (tooltip-mode -1)))

(setq frame-resize-pixelwise t
      window-resize-pixelwise t)
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (fboundp 'pixel-scroll-precision-mode)
              (pixel-scroll-precision-mode 1))))

(when (featurep 'native-compile)
  (when (boundp 'native-comp-async-report-warnings-errors)
    (setq native-comp-async-report-warnings-errors 'silent)))
(when (boundp 'comp-async-report-warnings-errors)
  (setq comp-async-report-warnings-errors 'silent))

;; Fonts: default-frame-alist IMMER setzen (auch im Daemon),
(cond
 ((eq system-type 'darwin)
  (setf (alist-get 'font default-frame-alist) "SF Mono-13"))
 ((eq system-type 'gnu/linux)
  (setf (alist-get 'font default-frame-alist) "DejaVu Sans Mono-11")))

;; Direkt anwenden, falls bereits GUI
(when (display-graphic-p)
  (pcase system-type
    ('darwin
     (set-face-attribute 'default        t :family "SF Mono"         :height 130)
     (set-face-attribute 'fixed-pitch    t :family "SF Mono"         :height 130)
     (set-face-attribute 'variable-pitch t :family "SF Pro Text"     :height 130))
    ('gnu/linux
     (set-face-attribute 'default        t :family "DejaVu Sans Mono" :height 110)
     (set-face-attribute 'fixed-pitch    t :family "DejaVu Sans Mono" :height 110)
     (set-face-attribute 'variable-pitch t :family "DejaVu Sans"      :height 120))))

;; Bei neu erstellten GUI-Frames anwenden
(add-hook 'after-make-frame-functions
          (lambda (f)
            (with-selected-frame f
              (when (display-graphic-p f)
                (pcase system-type
                  ('darwin
                   (set-face-attribute 'default        t :family "SF Mono"         :height 130)
                   (set-face-attribute 'fixed-pitch    t :family "SF Mono"         :height 130)
                   (set-face-attribute 'variable-pitch t :family "SF Pro Text"     :height 130))
                  ('gnu/linux
                   (set-face-attribute 'default        t :family "DejaVu Sans Mono" :height 110)
                   (set-face-attribute 'fixed-pitch    t :family "DejaVu Sans Mono" :height 110)
                   (set-face-attribute 'variable-pitch t :family "DejaVu Sans"      :height 120)))))))

(setq custom-file (locate-user-emacs-file "custom.el"))

(setq package-enable-at-startup t
      package-quickstart t)
