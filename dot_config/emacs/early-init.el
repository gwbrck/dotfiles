
;;; early-init.el --- early init -*- lexical-binding: t; -*-

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq frame-inhibit-implied-resize t)

(let ((is-mac (eq system-type 'darwin)))
  ;; default-frame-alist idempotent setzen
  (setf (alist-get 'menu-bar-lines       default-frame-alist) (if is-mac 1 0)
        (alist-get 'tool-bar-lines       default-frame-alist) 0
        (alist-get 'vertical-scroll-bars default-frame-alist) nil)
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

;; Pixelgenaues Resizing und Scrolling
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

(when (display-graphic-p)
  (cond
   ((eq system-type 'darwin)
    (let ((mono "SF Mono") (var "SF Pro Text") (mono-size 13) (var-size 13))
      (setf (alist-get 'font default-frame-alist) (format "%s-%d" mono mono-size))
      (set-face-attribute 'default        t :family mono :height (* 10 mono-size))
      (set-face-attribute 'fixed-pitch    t :family mono :height (* 10 mono-size))
      (set-face-attribute 'variable-pitch t :family var  :height (* 10 var-size))))
   ((eq system-type 'gnu/linux)
    (let ((mono "DejaVu Sans Mono") (var "DejaVu Sans") (mono-size 11) (var-size 12))
      (setf (alist-get 'font default-frame-alist) (format "%s-%d" mono mono-size))
      (set-face-attribute 'default        t :family mono :height (* 10 mono-size))
      (set-face-attribute 'fixed-pitch    t :family mono :height (* 10 mono-size))
      (set-face-attribute 'variable-pitch t :family var  :height (* 10 var-size))))))

(setq custom-file (locate-user-emacs-file "custom.el"))
(setq package-enable-at-startup t)
