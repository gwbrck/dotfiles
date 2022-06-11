;;; package --- Summary:
;;; Commentary:
;;; Code:
(defvar bitwarden--init-prompt-done-p nil)

(defvar bitwarden-after-init-prompt-hooks '()
  "Hooks to run after creating a new frame.  After init.  Once!")

(defun bitwarden-unlock ()
  "Minimal version of https://github.com/seanfarley/emacs-bitwarden."
  (interactive)
  (let* ((bws (shell-command-to-string
               (concat (executable-find "bw") " status")))
         (cmd (cond ((string-match "unauthenticated" bws)  "login")
                    ((string-match "locked" bws)  "unlock")
                    ((string-match "unlocked" bws) nil))))
    (when cmd
      (when (get-process "bitwarden")
        (delete-process "bitwarden"))
      (make-process :name "bitwarden"
                    :buffer nil
                    :connection-type 'pipe
                    :command (list (executable-find "bw") cmd)
                    :filter #'bitwarden--proc-filter))))

(defun bitwarden--proc-filter (proc string)
  "Interacts with PROC by sending line-by-line STRING."
  (when (string-match "^? Email address:" string)
    (process-send-string proc (concat (read-string "Bitwarden email: ") "\n")))
  (when (string-match "^? Master password:" string)
    (process-send-string
     proc (concat (read-passwd "Bitwarden master password: ") "\n")))
  (when (string-match "^Username or password is incorrect" string)
    (message "incorrect master password"))
  (when (string-match "^You are not logged in" string)
    (message "cannot unlock: not logged in"))
  (when (string-match "^? Two-step login code:" string)
    (process-send-string
     proc (concat (read-passwd "Bitwarden two-step login code: ") "\n")))
  (when (string-match "^Login failed" string)
    (message "incorrect two-step code"))
  (when (string-match "^You are already logged in" string)
    (string-match "You are already logged in as \\(.*\\)\\." string)
    (message "already logged in as %s" (match-string 1 string)))
  (when (string-match "^\\(You are logged in\\|Your vault is now unlocked\\)"
                      string)
    (string-match "export BW_SESSION=\"\\(.*\\)\"" string)
    (setenv "BW_SESSION" (match-string 1 string))
    (unless bitwarden--init-prompt-done-p
      (run-hooks 'bitwarden-after-init-prompt-hooks)
      (setq bitwarden--init-prompt-done-p t))
    (message "successfully logged in.")))

(defun bitwarden--init-prompt-run ()
    (unless bitwarden--init-prompt-done-p
      (bitwarden-unlock)))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook 'bitwarden--init-prompt-run 90)
  (add-hook 'after-init-hook 'bitwarden--init-prompt-run 90))
  
(provide 'bitwarden-min)

;;; bitwarden-min.el ends here

