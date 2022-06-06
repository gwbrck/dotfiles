;;; package --- Summary:
;;; Commentary:
;;; Code:

(defun bitwarden-unlock ()
  "Minimal version of https://github.com/seanfarley/emacs-bitwarden"
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
    (message "successfully logged in.")))

(add-hook 'emacs-startup-hook 'bitwarden-unlock)

(provide 'bitwarden-min)

;;; bitwarden-min.el ends here
