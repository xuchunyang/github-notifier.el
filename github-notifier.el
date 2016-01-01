;;; github-notifier.el --- Displays your GitHub notifications unread count in mode-line  -*- lexical-binding: t; -*-

;; Copyright (C) 2015, 2016  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; URL: https://github.com/xuchunyang/github-notifier.el
;; Package-Requires: ((emacs "24"))
;; Keywords: github, mode-line
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a global minor-mode. Turn it on everywhere with:
;;
;;   M-x github-notifier-mode

;;; Code:

(require 'url)
(require 'json)

(defgroup github-notifier nil
  "Github Notifier"
  :group 'emacs)


;;; Custom
(defcustom github-notifier-token nil
  "Access token to get Github Notifications.

To generate an access token, visit
URL `https://github.com/settings/tokens/new?scopes=notifications&description=github-notifier.el'

This is similar to how erc or jabber handle authentication in
emacs, but the following disclaimer always worth reminding.

DISCLAIMER
When you save this variable, DON'T WRITE IT ANYWHERE PUBLIC. This
token grants (very) limited access to your account.
END DISCLAIMER

If nil, Github-Notifier will ask you and remember your token via
`customize-save-variable'."
  :type '(choice (string :tag "Token")
                 (const :tag "Ask me" nil))
  :group 'github-notifier)

(defcustom github-notifier-mode-line
  '(:eval
    (let (unread-text help-text)
      (cond ((null github-notifier-unread-count)
             (setq unread-text "-?"
                   help-text "The Github notifications number is unknown."))
            ((zerop github-notifier-unread-count)
             (setq unread-text ""
                   help-text "Good job, you don't have unread notification."))
            (t
             (setq unread-text (format "-%d" github-notifier-unread-count)
                   help-text (if (= github-notifier-unread-count 1)
                                 "You have 1 unread notification.\nmouse-1 Read it on Github."
                               (format "You have %d unread notifications.\nmouse-1 Read them on Github."
                                       github-notifier-unread-count)))))
      (propertize (concat " GH" unread-text)
                  'help-echo help-text
                  'local-map github-notifier-mode-line-map
                  'mouse-face 'mode-line-highlight)))
  "Mode line lighter for Github Notifier."
  :type 'sexp
  :risky t
  :group 'github-notifier)

(defcustom github-notifier-update-interval 60
  "Seconds after which the github notifications count will be updated."
  :type 'integer
  :group 'github-notifier)

(defcustom github-notifier-only-participating nil
  "If non-nil, only counts notifications in which the user is directly participating or mentioned."
  :type 'boolean
  :group 'github-notifier)


;;; Variables
(defvar github-notifier-unread-count nil
  "Github notifications unread count.
Normally, this is a number, however, nil means unknown by Emacs.")

(defvar github-notifier-unread-json nil
  "JSON object contains latest (to github-notifier) unread notifications.")

(defvar github-notifier-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'github-notifier-visit-github)
    map))

(defvar github-notifier-update-timer nil)


;;; Function

;; FIXME: Even we use `url-retrieve' to retrieve network asynchronously, Emacs
;; still gets blocked frequently (?), especially when the network situation is
;; bad, once it blocks Emacs, you have to wait to it gets finised or interrupt
;; it by hitting C-g many times. This is very annoying.
;;
;; Maybe we can try to invoke curl(1) as asynchronous process.
(defun github-notifier-update-cb (_status)
  (set-buffer-multibyte t)
  (goto-char (point-min))
  (if (not (string-match "200 OK" (buffer-string)))
      (progn (message "[github-notifier] Problem connecting to the server")
             (setq github-notifier-unread-count nil))
    (re-search-forward "^$" nil 'move)
    (let (json-str (old-count github-notifier-unread-count))
      (setq json-str (buffer-substring-no-properties (point) (point-max))
            github-notifier-unread-json (json-read-from-string json-str))
      (setq github-notifier-unread-count (length github-notifier-unread-json))
      (unless (equal old-count github-notifier-unread-count)
        (force-mode-line-update t))
      ;; Debug
      ;; (setq a-json-string json-str)
      ;; (message "Github notification %d unread, updated at %s"
      ;;          github-notifier-unread-count (current-time-string))
      ))
  ;; Debug
  ;; (display-buffer (current-buffer))
  (kill-buffer)
  (when github-notifier-mode
    (setq github-notifier-update-timer
          (run-at-time github-notifier-update-interval nil #'github-notifier-update))))

(defun github-notifier-update (&optional force)
  "Update `github-notifier-unread-count'."
  (when (or force github-notifier-mode)
    (let ((url-request-extra-headers `(("Authorization" .
                                        ,(format "token %s" github-notifier-token))))
          (url (concat "https://api.github.com/notifications"
                       (when github-notifier-only-participating
                         "?participating=true"))))
      (url-retrieve url
                    #'github-notifier-update-cb
                    nil t t))))

(defun github-notifier-visit-github ()
  (interactive)
  (browse-url "https://github.com/notifications"))


;;; Glboal Minor-mode

;;;###autoload
(define-minor-mode github-notifier-mode
  "Toggle github notifications count display in mode line (Github Notifier mode).
With a prefix argument ARG, enable Github Notifier mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  :global t :group 'github-notifier
  (unless (stringp github-notifier-token)
    (browse-url "https://github.com/settings/tokens/new?scopes=notifications&description=github-notifier.el")
    (let (token)
      (unwind-protect
          (setq token (read-string "Paste Your Access Token: "))
        (if (stringp token)
            (customize-save-variable 'github-notifier-token token)
          (message "No Access Token")
          (setq github-notifier-mode nil)))))
  (unless global-mode-string
    (setq global-mode-string '("")))
  (if (not github-notifier-mode)
      (progn
        (setq global-mode-string
              (delq 'github-notifier-mode-line global-mode-string))
        (when github-notifier-update-timer
          (cancel-timer github-notifier-update-timer)
          (setq github-notifier-update-timer nil)))
    (add-to-list 'global-mode-string 'github-notifier-mode-line t)
    (github-notifier-update)))

(provide 'github-notifier)
;;; github-notifier.el ends here
