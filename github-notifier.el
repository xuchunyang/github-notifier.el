;;; github-notifier.el --- Displays your GitHub notifications unread count in mode-line  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; Keywords: github, mode-line

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

;;

;;; Code:

(require 'json)

(defvar github-notifier-unread-count 0)

(defvar github-notifier-token nil)

(defgroup github-notifier nil
  "Github Notifier"
  :group 'emacs)

(defun github-notifier-update ()
  "Update `github-notifier-unread-count'."
  (let ((url-request-extra-headers `(("Authorization" .
                                      ,(format "token %s" github-notifier-token))))
        json-str)
    (with-current-buffer (url-retrieve-synchronously "https://api.github.com/notifications")
      (goto-char (point-min))
      (when (not (string-match "200 OK" (buffer-string)))
        (error "Problem connecting to the server"))
      (re-search-forward "^$" nil 'move)
      (setq json-str (buffer-substring-no-properties (point) (point-max)))
      ;; Debug
      ;; (progn (setq jj json-str) (l "%s" jj))
      (kill-buffer))
    (setq github-notifier-unread-count (length (json-read-from-string json-str)))))

(defcustom github-notifier-mode-line
  '(:eval (format " -%d" github-notifier-unread-count))
  "Mode line lighter for Github Notifier."
  :type 'sexp
  :risky t
  :group 'github-notifier)

(defcustom github-notifier-update-interval 60
  "Seconds after which the github notifications count will be updated."
  :type 'integer
  :group 'github-notifier)

(defvar github-notifier-mode-line-string nil
  "String to display in the mode line.")
(put 'github-notifier-mode-line-string 'risky-local-variable t)

(defvar github-notifier-update-timer nil
  "Interval timer object.")

(defun github-notifier-update-handler ()
  (github-notifier-update)
  (sit-for 0))

;;;###autoload
(define-minor-mode github-notifier-mode
  "Toggle github notifications count display in mode line (Github Notifier mode).
With a prefix argument ARG, enable Github Notifier mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  :global t :group 'github-notifier
  (setq github-notifier-mode-line-string "")
  (unless global-mode-string
    (setq global-mode-string '("")))
  (when github-notifier-update-timer (cancel-timer github-notifier-update-timer))
  (if (not github-notifier-mode)
      (setq global-mode-string
            (delq 'github-notifier-mode-line-string global-mode-string))
    (add-to-list 'global-mode-string 'github-notifier-mode-line-string t)
    (setq github-notifier-update-timer (run-at-time nil github-notifier-update-interval
                                                    'github-notifier-update-handler))
    (github-notifier-update)
    (setq github-notifier-mode-line-string
          github-notifier-mode-line)))

(provide 'github-notifier)
;;; github-notifier.el ends here
