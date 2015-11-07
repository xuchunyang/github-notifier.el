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

(defvar github-notifier-unread-count nil
  "Github notifications unread count.
Normally, this is a number, however, nil means unknown by Emacs.")

(defvar github-notifier-token nil)

(defgroup github-notifier nil
  "Github Notifier"
  :group 'emacs)

(defun github-notifier-update-cb (_status)
  (set-buffer-multibyte t)
  (goto-char (point-min))
  (if (not (string-match "200 OK" (buffer-string)))
      (progn (message "Problem connecting to the server")
             (setq github-notifier-unread-count nil))
    (re-search-forward "^$" nil 'move)
    (let (json-str (old-count github-notifier-unread-count))
      (setq json-str (buffer-substring-no-properties (point) (point-max)))
      (setq github-notifier-unread-count (length (json-read-from-string json-str)))
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
  (run-at-time github-notifier-update-interval nil #'github-notifier-update))

(defun github-notifier-update (&optional force)
  "Update `github-notifier-unread-count'."
  (when (or force github-notifier-mode)
    (let ((url-request-extra-headers `(("Authorization" .
                                        ,(format "token %s" github-notifier-token)))))
      (url-retrieve "https://api.github.com/notifications"
                    #'github-notifier-update-cb
                    nil t t))))

;;; TODO: Add keymap to open https://github.com/notifications
(defcustom github-notifier-mode-line
  '(:eval (concat " GH"
                  (cond ((null github-notifier-unread-count) "-?")
                        ((zerop github-notifier-unread-count) "")
                        (t (format "-%d" github-notifier-unread-count)))
                  " "))
  "Mode line lighter for Github Notifier."
  :type 'sexp
  :group 'github-notifier)

(defcustom github-notifier-update-interval 60
  "Seconds after which the github notifications count will be updated."
  :type 'integer
  :group 'github-notifier)

(defvar github-notifier-mode-line-string nil
  "String to display in the mode line.")
(put 'github-notifier-mode-line-string 'risky-local-variable t)

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
  (if (not github-notifier-mode)
      (setq global-mode-string
            (delq 'github-notifier-mode-line-string global-mode-string))
    (add-to-list 'global-mode-string 'github-notifier-mode-line-string t)
    (github-notifier-update)
    (setq github-notifier-mode-line-string
          github-notifier-mode-line)))

(provide 'github-notifier)
;;; github-notifier.el ends here
