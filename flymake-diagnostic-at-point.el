;;; flymake-diagnostic-at-point.el --- Display flymake diagnostics at point  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Ricardo Martins

;; Author: Ricardo Martins <ricardo@scarybox.net>
;; URL: https://github.com/meqif/flymake-diagnostic-at-point
;; Keywords: convenience, languages, tools
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (posframe "0.4.1") (lv "0.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Display flymake diagnostics at point in a convenient way.
;;
;; The display function can be customized through the variable
;; `flymake-diagnostic-at-point-display-diagnostic-function' to show the
;; diagnostics in a popup, in the minibuffer, or in some other way.
;;
;; Check out the README for more information.

;;; Code:

(require 'flymake)
(require 'posframe)
(require 'lv)

(defcustom flymake-diagnostic-at-point-timer-delay 0.5
  "Delay in seconds before displaying errors at point."
  :group 'flymake-diagnostic-at-point
  :type 'number
  :safe #'numberp)

(defcustom flymake-diagnostic-at-point-error-prefix "➤ "
  "String to be displayed before every error line."
  :group 'flymake-diagnostic-at-point
  :type '(choice (const :tag "No prefix" nil)
                 string))

(defcustom flymake-diagnostic-at-point-display-diagnostic-function
  'flymake-diagnostic-at-point-display-popup
  "The function to be used to display the diagnostic message."
  :group 'flymake-diagnostic-at-point
  :type '(choice (const :tag "Display error messages in a popup"
                        flymake-diagnostic-at-point-display-popup)
                 (const :tag "Display error messages in the minibuffer"
                        flymake-diagnostic-at-point-display-minibuffer)
                 (function :tag "Error display function")))

(defvar-local flymake-diagnostic-at-point-timer nil
  "Timer to automatically show the error at point.")

(defvar-local flymake-diagnostic-at-point-posframe-buffer
  " *flymake-diagnostic-at-point-posframe-buffer*")

(defun flymake-diagnostic-at-point-get-diagnostic-text ()
  "Get the flymake diagnostic text for the thing at point."
  (flymake--diag-text (get-char-property (point) 'flymake-diagnostic)))

(defun flymake-diagnostic-at-point-display-popup (text)
  "Display the flymake diagnostic TEXT inside a popup."
  (with-current-buffer
      (get-buffer-create flymake-diagnostic-at-point-posframe-buffer)
    (erase-buffer))
  (posframe-show flymake-diagnostic-at-point-posframe-buffer
                 :string (concat flymake-diagnostic-at-point-error-prefix text)
                 :background-color (face-background 'popup-face)
                 :foreground-color (face-foreground 'popup-face)
                 :position (point))
  (add-hook 'pre-command-hook #'flymake-diagnostic-at-point-delete-popup nil t))

(defun flymake-diagnostic-at-point-display-minibuffer (text)
  "Display the flymake diagnostic TEXT persistently in the minibuffer."
  (lv-message (concat flymake-diagnostic-at-point-error-prefix text))
  (add-hook 'pre-command-hook #'lv-delete-window))

(defun flymake-diagnostic-at-point-maybe-display ()
  "Display the flymake diagnostic text for the thing at point.

The diagnostic text will be rendered using the function defined
in `flymake-diagnostic-at-point-display-diagnostic-function.'"
  (when (and flymake-mode
             (get-char-property (point) 'flymake-diagnostic))
    (let ((text (flymake-diagnostic-at-point-get-diagnostic-text)))
      (funcall flymake-diagnostic-at-point-display-diagnostic-function text))))

(defun flymake-diagnostic-at-point-delete-popup ()
  "Delete the popup with the diagnostic information."
  (posframe-delete-frame flymake-diagnostic-at-point-posframe-buffer))

;;;###autoload
(defun flymake-diagnostic-at-point-set-timer ()
  "Set the error display timer for the current buffer."
  (interactive)
  (flymake-diagnostic-at-point-cancel-timer)
  (unless flymake-diagnostic-at-point-timer
    (setq flymake-diagnostic-at-point-timer
          (run-with-idle-timer flymake-diagnostic-at-point-timer-delay
                               nil
                               #'flymake-diagnostic-at-point-maybe-display))))

;;;###autoload
(defun flymake-diagnostic-at-point-cancel-timer ()
  "Cancel the error display timer for the current buffer."
  (interactive)
  (let ((inhibit-quit t))
    (when flymake-diagnostic-at-point-timer
      (cancel-timer flymake-diagnostic-at-point-timer)
      (setq flymake-diagnostic-at-point-timer nil))))

(defun flymake-diagnostic-at-point-handle-focus-change ()
  "Set or cancel flymake message display timer after the frame focus changes."
  (if (frame-focus-state)
      (flymake-diagnostic-at-point-set-timer)
    (flymake-diagnostic-at-point-cancel-timer)))

(defun flymake-diagnostic-at-point-setup ()
  "Setup the hooks for `flymake-diagnostic-at-point-mode'."
  (add-hook 'post-command-hook #'flymake-diagnostic-at-point-set-timer nil 'local)
  (if (version< emacs-version "27.0")
      (progn
        (add-hook 'focus-out-hook
                  #'flymake-diagnostic-at-point-cancel-timer nil 'local)
        (add-hook 'focus-in-hook
                  #'flymake-diagnostic-at-point-set-timer nil 'local))
    (add-function :after
                  (local 'after-focus-change-function)
                  #'flymake-diagnostic-at-point-handle-focus-change)))

(defun flymake-diagnostic-at-point-teardown ()
  "Remove the hooks for `flymake-diagnostic-at-point-mode'."
  (remove-hook 'post-command-hook #'flymake-diagnostic-at-point-set-timer 'local)
  (if (version< emacs-version "27.0")
      (progn
        (remove-hook 'focus-out-hook
                     #'flymake-diagnostic-at-point-cancel-timer 'local)
        (remove-hook 'focus-in-hook
                     #'flymake-diagnostic-at-point-set-timer 'local))
    (remove-function 'after-focus-change-function
                     #'flymake-diagnostic-at-point-handle-focus-change)))

(define-minor-mode flymake-diagnostic-at-point-mode
  "Minor mode for displaying flymake diagnostics at point."
  :lighter nil
  :group flymake-diagnostic-at-point
  (cond
   (flymake-diagnostic-at-point-mode
    (flymake-diagnostic-at-point-setup))
   (t
    (flymake-diagnostic-at-point-teardown))))

(provide 'flymake-diagnostic-at-point)
;;; flymake-diagnostic-at-point.el ends here
