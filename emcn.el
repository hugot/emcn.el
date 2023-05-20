; emcn.el --- Nextcloud Notes Client for Emacs  -*- lexical-binding: t; -*-

;; Author: Hugo Thunnissen <devel@hugot.nl>
;; Keywords: notes, nextcloud, tools, convenience
;; Version: 0

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

;;; Code:

(require 'json)
(require 'emcn-client)
(require 'emcn-view)

(defcustom emcn-enable-drafts t
  "Whether or not EMCN should save notes automatically on idle."
  :type 'boolean
  :group 'emcn)

(defcustom emcn-draft-idle-time 5
  "Seconds of idle time before EMCN saves drafts when `emcn-enable-drafts' is enabled"
  :type 'integer
  :group 'emcn)

(defcustom emcn-category "EMCN Notes"
  "The category that EMCN should use to store notes under"
  :type 'string
  :group 'emcn)

(defcustom emcn-host nil
  "The nextcloud host that EMCN should use to store notes
  to. Notes are stored in the nextcloud notes app."
  :type 'string
  :group 'emcn)

(defun emcn--note-buffer-name (note-name)
  (concat "[ɘ] *note* " note-name))

(defvar-local emcn-note-id nil
  "The id of the note in the current buffer")

(defvar-local emcn-note-name nil
  "The name of the note in the current buffer")

(define-minor-mode emcn-mode
  "Minor mode to use Nextcloud Notes from Emacs"
  :lighter "EMCN"
  :keymap `((,(kbd "C-x 3") . emcn--split-window-right)
            (,(kbd "C-x C-s") . emcn-save))
  (if emcn-mode
      (if (and (require 'markdown-mode nil t)
               (not (eq major-mode 'markdown-mode)))
          (progn
            (markdown-mode)
            (when (not emcn-mode)
              (emcn-mode)))
        (progn
          (add-hook (make-local-variable 'window-configuration-change-hook)
                    'emcn--window-configuration-change-hook)
          (setq-local word-wrap t)
          (dolist (window (get-buffer-window-list (current-buffer)))
            (emcn-style-window window))
          (if (require 'emojify nil t) (emojify-mode 1))
          (when emcn-enable-drafts (emcn--save-on-idle (current-buffer)))))
    (progn
      (kill-local-variable 'window-configuration-change-hook)
      (kill-local-variable 'word-wrap)
      (dolist (window (get-buffer-window-list (current-buffer)))
        (set-window-margins window 0 0))
      (when emcn-note-idle-timer (cancel-timer emcn-note-idle-timer)))))


;;;###autoload
(defun emcn ()
  "Open a new note buffer (create a new note)."
  (interactive)
  (switch-to-buffer (generate-new-buffer "Note"))
  (emcn-mode 1))

(defun emcn-open (note)
  "Open a note."
  (interactive
   (list
    (let* ((notes (emcn--get-note-alist))
           (note-name (completing-read "Open Note: "
                                      (mapcar #'car notes)
                                      nil t)))
      (alist-get note-name notes nil nil #'string=))))
  (let* ((note-name (alist-get 'title note))
         (buffer (get-buffer-create (emcn--note-buffer-name note-name))))

    (switch-to-buffer buffer nil t)
    (unless emcn-mode (emcn-mode))
    (erase-buffer)
    (insert (alist-get 'content note))
    (setq-local emcn-note-id (alist-get 'id note))
    (emcn--set-note-name note-name)))

(defun emcn-save ()
  (interactive)
  (let ((client (emcn--get-client))
        (content (buffer-string)))
    (if emcn-note-id
        (emcn--client-update-note client
                                   emcn-note-id
                                   emcn-note-name
                                   content)
      (emcn--client-save-note client
                               (format-time-string "%Y, %b %d %H:%M")
                               content))
  (setq emcn-note-deleted nil)))

(defun emcn-set-name (name)
  (interactive (list (read-string "Note name: ")))
  (setq emcn-note-name name)
  (emcn-save))

(defun emcn-delete ()
  "Delete the note opened in the current buffer."
  (interactive)
  (when emcn-note-id
    (let ((client (emcn--get-client)))
      (emcn--client-delete-note client emcn-note-id)
      (setq emcn-note-id nil)
      (setq emcn-note-name nil)
      (setq emcn-note-deleted t)
      (rename-buffer (generate-new-buffer-name "[ɘ] DELETED")))))

(provide 'emcn)
