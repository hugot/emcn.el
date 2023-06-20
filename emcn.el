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
(require 'emcn-store)

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

(defun emcn-note-buffer-name (note-name)
  (concat "[ɘ] *note* " note-name))

(defvar-local emcn-note nil
  "Note that is active in the current buffer")

(defvar-local emcn-note-idle-timer nil
  "Idle timer that saves notes after a certain idle time. Buffer local.")

(defvar-local emcn-note-deleted nil
  "Whether or not the note in the current buffer has been
  deleted from the server after being instructed to do so by the
  user.")

(defvar-local emcn--save (make-mutex)
  "Mutex to hold for async note saving.")

(defun emcn--save-on-idle (buffer)
  (let ((timer))
    (setq timer
          (run-with-idle-timer
           5
           t
           (lambda ()
             (if (not (buffer-live-p buffer))
                 (cancel-timer timer)
               (with-current-buffer buffer
                 (when (and (not emcn-note-deleted) (buffer-modified-p buffer))
                   (emcn-save)
                   (set-buffer-modified-p nil)))))))
    (setq emcn-note-idle-timer timer)))

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
          (setq emcn--save (make-mutex))
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

(defun emcn--set-note-name (name)
  (setq-local emcn-note-name name)
  (rename-buffer (emcn-note-buffer-name name)))

(defun emcn--put-store-if-no-error (store)
  (lambda (err note)
    (if err
        (error err)
      (emcn-store-transact
          (emcn-store-put-note store note)))))

(defun emcn-sync ()
  (interactive)
  (let* ((client (emcn--get-client))
         (store (emcn--get-store))
         (notes (emcn-client-get-notes
                 client '("id" "etag" "modified"))))
    (emcn-store-transact store
      (dolist (json-note notes)
        (let* ((note (emcn-note-from-alist json-note))
               (existing (emcn-store-get-note store (emcn-note-id note))))
          (if existing
              (if (string= (emcn-note-etag note)
                           (emcn-note-etag existing))
                  (when (> (float-time (emcn-note-modified note))
                           (float-time (emcn-note-modified existing)))
                    (emcn-client-update-note
                     client existing (emcn--put-store-if-no-error store) 'sync))
                ;; Etag changed on the remote, if last modified time is later
                ;; locally, we probably want to keep the local version.
                (if (> (float-time (emcn-note-modified note))
                       (float-time (emcn-note-modified existing)))
                       (emcn-client-update-note
                        client existing (emcn--put-store-if-no-error store) 'sync)
                      ;; If not, overwrite the local version
                       (let ((note (emcn-client-get-note client (emcn-note-id note))))
                         (emcn-store-put-note store note))))
            (let ((note (emcn-client-get-note client (emcn-note-id note))))
              (emcn-store-put-note store note))))))

    (maphash (lambda (id note)
               ;; When note-id is 0, that means that it has not been saved to
               ;; the server yet.
               (when (= (emcn-note-id note) 0)
                 (emcn-client-save-note
                  client note (emcn--put-store-if-no-error store) 'sync)))
             (emcn-store-notes-by-local-id store))))

(defun emcn--get-note-alist ()
  (let* ((store (emcn--get-store))
         (notes (emcn-store-notes-by-local-id store))
         (alist))
    (maphash (lambda (id note)
               (push `(,(emcn-note-title note) . ,note)
                     alist))
             notes)
    alist))

(defun emcn-open (note &optional other-window)
  "Open a note."
  (interactive
   (list
    (let* ((notes (emcn--get-note-alist))
           (note-name (completing-read "Open Note: "
                                      (mapcar #'car notes)
                                      nil t)))
      (alist-get note-name notes nil nil #'string=))))
  (let* ((note-name (emcn-note-title note))
         (buffer (get-buffer-create (emcn-note-buffer-name note-name))))

    (if other-window
        (switch-to-buffer-other-window buffer)
      (switch-to-buffer buffer nil t))
    (unless emcn-mode (emcn-mode))
    (erase-buffer)
    (insert (emcn-note-content note))
    (setq-local emcn-note note)
    (emcn--set-note-name note-name)))

(defun emcn-save ()
  (interactive)
  (make-thread
   (lambda ()
     (with-mutex emcn--save
       (unless emcn-note
         (setq-local
          emcn-note
          (emcn--make-note :title (format-time-string "%Y, %b %d %H:%M"))))

       (let ((store (emcn--get-store))
             (content (buffer-string))
             (client (emcn--get-client))
             (local-id)
             (thread (current-thread))
             (note-buffer (current-buffer))
             (await-save (make-condition-variable emcn--save))
             (after-save-cb))
         (setf (emcn-note-content emcn-note) content)
         (emcn-store-transact store
           (emcn-store-update-note store emcn-note))

         (setq local-id (emcn-note-local-id emcn-note))

         (rename-buffer (emcn-note-buffer-name (emcn-note-title emcn-note)))
         (setq after-save-cb
               (lambda (err note)
                 (funcall (emcn--put-store-if-no-error store)
                          err note)
                 (if err
                     (message "[EMCN] Error: %s" err)
                   (with-current-buffer note-buffer
                     ;; Update buffer-local variables
                     (setf (emcn-note-local-id note) local-id)
                     (setq emcn-note note)
                     (setq emcn-note-deleted nil)
                     (message "[EMCN] Note saved to server.")))
                 (thread-signal thread 'saved nil)))

         (if (= (emcn-note-id emcn-note) 0)
             (emcn-client-save-note client emcn-note after-save-cb)
           (emcn-client-update-note client emcn-note after-save-cb))

         ;; Wait for save action to finish, keeping the mutex locked and
         ;; preventing other save threads linked to this buffer from executing.
         (condition-wait await-save))))))

(defun emcn-set-name (name)
  (interactive (list (read-string "Note name: ")))
  (setf (emcn-note-title emcn-note) name)
  (emcn-save))

(defun emcn-delete ()
  "Delete the note opened in the current buffer."
  (interactive)
  (when emcn-note
    (let ((store (emcn--get-store))
          (client (emcn--get-client)))
      (emcn-store-delete-note store emcn-note)
      (unless (= 0 (emcn-note-id emcn-note))
        (emcn-client-delete-note client emcn-note))
      (setq emcn-note nil)
      (setq emcn-note-deleted t)
      (rename-buffer (generate-new-buffer-name "[ɘ] DELETED")))))

(provide 'emcn)
