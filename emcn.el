;; -*- lexical-binding: t; -*-

(require 'json)

(defvar emcn-token-alist '()
  "Alist containing oauth tokens for note instances. The car
  of each element should be the hostname of an instance, and the
  cdr should be the access token for that instance.")

(defvar emcn-note-width 80 "Character width of a note")

(defvar emcn-enable-drafts nil
  "Whether or not EMCN should save drafts automatically on idle.")

(defvar emcn-category "EMCN Notes"
  "The category that EMCN should use to store notes under")

(defvar emcn-host nil
  "The nextcloud host that EMCN should use to store drafts
  to. Drafts are stored in the nextcloud notes app.")

(defvar-local emcn-note-idle-timer nil
  "Idle timer that saves drafts after a certain idle time. Buffer local.")

(defvar-local emcn-note-deleted nil
  "Whether or not the draft in the current buffer has been
  deleted from the server after being instructed to do so by the
  user.")

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
                   (message "saving %s with name %s and id %s" buffer emcn-note-name emcn-note-id)
                   (emcn-save)
                   (set-buffer-modified-p nil)))))))
    (setq emcn-note-idle-timer timer)))

(define-minor-mode emcn-mode "Minor mode to edit toots"
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

(defun emcn--collapse-margins (window)
  (set-window-margins window 0 0))

(defun emcn--split-window-right (&optional size)
  "Wraps `split-window-right` to still make it work with large
buffer margins"
  (interactive "P")
  (emcn--collapse-margins (selected-window))
  (split-window-right size))

(defun emcn--window-configuration-change-hook ()
  "Make sure the window keeps its styling when window sizes etc. change around"
  (if emcn-mode
      (emcn-style-window (selected-window))))

(defun emcn--get-viewport-width (window)
  "Get body width + margin widths"
  (let ((margins (window-margins window)))
    (+ (window-body-width window)
       (or (car margins) 0)
       (or (cdr margins) 0))))

(defun emcn--adjust-margins (window)
  "Adjust margins so that the window body is only as wide as
`emcn-note-width`"
  (let* ((width (emcn--get-viewport-width window))
         (margin (floor (/ (- width emcn-note-width) 2))))
    (set-window-margins window margin margin)))

(defun emcn-style-window (window)
  (emcn--adjust-margins window))

(defun emcn--secret-name (name)
  "Add prefix to make secret name recognizable"
  (concat "emcn-" name))

(defun emcn--get-token (mastodon-host)
  "Check emcn-token-alist for a matching entry, otherwise
  return the token stored using the secrets API"
  (or (alist-get mastodon-host emcn-token-alist nil nil 'string=)
      (secrets-get-secret "default" (emcn--secret-name mastodon-host))))

(defun emcn--save-token (host token)
  "Save oauth token with auth-source format in file"
  (secrets-create-item "default" (emcn--secret-name host) token))

(defun emcn--save-token? (token)
  (yes-or-no-p
   (format (concat "Your access token is \"%s\", would you like to save this to your keyring?\n"
                   "Note: this depends on there being a keyring daemon available on your system.\n"
                   "If you are logged in on a headless server you will likely not have one available.\n"
                   "In that case pick no, the token will then be added to your kill-ring so you can set\n"
                   "`emcn-token-alist` with it in your config. Use  C-h v emcn-token-alist\n"
                   "for more information about this variable\n")
           token)))

(defsubst emcn--json-serialize-utf8 (json)
  "Serialize a json object and encode the resulting string to UTF-8."
  (encode-coding-string (emcn--json-preset (json-serialize json))
                        'utf-8 t))

(defmacro emcn--json-preset (&rest body)
  "Define JSON preset to use when marshalling/unmarshalling json"

  `(let ((json-array-type 'list)
         (json-object-type 'alist)
         (json-key-type 'symbol))
     ,@body))

;;;###autoload
(defun emcn ()
  (interactive)
  (switch-to-buffer (generate-new-buffer "Note"))
  (emcn-mode 1))

(defun emcn--request-drafts-password ()
  (let* ((host (emcn--get-host))
         (username (read-string (format "Username for nextcloud instance (%s): "
                                        host)))
         (password (read-string (format "Password for nextcloud instance (%s): "
                                       host))))
    (when (and username password)
      (let ((token (base64-encode-string (concat username ":" password))))
        (when (emcn--save-token? token)
          (emcn--save-token host token))
        token))))

(defun emcn--get-password ()
  (or (alist-get (emcn--get-host) emcn-token-alist nil nil #'string=)
      (emcn--get-token (emcn--get-host))
      (emcn--request-drafts-password)))

(defun emcn--request-host ()
  (let ((drafts-host (read-string
                      (concat "Drafts can be saved to a nextcloud instance, "
                              "but I'd  need to know where your instance is! "
                              "please provide your nextcloud hostname: "))))
    (when drafts-host
      (setq emcn-host drafts-host)
      (customize-set-variable 'emcn-host drafts-host)
      (customize-save-variable 'emcn-host drafts-host))
    drafts-host))

(defun emcn-set-drafts-host ()
  (interactive)
  (emcn--request-host)
  (emcn--request-drafts-password))

(defun emcn--get-host ()
  (if emcn-host
      emcn-host
    (emcn--request-host)))

(defun emcn--get-client ()
  (let ((password (emcn--get-password))
        (host (emcn--get-host)))
    (emcn--make-client :password password
                        :host host)))


(cl-defstruct (emcn--client
               (:constructor emcn--make-client))
  "Nextcloud notes client. Can be used to communicate with a
nextcloud instance that has the notes app installed."
  (password nil
            :type string
            :documentation
            "Password to use to authenticate to nextcloud")
  (host nil
        :type string
        :documentation
        "Hostname that the nextcloud instance can be reached at"))

(cl-defmethod emcn--client-endpoint ((client emcn--client) route)
  (concat "https://" (emcn--client-host client) route))

(cl-defmethod emcn--client-url-headers ((client emcn--client))
  `(("Content-Type" . "application/json")
    ("Authorization" . ,(concat "Basic " (emcn--client-password client)))))

(cl-defmethod emcn--client-get-drafts ((client emcn--client))
  (let* ((url-request-extra-headers (emcn--client-url-headers client))
         (response (url-retrieve-synchronously
                    (emcn--client-endpoint
                     client
                     (concat "/apps/notes/api/v1/notes?"
                             (url-build-query-string `((category ,emcn-category))))))))
    (with-current-buffer (car (with-current-buffer response (mm-dissect-buffer t)))
      (goto-char (point-min))
      (condition-case err
          (emcn--json-preset (json-read))
        (json-error (message "Error parsing json from buffer: %s" (buffer-string)))))))

(defun emcn--set-note-name (name)
  (setq-local emcn-note-name name)
  (rename-buffer (emcn--note-buffer-name name)))

(defun emcn--note-buffer-name (draft-name)
  (concat "[ɘ] *note* " draft-name))

(defun emcn--handle-draft-saved (draft-name save-or-update)
  (let ((toot-buffer (current-buffer)))
    (lambda (status &rest args)
      (let ((status-code (url-http-parse-response)))
        (with-current-buffer (car (mm-dissect-buffer t))
          (goto-char (point-min))
          (condition-case err
              (progn
                (when (eq save-or-update 'save)
                  (let ((json (emcn--json-preset (json-read))))
                    (with-current-buffer toot-buffer
                      (setq emcn-note-id (alist-get 'id json)))))

                (if (not (or (= 201 status-code) (= 200 status-code)))
                    (message "Something went wrong while saving draft. Status code: %s" status-code)
                  (progn (with-current-buffer toot-buffer (emcn--set-note-name draft-name))
                         (message "[EMCN] Note saved."))))
            (json-error (message "[EMCN] Error parsing json from buffer: %s" (buffer-string)))))))))

(cl-defmethod emcn--client-save-draft ((client emcn--client) draft-name content)
  (let* ((url-request-extra-headers (emcn--client-url-headers client))
         (url-request-method "POST")
         (url-request-data (emcn--json-serialize-utf8
                             `((title . ,draft-name)
                               (content . ,content)
                               (category . ,emcn-category)))))
    (url-retrieve (emcn--client-endpoint client "/apps/notes/api/v1/notes")
                  (emcn--handle-draft-saved draft-name 'save)
                  nil t)))

(cl-defmethod emcn--client-update-draft ((client emcn--client) draft-id draft-name content)
  (message "From update, draft name: %s, draft id: %s" draft-id draft-name)
  (let* ((url-request-extra-headers (emcn--client-url-headers client))
         (url-request-method "PUT")
         (url-request-data (emcn--json-serialize-utf8
                             `((id . ,draft-id)
                               (title . ,draft-name)
                               (category . ,emcn-category)
                               (content . ,content)))))
    (url-retrieve (emcn--client-endpoint client (format "/apps/notes/api/v1/notes/%d" draft-id))
                  (emcn--handle-draft-saved draft-name 'update)
                  nil t)))

(cl-defmethod emcn--client-delete-draft ((client emcn--client) draft-id)
  (let* ((url-request-extra-headers (emcn--client-url-headers client))
         (url-request-method "DELETE"))
    (url-retrieve-synchronously
     (emcn--client-endpoint client (format "/apps/notes/api/v1/notes/%d" draft-id))
     t)))


(defvar-local emcn-note-id nil
  "The id of the draft in the current buffer")

(defvar-local emcn-note-name nil
  "The name of the draft in the current buffer")

(defun emcn-save ()
  (interactive)
  (let ((client (emcn--get-client))
        (content (buffer-string)))
    (if emcn-note-id
        (emcn--client-update-draft client
                                   emcn-note-id
                                   emcn-note-name
                                   content)
      (emcn--client-save-draft client
                               (format-time-string "%Y, %b %d %H:%M")
                               content))
  (setq emcn-note-deleted nil)))

(defun emcn-set-name (name)
  (interactive (list (read-string "Draft name: ")))
  (setq emcn-note-name name)
  (emcn-save))

(defun emcn--get-note-alist ()
  (let* ((client (emcn--get-client))
         (drafts (emcn--client-get-drafts client))
         (alist))
    (dolist (draft drafts)
      (push `(,(alist-get 'title draft) . ,draft)
            alist))

    alist))

(defun emcn-open ()
  (interactive)
  (let* ((drafts (emcn--get-note-alist))
         (draft-name (completing-read "Open Note: "
                                      (mapcar #'car drafts)
                                      nil t))
         (draft (alist-get draft-name drafts nil nil #'string=))
         (buffer (get-buffer-create (emcn--note-buffer-name draft-name))))

    (switch-to-buffer buffer nil t)
    (unless emcn-mode (emcn-mode))
    (erase-buffer)
    (insert (alist-get 'content draft))
    (setq-local emcn-note-id (alist-get 'id draft))
    (emcn--set-note-name draft-name)))

(defun emcn-delete ()
  (interactive)
  (when emcn-note-id
    (let ((client (emcn--get-client)))
      (emcn--client-delete-draft client emcn-note-id)
      (setq emcn-note-id nil)
      (setq emcn-note-name nil)
      (setq emcn-note-deleted t)
      (rename-buffer (generate-new-buffer-name "[ɘ] DELETED")))))


(provide 'emcn)
