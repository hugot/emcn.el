;; emcn-client.el --- Nextcloud Notes Client for Emacs  -*- lexical-binding: t; -*-

(defvar emcn-token-alist '()
  "Alist containing authentication tokens for note instances. The
  car of each element should be the hostname of an instance, and
  the cdr should be the access token for that instance.")


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

(cl-defmethod emcn--client-get-notes ((client emcn--client))
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

(defun emcn--handle-note-saved (note-name save-or-update)
  (let ((note-buffer (current-buffer)))
    (lambda (status &rest args)
      (let ((status-code (url-http-parse-response)))
        (with-current-buffer (car (mm-dissect-buffer t))
          (goto-char (point-min))
          (condition-case err
              (progn
                (when (eq save-or-update 'save)
                  (let ((json (emcn--json-preset (json-read))))
                    (with-current-buffer note-buffer
                      (setq emcn-note-id (alist-get 'id json)))))

                (if (not (or (= 201 status-code) (= 200 status-code)))
                    (let ((err-buffer (generate-new-buffer "emcn-error"))
                          (resp-contents (buffer-string)))
                      (message "Something went wrong while saving note. Status code: %s, see buffer %s"
                               status-code err-buffer)
                      (with-current-buffer err-buffer (insert resp-contents)))
                  (progn (with-current-buffer note-buffer (emcn--set-note-name note-name))
                         (message "[EMCN] Note saved."))))
            (json-error (message "[EMCN] Error parsing json from buffer: %s" (buffer-string)))))))))

(cl-defmethod emcn--client-save-note ((client emcn--client) note-name content)
  (let* ((url-request-extra-headers (emcn--client-url-headers client))
         (url-request-method "POST")
         (url-request-data (emcn--json-serialize-utf8
                             `((title . ,note-name)
                               (content . ,content)
                               (category . ,emcn-category)))))
    (url-retrieve (emcn--client-endpoint client "/apps/notes/api/v1/notes")
                  (emcn--handle-note-saved note-name 'save)
                  nil t)))

(cl-defmethod emcn--client-update-note ((client emcn--client) note-id note-name content)
  (let* ((url-request-extra-headers (emcn--client-url-headers client))
         (url-request-method "PUT")
         (url-request-data (emcn--json-serialize-utf8
                             `((id . ,note-id)
                               (title . ,note-name)
                               (category . ,emcn-category)
                               (content . ,content)))))
    (url-retrieve (emcn--client-endpoint client (format "/apps/notes/api/v1/notes/%d" note-id))
                  (emcn--handle-note-saved note-name 'update)
                  nil t)))

(cl-defmethod emcn--client-delete-note ((client emcn--client) note-id)
  (let* ((url-request-extra-headers (emcn--client-url-headers client))
         (url-request-method "DELETE"))
    (url-retrieve-synchronously
     (emcn--client-endpoint client (format "/apps/notes/api/v1/notes/%d" note-id))
     t)))

(defun emcn--set-note-name (name)
  (setq-local emcn-note-name name)
  (rename-buffer (emcn--note-buffer-name name)))


(defun emcn--get-client ()
  (let ((password (emcn--get-password))
        (host (emcn--get-host)))
    (emcn--make-client :password password
                       :host host)))

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

(defun emcn--save-password? (token)
  (yes-or-no-p
   (format (concat "Would you like to save the provided password to your keyring?\n"
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


(defun emcn--request-notes-password ()
  (let* ((host (emcn--get-host))
         (username (read-string (format "Username for nextcloud instance (%s): "
                                        host)))
         (password (read-passwd (format "Password for nextcloud instance (%s): "
                                        host) t)))
    (when (and username password)
      (let ((token (base64-encode-string (concat username ":" password))))
        (when (emcn--save-password? token)
          (emcn--save-token host token))
        token))))

(defun emcn--get-password ()
  (or (alist-get (emcn--get-host) emcn-token-alist nil nil #'string=)
      (emcn--get-token (emcn--get-host))
      (emcn--request-notes-password)))

(defun emcn--request-host ()
  (let ((notes-host (read-string
                      (concat "I  need to know where your instance is! "
                              "please provide your nextcloud hostname: "))))
    (when notes-host
      (setq emcn-host notes-host)
      (customize-set-variable 'emcn-host notes-host)
      (customize-save-variable 'emcn-host notes-host))
    notes-host))

(defun emcn-set-notes-host ()
  (interactive)
  (emcn--request-host)
  (emcn--request-notes-password))

(defun emcn--get-host ()
  (if emcn-host
      emcn-host
    (emcn--request-host)))

(defun emcn--get-note-alist ()
  (let* ((client (emcn--get-client))
         (notes (emcn--client-get-notes client))
         (alist))
    (dolist (note notes)
      (push `(,(alist-get 'title note) . ,note)
            alist))

    alist))

(provide 'emcn-client)
