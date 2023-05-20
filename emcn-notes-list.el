(require 'emcn)

(defun emcn-get-note-tags (title)
  (when (string-match "^\\[\\([^]]+\\)" title)
    (split-string (match-string 1 title) " " t "[[:blank:]]+")))

(defvar emcn-notes-list-icons
  `(("code" . "bootstrap/journal-code")
    ("default" . "bootstrap/journal")
    ("docs" . "bootstrap/journal-bookmark")
    ("article" . "bootstrap/journal-richtext")
    ("blog" . "bootstrap/journal-richtext")
    ("note" . "bootstrap/journal-text"))
  "Map tags to icons")

(defun emcn-notes-list-note (note)
  (let ((nl-note))
    ;; TITLE
    (push (cons "TITLE" (alist-get 'title note)) nl-note)
    (push (cons "FILENAME" (alist-get 'title note)) nl-note)
    ;; ICON and TAGS
    ;;;
    ;; Note: Nextcloud notes has no concept of tags, so we resort to using words
    ;; between angle brackets ([ and ]) at the start of the note title as
    ;; tags. The icon is then determined by the first tag that matches one of
    ;; `emcn-notes-list-icons'.
    (let* ((tags (emcn-get-note-tags (alist-get 'title note)))
           (icon
            (catch 'break
              (dolist (icon emcn-notes-list-icons)
                (when (member (car icon) tags)
                  (throw 'break (cdr icon)))))))
      (when (not icon)
        (setq icon
              (alist-get "default" emcn-notes-list-icons nil nil #'string=)))
      (push (cons "ICON" icon) nl-note)
      (push (cons "TAGS" tags) nl-note))

    (push (cons "TIME-CREATION" 0) nl-note)
    (push (cons "TIME-MODIFICATION" (alist-get 'modified note)) nl-note)
    (push (cons "TIME-ACCESS" (alist-get 'modified note)) nl-note)
    (push (cons "SUMMARY" (emcn-note-summary note)) nl-note)

    nl-note))

(defun emcn-note-summary (note)
  (let* ((content (alist-get 'content note))
         (lines (split-string content "\n" t)))
    (when (> (length lines) 1)
      (nth 1 lines))))

(defun emcn-notes-list-collect-notes ()
  (let* ((client (emcn--get-client))
         (cloud-notes (emcn--client-get-notes client))
         (notes))
    (dolist (note cloud-notes)
      (push (emcn-notes-list-note note) notes))
    notes))

(defun emcn-notes-list-open (title)
  (let* ((notes (emcn--get-note-alist))
         (note (alist-get title notes nil nil #'string=)))
    (if note
        (emcn-open note)
      (error "No note found by title %s" title))))

(provide 'emcn-notes-list)
