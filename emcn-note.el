;; emcn-note.el --- Nextcloud Notes Client for Emacs  -*- lexical-binding: t; -*-

(cl-defstruct (emcn-note
               (:constructor emcn--make-note))
  "For documentation on note attributes, see the Nextcloud Notes
API documentation at:
https://github.com/nextcloud/notes/blob/main/docs/api/v1.md"
  (id 0 :type number)
  (local-id nil
            :type string
            :documentation
            "Localized identifier for this note. Not synced with the server.")
  (etag "" :type string)
  (readonly nil :type bool)
  (content-string "" :type string)
  (content-hydrated t
                    :type bool
                    :documentation
                    "Whether the content has already been read from the filesystem.")
  (content-file ""
                :type string
                :documentation
                "Where the file containing the notes content is located, not always populated. Used in tandem with content-hydrated to lazy-load content.")
  (title "" :type string)
  (category "" :type string)
  (favourite nil :type bool)
  (modified (current-time) :type time)
  (committed t
             :type bool
             :documentation
             "Whether the note in its current form has been committed to on-disk storage yet or not."))

(cl-defmethod emcn-note-content ((note emcn-note))
  "Getter for note content."
  (unless (emcn-note-content-hydrated note)
    (setf (emcn-note-content-string note)
          (emcn--file-contents (emcn-note-content-file note)))
    (setf (emcn-note-content-hydrated note) t))

  (emcn-note-content-string note))

;; Define a setter for note content. This is not a "real" struct attribute to
;; make it possible to override the getter for lazy loading. Couldn't get
;; overriding a getter defined by `cl-defstruct` to work.
(gv-define-setter emcn-note-content (content note)
  `(progn
     (setf (emcn-note-content-hydrated ,note) t)
     (setf (emcn-note-content-string ,note) ,content)))

(cl-defmethod emcn-note-summary ((note emcn-note) &optional length)
  (unless length (setq length 130))
  (let ((summary-text ""))
  (if (emcn-note-content-hydrated note)
      (progn
        (when(< (length (emcn-note-content note)) length)
          (setq length (length (emcn-note-content note))))
        (setq summary-text (substring (emcn-note-content note) 0 length)))
    (setq summary-text (emcn--head-file (emcn-note-content-file note) length)))

  (let ((lines (split-string summary-text "\n" t "[[:blank:]]+")))
    (if (> (length lines) 1)
        (string-join (nthcdr 1 lines) " ")
      (string-join lines " ")))))


(cl-defmethod emcn-note-to-alist ((note emcn-note) &optional exclude-content exclude-id)
  (let ((note-alist
         `((etag . ,(emcn-note-etag note))
           (readonly . ,(or (emcn-note-readonly note) :false))
           (title . ,(emcn-note-title note))
           (category . ,(emcn-note-category note))
           (favourite . ,(or (emcn-note-favourite note) :false))
           (modified . ,(float-time (emcn-note-modified note))))))
    (unless exclude-content
      (push `(content . ,(emcn-note-content note))
            note-alist))

    (unless exclude-id
      (push `(id . ,(emcn-note-id note))
            note-alist))

    note-alist))

(defsubst emcn-note-from-alist (note)
  (let ((readonly (alist-get 'readonly note))
        (favourite (alist-get 'favourite note)))

    (when (eq readonly :false)
      (setq readonly nil))

    (when (eq favourite :false)
      (setq favourite nil))

  (emcn--make-note
   :id (alist-get 'id note)
   :etag (alist-get 'etag note)
   :readonly readonly
   :content-string (alist-get 'content note)
   :title (alist-get 'title note)
   :category (alist-get 'category note)
   :favourite favourite
   :modified (seconds-to-time (alist-get 'modified note)))))

(provide 'emcn-note)
