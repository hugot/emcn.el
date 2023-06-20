;; emcn-store.el --- Nextcloud Notes Client for Emacs  -*- lexical-binding: t; -*-

(require 'emcn-note)
(require 'emcn-util)

(defvar emcn-store nil
  "Central note storage manager.")

(defsubst emcn--get-store ()
  (unless emcn-store
    (setq emcn-store (emcn--make-store))
    (emcn-store-init emcn-store))

  emcn-store)

(defsubst emcn--genid ()
  (base64-encode-string (number-to-string (random t))))

(cl-defstruct (emcn-store
               (:constructor emcn--make-store))
  (directory (concat user-emacs-directory  "/emcn-store/")
             :type string
             :documentation
             "Directory in which notes should be stored.")
  (notes (make-hash-table :size 100
                          :rehash-size 100
                          :test 'equal)
         :type hash-map
         :documentation
         "Map of notes, indexed by note ID.")
  (notes-by-local-id (make-hash-table :size 100
                          :rehash-size 100
                          :test 'equal)
                     :type hash-map
                     :documentation
                     "Map of notes, indexed by local note ID.")
  (initialized nil
               :type bool
               :documentation
               "Whether or not the store has been initialized for usage"))

(cl-defmethod emcn-store-init ((store emcn-store))
  (let* ((directory (emcn-store-directory store))
         (notes-directory (concat directory "/notes")))
    (unless (file-directory-p directory)
      (make-directory directory t))

    (unless (file-directory-p notes-directory)
      (make-directory notes-directory t))

    (emcn-store-hydrate store)))

(cl-defmethod emcn-store-index-file ((store emcn-store))
  (concat (emcn-store-directory store) "/index.json"))

(cl-defmethod emcn-store-note-file
  ((store emcn-store) (note emcn-note))
  (format "%s/notes/%s.md"
          (emcn-store-directory store) (emcn-note-local-id note)))

(cl-defmethod emcn-store-hydrate ((store emcn-store))
  (let ((notes (make-hash-table :size 100
                                :rehash-size 100
                                :test 'equal))
        (notes-by-local-id (make-hash-table :size 100
                                            :rehash-size 100
                                            :test 'equal))
        (index
         (when (file-exists-p (emcn-store-index-file store))
           (emcn--json-preset
            (json-read-file (emcn-store-index-file store))))))
    (dolist (json-note index)
      (let ((note (emcn-note-from-alist (cdr json-note))))
        (setf (emcn-note-content-hydrated note) nil)
        (setf (emcn-note-local-id note) (symbol-name (car json-note)))
        (setf (emcn-note-content-file note)
              (emcn-store-note-file store note))
        (puthash
         (emcn-note-id note) note notes)

        (puthash
         (emcn-note-local-id note) note notes-by-local-id)))

    (setf (emcn-store-notes-by-local-id store) notes-by-local-id)
    (setf (emcn-store-notes store) notes)))

(cl-defmethod emcn-store-commit ((store emcn-store))
  (let ((index))
    (maphash
     (lambda (id note)
       (push
        `(,(make-symbol (emcn-note-local-id note)) . ,(emcn-note-to-alist note t))
        index)

       (unless (emcn-note-committed note)
         (emcn--write-string-to-file
          (emcn-note-content note)
          (emcn-store-note-file store note))
         (setf (emcn-note-committed note) t)))
     (emcn-store-notes store))

    (emcn--write-string-to-file
     (emcn--json-preset (json-serialize index))
     (emcn-store-index-file store))))

(cl-defmethod emcn-store-put-note ((store emcn-store) (note emcn-note))
  (setf (emcn-note-committed note) nil)
  (unless (emcn-note-local-id note)
    (let ((local-id (emcn--genid)))
      (while (gethash
              local-id (emcn-store-notes-by-local-id store))
        (setq local-id (emcn--genid)))
      (setf (emcn-note-local-id note) local-id)))

  (puthash
   (emcn-note-local-id note) note (emcn-store-notes-by-local-id store))
  (unless (= 0 (emcn-note-id note))
    (puthash (emcn-note-id note) note (emcn-store-notes store))))

(cl-defmethod emcn-store-update-note ((store emcn-store) (note emcn-note))
  (setf (emcn-note-modified note) (current-time))
  (emcn-store-put-note store note))

(cl-defmethod emcn-store-get-notes ((store emcn-store))
  (hash-table-values (emcn-store-notes store)))

(cl-defmethod emcn-store-get-note ((store emcn-store) (id number))
  (gethash id (emcn-store-notes store)))

(cl-defmethod emcn-store-get-note ((store emcn-store) (id string))
  (gethash id (emcn-store-notes-by-local-id store)))

(cl-defmethod emcn-store-delete-note ((store emcn-store) (note emcn-note))
  (remhash (emcn-note-id note) (emcn-store-notes store))
  (remhash (emcn-note-local-id note) (emcn-store-notes-by-local-id store)))

(cl-defmethod emcn-store-delete-note ((store emcn-store) id)
  (let ((note (emcn-store-get-note id)))
    (when note
      (emcn-store-delete-note store note))))

(defmacro emcn-store-transact (store &rest body)
  (declare (indent 1))
  `(progn
     ,@body
     (emcn-store-commit store)))

(provide 'emcn-store)
