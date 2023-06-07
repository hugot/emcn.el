;; emcn-util.el --- Nextcloud Notes Client for Emacs  -*- lexical-binding: t; -*-

(defsubst emcn--json-serialize-utf8 (json)
  "Serialize a json object and encode the resulting string to UTF-8."
  (encode-coding-string
   (emcn--json-preset (json-serialize json)) 'utf-8 t))

(defmacro emcn--json-preset (&rest body)
  "Define JSON preset to use when marshalling/unmarshalling json"

  `(let ((json-array-type 'list)
         (json-object-type 'alist)
         (json-key-type 'symbol)
         (json-false :false))
     ,@body))

(defsubst emcn--head-file (file &optional end)
  "First END characters of file as a string."
  (unless end (setq end 130))
  (with-temp-buffer
    (insert-file-contents file nil 0 end)
    (buffer-string)))

(defsubst emcn--file-contents (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defsubst emcn--write-string-to-file (string file)
  (with-temp-buffer
    (insert string)
    (write-region (point-min) (point-max) file nil 'no-message)))


(provide 'emcn-util)
