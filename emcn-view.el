(defcustom emcn-note-width 80 "Character width of a note"
  :type 'integer
  :group 'emcn)

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

(provide 'emcn-view)
