;;; funcs.el --- simulacrum layer utility functions -*- lexical-binding: t -*-

(defun sim-save-if-bufferfilename ()
  (if (buffer-file-name) (progn (save-buffer))
    (message "no file associated with this buffer: do nothing")))

(defun sim/cycle-theme ()
  "Cycle between light and dark scheme"
  (interactive)
  (if (eq sim-current-theme sim-dark-theme)
      (progn
        (sim/light)
        (setq sim-current-theme sim-light-theme))
    (progn
      (sim/dark)
      (setq sim-current-theme sim-dark-theme))))

(defun sim/light ()
  "Switch to light theme"
  (interactive)
  (disable-theme sim-dark-theme)
  (spacemacs/load-theme sim-light-theme)
  (setq org-bullets-bullet-list '(" "))
  (sim-org/reset-buffers)
  (beacon-mode -1))

(defun sim/dark ()
  "Switch to dark theme"
  (interactive)
  (disable-theme sim-light-theme)
  (spacemacs/load-theme sim-dark-theme)
  (setq org-bullets-bullet-list '("▶ "))
  (sim-org/reset-buffers)
  (beacon-mode +1))

(defun sim-org/reset-buffers ()
  "Reset org-mode in all buffers
  TODO Move this to a separate layer"
  (dolist (buff (buffer-list))
    (with-current-buffer buff
      (if (string-equal "org-mode" major-mode)
          (org-mode)))))

;(defun sim/org-open-other-frame ()
;  (interactive)
;  (let ((org-link-frame-setup (acons 'file 'find-file-other-frame org-link-frame-setup)))
;    (org-open-at-point)))

(defun my-mark-as-project ()
  (interactive)
  (org-set-property "COOKIE_DATA" "todo recursive")
  (org-back-to-heading t)
  (let* ((title (nth 4 (org-heading-components )))
         (keyword (nth 2 (org-heading-components))))
    (when (and (bound-and-true-p keyword) (string-prefix-p "[" title))
      (message "TODO keyword and progress indicator found"))
    (when (and (not (bound-and-true-p keyword)) (string-prefix-p "[" title))
                    (message "no TODO keyword but progress indicator found")
                    (forward-whitespace 1)
                    (insert "PROJECT "))
    (when (and (not (bound-and-true-p keyword)) (not (string-prefix-p "[" title)))
      (message "no TODO keyword and no progress indicator found")
      (forward-whitespace 1)
      (insert "PROJECT [/] "))
    (when (and (bound-and-true-p keyword) (not (string-prefix-p "[" title)))
      (message "TODO keyword but no progress indicator found")
      (forward-whitespace 2)
      (insert "[/] "))))
