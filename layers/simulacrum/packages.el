;;; packages.el --- simulacrum layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Jonathan Crum <krummja@simulacrum>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar simulacrum-packages nil)

(defmacro s|pkg (name &rest body)
  (declare (indent defun))
  (let ((id (if (listp name) (car name) name)))
    `(progn
       (defun ,(intern (format "simulacrum/init-%s" id)) ()
         (use-package ,id ,@body))
       (push ',name simulacrum-packages))))

;-------------------------------------------------------------------------------

(s|pkg beacon
  :config
  (beacon-mode)
  (setq beacon-color (face-attribute 'region :background nil t)
        beacon-blink-when-buffer-changes t
        beacon-bling-when-point-moves-vertically t))

(s|pkg doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-org-config))

;(s|pkg doom-modeline
;  :config
;  (doom-modeline-mode 1))

(s|pkg deft
  :config
  (setq deft-directory "~/Dropbox/Notebook/Slipbox")
  (setq deft-recursive t)
  (setq deft-extensions '("org" "md" "txt" "tex" "pdf"))
  (setq deft-default-extension "org")
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-auto-save-interval 0))

(s|pkg guide-key
  :config
  (setq guide-key/highlight-command-regexp
        '("rectangle"
          ("register" . font-lock-type-face)
          ("bookmark" . "hot pink")
          ("helm" . "light violet red")))
  (setq guide-key/popup-window-position 'bottom)
  (setq guide-key/recursive-key-sequence-flag t)
;  (setq guide-key/guide-key-sequence '("SPC"
;                                      "C-c"
;                                      "C-x"
;                                      "M-m"))
  (guide-key-mode 1))

(s|pkg org-index)
(s|pkg org-working-set)
(s|pkg org-dashboard)
(s|pkg org-sidebar)
(s|pkg org-noter)

(s|pkg (s-org :location local)
  :after (org hydra)
  :config
  (s-org/setup-general)
  (s-org/setup-babel))

(s|pkg (s-utils :location local))

(s|pkg writeroom-mode)

(s|pkg (zetteldeft :location local)
  :after deft)

; (s|pkg zotxt)
