;;; simulacrum org configuration

(require 'helm-bibtex)
(require 'hydra)
(require 'org)
(require 'org-pomodoro)
(require 'org-ref)
;(require 'org-super-agenda)

(defun s-org/setup-babel ()
  "Setup org babel."
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t )
     (emacs-lisp . t)
     (haskell . t)
     (latex . t)
     (lisp . t)
     (python . t)
     (shell . t))))

(defun s-org/reset-buffers ()
  "Reset org-mode in all org buffers"
  (dolist (buff (buffer-list))
    (with-current-buffer buff
      (if (string-equal "org-mode" major-mode)
          (org-mode)))))

(defun s-org/setup-notes ()
  "Setup agenda/captures and other notes-related things"

  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (with-eval-after-load 'org
    (setq org-directory "~/Dropbox/Notebook/Organizer/"
;          org-capture-templates
          )
    (setq org-refile-use-outline-path 'file  ; allows refiling as top-level header
          org-outline-path-complete-in-steps nil)))

;;;###autoload
(defun s-org/setup-general ()
  "Miscellaneous settings"
  (setq org-startup-indented t
        org-clock-idle-time 5
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-agenda-block-separator ""
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-catch-invisible-edits 'show-and-error
        org-cycle-separator-lines 0
        spaceline-org-clock-p t
        org-bullets-bullet-list '("▶")
        org-todo-keywords '((sequence "TODO" "|")
                            (sequence "PROG" "WAIT" "PROJECT" "|")
                            (sequence "|" "DONE"))
        org-todo-keyword-faces
        '(("TODO" . "OrangeRed")
          ("PROG" . "DeepSkyBlue")
          ("WAIT" . "yellow")
          ("PROJECT" . "magenta")
        org-modules '(org-bibtex
                      org-docview
                      org-habit
                      org-info)))

  (setq org-agenda-files (list "~/Dropbox/Notebook/Organizer")
        org-agenda-include-deadlines t)

  (defhydra hydra-clock (global-map "C-c w" :exit t)
    ("i" s-org/clock-in "clock in")
    ("c" org-pomodoro "clock in current")
    ("o" s-org/clock-out "clock-out")))

(provide 's-org)
