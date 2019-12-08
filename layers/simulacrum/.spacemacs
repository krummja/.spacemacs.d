;; -*- mode: emacs-lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DOTSPACEMACS LAYER DECLARATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs-base
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(;; Pure languages
     common-lisp
     emacs-lisp
     html
     javascript
     python
     typescript
     ;; Other languages
     bibtex
     (latex :variables
            latex-enable-folding t
            latex-enable-auto-fill t)
     (markdown :variables markdown-live-preview-engine 'vmd)
     ;; Everything else
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-private-snippets-directory "~/.emacs.d/private/simulacrum/snippets")
     better-defaults
     colors
     helm
     git
     github
     (org :variables
          org-enable-github-support t)
     pandoc
     pdf
     simulacrum
     slack
     theming
     typography
     (version-control :variables
                      version-control-diff-tool 'git-gutter
                      version-control-global-margin t
                      version-control-diff-side 'left)
     )
   dotspacemacs-additional-packages '()
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(vi-tilde-fringe emoji)
   dotspacemacs-install-packages 'used-only))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DOTSPACEMACS INIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-mode-line-theme 'doom
   dotspacemacs-editing-style 'hybrid
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner '005
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'org-mode
   dotspacemacs-themes '(doom-molokai
                         spacemacs-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Fira Code"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode t
   dotspacemacs-smart-closing-parenthesis t
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server t
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; USER INIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dotspacemacs/user-init ()

  ;; Directories
  (defconst user-layer-dir (file-name-as-directory "~/.emacs.d/private/simulacrum"))
;  (defconst user-project-dir (file-name-as-directory (getenv "PROJECTS_DIR")))

;  (setq custom-file "~/.emacs-custom.el")
;  (load custom-file)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; USER CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dotspacemacs/user-config ()


  ;; Org Babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python .t)))

  ;; TeX Configuration
  (setq-default TeX-master nil)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq org-latex-create-formula-image-program 'dvipng)

  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (push
                                '("xelatex" "xelatex %s" TeX-run-TeX nil t
                                  :help "Run xelatex on file")
                                TeX-command-list)))

  ;; Miscellaneous Mode Hooks
  (add-hook 'evil-hybrid-state-exit-hook 'sim-save-if-bufferfilename)
  (add-hook 'org-mode-hook #'org-zotxt-mode)
  (add-hook 'org-mode-hook #'org-indent-mode)
  (add-hook 'text-mode-hook #'visual-line-mode)

  (global-prettify-symbols-mode 1)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (mapc (lambda (pair) (push pair prettify-symbols-alist))
                    '(;;Syntax
                      ("defun" . #x1D487)
                      ("def" . #x2131)
                      ("not" . #x2757)
                      ("in" . #x2208)
                      ("not in" . #x2209)
                      ("return" . #x27fc)
                      ("yield" . #x27fb)
                      ("for" . #x2200)
                      ;; Base Types
                      ))))
  (add-hook 'python-mode-hook
            (lambda ()
              (mapc (lambda (pair) (push pair prettify-symbols-alist))
                    '(;;Syntax
                      ("def" . #x1D487)
                      ))))

  ;; zotxt Configuration
  (defconst zotxt-url-base
    "http://localhost:23119/zotxt")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (slack emojify circe yapfify writeroom-mode visual-fill-column web-mode web-beautify vmd-mode unfill typo tide typescript-mode flycheck tagedit slime-company slime slim-mode oauth2 websocket scss-mode sass-mode rainbow-mode rainbow-identifiers pyvenv pytest pyenv-mode py-isort pug-mode pip-requirements pandoc-mode ox-pandoc ox-gfm org-ref pdf-tools key-chord ivy tablist org-noter mwim magit-gh-pulls livid-mode skewer-mode simple-httpd live-py-mode json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc hy-mode helm-pydoc helm-css-scss helm-company helm-c-yasnippet helm-bibtex parsebib haml-mode guide-key popwin github-search github-clone github-browse-file git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gist gh marshal logito pcache ht fuzzy emmet-mode doom-modeline shrink-path all-the-icons memoize diff-hl deft cython-mode company-web web-completion-data company-tern dash-functional tern company-statistics company-quickhelp pos-tip company-auctex company-anaconda company common-lisp-snippets color-identifiers-mode coffee-mode biblio biblio-core beacon auto-yasnippet yasnippet auctex anaconda-mode pythonic f ac-ispell auto-complete smeargle orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download mmm-mode markdown-toc s markdown-mode magit-gitflow magit-popup htmlize helm-gitignore request gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md evil-magit magit transient git-commit with-editor dash which-key use-package pcre2el macrostep hydra lv helm-themes helm-swoop helm-projectile projectile pkg-info epl helm-mode-manager helm-make helm-flx flx helm-descbinds helm-ag exec-path-from-shell evil-visualstar evil-escape evil goto-chg undo-tree elisp-slime-nav diminish bind-map bind-key auto-compile packed ace-window ace-jump-helm-line helm avy helm-core popup async))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-current-time ((t (:foreground "#81A1C1"))))
 '(org-agenda-date ((t (:foreground "#7a87a0" :inherit variable-pitch :height 1.2))))
 '(org-agenda-date-today ((t (:height 1.4 :foreground "#81A1C1" :inherit variable-pitch))))
 '(org-agenda-date-weekend ((t (:inherit org-agenda-date :height 1.0 :foreground "#4f5a6f"))))
 '(org-agenda-done ((t (:inherit nil :strike-through t :foreground "#7a87a0"))))
 '(org-agenda-structure ((t (:height 1.3 :foreground "#7a87a0" :weight normal :inherit variable-pitch))))
 '(org-block ((t (:background "#191d24"))))
 '(org-block-begin-line ((t (:background "#191d24"))))
 '(org-block-end-line ((t (:background "#191d24"))))
 '(org-code ((t (:foreground "#8FBCBB" :height 1.0))))
 '(org-column ((t (:background nil :weight bold))))
 '(org-column-title ((t (:background nil :underline t))))
 '(org-date ((t (:foreground "#7a87a0"))))
 '(org-document-info ((t (:foreground "#7a87a0" :slant italic))))
 '(org-document-info-keyword ((t (:foreground "#4f5a6f"))))
 '(org-document-title ((t (:inherit nil :height 1.3 :weight bold :foreground "#fb2874"))))
 '(org-done ((t (:inherit variable-pitch :foreground "#5E81AC" :background "#191d24"))))
 '(org-ellipsis ((t (:underline nil :background "#191d24" :foreground "#7a87a0"))))
 '(org-formula ((t (:foreground "#8FBCBB"))))
 '(org-headline-done ((t (:strike-through t))))
 '(org-indent ((t (:inherit org-hide))))
 '(org-level-1 ((t (:inherit nil :height 1.1 :weight bold :foreground "#fb2874" :background "#191d24"))))
 '(org-level-2 ((t (:inherit nil :weight normal :height 1.0 :foreground "mediumorchid1" :background "#191d24"))))
 '(org-level-3 ((t (:inherit nil :weight normal :height 1.0 :foreground "plum2" :background "#191d24"))))
 '(org-level-4 ((t (:inherit nil :weight normal :height 1.0 :foreground "pale violet red" :background "#191d24"))))
 '(org-level-5 ((t (:inherit nil :weight normal :height 1.0 :foreground "pale violet red" :background "#191d24"))))
 '(org-level-6 ((t (:inherit nil :weight normal :height 1.0 :foreground "pale violet red" :background "#191d24"))))
 '(org-level-7 ((t (:inherit nil :weight normal :height 1.0 :foreground "pale violet red" :background "#191d24"))))
 '(org-level-8 ((t (:inherit nil :weight normal :height 1.0 :foreground "pale violet red" :background "#191d24"))))
 '(org-link ((t (:underline nil :weight normal :foreground "#81A1C1"))))
 '(org-list-dt ((t (:foreground "#88C0D0"))))
 '(org-quote ((t (:background "#191d24"))))
 '(org-ref-cite-face ((t (:foreground "#8FBCBB"))))
 '(org-ref-ref-face ((t (:foreground nil :inherit org-link))))
 '(org-scheduled ((t (:foreground "#7a87a0"))))
 '(org-scheduled-previously ((t (:foreground "#81A1C1"))))
 '(org-scheduled-today ((t (:foreground "#ECEFF4"))))
 '(org-special-keyword ((t (:height 0.9 :foreground "#4f5a6f"))))
 '(org-table ((t (:inherit fixed-pitch :background nil :foreground "#7a87a0"))))
 '(org-tag ((t (:foreground "#7a87a0"))))
 '(org-time-grid ((t (:foreground "#4f5a6f"))))
 '(org-todo ((t (:foreground "#8FBCBB" :background "#191d24"))))
 '(org-upcoming-deadline ((t (:foreground "#81A1C1"))))
 '(org-variable-pitch-face ((t (:height 0.9))))
 '(org-verbatim ((t (:foreground "#8FBCBB"))))
 '(org-warning ((t (:foreground "#8FBCBB")))))
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-compile-mode-line-counter t)
 '(blink-cursor-mode t)
 '(custom-safe-themes
   (quote
    ("4bdc0dfc53ae06323e031baf691f414babf13c9c9c35014dd07bb42c4db27c24" default)))
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "#555556")
 '(global-hl-line-mode t)
 '(guide-key/highlight-command-regexp
   (quote
    ("rectangle"
     ("register" . font-lock-type-face)
     ("bookmark" . "hot pink")
     ("helm" . "light violet red"))))
 '(helm-fuzzy-matching-highlight-fn (quote helm-flx-fuzzy-highlight-match))
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#fd971f"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#b6e63e"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#525254"))
 '(objed-cursor-color "#e74c3c")
 '(package-selected-packages
   (quote
    (poet-theme slack emojify circe yapfify writeroom-mode visual-fill-column web-mode web-beautify vmd-mode unfill typo tide typescript-mode flycheck tagedit slime-company slime slim-mode oauth2 websocket scss-mode sass-mode rainbow-mode rainbow-identifiers pyvenv pytest pyenv-mode py-isort pug-mode pip-requirements pandoc-mode ox-pandoc ox-gfm org-ref pdf-tools key-chord ivy tablist org-noter mwim magit-gh-pulls livid-mode skewer-mode simple-httpd live-py-mode json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc hy-mode helm-pydoc helm-css-scss helm-company helm-c-yasnippet helm-bibtex parsebib haml-mode guide-key popwin github-search github-clone github-browse-file git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gist gh marshal logito pcache ht fuzzy emmet-mode doom-modeline shrink-path all-the-icons memoize diff-hl deft cython-mode company-web web-completion-data company-tern dash-functional tern company-statistics company-quickhelp pos-tip company-auctex company-anaconda company common-lisp-snippets color-identifiers-mode coffee-mode biblio biblio-core beacon auto-yasnippet yasnippet auctex anaconda-mode pythonic f ac-ispell auto-complete smeargle orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download mmm-mode markdown-toc s markdown-mode magit-gitflow magit-popup htmlize helm-gitignore request gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md evil-magit magit transient git-commit with-editor dash which-key use-package pcre2el macrostep hydra lv helm-themes helm-swoop helm-projectile projectile pkg-info epl helm-mode-manager helm-make helm-flx flx helm-descbinds helm-ag exec-path-from-shell evil-visualstar evil-escape evil goto-chg undo-tree elisp-slime-nav diminish bind-map bind-key auto-compile packed ace-window ace-jump-helm-line helm avy helm-core popup async)))
 '(vc-annotate-background "#1c1e1f")
 '(vc-annotate-color-map
   (list
    (cons 20 "#b6e63e")
    (cons 40 "#c4db4e")
    (cons 60 "#d3d15f")
    (cons 80 "#e2c770")
    (cons 100 "#ebb755")
    (cons 120 "#f3a73a")
    (cons 140 "#fd971f")
    (cons 160 "#fc723b")
    (cons 180 "#fb4d57")
    (cons 200 "#fb2874")
    (cons 220 "#f43461")
    (cons 240 "#ed404e")
    (cons 260 "#e74c3c")
    (cons 280 "#c14d41")
    (cons 300 "#9c4f48")
    (cons 320 "#77504e")
    (cons 340 "#555556")
    (cons 360 "#555556")))
 '(vc-annotate-very-old-color nil)
 '(yas-triggers-in-field t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "black" :foreground "black"))))
 '(font-lock-comment-face ((t (:background "#e0d8c1" :foreground "#2aa1ae" :slant normal))))
 '(fringe ((t (:background "#e0d8c1" :foreground "red"))))
 '(hl-line ((t (:background "#e0d8c1"))))
 '(line-number-current-line ((t (:inherit line-number :background "thistle2" :foreground "#655370"))))
 '(magit-section-highlight ((t (:background "thistle2"))))
 '(mode-line ((t (:background "#fbf8ef" :foreground "#655370" :box (:line-width 1 :color "#b3b9be")))))
 '(org-agenda-current-time ((t (:foreground "#81A1C1"))))
 '(org-agenda-date ((t (:foreground "#7a87a0" :inherit variable-pitch :height 1.2))))
 '(org-agenda-date-today ((t (:height 1.4 :foreground "#81A1C1" :inherit variable-pitch))))
 '(org-agenda-date-weekend ((t (:inherit org-agenda-date :height 1.0 :foreground "#4f5a6f"))))
 '(org-agenda-done ((t (:inherit nil :strike-through t :foreground "#7a87a0"))))
 '(org-agenda-structure ((t (:height 1.3 :foreground "#7a87a0" :weight normal :inherit variable-pitch))))
 '(org-block ((t (:background "#191d24"))))
 '(org-block-begin-line ((t (:background "#191d24"))))
 '(org-block-end-line ((t (:background "#191d24"))))
 '(org-code ((t (:foreground "#8FBCBB" :height 1.0))))
 '(org-column ((t (:background nil :weight bold))))
 '(org-column-title ((t (:background nil :underline t))))
 '(org-date ((t (:foreground "#7a87a0"))))
 '(org-document-info ((t (:foreground "#7a87a0" :slant italic))))
 '(org-document-info-keyword ((t (:foreground "#4f5a6f"))))
 '(org-document-title ((t (:inherit nil :height 1.3 :weight bold :foreground "#fb2874"))))
 '(org-done ((t (:inherit variable-pitch :foreground "#5E81AC" :background "#191d24"))))
 '(org-ellipsis ((t (:underline nil :background "#191d24" :foreground "#7a87a0"))))
 '(org-formula ((t (:foreground "#8FBCBB"))))
 '(org-headline-done ((t (:strike-through t))))
 '(org-hide ((t (:foreground "#FFF2E6"))))
 '(org-indent ((t (:inherit org-hide))))
 '(org-level-1 ((t (:inherit nil :height 1.1 :weight bold :foreground "#fb2874" :background "#191d24"))))
 '(org-level-2 ((t (:inherit nil :weight normal :height 1.0 :foreground "mediumorchid1" :background "#191d24"))))
 '(org-level-3 ((t (:inherit nil :weight normal :height 1.0 :foreground "plum2" :background "#191d24"))))
 '(org-level-4 ((t (:inherit nil :weight normal :height 1.0 :foreground "pale violet red" :background "#191d24"))))
 '(org-level-5 ((t (:inherit nil :weight normal :height 1.0 :foreground "pale violet red" :background "#191d24"))))
 '(org-level-6 ((t (:inherit nil :weight normal :height 1.0 :foreground "pale violet red" :background "#191d24"))))
 '(org-level-7 ((t (:inherit nil :weight normal :height 1.0 :foreground "pale violet red" :background "#191d24"))))
 '(org-level-8 ((t (:inherit nil :weight normal :height 1.0 :foreground "pale violet red" :background "#191d24"))))
 '(org-link ((t (:underline nil :weight normal :foreground "#81A1C1"))))
 '(org-list-dt ((t (:foreground "#88C0D0"))))
 '(org-quote ((t (:background "#191d24"))))
 '(org-ref-cite-face ((t (:foreground "#8FBCBB"))))
 '(org-ref-ref-face ((t (:foreground nil :inherit org-link))))
 '(org-scheduled ((t (:foreground "#7a87a0"))))
 '(org-scheduled-previously ((t (:foreground "#81A1C1"))))
 '(org-scheduled-today ((t (:foreground "#ECEFF4"))))
 '(org-special-keyword ((t (:height 0.9 :foreground "#4f5a6f"))))
 '(org-table ((t (:inherit fixed-pitch :background nil :foreground "#7a87a0"))))
 '(org-tag ((t (:foreground "#7a87a0"))))
 '(org-time-grid ((t (:foreground "#4f5a6f"))))
 '(org-todo ((t (:foreground "#8FBCBB" :background "#191d24"))))
 '(org-upcoming-deadline ((t (:foreground "#81A1C1"))))
 '(org-variable-pitch-face ((t (:height 0.9))))
 '(org-verbatim ((t (:foreground "#8FBCBB"))))
 '(org-warning ((t (:foreground "#8FBCBB"))))
 '(widget-field ((t (:background "LavenderBlush2"))))
 '(window-divider ((t (:foreground "thistle4"))))
 '(window-divider-first-pixel ((t (:foreground "thistle4")))))
)
