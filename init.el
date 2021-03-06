(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs-base
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(;; Simulacrum Spacemacs
     simulacrum
     ;; Programming
     bibtex
     common-lisp 
     emacs-lisp
     html
     (latex :variables
            latex-enable-folding t
            latex-enable-auto-fill t)
     (markdown :variables
               markdown-live-preview-engine 'vmd)
     python
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-private-snippets-directory "~/.spacemacs.d/snippets")
     ;; Miscellaneous Config Layers
     better-defaults
     colors
     helm
     git
     github
     (org :variables
          org-enable-github-support t)
     pandoc
     pdf
     theming
     typography
     (version-control :variables
                      version-control-diff-tool 'git-gutter
                      version-control-global-margin t
                      version-control-diff-side 'left)
     )
   dotspacemacs-additional-packages
   '(
     poet-theme
	 elpy
     )
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'hybrid
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner '005
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(poet
                         spacemacs-dark)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Fira Code"
                               :size 12
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-mode-line-theme 'spacemacs
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
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing
  ))

(defun dotspacemacs/user-init ()
  (defconst user-layer-dir (file-name-as-directory
                            "~/.spacemacs.d/layers/simulacrum"))

  (setq custom-file "~/.spacemacs.d/.custom-settings.el")
  (load custom-file))

(defun dotspacemacs/user-config ()
  (use-package elpy
    :ensure t
    :init
    (elpy-enable))

  (setq-default TeX-master nil)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq writeroom-width 100)
  (setq org-latex-create-formula-image-program 'dvipng)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (push
                                '("xelatex" "xelatex %s" TeX-run-TeX nil t
                                  :help "Run xelatex on file")
                                (TeX-command-list))))

  (setq poet-variable-headers t)
  (set-face-attribute 'default nil :family "Fira Code")
  (set-face-attribute 'fixed-pitch nil :family "Fira Code" :height 90)
  (set-face-attribute 'variable-pitch nil :family "Gentium" :height 130)
  (add-hook 'evil-hybrid-state-exit-hook 'sim-save-if-bufferfilename)
  (add-hook 'text-mode-hook
            (lambda ()
              (variable-pitch-mode 1)))
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'text-mode-hook 'visual-line-mode)
  (add-hook 'after-save-hook 'sim-tangle))
