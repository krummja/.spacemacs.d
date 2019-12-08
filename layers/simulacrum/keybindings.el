(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-`") 'helm-bookmarks)


(global-set-key (kbd "M-m T n") 'sim/cycle-theme)

(spacemacs/declare-prefix "o" "Simulacrum")

(spacemacs/declare-prefix "ov" "Workspace View")
(spacemacs/set-leader-keys "ovc" 'wg-create-workgroup)
(spacemacs/set-leader-keys "ovd" 'wg-delete-workgroup)
(spacemacs/set-leader-keys "ovR" 'wg-reload-session)
(spacemacs/set-leader-keys "ovs" 'wg-save-session)
(spacemacs/set-leader-keys "ov`" 'wg-switch-to-workgroup)

(spacemacs/declare-prefix "oz" "Zettelkasten" )
  (spacemacs/set-leader-keys "ozd" 'deft)
  (spacemacs/set-leader-keys "ozD" 'zetteldeft-deft-new-search )
  (spacemacs/set-leader-keys "ozc" 'org-copy )
  (spacemacs/set-leader-keys "ozC" 'org-refile )
  (spacemacs/set-leader-keys "ozi" 'zetteldeft-find-file-id-insert )
  (spacemacs/set-leader-keys "ozI" 'zetteldeft-find-file-full-title-insert )
  (spacemacs/set-leader-keys "ozf" 'zetteldeft-find-file )
  (spacemacs/set-leader-keys "ozn" 'zetteldeft-new-file )
  (spacemacs/set-leader-keys "ozN" 'zetteldeft-new-file-and-link )
  (spacemacs/set-leader-keys "ozR" 'deft-refresh )
  (spacemacs/set-leader-keys "ozo" 'deft-open-file-other-window )

(spacemacs/declare-prefix "ozx" "Management" )
  (spacemacs/set-leader-keys "ozxd" 'deft-delete-file)
  (spacemacs/set-leader-keys "ozxr" 'deft-rename-file)

(spacemacs/declare-prefix "ob" "Bibliography" )
(spacemacs/set-leader-keys "obi" 'org-zotxt-insert-reference-link)
(spacemacs/set-leader-keys "obn" 'org-zotxt-noter)
(spacemacs/set-leader-keys "oba" 'org-zotxt-add-attachment)
(spacemacs/set-leader-keys "obR" 'org-zotxt-update-all-reference-links)

(spacemacs/declare-prefix "ow" "Writeroom")
(spacemacs/set-leader-keys "oww" 'writeroom-mode)
(spacemacs/set-leader-keys "ow+" 'writeroom-increase-width)
(spacemacs/set-leader-keys "ow-" 'writeroom-decrease-width)
(spacemacs/set-leader-keys "owv" 'visual-line-mode)
(spacemacs/set-leader-keys "owf" 'auto-fill-mode)
(spacemacs/set-leader-keys "owm" 'writeroom-toggle-mode-line)

(spacemacs/declare-prefix "op" "Projectile")
(spacemacs/set-leader-keys "opp" 'helm-projectile)
(spacemacs/set-leader-keys "opa" 'projectile-add-known-project)
(spacemacs/set-leader-keys "opd" 'make-directory)
(spacemacs/set-leader-keys "opm" 'my-mark-as-project)


;; split-window-below
;; split-window-right

