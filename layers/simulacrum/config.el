;;; config.el --- simulacrum layer config file for Spacemacs
;; Mostly theme configuration

(require 'color)

(defvar sim-dark-theme 'doom-molokai)
(defvar sim-light-theme 'spacemacs-light)

(defvar sim-current-theme sim-dark-theme)

(defun color-12-to-6 (color-str)
  "Convert 12 char color representation to 6 char"

  (if  (= 7 (length color-str))
      color-str
    (apply #'color-rgb-to-hex `(,@(color-name-to-rgb color-str) 2))))

;; Macro Syntax
;; Elisp macros are pretty neat, and handy as well.
;;
;; The following is a macro for 'when' which is essentially 'if' where
;; the 'else' component is set to nil.
;;
;; (defmacro when (condition action)
;; `(if ,condition ,action nil))
;;
;; The backtick ` means 'I'm a template'
;; The comma , means 'insert contents here'.

(defmacro sim-set-pair-faces (themes consts faces-alist)
  "Macro for pair setting of custom faces."

  (defmacro sim-get-proper-faces ()
    `(let* (,@consts)
       (backquote ,faces-alist)))

  `(setq theming-modifications
         ',(mapcar (lambda (theme)
                     `(,theme ,@(cl-remove-if
                                 (lambda (x) (equal x "NA"))
                                 (mapcar (lambda (face)
                                           (let ((face-name (car face))
                                                 (face-attrs (nth (cl-position theme themes)
                                                                  (cdr face))))
                                             (if face-attrs
                                                 `(,face-name ,@face-attrs)
                                               "NA"))) (sim-get-proper-faces)))))
                   themes)))

(sim-set-pair-faces
  ;; sim-set-pair-faces THEMES ... ...
 (doom-molokai spacemacs-light)

  ;; sim-set-pair-faces ... CONSTS ...
 (;; Color Palette
  (dark-1             "#2E3440")
  (dark-2             "#3B4252")
  (dark-3             "#434C5E")
  (dark-4             "#4C566A")
  (light-1            "#D8DEE9")
  (light-2            "#E5E9F0")
  (light-3            "#ECEFF4")
  (accent-dark        "#1C2028")
  (accent-dark-gray   (color-12-to-6 (color-darken-name accent-dark 1)))
  (accent-light       "#8a9899")
  (accent-shade-1     "#8FBCBB")
  (accent-shade-2     "#88C0D0")
  (accent-shade-3     "#81A1C1")
  (accent-shade-4     "#5E81AC")
  (colors-blue        accent-shade-4)
  (colors-blue-2      accent-shade-3)
  (colors-red         "#BF616A")
  (colors-orange      "#8FBCBB")
  (colors-yellow      "#8a9899")
  (colors-green       "#A3BE8C")
  (colors-purple      "#B48EAD")

  ;; For use in levelified faces set
  (level-1            colors-blue)
  (level-2            colors-blue-2)
  (level-3            colors-purple)
  (level-4            colors-orange)
  (level-5            accent-shade-3)
  (level-6            colors-green)
  (level-7            accent-shade-2)
  (level-8            colors-yellow)
  (level-9            accent-shade-1)

  ;; Base gray shades
  (bg-white           "#e0d8c1")
  (bg-dark            accent-dark-gray)
  (bg-darker          accent-dark)
  (bg-dark-solaire    (color-12-to-6 (color-lighten-name accent-dark 2)))
  (fg-white           light-3)
  (shade-white        (color-12-to-6 (color-lighten-name light-1 10)))
  (highlight          (color-12-to-6 (color-lighten-name accent-dark 4)))
  (region-dark        (color-12-to-6 (color-lighten-name accent-dark 2)))
  (region             dark-3)
  (slate              accent-shade-3)
  (gray               (color-12-to-6 (color-lighten-name dark-4 20)))

  ;; Programming
  (comment            (color-12-to-6 (color-lighten-name dark-4 2)))
  (doc                (color-12-to-6 (color-lighten-name dark-4 20)))
  (keyword            colors-blue-2)
  (builtin            colors-orange)
  (variable-name      colors-yellow)
  (function-name      accent-shade-2)
  (constant           colors-purple)
  (type               accent-shade-1)
  (string             colors-green)

  ;; Fonts
  (sans-font          "Source Sans Pro")
  (et-font            "EtBembo")
  (mono-font          "Iosevka"))

  ;; sim-set-pair-faces ... ... FACES-ALIST
 ((default
    (:background, bg-dark)
    (:background, bg-white))
  (org-agenda-current-time
   (:foreground ,slate)
   nil)
  (org-agenda-date
   (:foreground ,doc
                :inherit variable-pitch
                :height 1.2)
   (:inherit nil))
  (org-agenda-date-today
   (:height 1.4
            :foreground ,keyword
            :inherit variable-pitch)
   nil)
  (org-agenda-date-weekend
   (:inherit org-agenda-date
             :height 1.0
             :foreground ,comment)
   nil)
  (org-agenda-done
   (:inherit nil
             :strike-through t
             :foreground ,doc)
   (:height 1.0
            :strike-through t
            :foreground ,doc))
  (org-agenda-structure
   (:height 1.3
            :foreground ,doc
            :weight normal
            :inherit variable-pitch)
   nil)
  (org-block
   (:background ,bg-dark)
   (:background nil
                :foreground ,bg-dark))
  (org-block-begin-line
   (:background ,bg-dark)
   (:background nil
                :height 0.8
                :foreground ,slate))
  (org-block-end-line
   (:background ,bg-dark)
   (:background nil
                :height 0.8
                :foreground ,slate))
  (org-code
   (:foreground ,builtin
                :height 1.0)
   (:inherit nil
             :foreground ,comment))
  (org-column
   (:background nil
                :weight bold)
   nil)
  (org-column-title
   (:background nil
                :underline t)
   nil)
  (org-date
   (:foreground ,doc)
   (:height 0.8))
  (org-document-info
   (:foreground ,gray
                :slant italic)
   (:height 1.2
            :slant italic))
  (org-document-info-keyword
   (:foreground ,comment)
   (:height 0.8
            :foreground ,gray))
  (org-document-title
   (:inherit nil
             :height 1.3
             :weight bold
             :foreground ,"#fb2874")
   (:inherit nil
             :family ,et-font
             :height 1.4
             :foreground ,bg-dark
             :underline nil))
  (org-done
   (:inherit variable-pitch
             :foreground ,colors-blue
             :background ,bg-dark)
   (:strike-through t
                    :family ,et-font))
  (org-ellipsis
   (:underline nil
               :background ,accent-dark-gray
               :foreground ,doc)
   (:underline nil
               :foreground ,comment))
  (org-formula
   (:foreground ,type)
   nil)
  (org-headline-done
   (:strike-through t)
   (:family ,et-font
            :strike-through t))
  (org-hide
   nil
   (:foreground ,bg-white))
  (org-indent
   (:inherit org-hide)
   (:inherit (org-hide fixed-pitch)))
  (org-level-1
   (:inherit nil
             :height 1.1
             :weight bold
             :foreground ,"Magenta"
             :background ,bg-dark)
   (:inherit nil
             :family ,et-font
             :height 1.4
             :weight normal
             :slant normal
             :foreground ,bg-dark))
  (org-level-2
   (:inherit nil
             :weight normal
             :height 1.0
             :foreground ,"mediumorchid1"
             :background ,bg-dark)
   (:inherit nil
             :family ,et-font
             :weight normal
             :height 1.3
             :slant normal
             :foreground ,bg-dark))
  (org-level-3
   (:inherit nil
             :weight normal
             :height 1.0
             :foreground ,"plum2"
             :background ,bg-dark)
   (:inherit nil
             :family ,et-font
             :weight normal
             :slant normal
             :height 1.2
             :foreground ,bg-dark))
  (org-level-4
   (:inherit nil
             :weight normal
             :height 1.0
             :foreground ,"pale violet red"
             :background ,bg-dark)
   (:inherit nil
             :family ,et-font
             :weight normal
             :slant italic
             :height 1.2
             :foreground ,bg-dark))
  (org-level-5
   (:inherit nil
             :weight normal
             :height 1.0
             :foreground ,"pale violet red"
             :background ,bg-dark)
   (:inherit nil
             :family ,et-font
             :weight normal
             :slant italic
             :height 1.2
             :foreground ,bg-dark))
  (org-level-6
   (:inherit nil
             :weight normal
             :height 1.0
             :foreground ,"pale violet red"
             :background ,bg-dark)
   (:inherit nil
             :family ,et-font
             :weight normal
             :slant italic
             :height 1.2
             :foreground ,bg-dark))
  (org-level-7
   (:inherit nil
             :weight normal
             :height 1.0
             :foreground ,"pale violet red"
             :background ,bg-dark)
   (:inherit nil
             :family ,et-font
             :weight normal
             :slant italic
             :height 1.2
             :foreground ,bg-dark))
  (org-level-8
   (:inherit nil
             :weight normal
             :height 1.0
             :foreground ,"pale violet red"
             :background ,bg-dark)
   (:inherit nil
             :family ,et-font
             :weight normal
             :slant italic
             :height 1.2
             :foreground ,bg-dark))
  (org-link
   (:underline nil
               :weight normal
               :foreground ,slate)
   (:foreground ,bg-dark))
  (org-list-dt
   (:foreground ,function-name)
   nil)
  (org-quote
   (:background ,bg-dark)
   (:slant italic
           :family ,et-font))
  (org-ref-cite-face
   (:foreground ,builtin)
   (:foreground ,builtin))
  (org-ref-ref-face
   (:foreground nil :inherit org-link)
   (:foreground nil :inherit org-link))
  (org-scheduled
   (:foreground ,gray)
   nil)
  (org-scheduled-previously
   (:foreground ,slate)
   nil)
  (org-scheduled-today
   (:foreground ,fg-white)
   nil)
  (org-special-keyword
   (:height 0.9
            :foreground ,comment)
   (:height 0.8))
  (org-table
   (:inherit fixed-pitch
             :background nil
             :foreground ,doc)
   (:inherit fixed-pitch
             :height 0.9
             :background ,bg-white))
  (org-tag
   (:foreground ,doc)
   (:foreground ,doc))
  (org-time-grid
   (:foreground ,comment)
   nil)
  (org-todo
   (:foreground ,builtin
                :background ,bg-dark)
   nil)
  (org-upcoming-deadline
   (:foreground ,keyword)
   nil)
  ;; NOTE: Name is confusing, this is fixed pitch from org-variable-pitch package
  (org-variable-pitch-face
   (:height 0.9)
   nil)
  (org-verbatim
   (:foreground ,type)
   nil)
  (org-warning
   (:foreground ,builtin)
   nil)
  )
 )

(with-eval-after-load 'highlight-parentheses
  (setq hl-paren-colors '("#88C0D0" "#D08770" "#A3BE8C" "#EBCB8B")))
