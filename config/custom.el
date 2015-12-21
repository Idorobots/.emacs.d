
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-latex-default-packages-alist
   (quote
    (("T1" "fontenc" t)
     ("" "fontspec")
     ("" "xunicode")
     ("" "xltxtra")
     ("" "fixltx2e" nil)
     ("" "graphicx" t)
     ("" "longtable" nil)
     ("" "float" nil)
     ("" "wrapfig" nil)
     ("" "rotating" nil)
     ("normalem" "ulem" t)
     ("" "amsmath" t)
     ("" "textcomp" t)
     ("" "marvosym" t)
     ("" "wasysym" t)
     ("" "amssymb" t)
     ("" "hyperref" nil)
     "\\tolerance=1000")))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "evince %s"))))
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-action-face ((t (:inherit outline-1 :weight bold))))
 '(erc-bold-face ((t (:weight bold))))
 '(erc-command-indicator-face ((t (:weight bold))))
 '(erc-current-nick-face ((t (:inherit font-lock-warning-face :weight bold))))
 '(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
 '(erc-default-face ((t nil)))
 '(erc-direct-msg-face ((t (:inherit outline-4))))
 '(erc-error-face ((t (:inherit font-lock-warning-face))))
 '(erc-fool-face ((t (:inherit outline-4 :weight bold))))
 '(erc-input-face ((t nil)))
 '(erc-keyword-face ((t (:inherit outline-5 :weight bold))))
 '(erc-my-nick-face ((t (:inherit outline-4 :slant italic :weight bold))))
 '(erc-nick-default-face ((t (:weight bold))))
 '(erc-nick-msg-face ((t (:inherit outline-4 :weight bold))))
 '(erc-notice-face ((((class color) (min-colors 88)) (:inherit outline-3 :weight bold))))
 '(erc-pal-face ((t (:inherit outline-3 :weight bold))))
 '(erc-timestamp-face ((t (:foreground "PaleGreen" :weight bold))))
 '(fg:erc-color-face0 ((t (:foreground "White"))))
 '(flymake-errline ((((class color) (background dark)) (:background "Firebrick3"))))
 '(flymake-warnline ((((class color) (background dark)) (:background "DarkOrange3"))))
 '(mode-line ((t (:background "#555753" :foreground "#eeeeec" :weight light))))
 '(newsticker-date-face ((t (:slant italic :height 0.8))))
 '(newsticker-feed-face ((t (:foreground "misty rose" :weight bold :height 1.2))))
 '(newsticker-immortal-item-face ((((class color) (background dark)) (:foreground "orange" :slant italic :weight bold))))
 '(newsticker-new-item-face ((t (:weight bold))))
 '(newsticker-obsolete-item-face ((t (:strike-through t :weight bold))))
 '(newsticker-old-item-face ((((class color) (background dark)) (:foreground "orange3" :weight bold))))
 '(newsticker-statistics-face ((t (:slant italic :height 0.8))))
 '(newsticker-treeview-face ((t (:inherit default :foreground "misty rose"))))
 '(org-hide ((((background dark)) (:foreground "gray10"))))
 '(org-level-2 ((t (:inherit outline-4 :weight bold))))
 '(org-level-4 ((t (:inherit outline-6))))
 '(org-level-5 ((t (:inherit outline-8))))
 '(org-level-6 ((t (:inherit outline-4))))
 '(org-level-8 ((t (:foreground "#76ab35")))))
