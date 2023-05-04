(deftheme custom-erc-mode
  "Created 2023-05-04.")

(custom-theme-set-faces
 'custom-erc-mode
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
  '(fg:erc-color-face0 ((t (:foreground "White")))))

(provide-theme 'custom-erc-mode)
