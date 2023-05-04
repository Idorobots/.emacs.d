(deftheme custom-org-mode
  "Created 2023-05-04.")

(custom-theme-set-faces
 'custom-org-mode
 '(org-hide ((t (:foreground "gray10"))))
 '(org-level-2 ((t (:inherit 'outline-4 :weight bold))))
 '(org-level-4 ((t (:inherit 'outline-6))))
 '(org-level-5 ((t (:inherit 'outline-8))))
 '(org-level-6 ((t (:inherit 'outline-4))))
 '(org-level-8 ((t (:foreground "#76ab35"))))
 '(org-headline-done ((t (:foreground "dim gray")))))

(provide-theme 'custom-org-mode)
