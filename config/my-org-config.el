;;; my-org-config.el --- My Org-Mode config.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL
;;;;;;;;;;;;;;;;;;;;

(require 'org)
(require 'org-install)

;; Org file autoload patterns:
(add-to-list 'auto-mode-alist '("TODO$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("ARCHIVE$" . org-mode))

;; File opening:
(setq org-file-apps
      '((auto-mode . emacs)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . default)
        ("\\.pdf\\'" . "evince %s")))

;; Editing settings:
(setq org-completion-use-ido t)

;; Display settings:
(setq org-tags-column -100)
(setq org-hide-leading-stars t)
(setq org-hide-block-startup t)
(setq org-startup-folded 'content)
(setq org-log-into-drawer 't)
(setq org-element-use-cache 't)

(add-hook 'org-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (visual-line-mode t)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d!)")
        (sequence "SUSPENDED(s@/!)" "|" "CANCELLED(c@)" "DELEGATED(D@)")))

(setq org-todo-keyword-faces
      '(("SUSPENDED" . my-yellow-face)
        ("CANCELLED" . my-red-face)))

;; Display faces:
(require 'color-theme)

(color-theme-install-faces
 '((org-hide ((t (:foreground "gray10"))))
   (org-level-2 ((t (:inherit 'outline-4 :weight bold))))
   (org-level-4 ((t (:inherit 'outline-6))))
   (org-level-5 ((t (:inherit 'outline-8))))
   (org-level-6 ((t (:inherit 'outline-4))))
   (org-level-8 ((t (:foreground "#76ab35"))))
   (org-headline-done ((t (:foreground "dim gray"))))))

;; Auto-suspend SUSPENDED tasks:
(defmacro deftoggler (name state)
  `(defun ,name (arg)
     (when (and (equal (plist-get arg :type) 'todo-state-change)
                (or (equal (plist-get arg :to) ,state)
                    (equal (plist-get arg :from) ,state)))
       (goto-char (plist-get arg :position))
       (save-excursion
         (save-restriction
           (org-narrow-to-subtree)
           (when (re-search-forward org-keyword-time-regexp nil t)
             (org-toggle-timestamp-type)))))
     t))

(deftoggler my-toggle-suspend "SUSPENDED")

(add-hook 'org-trigger-hook 'my-toggle-suspend)

;; Links and hyperrefs:
(setq org-return-follows-link t)
(setq org-link-search-must-match-exact-headline nil)

;; Pretty images in a buffer: ;; TODO Make this org-mode local.
(global-set-key (kbd "M-i") 'org-toggle-inline-images)

;; Other modes:
(require 'autopair)
(add-hook 'org-mode-hook 'autopair-mode)

(require 'whitespace)
(add-hook 'org-mode-hook 'whitespace-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AGENDA
;;;;;;;;;;;;;;;;;;;;

(require 'org-agenda)

;; General settings:
(global-set-key [f12] 'org-agenda)

;; Displaying the agenda:
(setq org-agenda-tags-column -110)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-timestamp-if-done t)
(setq org-agenda-span 'day)
(setq org-agenda-log-mode-items '(closed clock state))
(setq org-agenda-current-time-string #("------NOW------" 0 15 (org-heading t)))
(setq org-agenda-time-grid '((daily today require-timed remove-match)
                             (700 800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300)
                             #("......" 0 6 (org-heading t))
                             #("---------------" 0 15 (org-heading t))))
(setq org-agenda-start-on-weekday nil)            ; Start on 'today.
(setq org-agenda-scheduled-leaders '("" "%dd. ago: "))
(setq org-agenda-deadline-leaders '("" "In %dd.: " "%dd. ago: "))

;; Org-Agenda files stored in one place:
(setq org-agenda-files '("~/org/"))
(setq org-archive-location "~/org/ARCHIVE::")
(setq org-default-notes-file "~/org/refile.org")

;; Refile should ignore DONE tasks:
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a DONE state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; Other refile settings:
(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))
(setq org-refile-use-outline-path nil)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Org capture:
(setq org-capture-templates
      '(("t" "New TODO item." entry (file "")
         "* TODO %?\n  SCHEDULED: %t\n:PROPERTIES:\n:capture_time: %U\n:context: %a\n:END:\n")
        ("n" "New note to self." entry (file+olp "~/org/doodles.org" "Notes")
         "* %?\n:PROPERTIES:\n:capture_time: %U\n:context: %a\n:END:\n")
        ("b" "Stuff to blog about." entry (file+olp "~/org/blog.org" "POSTS IDEAS" "Other")
         "* SUSPENDED %?\n:PROPERTIES:\n:capture_time: %U\n:context: %a\n:END:\n")
        ("B" "Stuff to buy." entry (file+olp "~/org/stock.org" "BUY" "Other" "Other")
         "* %?\n:PROPERTIES:\n:capture_time: %U\n:context: %a\n:END:\n")
        ("R" "Stuff to read." entry (file+headline "~/org/someday.org" "READING")
         "* %?\n:PROPERTIES:\n:capture_time: %U\n:context: %a\n:END:\n")))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-M-r") 'org-capture)

                                        ; Additional Agenda commands:
(setq org-agenda-custom-commands
      '(("S" "Suspended tasks." todo "SUSPENDED")))

;; Use appt for agenda notifications:
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

(require 'appt)

(defadvice org-agenda-to-appt (before wickedcool activate)
  "Clear the appt-time-msg-list."
  (setq appt-time-msg-list nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SPACED REPETITION
;;;;;;;;;;;;;;;;;;;;

(add-to-list 'org-capture-templates
             `("r" "New spaced repetition item." entry (file "~/org/repeat.org")
               ,(concat "* TODO %? %^G\n"
                        "  SCHEDULED: %(spaced-repetition-next-time 1)\n"
                        ":PROPERTIES:\n"
                        ":spaced_repetition: 1\n"
                        ":gamify_exp: (1 1)\n"
                        ":capture_time: %U\n"
                        ":context: %a\n"
                        ":END:\n")))

(defvar spaced-repetition-property "spaced_repetition")
(defvar spaced-repetition-time-format "%Y-%m-%d %a")

(defun fibonacci (n)
  (round (/ (expt 1.6180339887 n) (sqrt 5))))

(defun spaced-repetition-next-time (num-repeats &optional time)
  (format "<%s +%dd>"
          (format-time-string spaced-repetition-time-format
                              (or time (current-time)))
          (fibonacci num-repeats)))

(defun spaced-repetition-change-interval (arg)
  (when (and (equal (plist-get arg :type) 'todo-state-change)
             (or (equal (plist-get arg :to) "DONE")
                 (equal (plist-get arg :to) "CANCELLED")))
    (let* ((curr-time (current-time))
           (pos (plist-get arg :position))
           (done-p (equal (plist-get arg :to) "DONE"))
           (repetition (org-entry-get pos spaced-repetition-property)))
      (when repetition
        (goto-char pos)
        (save-excursion
          (save-restriction
            (save-match-data
              (org-narrow-to-subtree)
              (let ((rep (string-to-number repetition)))
                (when (re-search-forward org-scheduled-time-regexp nil t)
                  (replace-match (concat "SCHEDULED: "
                                         (spaced-repetition-next-time (if done-p rep 1) curr-time))
                                 nil nil))
                (org-set-property spaced-repetition-property
                                  (format "%d" (if done-p (+ 1 rep) 1))))))))))
  t) ;; Always ok.

;; NOTE Has to be in org-blocker-hook in order to replace scheduled-time correctly.
(add-hook 'org-blocker-hook 'spaced-repetition-change-interval)

;; Toggle repetition tasks
(defvar spaced-repetition-display-p t)

(defun my-skip-repeat ()
  (when (and (not spaced-repetition-display-p)
             (string= org-category "repeat"))
    (org-agenda-skip-entry-if 'nottodo 'done)))

(setq org-agenda-skip-function-global 'my-skip-repeat)

(defun spaced-repetition-toggle-display ()
  (interactive)
  (if spaced-repetition-display-p
      (setq spaced-repetition-display-p nil)
    (setq spaced-repetition-display-p t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GAMIFY
;;;;;;;;;;;;;;;;;;;;

(setq org-capture-templates
      (list* `("1" "New easy quest." entry (file "")
               ,(concat "* TODO %? %^G\nSCHEDULED: %t\n"
                        ":PROPERTIES:\n"
                        ":gamify_exp: %(gamify-assign-some-exp 5 2)\n"
                        ":capture_time: %U\n"
                        ":context: %a\n"
                        ":END:\n"))
             `("2" "New medium quest." entry (file "")
               ,(concat "* TODO %? %^G\nSCHEDULED: %t\n"
                        ":PROPERTIES:\n"
                        ":gamify_exp: %(gamify-assign-some-exp)\n"
                        ":capture_time: %U\n"
                        ":context: %a\n"
                        ":END:\n"))
             `("3" "New hard quest." entry (file "")
               ,(concat "* TODO %? %^G\nSCHEDULED: %t\n"
                        ":PROPERTIES:\n"
                        ":gamify_exp: %(gamify-assign-some-exp 30 10)\n"
                        ":capture_time: %U\n"
                        ":context: %a\n"
                        ":END:\n"))
             org-capture-templates))

(defun org-agenda-gamify-prefix ()
  (let* ((gamify-exp (org-entry-get nil gamify-exp-property))
         (gamify-achievement (org-entry-get nil gamify-achievement-property))
         (exp-val (when gamify-exp
                    (read gamify-exp))))
    (concat (cond ((numberp exp-val)
                   (number-to-string exp-val))
                  ((null exp-val) "")
                  ((listp exp-val)
                   (let ((base (car exp-val))
                         (delta (cadr exp-val)))
                     (format "~%d"
                             (+ (or base gamify-default-exp)
                                (/ (or delta gamify-default-exp-delta) 2))))))
            (if gamify-achievement "!" ""))))

(setf (cdr (assoc 'agenda org-agenda-prefix-format))
      (concat
       " %-3(org-agenda-gamify-prefix)"
       " %-12:c%?-12t%s"))

(eval-after-load "org-agenda"
  '(let ((map org-agenda-mode-map))
     (define-key map (kbd "x") 'gamify-org-agenda-tasks)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LATEX EXPORT
;;;;;;;;;;;;;;;;;;;;

;; Use XeLaTeX:
(setq org-latex-to-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode %f"
        "bibtex %b"
        "xelatex -shell-escape -interaction nonstopmode %f"))

(setq org-latex-default-packages-alist
      '(("T1" "fontenc" t)
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
        "\\tolerance=1000"))

(setq org-format-latex '(:foreground default
                         :background default
                         :scale 1.5
                         :html-foreground "Black"
                         :html-background "Transparent"
                         :html-scale 1.0
                         :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

(setq org-latex-pdf-process org-latex-to-pdf-process)

;; Use a sane way to cite stuff.
(org-add-link-type "cite" 'ebib
                   (lambda (path desc format)
                     (cond
                      ((eq format 'latex)
                       (format "\\cite{%s}" path)))))

(org-add-link-type "ref" 'erefs
                   (lambda (path desc format)
                     (cond
                      ((eq format 'latex)
                       (format "\\ref{%s}" path)))))

;; Pretty code in code fragments and LaTeX export:
(setq org-src-fontify-natively t)
(setq org-src-preserve-indentation t)
(setq org-export-latex-listings 'minted)
(setq org-latex-listings 'minted)

;; Fix inline latex images with minted.
(defun org-create-formula-image (string tofile options buffer)
  "This calls dvipng."
  (require 'org-latex)
  (let* ((tmpdir (if (featurep 'xemacs)
                     (temp-directory)
                   temporary-file-directory))
         (texfilebase (make-temp-name
                       (expand-file-name "orgtex" tmpdir)))
         (texfile (concat texfilebase ".tex"))
         (pdffile (concat texfilebase ".pdf"))
         (pngfile (concat texfilebase ".png"))
         (fnh (if (featurep 'xemacs)
                  (font-height (get-face-font 'default))
                (face-attribute 'default :height nil)))
         (scale (or (plist-get options (if buffer :scale :html-scale)) 1.0))
         (dpi (number-to-string (* scale (floor (* 0.9 (if buffer fnh 140.))))))
         (fg (or (plist-get options (if buffer :foreground :html-foreground))
                 "Black"))
         (bg (or (plist-get options (if buffer :background :html-background))
                 "Transparent")))
    (with-temp-file texfile
      (insert (org-splice-latex-header
               org-format-latex-header
               org-export-latex-default-packages-alist
               org-export-latex-packages-alist t
               org-format-latex-header-extra))
      (insert "\n\\usepackage[active,tightpage, xetex, displaymath]{preview}\n"
              (apply #'format
                     "\\definecolor{export-bg}{rgb}{%s, %s, %s}\n"
                     (cdr (split-string (org-dvipng-color :background))))
              (apply #'format
                     "\\definecolor{export-fg}{rgb}{%s, %s, %s}\n"
                     (cdr (split-string (org-dvipng-color :foreground))))
              "\\begin{document}\n"
              "\\pagecolor{export-bg}\n" ;; FIXME
              "\\begin{preview}\n"
              "\\special{x:backgroundcolor=export-bg}\n"
              "\\textcolor{export-fg}{" string "}\n"
              "\\end{preview}\n"
              "\\end{document}\n")
      (require 'org-latex)
      (org-export-latex-fix-inputenc))
    (let ((dir default-directory))
      (condition-case nil
          (progn
            (cd tmpdir)
            (call-process "xelatex" nil nil nil
                          "-shell-escape"
                          texfile))
        (error nil))
      (cd dir))
    (if (not (file-exists-p pdffile))
        (progn (message "Failed to create pdf file from %s" texfile) nil)
      (condition-case nil
          (call-process "convert" nil nil nil
                        pdffile
                        pngfile)
        (error nil))
      (if (not (file-exists-p pngfile))
          (if org-format-latex-signal-error
              (error "Failed to create png file from %s" texfile)
            (message "Failed to create png file from %s" texfile)
            nil)
        ;; Use the requested file name and clean up
        (copy-file pngfile tofile 'replace)
        (loop for e in '(".pyg" ".pdf" ".tex" ".aux" ".log" ".png") do
              (let ((f (concat texfilebase e)))
                (when (file-exists-p f)
                  (delete-file f))))
        pngfile))))

;; 8.2 is shiiiiiit:
(require 'ox)
(add-to-list 'org-export-filter-src-block-functions
             (lambda (data backend options)
               (replace-regexp-in-string "\t" "    " data)))

;; Org-Babel settings:
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (python . t)
   (gnuplot . t)
   (latex . t)
   (matlab . t)
   (ruby . t)
   (scheme . t)))

(provide 'my-org-config)
