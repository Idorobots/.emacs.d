;;; my-editor-config.el --- My Editor config.

;; Pretty themes:
(setq custom-safe-themes
   '("d4ab40cbb11eae102dc167b8da5f5ef2a8d3dd10081cec7e7d64e579836188ed"
     "c64b66821065a5c282f1eb41e3d98f2ef9e60f9214ef329dfe6ff80be56d0ea4"
     "911ff1eb36c021a4f172bd3ac48b4d64ac5ceff633e12398fcc5b7b420626b59"
     default))
(load-theme 'custom-org-mode t)
(load-theme 'custom-erc-mode t)
(load-theme 'old-dark-tango t)

(set-frame-parameter (selected-frame) 'alpha '(97 97))

;; Make Emacs sane:
(set-face-attribute 'default nil :height 90)  ; Reasonable font size.
(setq inhibit-startup-message t)
(setq next-line-add-newlines nil)             ; No newlines at the end of buffer.
(setq scroll-preserve-screen-position t)
(put 'overwrite-mode 'disabled t)             ; Last time I used it was when I couldn't turn it off.
(menu-bar-mode -1)                            ; No need for this.
(tool-bar-mode -1)                            ; ditto
(scroll-bar-mode -1)                          ; ditto
(fset 'yes-or-no-p 'y-or-n-p)
(setq savehist-file (concat my-sessions-dir "history"))
(savehist-mode 1)                             ; Save input history.
(global-unset-key (kbd "C-z"))                ; Unset the annoying minimize function.
(global-unset-key (kbd "C-t"))                ; Either this, or my sanity is lost forever.
(global-unset-key (kbd "M-t"))
(global-unset-key (kbd "C-M-t"))
(global-unset-key (kbd "C-x C-t"))
(global-unset-key (kbd "C-x C-n"))

;; Add some nifty features:
(require 'tramp)

;; Sensible C-a
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)

;; Text editing:
(line-number-mode t)
(column-number-mode t)

;; Make auto-indent sane...
(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (point-min) (point-max)))))
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

(global-display-line-numbers-mode)

(global-hl-line-mode t)                       ; Line highlighting.
(show-paren-mode t)                           ; Show matching parentheses.
(global-set-key [f5] 'call-last-kbd-macro)    ; Quite useful.

;; Modeline & minibuffer:
(setq display-time-24hr-format t)
(display-time-mode t)                         ; NOTE Needed by appt.
(icomplete-mode t)                            ; Completition in the minibuffer.

;; Backups:
(setq make-backup-files t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-old-versions 2)
(setq kept-new-versions 10)
(setq backup-directory-alist `((".*" . ,my-backups-dir)))

;; Bookmarks:
(setq bookmark-default-file (concat my-tmp-dir "bmk.lst"))

;; Remote editing:
(setq tramp-persistency-file-name (concat my-tmp-dir "tramp"))

;; Useful keyboard shortcuts:
(global-set-key (kbd "M-%") 'replace-regexp) ; Search and replace.
(global-set-key (kbd "C-t") 'ansi-term)      ; Much more useful.

;; Interactive do:
(require 'ido)
(setq ido-save-directory-list-file (concat my-tmp-dir "ido.lst"))
(setq ido-max-directory-size 100000)
(setq ido-enable-flex-matching t)
(ido-everywhere t)
(ido-mode 'both)                             ; Use ido for files and Iswitchb for buffers.

(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
             (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Recently edited files management:
(require 'recentf)
(setq recentf-save-file (concat my-tmp-dir "recentf.lst"))
(setq recentf-auto-cleanup 'never)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 25)
(recentf-mode t)

(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Browser settings:
(setq browse-url-generic-program "firefox")
(setq browse-url-browser-function 'browse-url-generic)

;; Session saving and restoring:
(require 'desktop)
(add-to-list 'desktop-modes-not-to-save 'erc)

;; Appointment tracking:
(require 'appt)
(setq appt-message-warning-time 10)
(setq appt-display-interval 5)
(setq appt-display-format 'window)            ; NOTE Needed in order to use notify-send.

;; A little façade-function for appt.
(defun my-appt-popup (min-to-app new-time msg)
  (notify-send (if (string= "0" min-to-app)
                   "A taks starts now."
                 (format "A task starts in %s minutes." min-to-app)) msg))

(setq appt-disp-window-function (function my-appt-popup))
(appt-activate t)                             ; NOTE Requires (display-time)

(provide 'my-editor-config)
