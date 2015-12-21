;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL SETTINGS
;;;;;;;;;;;;;;;;;;;;

;; Extended load-path:
(add-to-list 'load-path (expand-file-name "/usr/share/emacs/site-lisp/org-mode/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/scala-mode"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/yasnippet"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/org2blog"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/config/"))

;; .emacs.d config layout:
(setq my-sessions-dir (expand-file-name "~/.emacs.d/sessions/"))
(setq my-backups-dir (expand-file-name "~/.emacs.d/backups/"))
(setq my-config-dir (expand-file-name "~/.emacs.d/config/"))
(setq my-stuff-dir (expand-file-name "~/.emacs.d/stuff/"))
(setq my-site-dir (expand-file-name "~/.emacs.d/site-lisp/"))
(setq my-tmp-dir (expand-file-name "~/.emacs.d/tmps/"))
(setq my-logs-dir (expand-file-name "~/.emacs.d/logs/"))

;; Maximize on start
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Pretty color theme:
(require 'color-theme)
(color-theme-initialize)
(color-theme-tango)
(set-frame-parameter (selected-frame) 'alpha '(97 97))

;; Customized custom file:
(setq custom-file (concat my-config-dir "custom.el"))

;; Common Lisp interaction:
(setq inferior-lisp-program "/usr/bin/sbcl")
(load (concat my-site-dir "quicklisp/slime-helper.el"))

;; Browser settings:
(setq browse-url-generic-program "firefox")
(setq browse-url-browser-function 'browse-url-generic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EDITOR SETTINGS
;;;;;;;;;;;;;;;;;;;;

;; Make Emacs sane:
(set-face-attribute 'default nil :height 75)  ; Reasonable font size.
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
(require 'dired-sync)

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
;;(pc-selection-mode t)                         ; Reasonable text selection.
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

(setq linum-mode-exclude-list                 ; Linum mode
      '(eshell-mode
        term-mode
        erc-mode
        org-agenda-mode
        ack-mode))

(defadvice linum-on (around linum-on-inhibit-for-modes)
  "Stop the load of linum-mode for some major modes."
  (unless (member major-mode linum-mode-exclude-list)
    ad-do-it))
(ad-activate 'linum-on)

(global-linum-mode t)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADDITIONAL SETTINGS
;;;;;;;;;;;;;;;;;;;;

;; Interactive do:
(require 'ido)
(setq ido-save-directory-list-file (concat my-tmp-dir "ido.lst"))
(setq ido-max-directory-size 100000)
(setq ido-enable-flex-matching t)
(ido-everywhere t)
(ido-mode 'both)                             ; Use ido for files and Iswitchb for buffers.
;;(iswitchb-mode t)                            ; Easier buffer switching.

(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
             (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Recently edited files management:
(require 'recentf)
(setq recentf-save-file (concat my-tmp-dir "recentf.lst"))
(setq recentf-auto-cleanup 'never)
(setq recentf-max-saved-items 100)
(setq recentf-max-menu-items 25)
(recentf-mode t)

(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; My Gamify config:
(require 'my-gamify-config)
(gamify-start)

;; My Org-Mode config:
(require 'my-org-config)

;; My ERC config:
(require 'my-erc-config)

;; My programming conig:
(require 'my-programming-config)

;; Session saving and restoring:
(require 'desktop)
(require 'desktop-menu)

(setq desktop-menu-autosave 600)
(setq desktop-menu-directory my-sessions-dir)
(setq desktop-menu-base-filename "session")
(setq desktop-menu-list-file "list")

(add-to-list 'desktop-modes-not-to-save 'erc)

(global-set-key (kbd "C-c s") 'desktop-menu)

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

;; Same-frame speedbar for convinience.
;;(require 'sr-speedbar)
;;(setq sr-speedbar-right-side nil)
;;(global-set-key (kbd "s-s") 'sr-speedbar-toggle)

;; Useful programming shortcuts:
(global-set-key (kbd "C-S-c") 'comment-region)
(global-set-key (kbd "C-S-u") 'uncomment-region)
(global-set-key (kbd "C-S-g") 'goto-line)
(global-set-key (kbd "<tab>") 'indent-relative)
(global-set-key (kbd "C-<tab>") 'hs-toggle-hiding)
(global-set-key (kbd "C-M-S-x") 'replace-last-sexp)         ; Replace previous sexp with its value.
(global-set-key (kbd "RET") 'newline-and-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STATS'N'SHIT
;;;;;;;;;;;;;;;;;;;;

(require 'battery)
(setq battery-status-function 'battery-linux-sysfs)
(setq battery-mode-line-format "#%b %p %t")   ; NOTE Used by custom mode-line-format, do not change.
(setq battery-load-critical 8)
(setq battery-load-low 25)
(setq battery-level-warning t)
(display-battery-mode t)

(require 'network-speed)                      ; NOTE Used by custom mode-line-format, do not change.
(setq network-speed-update-interval 5)
(setq network-speed-precision 1)
(setq network-speed-interface-list (if (string= (shell-command-to-string "ifconfig | grep wlp2s0") "")
                                       '("enp4s0")
                                     '("wlp2s0")))
(setq network-speed-format-string "%NI#%RB#%TB#%RX#%TX#%AX")
(network-speed-start)

(require 'cpu-stats)
(setq cpu-usage-update-interval 5)
(setq cpu-usage-format "%A %C0 %C1")
(cpu-usage-start)

(require 'memory-stats)
(setq memory-usage-update-interval 5)
(setq memory-usage-format "%R %F %S")
(memory-usage-start)

(require 'misc-stats)
;; NOTE Quite redundant.
;;(setq misc-stats-format "%T %D %E %S %L")
;;(misc-stats-start)

(defvar load-average-val (load-average))
(defvar load-average-update-interval 10)
(defvar load-average-timer ())

(defun load-stats-start ()
  "Start displaying load average stats."
  (interactive)
  (setq load-average-timer (run-at-time load-average-update-interval
                                        load-average-update-interval
                                        (lambda ()
                                          (setq load-average-val (load-average))
                                          (force-mode-line-update)
                                          (sit-for 0)))))

(defun load-stats-stop ()
  "Stop displaying load average stats."
  (interactive)
  (setq load-average-timer (and load-average-timer
                                (cancel-timer load-average-timer))))

(load-stats-start) ; Start it!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODELINE ; TODO Don't load images in text-only sessions.
;;;;;;;;;;;;;;;;;;;;

;;; Faces:

(defmacro alias-face (name face)
  "Creates an alias `name' to face `face'."
  `(progn (defface ,name '((default :inherit ,face :slant r))
            "A macro-defined alias face."
            :group 'default)
          (defvar ,name ',name)))

(alias-face my-green-face  font-lock-constant-face)
(alias-face my-yellow-face font-lock-function-name-face)
(alias-face my-red-face    font-lock-warning-face)

(alias-face my-important-face   font-lock-keyword-face)
(alias-face my-unimportant-face font-lock-comment-face)
(alias-face my-note-face        font-lock-doc-face)

(defvar my-battery-status-alist '(("#" "discharging" my-unimportant-face)
                                  ("#+" "charging" my-green-face)
                                  ("#-" "low" my-yellow-face)
                                  ("#!" "critically low" my-red-face)))

(defvar my-vc-alist '((ignored "Ignored" my-unimportant-face)
                      ;; Everything is ok:
                      (up-to-date "Up to date" my-green-face)
                      ;; Kinda important:
                      (unregistered "Unknown" my-yellow-face)
                      (edited "Edited" my-yellow-face)
                      (added "Added" my-yellow-face)
                      ;; Most important:
                      (removed "Scheduled for removal" my-red-face)
                      (conflict "Has conflicts" my-red-face)
                      (missing "Missing" my-red-face)
                      (needs-update "Needs update" my-red-face)
                      (needs-merge "Needs merge" my-red-face)
                      (unlocked-changes "Has unlocked changes" my-red-face)))

(defvar my-load-average-threshold 5.0)

(setq-default mode-line-format
              (list "-"
                    'mode-line-mule-info
                    '(:eval (cond (buffer-read-only
                                   (propertize "%%%%" 'face 'my-yellow-face
                                               'help-echo "Buffer is read-only."
                                               'mouse-face 'mode-line-highlight))
                                  ((buffer-modified-p)
                                   (propertize "**" 'face 'my-red-face
                                               'help-echo "Buffer has been modified."
                                               'mouse-face 'mode-line-highlight))
                                  (t (propertize "--" 'help-echo "Buffer is unmodified."
                                                 'mouse-face 'mode-line-highlight))))
                    '(:eval (propertize "%@" 'help-echo (concat "Default directory is: " default-directory)
                                        'mouse-face 'mode-line-highlight))

                    " "
                    '(:eval (let ((file-name (buffer-file-name)))
                              (if file-name
                                  (let ((state-face (assoc (vc-state file-name) my-vc-alist))
                                        (revision (vc-working-revision file-name))
                                        (backend (vc-backend file-name)))
                                    (propertize (concat "%b"
                                                        (when revision
                                                          (concat "/" revision)))
                                                'face (caddr state-face)
                                                'mouse-face 'mode-line-highlight
                                                'help-echo (concat file-name "\n"
                                                                   (if backend
                                                                       (concat "Version controlled by "
                                                                               (symbol-name backend) "\n"
                                                                               "Status: " (cadr state-face))
                                                                     "Not version controlled."))))
                                "%b")))

                    ;;'mode-line-position
                    '(:eval (propertize " %c,%l (%p) "
                                        'help-echo "TODO"
                                        'mouse-face 'mode-line-highlight))

                    "%[["
                    ;; NOTE Fucking Org-Agenda, man...
                    '(:eval (let* ((name (if (stringp mode-name) mode-name (car mode-name)))
                                   (rest (when (listp mode-name) (cdr mode-name))))
                              (list (propertize name 'face 'my-important-face)
                                    rest)))

                    'minor-mode-alist
                    "%]]"

                    ;; Current battery status.
                    ;; NOTE Required some hacking - defining custom format-battery-string breaks transient- mark-mode.
                    '(:eval (let* ((battery (split-string battery-mode-line-string))
                                   (battery-level (string-to-number (cadr battery))))
                              (unless (or (not battery) (string= (cadr battery) "N/A"))
                                (if (>= battery-level battery-load-critical)
                                    (when (not battery-level-warning)
                                      (setq battery-level-warning t))
                                  (when battery-level-warning
                                    (setq battery-level-warning nil)
                                    (notify-send "Battery critically low!"
                                                 (format "Time remaining: %sh"
                                                         (caddr battery)))))
                                (propertize (concat " " (cadr battery) "%%")
                                            ;; NOTE battery-status-function inconvinience workarround.
                                            'face (caddr (assoc (cond ((string= (car battery) "#+") "#+")
                                                                      ((<= battery-level battery-load-critical) "#!")
                                                                      ((<= battery-level battery-load-low) "#-")
                                                                      (t (car battery)))
                                                                my-battery-status-alist))
                                            'help-echo (format "Battery status: %s\nTime remaining: %sh"
                                                               (cadr (assoc (car battery)
                                                                            my-battery-status-alist))
                                                               (caddr battery))
                                            'mouse-face 'mode-line-highlight))))

                    ;; CPU usage.
                    '(:eval (let* ((usage (split-string cpu-usage-mode-line-string))
                                   (usage-level (string-to-number (or (car usage) "0"))))
                              (propertize (format " %d%s" usage-level "%%")
                                          'face (cond ((>= usage-level 90.0) 'my-red-face)
                                                      ((>= usage-level 75.0) 'my-yellow-face)
                                                      (t 'my-unimportant-face))
                                          'mouse-face 'mode-line-highlight
                                          'help-echo (concat "Usage:\n"
                                                             "CPU0: " (cadr usage) "%\n"
                                                             "CPU1: " (caddr usage) "%\n"
                                                             "Average: " (car usage) "%\n"))))

                    ;; RAM usage.
                    '(:eval (let* ((usage (split-string memory-usage-mode-line-string))
                                   (free-mem (string-to-number (or (cadr usage) "0"))))
                              (propertize (concat " " (car usage) "%%")
                                          'face (cond ((<= free-mem 256) 'my-red-face)
                                                      ((<= free-mem 512) 'my-yellow-face)
                                                      (t 'my-unimportant-face))
                                          'mouse-face 'mode-line-highlight
                                          'help-echo (concat "Main memory:\n"
                                                             "Usage: " (car usage) "%\n"
                                                             "Free: " (cadr usage) " MB\n"
                                                             "Swap:\n"
                                                             "Usage: " (caddr usage) "%\n"))))

                    ;; Current system load average.
                    '(:eval (let* ((load (mapcar (lambda (x) (/ x 100.0))
                                                 load-average-val))
                                   (load5 (cadr load)))
                              (propertize (concat " " (number-to-string load5))
                                          'face (cond ((>= load5 my-load-average-threshold)
                                                       'my-red-face)
                                                      ((>= load5 (/ my-load-average-threshold 2.0))
                                                       ' my-yellow-face)
                                                      (t
                                                       'my-unimportant-face))
                                          'help-echo (format (concat "Average system load:\n"
                                                                     "1 minute: %s\n"
                                                                     "5 minutes: %s\n"
                                                                     "15 minutes: %s")
                                                             (car load)
                                                             (cadr load)
                                                             (caddr load))
                                          'mouse-face 'mode-line-highlight)))

                    ;; Network load.
                    '(:eval (let ((net (split-string network-speed-mode-line-string "#")))
                              (propertize (concat " " (nth 5 net))
                                          'face 'my-unimportant-face
                                          'help-echo (format (concat "Interface: %s\n"
                                                                     "Received bytes: %s\n"
                                                                     "Transmitted bytes: %s\n"
                                                                     "Download speed: %s\n"
                                                                     "Upload speed: %s")
                                                             (car net)
                                                             (nth 1 net)
                                                             (nth 2 net)
                                                             (nth 3 net)
                                                             (nth 4 net))
                                          'mouse-face 'mode-line-highlight)))

                    ;; Pomodoro and current uptime.
                    ;; NOTE Required some hacking as it breaks transient mark mode...
                    ;; '(:eval (let* ((help-str (concat (format-time-string "Time: %Y-%m-%d %H:%M:%S\n")
                    ;;                                  (format-sut-string "SUT: %Y-%m-%d")
                    ;;                                  "\n"
                    ;;                                  (emacs-uptime "Emacs uptime: %H, %M\n")
                    ;;                                  (format-seconds
                    ;;                                   "System uptime: %H, %M"
                    ;;                                   (- (float-time (current-time))
                    ;;                                      (string-to-number
                    ;;                                       (shell-command-to-string
                    ;;                                        "cat /proc/stat | awk '{if(NR == 6) print $2}'"))))))
                    ;;                (image "")
                    ;;                (str (emacs-uptime "%h:%.2m"))
                    ;;                (face 'my-unimportant-face))
                    ;;           (when (pomodoro-running-p)
                    ;;             (let* ((ps (split-string pomodoro-display-string))
                    ;;                    (phase (car ps))
                    ;;                    (set (cadr ps))
                    ;;                    (time-left (string-to-number (caddr ps)))
                    ;;                    (time-str (format "%d:%02d" (/ time-left 60) (% time-left 60))))
                    ;;               (setq image "⌛")
                    ;;               (setq str time-str)
                    ;;               (setq face (if (string= phase "W")
                    ;;                              (cond ((<= time-left pomodoro-warn-time)
                    ;;                                     'my-red-face)
                    ;;                                    ((<= time-left (* 2 pomodoro-warn-time))
                    ;;                                     'my-yellow-face))
                    ;;                            'my-green-face))
                    ;;               (setq help-str (concat help-str
                    ;;                                      (format "\n\nPomodoro: %s\nSet: %s\nTime left: %s"
                    ;;                                              (pomodoro-current-state)
                    ;;                                              set
                    ;;                                              time-str)))))
                    ;;           (propertize (concat " " image str)
                    ;;                       'face face
                    ;;                       'help-echo help-str
                    ;;                       'mouse-face 'mode-line-highlight)))

                    ;; Gamify stats
                    '(:eval (let* ((stats (split-string gamify-mode-line-string))
                                   (xp (if gamify-focus-stats
                                           (nth 3 stats)
                                         (car stats)))
                                   (xp-val (string-to-number xp))
                                   (total (nth 2 stats))
                                   (level (nth 1 stats)))
                              (propertize (concat " " xp "%%")
                                          'face (cond ((>= xp-val 95.0) 'my-green-face)
                                                      ((<= xp-val 5.0) 'my-red-face)
                                                      (t 'my-unimportant-face))
                                          'help-echo (concat (format "You are %s with %s total XP.\n\n" level total)
                                                             (gamify-get-pretty-stats gamify-skip-levels))
                                          'mouse-face 'mode-line-highlight)))

                    ;; ERC track with minimal distraction.
                    '(:eval (progn (setq icon-title-format
                                         (setq frame-title-format
                                               (concat "Emacs" erc-modified-channels-object)))
                                   erc-modified-channels-object))
                    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTILS
;;;;;;;;;;;;;;;;;;;;

(require 'misc-utils)

(defun array-index-of (element array)
  (cl-labels ((iterate (index)
                       (when (< index (length array))
                         (if (equal element (aref array index))
                             index
                           (iterate (+ 1 index))))))
    (iterate 0)))

(defun tr-text (from to text)
  (map 'string
       (lambda (letter)
         (let ((index (array-index-of letter from)))
           (if index
               (aref to index)
             letter)))
       text))

(defun prettify-text (text)
  "Prettifies text using unicode math symbols."
  (tr-text "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
           "𝔞𝔟𝔠𝔡𝔢𝔣𝔤𝔥𝔦𝔧𝔨𝔩𝔪𝔫𝔬𝔭𝔮𝔯𝔰𝔱𝔲𝔳𝔴𝔵𝔶𝔷𝕬𝕭𝕮𝕯𝕰𝕱𝕲𝕳𝕴𝕵𝕶𝕷𝕸𝕹𝕺𝕻𝕼𝕽𝕾𝕿𝖀𝖁𝖂𝖃𝖄𝖅"
           text))

(defun replace-last-sexp ()
  "Evaluates and replaces the previous sexp with its value. Similar to `C-u C-x C-e'."
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))

(defun limit (str len &optional elipsis)
  (if (> (length str) len)
      (let ((part (substring str 0 (min (length str) (max (- len (length elipsis)) 0)))))
        (concat part elipsis))
    str))

;; Don't use a pop-up dialog box asking for the passphrase.
(when (file-executable-p "/usr/bin/gpg1")
  (setq epg-gpg-program "/usr/bin/gpg1"))

(defadvice epg--start (around advice-epg-disable-agent activate)
  "Make epg--start not able to find a gpg-agent"
  (let ((agent (getenv "GPG_AGENT_INFO")))
    (setenv "GPG_AGENT_INFO" nil)
    ad-do-it
    (setenv "GPG_AGENT_INFO" agent)))

(defun ddgo (keywords)
  "Search DuckDuckGo for the selection region of text.  If no
region is selected then search for the word at point, prompting
the user to make sure his query is correct."
  (interactive
   (list
    (if (use-region-p)
        (buffer-substring (region-beginning) (region-end))
      (read-string "Search DuckDuckGo for: " (propertize (format "%s" (thing-at-point 'word))
                                                         'read-only nil)))))
  (browse-url (concat "http://duckduckgo.com/?q="
                      (replace-regexp-in-string
                       "[[:space:]]+"
                       "+"
                       keywords))))

(defun dashboard ()
  (interactive)
  (setq w3m-default-display-inline-images t)
  (w3m-browse-url "localhost/PiWL/index.html"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INIT
;;;;;;;;;;;;;;;;;;;;

;; Override the defaults with customized values:
(load custom-file)

;; Load Org-Agenda buffer and center on the refile list:
(find-file "~/org/refile.org")
(add-hook 'window-setup-hook 'org-agenda-list)
(put 'set-goal-column 'disabled nil)

;; Start the Emacs Server:
(server-start)
