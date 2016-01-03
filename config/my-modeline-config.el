;;; my-modeline-config.el --- My modeline config.

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
;; MODELINE
;;;;;;;;;;;;;;;;;;;;

;;; Faces:

(set-face-attribute 'mode-line nil
                    :background "#555753"
                    :foreground "#eeeeec"
                    :weight 'light)

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

(provide 'my-modeline-config)
