;;; my-erc-config.el --- My ERC config.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL
;;;;;;;;;;;;;;;;;;;;

(require 'erc)

;; ERC theme:
(require 'color-theme)

(color-theme-install-faces
 '((erc-action-face ((t (:inherit outline-1 :weight bold))))
   (erc-bold-face ((t (:weight bold))))
   (erc-command-indicator-face ((t (:weight bold))))
   (erc-current-nick-face ((t (:inherit font-lock-warning-face :weight bold))))
   (erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   (erc-default-face ((t nil)))
   (erc-direct-msg-face ((t (:inherit outline-4))))
   (erc-error-face ((t (:inherit font-lock-warning-face))))
   (erc-fool-face ((t (:inherit outline-4 :weight bold))))
   (erc-input-face ((t nil)))
   (erc-keyword-face ((t (:inherit outline-5 :weight bold))))
   (erc-my-nick-face ((t (:inherit outline-4 :slant italic :weight bold))))
   (erc-nick-default-face ((t (:weight bold))))
   (erc-nick-msg-face ((t (:inherit outline-4 :weight bold))))
   (erc-notice-face ((((class color) (min-colors 88)) (:inherit outline-3 :weight bold))))
   (erc-pal-face ((t (:inherit outline-3 :weight bold))))
   (erc-timestamp-face ((t (:foreground "PaleGreen" :weight bold))))
   (fg:erc-color-face0 ((t (:foreground "White"))))))

;; Basic stuff:
(erc-keep-place-mode t)                       ; Save the point of out-of-focus buffers.
(erc-button-mode nil)                         ; Too slow.
(setq erc-email-userid "nope")                ; Apparently ERC uses this as Username.

;; Completition and spelling:
(add-hook 'erc-mode-hook
          (lambda ()
            (pcomplete-erc-setup)
            (erc-completion-mode 1)))

(add-hook 'erc-after-connect
          (lambda (&rest skip)
            (erc-spelling-mode 1)
            (setq erc-spelling-dictionaries '(("localhost:6667" "polish")
                                              ("#stosowana" "polish")
                                              ("#lisp-pl" "polish")
                                              ("irc.freenode.net:6667" "american")))))

;; Tracking different events:
(require 'erc-track)

;; Don't track irrelevant events:
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
(setq erc-track-exclude-server-buffer t)
(setq erc-track-priority-faces-only 'all)
(setq erc-track-faces-priority-list '(erc-error-face
                                      erc-current-nick-face
                                      erc-keyword-face
                                      erc-nick-msg-face
                                      erc-direct-msg-face
                                      erc-dangerous-host-face
                                      erc-notice-face
                                      erc-prompt-face))

                                        ; Query buffers should be untouched.
(defadvice erc-track-modified-channels (around erc-track-modified-channels-promote-query activate)
  (if (erc-query-buffer-p) (setq erc-track-priority-faces-only 'nil))
  ad-do-it
  (if (erc-query-buffer-p) (setq erc-track-priority-faces-only 'all)))

(erc-track-mode t)                ;; NOTE Has to be setup before erc-stamp for some reason.
(setq erc-track-when-inactive t)  ;; NOTE Has to be set after erc-track. Dunno, I'm just hacking here.

;; Highlighting stuff in the messages:
(require 'erc-match)
(setq erc-current-nick-highlight-type 'keyword)

(defun erc-cmd-TRACK (keyword)                     ; For simple topic tracking.
  (interactive)
  (unless (member keyword erc-keywords)
    (add-to-list 'erc-keywords keyword)
    (message (concat "Tracking `" keyword "'."))))

(defun erc-cmd-UNTRACK (keyword)
  (interactive)
  (when (member keyword erc-keywords)
    (setq erc-keywords (delete keyword erc-keywords))
    (message (concat "Not tracking `" keyword "' anymore."))))

(defun erc-cmd-LENNY (&rest args)
  (erc-send-action (erc-default-target) "( ͡° ͜ʖ ͡°)"))

(setq erc-keywords `((,erc-button-url-regexp (:inherit link)) ; Better url display.
                     "Clojure" "clojure" "Emacs" "emacs" "Erlang" "erlang"
                     "Lisp" "lisp" "Racket" "racket" "Scheme" "scheme"))

(erc-match-mode t) ;; NOTE Has to be setup before erc-stamp for some reason.

;; Pretty format the timestamp:
(require 'erc-stamp)
(setq erc-hide-timestamps nil)                     ; Don't hide the timestamp.
(setq erc-timestamp-only-if-changed-flag nil)      ; Always show it.
(setq erc-timestamp-format "(%H:%M:%S) ")          ; Use this formatting for time.

(make-variable-buffer-local
 (defvar my-erc-last-datestamp nil))              ; Format used for the date.
(defvar my-erc-datestamp-format "The date is %d.%m.%Y.\n")

;; FIXME Fix this or commit a bugfix to erc-stamp.el
(defun old-erc-insert-timestamp-left (string)      ; A massive hack to displaf the date :S
  "Insert timestamps at the beginning of the line.
   This function is taken from erc-stamp.el as is for hacking purposes."
  (goto-char (point-min))
  (let* ((ignore-p (and erc-timestamp-only-if-changed-flag
                        (string-equal string erc-timestamp-last-inserted)))
         (len (length string))
         (s (if ignore-p (make-string len ? ) string)))
    (unless ignore-p (setq erc-timestamp-last-inserted string))
    (erc-put-text-property 0 len 'field 'erc-timestamp s)
    (erc-put-text-property 0 len 'invisible 'timestamp s)
    (insert s)))

(defun erc-insert-timestamp-left (string)       ; A little hack to display the date.
  "Redefines the default function in order to provide full functionality.
   Different named function seemed to limit message formatting for some reason."
  (old-erc-insert-timestamp-left string)
  (let* ((time (current-time))
         (datestamp (erc-format-timestamp time my-erc-datestamp-format)))
    (unless (string= datestamp my-erc-last-datestamp)
      (setq my-erc-last-datestamp datestamp)
      (old-erc-insert-timestamp-left (concat string datestamp)))))

;;(setq erc-insert-timestamp-function 'my-erc-insert-timestamp-left)
(setq erc-insert-timestamp-function 'erc-insert-timestamp-left)

(add-hook 'erc-after-connect                       ; Dynamic sizing!
          (lambda (server nick)
            (setq erc-fill-column (- (window-width) 4))))

(erc-timestamp-mode t)

;; Manage autojoining:
(require 'erc-join)
(setq erc-autojoin-channels-alist '(("freenode.net"
                                     "#clojure"
                                     "#d"
                                     "#erlang"
                                     "#lisp-pl"
                                     "#stosowana")
                                    ("localhost" "&bitlbee")))

;; Make channels floodable by default.
(setq erc-server-flood-penalty 0)

;; Autojoin on KICK:
(setq erc-server-KICK-functions (lambda (proc msg)
                                  (erc-server-KICK proc msg)
                                  (erc-autojoin-channels (erc-compute-server)
                                                         (erc-compute-nick))))

;; Autojoin on netsplit:
(require 'erc-netsplit)
(erc-netsplit-mode t)

;; Logging:
(require 'erc-log)
(setq erc-log-channels-directory my-logs-dir)
(setq erc-log-insert-log-on-open nil)
(setq erc-save-buffer-on-part nil)
(setq erc-save-queries-on-quit nil)
(setq erc-log-write-after-send nil)
(setq erc-log-write-after-insert nil)
(setq erc-log-file-coding-system 'utf-8)

(defun my-erc-log-file-name (buffer target nick server port)
  (let ((file (concat (or (with-current-buffer buffer (buffer-name))
                          target
                          "unknown")
                      "@"
                      (or (with-current-buffer buffer (erc-network-name))
                          server
                          "unknown")
                      ".txt")))
    (convert-standard-filename file)))

(setq erc-generate-log-file-name-function 'my-erc-log-file-name)
(erc-log-mode t)

(defvar erc-log-auto-save-interval 120)
(defvar erc-log-timer nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Speedbar integration
;;;;;;;;;;;;;;;;;;;;

(require 'erc-speedbar)
(require 'speedbar)

(define-key speedbar-mode-map "E"
  (lambda ()
    (interactive)
    (speedbar-change-initial-expansion-list "ERC")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LAYOUT
;;;;;;;;;;;;;;;;;;;;

(require 'html2text)

;; Display "nick:"  instead of "<nick>":
(defun erc-format-privmessage (nick msg privp msgp)
  (let ((nicky (if msgp
                   nick
                 (concat "** " nick )))
        (nick-face (if privp
                       'erc-nick-msg-face
                     'erc-nick-default-face))
        (msg-face (if privp
                      'erc-direct-msg-face
                    'erc-default-face)))
    (concat (erc-propertize nicky 'face nick-face)
            (erc-propertize ": " 'face msg-face)
            (erc-propertize (html2text-string msg) 'face msg-face))))

(defun erc-format-my-nick ()
  (let ((nick (if erc-show-my-nick
                  (erc-current-nick)
                "Pan i Władca")))
    (concat (erc-propertize nick 'face 'erc-my-nick-face)
            (erc-propertize ": " 'face 'erc-default-face))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BITLBEE TODO Parse the BitlBee config files.
;;;;;;;;;;;;;;;;;;;;

(setq bitlbee-is-running-p nil)
(setq bitlbee-config-dir (expand-file-name "~/.bitlbee"))

(defun bitlbee-start-if-not-running ()
  (unless (or bitlbee-is-running-p
              ;;(is-process-running "bitlbee")
              )
    (setq bitlbee-is-running-p t)
    (shell-command (concat "bitlbee -F -d "
                           bitlbee-config-dir))))

(defun bitlbee-add-buddies ()
  (interactive)
  (erc-message "PRIVMSG" "&bitlbee yes")
  (erc-message "PRIVMSG" "&bitlbee yes")
  (dolist (buddy bitlbee-buddy-list)
    (erc-message "PRIVMSG" (format "&bitlbee add gg %s %s"  ;; FIXME Another hack to go around BitlBees
                                   (car buddy)              ;; FIXME problems with Gadu-Gadu protocol.
                                   (cdr buddy))))
  (erc-message "PRIVMSG" "&bitlbee blist all")
  (message "Buddy list loaded!"))

(add-hook 'erc-join-hook
          (lambda ()
            (when (and (string= "localhost" erc-session-server)
                       (string= "&bitlbee" (buffer-name)))
              (bitlbee-add-buddies))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTILS
;;;;;;;;;;;;;;;;;;;;

(defun start-bitlbee (&optional port)
  "Start wasting time on BitlBee..."
  (interactive)
  (bitlbee-start-if-not-running)
  (require 'secrets secrets-file)
  ;; Enable sane logging:
  (and erc-log-timer (cancel-timer erc-log-timer))
  (setq erc-log-timer (run-at-time erc-log-auto-save-interval
                                   erc-log-auto-save-interval
                                   (lambda ()
                                     (erc-log-save-all-buffers)
                                     (sit-for 0))))
  (erc :server "localhost"
       :port (if port port 6667)
       :nick bitlbee-username
       :password bitlbee-pass))

(defun start-irc (&optional port)
  "Start to waste time on IRC with ERC."
  (interactive)
  (require 'secrets secrets-file)
  (require 'tls)
  ;; Enable sane logging:
  (and erc-log-timer (cancel-timer erc-log-timer))
  (setq erc-log-timer (run-at-time erc-log-auto-save-interval
                                   erc-log-auto-save-interval
                                   (lambda ()
                                     (erc-log-save-all-buffers)
                                     (sit-for 0))))
  (erc-tls :server "irc.freenode.net"
           :port (if port port 6697)
           :nick freenode-username
           :password freenode-pass))

(defun start-slack (&optional port)
  "Start to waste time on IRC with ERC."
  (interactive)
  (require 'secrets secrets-file)
  (require 'tls)
  ;; Enable sane logging:
  (and erc-log-timer (cancel-timer erc-log-timer))
  (setq erc-log-timer (run-at-time erc-log-auto-save-interval
                                   erc-log-auto-save-interval
                                   (lambda ()
                                     (erc-log-save-all-buffers)
                                     (sit-for 0))))
  (erc-tls :server slack-server
           :port (if port port 6667)
           :nick slack-username
           :password slack-pass))

(defun start-chat ()
  "Starts all the chats"
  (interactive)
  (start-irc)
  (start-bitlbee))

(provide 'my-erc-config)
