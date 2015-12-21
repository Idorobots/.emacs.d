;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL SETTINGS
;;;;;;;;;;;;;;;;;;;;

;; Extended load-path:
(add-to-list 'load-path (expand-file-name "/usr/share/emacs/site-lisp/org-mode/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/scala-mode"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/yasnippet"))
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

;; Customized custom file:
(setq custom-file (concat my-config-dir "custom.el"))

;; My Editor config:
(require 'my-editor-config)

;; My Gamify config:
(require 'my-gamify-config)
(gamify-start)

;; My Org-Mode config:
(require 'my-org-config)

;; My ERC config:
(require 'my-erc-config)

;; My programming config:
(require 'my-programming-config)

;; My modeline config:
(require 'my-modeline-config)

;; Some nifty utils:
(require 'misc-utils)

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
