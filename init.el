;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL SETTINGS
;;;;;;;;;;;;;;;;;;;;

;; ELPA stuff:
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(setq use-package-always-ensure t)

;; Some required packages:
(use-package autopair)
(use-package cider)
(use-package clojure-mode)
(use-package color-theme)
(use-package d-mode)
(use-package erlang)
(use-package fic-mode)
(use-package full-ack)
(use-package git)
(use-package git-blame)
(use-package google-c-style)
(use-package org)
(use-package slime)
(use-package sml-mode)

;; .emacs.d config layout:
(setq my-sessions-dir (expand-file-name "~/.emacs.d/sessions/"))
(setq my-backups-dir (expand-file-name "~/.emacs.d/backups/"))
(setq my-config-dir (expand-file-name "~/.emacs.d/config/"))
(setq my-stuff-dir (expand-file-name "~/.emacs.d/stuff/"))
(setq my-site-dir (expand-file-name "~/.emacs.d/site-lisp/"))
(setq my-tmp-dir (expand-file-name "~/.emacs.d/tmps/"))
(setq my-logs-dir (expand-file-name "~/.emacs.d/logs/"))

;; Extended load-path:
(add-to-list 'load-path my-site-dir)
(add-to-list 'load-path my-config-dir)

;; Some important files:
(setq custom-file (concat my-config-dir "custom.el"))
(setq secrets-file (concat my-config-dir "secrets.el.gpg"))

;; My Editor config:
(require 'my-editor-config)

;; My Gamify config:
(require 'my-gamify-config)

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
