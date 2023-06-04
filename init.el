;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL SETTINGS
;;;;;;;;;;;;;;;;;;;;

;; .emacs.d config layout:
(setq my-config-dir (expand-file-name "~/.emacs.d/config/"))
(setq my-themes-dir (expand-file-name "~/.emacs.d/config/themes/"))
(setq my-site-dir (expand-file-name "~/.emacs.d/config/site-lisp/"))

(setq my-sessions-dir (expand-file-name "~/.emacs.d/sessions/"))
(setq my-backups-dir (expand-file-name "~/.emacs.d/backups/"))
(setq my-stuff-dir (expand-file-name "~/.emacs.d/stuff/"))
(setq my-logs-dir (expand-file-name "~/.emacs.d/logs/"))
(setq my-tmp-dir (expand-file-name "~/.emacs.d/tmps/"))

;; Extended load-path:
(add-to-list 'load-path my-config-dir)
(add-to-list 'load-path my-site-dir)
(add-to-list 'custom-theme-load-path my-themes-dir)

;; Some important files:
(setq secrets-file (concat my-config-dir "secrets.el.gpg"))

;; Theme & stuff
(toggle-frame-maximized)
(require 'my-editor-config)

;; Extra packages
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)

;; Some required packages:
(use-package ac-emoji)
(use-package ac-etags)
(use-package arduino-mode)
(use-package cider)
(use-package clojure-mode)
(use-package company)
(use-package d-mode)
(use-package dap-mode)
(use-package elm-mode)
(use-package emojify)
(use-package erlang)
(use-package fic-mode)
(use-package flycheck)
(use-package full-ack)
(use-package geiser)
(use-package git)
(use-package go-mode)
(use-package google-c-style)
(use-package lsp-metals)
(use-package lsp-mode)
(use-package lsp-ui)
(use-package markdown-mode)
(use-package org)
(use-package posframe)
(use-package protobuf-mode)
(use-package quelpa)
(use-package racket-mode)
(use-package sbt-mode)
(use-package sbt-mode)
(use-package scala-mode)
(use-package slime)
(use-package sml-mode)
(use-package sr-speedbar)
(use-package terraform-mode)
(use-package typescript-mode)
(use-package yaml-mode)

;; QUELPA packages:
(quelpa '(chatgpt-shell :repo Idorobots/chatgpt-shell :fetcher github))
(quelpa '(gamify :repo Idorobots/gamify-el :fetcher github))
(quelpa '(mode-line-stats :repo Idorobots/mode-line-stats :fetcher github))

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

;; Load Org-Agenda buffer and center on the refile list:
(find-file "~/org/refile.org")
(add-hook 'window-setup-hook 'org-agenda-list)
(put 'set-goal-column 'disabled nil)

;; Start the Emacs Server:
(server-start)

;; Custom variables:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(mode-line-stats gamify racket-mode yaml-mode use-package typescript-mode terraform-mode sr-speedbar sml-mode slime sbt-mode quelpa protobuf-mode paredit lsp-ui lsp-metals google-c-style go-mode git geiser full-ack flycheck fic-mode erlang emojify elm-mode d-mode company cider chatgpt-shell arduino-mode ac-etags ac-emoji)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
