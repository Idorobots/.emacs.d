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

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

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
(use-package gnuplot)
(use-package gptel)
(use-package lsp-metals)
(use-package lsp-mode)
(use-package lsp-ui)
(use-package markdown-mode)
(use-package org)
(use-package ox-gfm)
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

(use-package acp)
(use-package agent-shell)
(use-package ellama
  :hook (org-ctrl-c-ctrl-c-final . ellama-chat-send-last-message))

;; QUELPA packages:
(quelpa '(whisper :repo Idorobots/whisper.el :fetcher github))
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

;; My AI config:
(require 'my-ai-config)

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
 '(chatgpt-shell-request-timeout 360)
 '(package-selected-packages
   '(ac-emoji ac-etags acp agent-shell arduino-mode chatgpt-shell cider
              company d-mode ellama elm-mode emojify erlang fic-mode
              flycheck full-ack gamify geiser git gnuplot go-mode
              google-c-style gptel lsp-metals lsp-ui mode-line-stats
              ox-gfm paredit protobuf-mode quelpa racket-mode sbt-mode
              shell-maker slime sml-mode sr-speedbar terraform-mode
              typescript-mode use-package whisper yaml-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
