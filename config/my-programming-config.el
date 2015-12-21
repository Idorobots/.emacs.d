;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL STUFF
;;;;;;;;;;;;;;;;;;;;

(setq comment-style 'indent)
(setq standard-indent 4)

;; Additional programming modes:
(autoload 'd-mode "d-mode" "Major mode for editing D code." t)
(autoload 'scala-mode "scala-mode" "Major mode for editing Scala code." t)
(autoload 'ack-same "full-ack" "Ack source search." t)
(autoload 'ack "full-ack" "Ack source search." t)
(autoload 'ack-find-same-file "full-ack" "Ack source search." t)
(autoload 'ack-find-file "full-ack" "Ack source search." t)
(autoload 'octave-mode "octave" "Major mode for editing Octave code." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog code." t)

;; Octave mode stuff:
;(define-key octave-mode-map (kbd "C-M-x") 'octave-send-region) ;; FIXME

;; Common Lisp interaction:
(setq inferior-lisp-program "/usr/bin/sbcl")
(load (concat my-site-dir "quicklisp/slime-helper.el"))

(require 'geiser)
(setq geiser-active-implementations '(racket))
(setq geiser-scheme-dir "/usr/share/geiser")

;; Code highlighting modes settings:
(eval-after-load "full-ack"
  (setq ack-executable (executable-find "ack")))

;; SML mode is needy:
(setq exec-path (cons "/opt/bin/SML/bin" exec-path))
;(define-key sml-mode-map (kbd "C-M-x") 'sml-send-region) ;; FIXME

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TOOLS
;;;;;;;;;;;;;;;;;;;;

;; (require 'yasnippet)
;; (require 'dropdown-list)
;; (setq yas-snippet-dirs (concat my-site-dir "yasnippet/snippets"))
;; (setq yas-prompt-functions '(yas-dropdown-prompt
;;                              yas-ido-prompt
;;                              yas-completing-prompt))

;; (define-key yas-minor-mode-map [(tab)]       nil)
;; (define-key yas-minor-mode-map (kbd "TAB")   nil)
;; (define-key yas-minor-mode-map (kbd "C-M-/") 'yas-expand)
;; (yas-reload-all nil)

;; Whitespace highlighting mode:
(require 'whitespace)
(setq whitespace-style '(face spaces tabs trailing space-mark tab-mark))
(setq whitespace-space 'whitespace-newline)
(setq whitespace-tab 'whitespace-newline)

;; FIXME, TODO, BUG, NOTE, etc highlighting:
(require 'fic-mode)

;; Automatic parentheses pairing:
(require 'autopair)

;; StumpWM support:
(require 'stumpwm-mode)
(setq stumpwm-shell-program "stumpish")

;; Git support:
(require 'git)
(require 'git-blame)
(setq vc-display-status t)

(defun common-programming-settings ()
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (hs-minor-mode t)
  ;;(yas-minor-mode t)
  (unless (null (buffer-file-name))
    (flymake-mode t))
  (fic-mode t)
  (whitespace-mode t)
  (autopair-mode t))
  ;;(electric-pair-mode t))

(add-hook 'c-mode-common-hook 'common-programming-settings)
(add-hook 'emacs-lisp-mode-hook 'common-programming-settings)
(add-hook 'lisp-mode-hook 'common-programming-settings)
(add-hook 'scheme-mode-hook 'common-programming-settings)
(add-hook 'sml-mode-hook 'common-programming-settings)
(add-hook 'latex-mode-hook 'common-programming-settings)
(add-hook 'ruby-mode-hook 'common-programming-settings)
(add-hook 'scala-mode-hook 'common-programming-settings)
(add-hook 'sql-mode-hook 'common-programming-settings)
(add-hook 'erlang-mode-hook 'common-programming-settings)
(add-hook 'octave-mode 'common-programming-settings)
(add-hook 'prolog-mode 'common-programming-settings)
(add-hook 'clojure-mode-hook 'common-programming-settings)
(add-hook 'python-mode-hook 'common-programming-settings)
(add-hook 'octave-mode-hook 'common-programming-settings)

(require 'google-c-style)

(defun my-c-mode-hook ()
  (google-set-c-style)
;  (setf (cdr (assoc 'other c-default-style)) "Google")
  (c-toggle-electric-state 1)
  (c-toggle-auto-newline 1))

(add-hook 'c-mode-common-hook 'my-c-mode-hook)

;; Autoload patterns:
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

;; Compilation setting:
(require 'compile)
(setq compilation-scroll-output t)            ; Scroll the compilation buffer automatically.
(add-to-list 'compilation-error-regexp-alist  ; Additional error regex for the D language.
    '("^\\([^ \n]+\\)(\\([0-9]+\\)): \\(?:error\\|.\\|warnin\\(g\\)\\|remar\\(k\\)\\)"
      1 2 nil (3 . 4)))

;; Flymake settings:
(require 'flymake)
(require 'flymake-d)
(require 'flymake-ruby)
(require 'flymake-erlang)

(setq flymake-gui-warnings-enabled nil)
(setq flymake-start-syntax-check-on-newline nil)

;; Sublime-text-esque multiple cursors:
(add-to-list 'load-path (concat my-site-dir "multiple-cursors"))
(require 'multiple-cursors)
(setq mc/list-file (concat my-tmp-dir "mc.lst"))

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; ASM mode <3:
(require 'asm-mode)
(add-to-list 'auto-mode-alist '("\\.asm\\'" . asm-mode))

;; Pretty code mode:
;; (require 'pretty-mode)
;; (global-pretty-mode t)

;; Utility for evaluating Emacs Lisp:
(defun replace-last-sexp ()
  "Evaluates and replaces the previous sexp with its value. Similar to `C-u C-x C-e'."
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))

;; Useful programming shortcuts:
(global-set-key (kbd "C-S-c") 'comment-region)
(global-set-key (kbd "C-S-u") 'uncomment-region)
(global-set-key (kbd "C-S-g") 'goto-line)
(global-set-key (kbd "<tab>") 'indent-relative)
(global-set-key (kbd "C-<tab>") 'hs-toggle-hiding)
(global-set-key (kbd "C-M-S-x") 'replace-last-sexp)         ; Replace previous sexp with its value.
(global-set-key (kbd "RET") 'newline-and-indent)

;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'my-programming-config)
