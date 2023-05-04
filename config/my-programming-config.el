;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL STUFF
;;;;;;;;;;;;;;;;;;;;

;; Indentation style:
(setq comment-style 'indent)
(setq standard-indent 4)

;; Additional programming modes:
(autoload 'd-mode "d-mode" "Major mode for editing D code." t)
(autoload 'ack-same "full-ack" "Ack source search." t)
(autoload 'ack "full-ack" "Ack source search." t)
(autoload 'ack-find-same-file "full-ack" "Ack source search." t)
(autoload 'ack-find-file "full-ack" "Ack source search." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog code." t)
(autoload 'scad-mode "scad" "Major mode for editing OpenSCAD files." t)

;; Common Lisp interaction:
(setq inferior-lisp-program "/usr/bin/sbcl")

(require 'geiser)
(setq geiser-active-implementations '(racket))
(setq geiser-scheme-dir "/usr/share/geiser")

;; Code highlighting modes settings:
(eval-after-load "full-ack"
  (setq ack-executable (executable-find "ack")))

;; SML mode is needy:
(setq exec-path (cons "/opt/bin/SML/bin" exec-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TOOLS
;;;;;;;;;;;;;;;;;;;;

(setq exec-path (cons "/usr/bin/sbt" exec-path))

;; Whitespace highlighting mode:
(require 'whitespace)
(setq whitespace-style '(face spaces tabs trailing space-mark tab-mark))
(setq whitespace-space 'whitespace-newline)
(setq whitespace-tab 'whitespace-newline)

;; FIXME, TODO, BUG, NOTE, etc highlighting:
(require 'fic-mode)

;; Automatic parentheses pairing:
(require 'phi-autopair)
(set phi-autopair-cautious-close nil)

;; Git support:
(require 'git)
(setq vc-display-status t)

(require 'google-c-style)

;; LSP support:
(require 'lsp-mode)

(defun common-programming-settings ()
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (hs-minor-mode t)
  (unless (null (buffer-file-name))
    (flycheck-mode t))
  (fic-mode t)
  (whitespace-mode t)
  (phi-autopair-mode t))

(add-hook 'emacs-lisp-mode-hook 'common-programming-settings)
(add-hook 'lisp-mode-hook 'common-programming-settings)
(add-hook 'scheme-mode-hook 'common-programming-settings)
(add-hook 'sml-mode-hook 'common-programming-settings)
(add-hook 'latex-mode-hook 'common-programming-settings)
(add-hook 'yaml-mode-hook 'common-programming-settings)
(add-hook 'ruby-mode-hook 'common-programming-settings)
(add-hook 'sql-mode-hook 'common-programming-settings)
(add-hook 'prolog-mode 'common-programming-settings)
(add-hook 'clojure-mode-hook 'common-programming-settings)
(add-hook 'python-mode-hook 'common-programming-settings)
(add-hook 'markdown-mode-hook 'common-programming-settings)
(add-hook 'vhdl-mode-hook 'common-programming-settings)
(add-hook 'elm-mode-hook 'common-programming-settings)
(add-hook 'js-mode-hook (lambda ()
                           (common-programming-settings)
                           (lsp)
                           (setq js-indent-level tab-width)))
(add-hook 'typescript-mode-hook (lambda ()
                                  (common-programming-settings)
                                  (lsp)
                                  (setq typescript-indent-level tab-width)))
(add-hook 'css-mode-hook (lambda ()
                           (common-programming-settings)
                           (setq css-indent-offset tab-width)))
(add-hook 'c-mode-common-hook (lambda ()
                                (google-set-c-style)
                                (common-programming-settings)
                                (c-toggle-electric-state 1)
                                (c-toggle-auto-newline 1)))
(add-hook 'scala-mode-hook (lambda ()
                             (common-programming-settings)
                             (lsp)))
(add-hook 'sbt-mode-hook (lambda ()
                           (common-programming-settings)
                           (setq sbt:program-options '("-Dsbt.supershell=false"))
                           (lsp)))
(add-hook 'lsp-mode-hook (lambda ()
                           (auto-complete-mode)
                           (dap-mode t)
                           (dap-ui-mode t)
                           (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
                           (setq lsp-prefer-flymake nil)
                           (setq lsp-enable-snippet nil)
                           (setq lsp-metals-treeview-show-when-views-received nil)))
(add-hook 'erlang-mode-hook (lambda ()
                              (common-programming-settings)
                              (lsp)))

;; Autoload patterns:
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

;; Compilation setting:
(require 'compile)
(setq compilation-scroll-output t)            ; Scroll the compilation buffer automatically.
(add-to-list 'compilation-error-regexp-alist  ; Additional error regex for the D language.
             '("^\\([^ \n]+\\)(\\([0-9]+\\)): \\(?:error\\|.\\|warnin\\(g\\)\\|remar\\(k\\)\\)"
               1 2 nil (3 . 4)))

;; Flymake settings:
(require 'flymake)

(set-face-attribute 'flymake-errline nil :background "Firebrick3")
(set-face-attribute 'flymake-warnline nil :background "DarkOrange3")

(defun flymake-D-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (or (flymake-simple-make-init)
        (list "dmd" (list "-c" local-file)))))

(add-to-list 'flymake-allowed-file-name-masks
             '(".+\\.d$"
               flymake-D-init
               flymake-simple-cleanup
               flymake-get-real-file-name))

(defun flymake-erlang-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name temp-file
                                         (file-name-directory buffer-file-name))))
    (list (concat my-stuff-dir "/flymake-erlang-script")
          (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.erl\\'" flymake-erlang-init))

(setq flymake-gui-warnings-enabled nil)
(setq flymake-start-syntax-check-on-newline nil)

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

;; Speedbar stuff:
(require 'speedbar)
(require 'sr-speedbar)

(eval-after-load "speedbar"
  (lambda ()
    (speedbar-add-supported-extension ".erl")
    (speedbar-add-supported-extension ".clj")))

(setq speedbar-show-unknown-files t)
(setq speedbar-use-images nil)
(setq sr-speedbar-auto-refresh t)

;; Make sr-speedbar sane:
(advice-add 'sr-speedbar-open :after
            (lambda ()
              (set-window-dedicated-p sr-speedbar-window nil)))

(advice-add 'sr-speedbar-close :around
            (lambda (ignored &rest args)
              (when (sr-speedbar-exist-p)
                ;; NOTE Might be better to delete the window.
                (kill-buffer sr-speedbar-buffer-name))))

(advice-add 'sr-speedbar-get-window :around
            (lambda (ignored &rest args)
              (setq sr-speedbar-window (or (split-window-sensibly)
                                           (selected-window)))))

(global-set-key (kbd "C-c s") 'sr-speedbar-toggle)

;;;;;;;;;;;;;;;;
;; ChatGPT
;;;;;;;;;;;

(require 'chatgpt-shell)

(setq chatgpt-shell-chatgpt-streaming t)
(add-to-list 'chatgpt-shell-language-mapping
             '("racket" . "scheme"))
(add-to-list 'chatgpt-shell-language-mapping
             '("scala" . "scala"))
(add-to-list 'chatgpt-shell-language-mapping
             '("python" . "python"))

;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'my-programming-config)
