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
