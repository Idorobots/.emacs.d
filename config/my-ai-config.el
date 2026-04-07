;;; my-org-config.el --- My Org-Mode config.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AI
;;;;;;;;;;;;;;;;;;;;

(require 'gptel)

(setq gptel-api-key (lambda ()
                     (require 'secrets secrets-file)
                     openai-api-key))

(require 'llm-openai)
(require 'ellama)

(setq ellama-auto-scroll 't)
(setq ellama-provider (make-llm-openai
                       :key
                       (lambda ()
                         (require 'secrets secrets-file)
                         openai-api-key)))

(require 'agent-shell)

(setq agent-shell-opencode-authentication
      (agent-shell-opencode-make-authentication
       :api-key (lambda ()
                  (require 'secrets secrets-file)
                  openai-api-key)))

(setq agent-shell-preferred-agent-config
      (agent-shell-opencode-make-agent-config))

(setq agent-shell-opencode-acp-command '("opencode" "acp"))
(setq agent-shell-container-command-runner '("devcontainer" "exec" "--workspace-folder" "."))
;;(setq agent-shell-container-command-runner 'nil)
(setq agent-shell-show-context-usage-indicator 'detailed)
(setq agent-shell-show-usage-at-turn-end 't)
(setq agent-shell-header-style 'text)
(setq agent-shell-path-resolver-function #'agent-shell--resolve-devcontainer-path)
(setq agent-shell-text-file-capabilities nil)
(setq agent-shell-thought-process-expand-by-default 't)
(setq agent-shell-session-strategy 'new)

(require 'whisper)
(setq whisper-install-directory "/home/k/projects/software/")
(setq whisper-model "base")
(setq whisper-language "en")
(setq whisper-use-threads (/ (num-processors) 2))
(setq whisper-server-mode 'local)
(setq whisper-return-cursor-to-start nil)

(global-set-key (kbd "C-c w") 'whisper-run)

(provide 'my-ai-config)
