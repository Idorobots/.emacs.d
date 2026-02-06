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

;(setq agent-shell-opencode-command '("opencode" "acp" "--model" "anthropic/claude-opus-4-5" "--agent" "plan"))
(setq agent-shell-opencode-command '("opencode" "acp"))

(require 'whisper)
(setq whisper-install-directory "/home/k/projects/software/")
(setq whisper-model "base")
(setq whisper-language "en")
(setq whisper-use-threads (/ (num-processors) 2))
(setq whisper-server-mode 'local)
(setq whisper-return-cursor-to-start nil)

(provide 'my-ai-config)
