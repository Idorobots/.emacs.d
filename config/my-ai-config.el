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

(provide 'my-ai-config)
