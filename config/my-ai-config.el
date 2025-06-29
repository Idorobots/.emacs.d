;;; my-org-config.el --- My Org-Mode config.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AI
;;;;;;;;;;;;;;;;;;;;

(require 'secrets secrets-file)
(setq gptel-api-key openai-api-key)

(require 'llm-openai)
(setq ellama-auto-scroll 't)
(setq ellama-provider (make-llm-openai :key openai-api-key))

(provide 'my-ai-config)
