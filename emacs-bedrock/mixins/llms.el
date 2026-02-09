(use-package gptel
  :ensure t
  :config
  (setq
   gptel-model 'claude-opus-4-20250514 
   gptel-backend (gptel-make-anthropic "Claude"
                   :stream t :key (lambda ()
                                   (auth-source-pick-first-password
                                    :host "api.anthropic.com"))))
  )
