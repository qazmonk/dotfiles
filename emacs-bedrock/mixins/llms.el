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

;; install required inheritenv dependency:
(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

;; install claude-code.el
(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config
  (claude-code-mode)
  (setq claude-code-terminal-backend 'vterm)
  :bind-keymap ("C-c c" . claude-code-command-map)
  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))
