(if (version< emacs-version "27.0")
    (package-initialize))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))


(require 'package)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)


(use-package god-mode
  :ensure t
  :config (god-mode))


(use-package org
  :bind (("C-c r" . org-capture)
	 ("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c b" . org-iswitchb)
	 :map org-mode-map
	 ("C-M-<left>" . org-promote-subtree)
	 ("C-M-<right>" . org-demote-subtree)
	 ("C-M-u" . org-up-element)
	 ("C-M-f" . org-forward-element)
	 ("C-M-b" . org-backward-element)
	 ("C-M-d" . org-down-element)))

