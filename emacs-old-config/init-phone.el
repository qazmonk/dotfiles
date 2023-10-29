(setq inhibit-splash-screen t)

(if (version< emacs-version "27.0")
    (package-initialize))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))


(require 'package)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)


(use-package god-mode
  :ensure t
  :demand t
  :config ;(god-mode)
  (add-to-list 'god-exempt-major-modes 'org-agenda-mode)
  (god-mode))
(use-package htmlize
  :ensure t)

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
	 ("C-M-d" . org-down-element))
  :config
  (add-to-list 'org-agenda-files "storage/shared/Org/phone-journal.org")
  (setq org-agenda-custom-commands
      '(("h" "Daily habits" 
         ((agenda ""))
         ((org-agenda-show-log t)
          (org-agenda-span 14)
          (org-agenda-log-mode-items '(state))
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":HABIT:")))
	 ("storage/shared/Org/habits.html"))))
  (defun org-agenda-remove-tags ()
    (save-excursion
      (beginning-of-buffer)
      (while (search-forward ":HABIT:" nil t)
	(replace-match "" nil t))))

  (defun org-agenda-remove-prelude ()
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward "phone-journal\\\(:Scheduled:\\\)?" nil t)
	(replace-match ""))))

  (setq org-agenda-finalize-hook nil)
  (add-hook 'org-agenda-finalize-hook (lambda ()
					(org-agenda-remove-tags)
					(org-agenda-remove-prelude))))

(use-package selectrum
    :ensure t
    :config
    (selectrum-mode +1))

(use-package selectrum-prescient
  :ensure t
  :config
  ;; to make sorting and filtering more intelligent
  (selectrum-prescient-mode +1)
  ;; to save your command history on disk, so the sorting gets more
  ;; intelligent over time
  (prescient-persist-mode +1))

(require 'spacemacs-common)
(load-theme 'spacemacs-dark t)


(add-hook 'after-init-hook
	  (lambda ()
	    (org-agenda nil "h")
	    (delete-other-windows)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(god-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



