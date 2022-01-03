(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-html "xdg-open")
     (output-pdf "Okular")))
 '(company-idle-delay 0.1)
 '(display-line-numbers t)
 '(display-line-numbers-major-tick 200)
 '(org-agenda-compact-blocks t)
 '(org-agenda-files '("/home/nate/Documents/org/phone-journal.org"))
 '(org-format-latex-options
   '(:foreground default :background default :scale 2.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
		 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(package-selected-packages
   '(spacemacs-common spacemacs-theme solarized-theme solarized projectile conda eglot iedit marginalia ctrlf disk-usage moody selectrum-prescient selectrum-precient selectrum esup lsp-python-ms lsp-pyright yaml-mode lsp-jedi dash lsp-mode t persistent-scratch chess picpocket lsp-ui dired-subtree zenburn-theme yasnippet-snippets xterm-color vterm visible-mark use-package undo-tree tuareg tagedit sublime-themes smex slime-company pyenv-mode org-ref neotree names matlab-mode magit-popup magit lua-mode lorem-ipsum ivy-yasnippet ivy-hydra interleave imenu-anywhere image+ ido-ubiquitous highlight-parentheses highlight-indent-guides help-fns+ gscholar-bibtex god-mode gitignore-mode ghub font-lock-studio flycheck-package find-file-in-project exec-path-from-shell eval-in-repl elpy electric-spacing electric-operator electric-case diminish crontab-mode counsel company-auctex cmake-mode beacon backup-each-save ace-jump-mode))
 '(safe-local-variable-values
   '((eval progn
	   (message "hi there!"))
     (eval progn
	   (server-sync-mode)
	   (message "hi there!"))
     (eval progn
	   (message "hi there!")
	   (server-sync-mode))
     (unison-sync-remote-host . "autobot")
     (unison-sync-remote-path . "/home/nchodosh")
     (unison-sync-local-directory . "/home/nate/sync/nate-nerf")
     (server-sync-local-directory . "/home/nate/sync/nate-nerf")
     (server-sync-remote-host . "autobot")
     (server-sync-remote-path . "/home/nchodosh"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
