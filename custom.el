(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "/tmp/\\2" t))))
 '(browse-url-browser-function (quote browse-url-firefox))
 '(company-idle-delay 0.5)
 '(org-src-fontify-natively t)
 '(package-selected-packages
   (quote
    (multiple-cursors eval-in-repl-python gscholar-bibtex flycheck-package exec-path-from-shell pyenv-mode highlight-indent-guides company-anaconda anaconda-mode company-shell use-package undo-tree tagedit sublime-themes smex slime-company paredit org-ref neotree magit interleave imenu-anywhere ido-completing-read+ highlight-parentheses counsel company-auctex cmake-mode beacon auctex-latexmk ace-jump-mode)))
 '(term-bind-key-alist
   (quote
    (("C-c C-c" . term-interrupt-subjob)
     ("C-p" . previous-line)
     ("C-n" . next-line)
     ("C-s" . isearch-forward)
     ("C-r" . isearch-backward)
     ("C-m" . term-send-raw)
     ("M-f" . term-send-forward-word)
     ("M-b" . term-send-backward-word)
     ("M-o" . term-send-backspace)
     ("M-p" . term-send-up)
     ("M-n" . term-send-down)
     ("M-d" . term-send-forward-kill-word)
     ("M-<delete>" . term-send-backward-kill-word)
     ("M-r" . term-send-reverse-search-history)
     ("M-," . term-send-input)
     ("M-." . comint-dynamic-complete)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(term-color-blue ((t (:background "deep sky blue" :foreground "deep sky blue"))))
 '(term-color-green ((t (:background "forest green" :foreground "forest green"))))
 '(term-color-red ((t (:background "dark red" :foreground "dark red")))))
