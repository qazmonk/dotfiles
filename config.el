;; Don't edit this file, edit config.org' instead ...

    (setq debug-on-error t)
    (setq-default fill-column 90)
    (global-unset-key (kbd "C-z"))
    (setq inhibit-splash-screen t)
    ;; add custom packages to load path
    (add-to-list 'load-path "~/.emacs.d/lisp")
  (require 'package) ;; You might already have this line
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                      (not (gnutls-available-p))))
         (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
    (add-to-list 'package-archives (cons "melpa" url) t))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
  (package-initialize)
  (eval-when-compile
    (require 'use-package))
  (when window-system 
    (when tool-bar-mode
      (tool-bar-mode -1)))
      ;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
  (defun unfill-paragraph (&optional region)
    "Takes a multi-line paragraph and makes it into a single line of text."
    (interactive (progn (barf-if-buffer-read-only) '(t)))
    (let ((fill-column (point-max))
          ;; This would override `fill-column' if it's an integer.
          (emacs-lisp-docstring-fill-column t))
      (fill-paragraph nil region)))
  (define-key global-map "\M-Q" 'unfill-paragraph)
  ;;dont store backups in the same directory as files and periodically delete them as well
  (let ((temporary-file-directory "/tmp/"))
    (setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" , temporary-file-directory t)))
    (message "Deleting old backup files...")
    (let ((week (* 60 60 24 7))
          (current (float-time (current-time))))
      (dolist (file (directory-files temporary-file-directory t))
        (when (and (backup-file-name-p file)
                   (> (- current (float-time (nth 5 (file-attributes file))))
                      week))
          (message "%s" file)
          (delete-file file)))))

  ;;change keys for mac keyboard
  (when (equal system-type 'darwin)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super))

  ;;GET THOSE GOD DAMN TABS OUT OF HERE
  (setq-default indent-tabes-mode nil)

  (add-hook 'text-mode-hook (lambda () 
                              (flyspell-mode)))

  (global-set-key (kbd "C-'") 'set-mark-command)

  (global-set-key (kbd "<C-right>") 'windmove-right)
  (global-set-key (kbd "<C-left>") 'windmove-left)
  (global-set-key (kbd "<C-up>") 'windmove-up)
  (global-set-key (kbd "<C-down>") 'windmove-down)

  (defun push-mark-no-activate ()
    "Pushes `point' to `mark-ring' and does not activate the region
         Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
    (interactive)
    (push-mark (point) t nil)
    (message "Pushed mark to ring"))

  (global-set-key (kbd "C-`") 'push-mark-no-activate)

  (defun jump-to-mark ()
    "Jumps to the local mark, respecting the `mark-ring' order.
        This is the same as using \\[set-mark-command] with the prefix argument."
    (interactive)
    (set-mark-command 1))
  (global-set-key (kbd "M-SPC") 'jump-to-mark)


  (defadvice yes-or-no-p (around prevent-dialog activate)
    "Prevent yes-or-no-p from activating a dialog"
    (let ((use-dialog-box nil))
      ad-do-it))
  (defadvice y-or-n-p (around prevent-dialog-yorn activate)
    "Prevent y-or-n-p from activating a dialog"
    (let ((use-dialog-box nil))
      ad-do-it))

  (setq glyphless-char-display-control 
        (quote ((format-control . hex-code) (no-font . hex-code))))
  (use-package custom
    :config
    (setq custom-enabled-themes (quote (sanityinc-tomorrow-night)))
    (setq custom-safe-themes
          (quote
           ("e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4"
            "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6"
            "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02"
            "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e"
            "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607"
            "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265"
            "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950"
            "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5"
            "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1"
            "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26"
            "1e3b2c9e7e84bb886739604eae91a9afbdfb2e269936ec5dd4a9d3b7a943af7f"
            "c4465c56ee0cac519dd6ab6249c7fd5bb2c7f7f78ba2875d28a50d3c20a59473"
            "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016"
            "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e"
            "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58"
            "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a"
            "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d"
            "68d36308fc6e7395f7e6355f92c1dd9029c7a672cbecf8048e2933a053cf27e6"
            "3dafeadb813a33031848dfebfa0928e37e7a3c18efefa10f3e9f48d1993598d3"
            "05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" 
            default)))
    (setq custom-theme-load-path
          (quote
           ("/Users/Nate/.emacs.d/elpa/color-theme-sanityinc-tomorrow-20160413.150/"
            "/Users/Nate/.emacs.d/elpa/monokai-theme-20160419.1444/"
            "/Users/Nate/.emacs.d/elpa/zenburn-theme-20160416.1011/"
            custom-theme-directory
            "/Users/Nate/.emacs.d/emacs-color-theme-solarized"
            "/home/nate/.emacs.d/emacs-color-theme-solarized"
            "/home/nate/.emacs.d/themes")))
    
    (if (display-graphic-p) 
        (progn
          (dolist (theme custom-enabled-themes)
            (disable-theme theme))
          (load-theme 'brin))
      (progn 
        (dolist (theme custom-enabled-themes)
          (disable-theme theme))      
        (load-theme 'solarized t))))

  (use-package help-fns+)
  (use-package smex
    :config
    (smex-initialize)
    (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ; this is the old M-x
    :bind (("M-x" . smex)
           ("M-X" . smex-major-mode-commands)))
  (use-package ace-jump-mode
    :config
    (setq ace-jump-mode-scope 'window)
    :bind (("C-c SPC" . ace-jump-mode)))
  (use-package image
    :config
    (imagex-global-sticky-mode 1))
  (use-package neotree
    :bind ([f8] . neotree-toggle)
    :config
    (setq neo-theme (quote nerd)))

  (use-package multi-scratch
    :load-path "~/.emacs.d/multi-scratch/")
  (use-package paredit
    :load-path "~/.emacs.d/paredit/"
    :commands enable-paredit-mode)
  (use-package imenu-anywhere
    :bind (("M-i" . imenu-anywhere)))
  ;;MINIMAP
  (when window-system
    (load "natesminimap.el")
    (scroll-bar-mode -1)
    (minimap-mode)
    (defun minimap-toggle ()
      "Toggle minimap for current buffer."
      (interactive)
      (if (not (get-buffer-window minimap-buffer-name))
          (progn (minimap-create)
                 (setf minimap-exists t))
        (progn (minimap-kill)
               (setf minimap-exists t)))))
  (use-package ido
    :bind (("C-x C-f" . ido-find-file))
    :config
    (ido-mode t)
    (ido-everywhere)
    (setq ido-mode (quote both))
    (setq ido-enable-flex-matching t))
  (use-package ido-completing-read+
    :config
    (require 'ido)
    (ido-ubiquitous-mode t)
    (put 'dired-do-rename 'ido 'find-file)
    (put 'dired-do-copy 'ido 'find-file))
  (use-package flyspell
    :config
    (add-hook 'flyspell-mode-hook (lambda ()
                                    (define-key flyspell-mode-map (kbd "M-<f1>")
                                      #'flyspell-check-previous-highlighted-word))))

  ;; adaptive-fill-mode
  (setq adaptive-fill-mode t)

  (use-package autoinsert
    :config
    (auto-insert-mode)
    (defun autoinsert-yas-expand ()
      "Replace text in yasnipped template."
      (yas-expand-snippet (buffer-string) (point-min) (point-max)))
    (setq auto-insert-directory "~/dotfiles/autoinserts/")
    (define-auto-insert "\\.tex$" "standard.tex"))

  (use-package highlight-parentheses
    :config
    (setq hl-paren-colors
          (quote
           ("PaleGreen1" "SpringGreen1" "SpringGreen3" "SpringGreen4"))))
  (use-package re-builder
    :config
    (setq reb-re-syntax (quote string)))
  (use-package beacon
    :if window-system
    :config
    (setf beacon-color (face-background 'cursor))
    (setq ring-bell-function 
          (lambda ()
            (beacon-blink)))
    (beacon-mode t))
  (use-package undo-tree
    :config
    (global-undo-tree-mode t))
  (use-package ansi-color 
    :config
    (setq ansi-color-faces-vector
         [default bold shadow italic underline bold bold-italic bold])
    (setq ansi-color-names-vector
          ["gray100" "#d54e53" "light green" "light green" "#7aa6da" "#c397d8" "#70c0b1" "#000000"]))
  (use-package term
    :config
    (setq term-buffer-maximum-size 0))

  (use-package eshell
    :config
    (require 'em-smart)
    (setq eshell-where-to-jump 'begin)
    (setq eshell-review-quick-commands nil)
    (setq eshell-smart-space-goes-to-end t)
    (setq eshell-prompt-regexp "[^#$|
  ]* \\([#$]\\|\\(|->\\)\\) ")    
    (defmacro with-face (str &rest properties)
      (if (> (length properties) 1)
          `(propertize ,str 'face (list ,@properties))
        (if (= (length properties) 1)
            `(propertize ,str 'face ,@properties)
          str)))
    (defvar eshell-prev-dir "")
    (defvar eshell-prev-time '(0 0 0 0))
    (defun nates-eshell-hook ()
      (set (make-local-variable 'eshell-prev-dir) (eshell/pwd)))
    (defun fancy-prompt ()
      (let (prompt) 
        (setq prompt
              (concat
               (when (or (not (string= eshell-prev-dir (eshell/pwd)))
                         (not (time-less-p (time-subtract (current-time)
                                                          eshell-prev-time)
                                           '(0 30 0 0))))
                 (setq eshell-prev-dir (eshell/pwd))
                 (setq eshell-prev-time (current-time))
                 (concat
                  (with-face user-login-name
                             'eshell-ls-readonly-face)
                  (with-face " @ "
                             'eshell-ls-symlink-face)
                  (with-face (eshell/pwd) 
                             'eshell-ls-directory-face)
                  (with-face "\n")))
               (with-face " |-> " 'font-lock-constant-face)))
        (put-text-property 0 (length prompt) 'read-only t prompt)
        (put-text-property 0 (length prompt) 
                           'rear-nonsticky t prompt)
        prompt))
    
    (defun simple-prompt ()
      " $ ")
    (add-hook 'eshell-mode-hook 'nates-eshell-hook)
    (setq eshell-prompt-function 'fancy-prompt)
    (setq eshell-highlight-prompt nil))
(add-hook 'shell-mode-hook (lambda ()
                             (setq-local company-backends 
                                         '((company-files
                                            company-dabbrev-code)))))
  (use-package compile
    :config
    (setq compilation-auto-jump-to-first-error t)
    (setq compilation-message-face (quote default))
    (setq special-display-buffer-names
          '("*compilation*"))

    (setq special-display-function
          (lambda (buffer &optional args)
            (split-window)
            (get-buffer-window buffer 0)))

    ;; Close the compilation window when compilation succedes
    (setq 
     compilation-exit-message-function
     (lambda (status code msg)
       ;; If M-x compile exists with a 0
       (when (and (eq status 'exit) (zerop code))
         ;; then bury the *compilation* buffer, so that C-x b doesn't go there
         ;;(switch-to-prev-buffer (get-buffer-window "*compilation*") 'kill)
         (bury-buffer)
         ;; and delete the *compilation* window

         (delete-window (get-buffer-window (get-buffer "*compilation*"))))
       ;; Always return the anticipated result of compilation-exit-message-function
       (cons msg code))))
  (use-package cmake-mode)
(defun nates-git-ignore-mode ()
  (add-to-list (make-local-variable 'company-backends) 'company-files))
(add-to-list 'auto-mode-alist '("\\.gitignore\\'" . nates-gitignore-mode))
  (use-package cc-mode
    :config
    (setq c-default-style
          (quote
           ((c++-mode . "k&r")
            (java-mode . "java")
            (awk-mode . "awk")
            (other . "gnu"))))
    (setq c-offsets-alist (quote ((statement-cont first c-lineup-assignments +))))
    (defun my-merge-imenu ()
      (interactive)
      (let ((mode-imenu (imenu-default-create-index-function))
            (custom-imenu (imenu--generic-function imenu-generic-expression)))
        ;;    (delete-dups (append mode-imenu custom-imenu))))
        custom-imenu))
    (defun my-c++-mode-hook ()
      (add-to-list
       'imenu-generic-expression
       '("Function Header" 
         "^\\s-*\\([a-zA-Z0-9_:><]+\\s-+\\)+\\([a-zA-Z0-9_]+\\)([-a-zA-Z0-9[:space:]:<>,=_*&()\n]*);"
         2))
      (setq imenu-create-index-function 'my-merge-imenu))

    (add-hook 'c++-mode-hook 'my-c++-mode-hook)

    (defun my-c-mode-common-hook ()
      ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
      (c-set-offset 'substatement-open 0)
      ;; other customizations can go here

      (setq c++-tab-always-indent t)
      (setq c-basic-offset 2)                  ;; Default is 2
      (setq c-indent-level 2)                  ;; Default is 2

      (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
      (setq tab-width 2)
      (setq indent-tabs-mode nil)
      (setq column-number-mode t)
      (setq compile-command "make all")
      (local-set-key (kbd "C-c C-c") 'recompile))


    (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

    (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode)))
    (setq inferior-lisp-program "/usr/local/bin/sbcl")

  (use-package slime
    :config
    (require 'slime-autoloads)
    (add-to-list 'slime-contribs 'inferior-slime)
    (add-to-list 'slime-contribs 'slime-fancy)
    (add-to-list 'slime-contribs 'slime-autodoc)
    (slime-setup '(slime-fancy slime-company))
    (setq slime-use-autodoc-mode nil)
    (setq slime-company-major-modes
          (quote
           (lisp-mode clojure-mode slime-repl-mode scheme-mode emacs-lisp-mode))))

  (defvar electrify-return-match
    "[\]}\)\"]"
    "If this regexp matches the text after the cursor, do an \"electric\"
      return.")
  (defun electrify-return-if-match (arg)
    "If the text after the cursor matches `electrify-return-match' then
      open and indent an empty line between the cursor and the text.  Move the
      cursor to the new line."
    (interactive "P")
    (let ((case-fold-search nil))
      (if (looking-at electrify-return-match)
          (save-excursion (newline-and-indent)))
      (newline arg)
      (indent-according-to-mode)))
  (defun nates-general-lisp-mode ()
    (company-mode)
    (enable-paredit-mode)
    (show-paren-mode t)   
    (highlight-parentheses-mode t)
    (local-set-key (kbd "RET") 'electrify-return-if-match)
    (define-key company-active-map (kbd "\C-n") 'company-select-next)
    (define-key company-active-map (kbd "\C-p") 'company-select-previous)
    (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
    (define-key company-active-map (kbd "M-.") 'company-show-location)
    (setq-local company-backends '((company-files
                                    company-capf)
                                   (company-keywords 
                                    company-dabbrev-code))))
  (defun nates-lisp-mode ()
    (slime-mode)
    (local-set-key (kbd "C-M-S-s-r")
                   (lambda ()
                     (interactive)
                     (shell-command "osascript ~/Dropbox/AppleScript/refresh-preview.scpt")))
    (set (make-local-variable 'lisp-indent-function)
         'common-lisp-indent-function)
    (nates-general-lisp-mode)
    (slime-autodoc-mode))

  (defun nates-inferior-lisp-mode ()
    (define-key slime-repl-mode-map
      (read-kbd-macro paredit-backward-delete-key) nil)
    (eldoc-mode t)
    (message "turning on eldoc-mode")
    (local-set-key (kbd "C-M-S-s-r")
                   (lambda ()
                     (interactive)
                     (shell-command "osascript ~/Dropbox/AppleScript/refresh-preview.scpt")))
    (nates-general-lisp-mode))


  (defun nates-emacs-lisp-mode ()
    (eldoc-mode t)
    (nates-general-lisp-mode)
    (setq-local company-backends
                '((company-elisp company-capf)
                  company-files
                  (company-keywords 
                   company-dabbrev-code))))

  (add-hook 'lisp-mode-hook 'nates-lisp-mode)

  (add-hook 'emacs-lisp-mode-hook 'nates-emacs-lisp-mode)
  (add-hook 'slime-repl-mode-hook 'nates-inferior-lisp-mode)
    (defun nates-slime ()
      (interactive)
      (save-excursion
        (slime))
      (eldoc-mode t))
    ;; (add-hook 'slime-mode-hook
    ;;           (lambda ()
    ;;             (unless (slime-connected-p)
    ;;               (print (current-buffer))
    ;;               (unless (equalp (current-buffer) (get-buffer "*scratch*"))
    ;;                 (save-excursion (slime))))))

    (add-to-list 'auto-mode-alist '("\\.j\\'" . lisp-mode))

    (eval-after-load 'slime
      `(define-key slime-mode-map (kbd "M-?") nil))

    (require 'cl)

  (use-package matlab
    :config
    (setq mlint-programs
     (quote
      ("/Applications/MATLAB_R2015b.app/bin/maci64/mlint" "/usr/local/MATLAB/R2017a/bin/glnxa64/mlint")))
    (setq matlab-shell-command-switches (quote ("-nodesktop -nosplash")))
    (setq matlab-functions-have-end t)
    (defun matlab-set-breakpoint ()
      (interactive)
      (matlab-shell-run-command (format "dbstop in %s at %d" 
                                        (file-name-nondirectory (buffer-file-name))
                                        (line-number-at-pos))))

    (defun matlab-set-condition-breakpoint (condition)
      (interactive "sEnter condition: ")
      (message (format "dbstop in %s at %d if (%s)" 
                       (file-name-nondirectory (buffer-file-name))
                       (line-number-at-pos)
                       condition))  (matlab-shell-run-command (format "dbstop in %s at %d if (%s)" 
                                                                      (file-name-nondirectory (buffer-file-name))
                                                                      (line-number-at-pos)
                                                                      condition)))

    (defun nates-matlab-mode ()
      (mlint-minor-mode t)
      (matlab-toggle-show-mlint-warnings))

    (defun nates-matlab-shell-mode ()
      (setq-local company-backends 
                  '((company-files
                     company-capf)
                    company-matlab-shell
                    (company-keywords 
                     company-dabbrev-code))))
    (add-hook 'matlab-mode-hook 'nates-matlab-mode)
    (add-hook 'matlab-shell-mode-hook 'nates-matlab-shell-mode))
  (use-package js
    :config
    (setq js-indent-level 2))
  (use-package sh-script
    :demand
    :config
    (add-to-list 'auto-mode-alist 
                 '("\\.bashrc.*" . shell-script-mode)))
  (use-package tagedit
    :config
    (eval-after-load "sgml-mode"
      '(progn (tagedit-add-paredit-like-keybindings)
              (add-hook 'html-mode-hook (lambda () (tagedit-mode 1))))))
  (use-package company-auctex)
  (use-package tex
    :ensure auctex
    :config
    (setq LaTeX-indent-environment-list
          (quote
           (("verbatim" current-indentation)
            ("verbatim*" current-indentation)
            ("tabu" LaTeX-indent-tabular)
            ("tabular" LaTeX-indent-tabular)
            ("tabular*" LaTeX-indent-tabular)
            ("align" LaTeX-indent-tabular)
            ("align*" LaTeX-indent-tabular)
            ("array" LaTeX-indent-tabular)
            ("eqnarray" LaTeX-indent-tabular)
            ("eqnarray*" LaTeX-indent-tabular)
            ("displaymath")
            ("equation")
            ("equation*")
            ("picture")
            ("tabbing")
          ("table")
          ("table*"))))

      (setq TeX-auto-save t)
      (setq TeX-parse-self t)
                                            ;(setq-default TeX-master nil)
      (add-hook 'LaTeX-mode-hook 'visual-line-mode)
      (add-hook 'LaTeX-mode-hook 'flyspell-mode)
      (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
      (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
      (setq reftex-plug-into-AUCTeX t)
      (setq TeX-PDF-mode t)
      (setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)"))) 

      ;; Use Skim as viewer, enable source <-> PDF sync
      ;; make latexmk available via C-c C-c
      ;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
      (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))

      (if (equal system-type 'darwin)
          (progn 
            (add-hook 'LaTeX-mode-hook (lambda ()
                                         (setq-local company-backends 
                                                     '((company-files
                                                        company-capf)
                                                       (company-keywords 
                                                        company-dabbrev)))
                                         (auto-fill-mode -1)
                                         (company-auctex-init)
                                         (push
                                          '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
                                            :help "Run latexmk on file")
                                          TeX-command-list)))
            (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))
            (setq TeX-view-program-list
                  '(("PDF Viewer"
                     "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b"))))
        (progn
          (setq TeX-view-program-list
                '(("PDF Viewer"
                   "okular --page %(outpage) %o")))))
      
      ;; ##### Enable synctex correlation. From Okular just press
      ;; ##### Shift + Left click to go to the good line.
      (setq TeX-source-correlate-mode t
            TeX-source-correlate-start-server t)
      
      (setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) "Okular"))

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
    (setq org-directory "~/Dropbox/org")
    (setq org-default-notes-file "~/Dropbox/org/agenda/notes.org")
    (setq org-log-done 'time)
    (setq org-capture-templates
          '(("W" "Work todo" entry (file+headline "~/Dropbox/org/agenda/work.org" "Tasks")
             "* TODO %?\t:WORK:\nDEADLINE: %^{Deadline}t\n  %i\n")
            ("H" "Home todo" entry (file+headline "~/Dropbox/org/agenda/tasks.org" "Tasks")
             "* TODO %?\t:HOME:\nDEADLINE: %^{Deadline}t\n  %i\n")
            ("S" "School todo" entry (file "~/Dropbox/org/agenda/school-work.org")
             "* TODO %?\t:SCHOOL:%^g\nDEADLINE: %^{Deadline}t\n  %i\n")
            ("P" "PClassic TODO" entry (file+headline "~/Dropbox/org/agenda/pclassic.org" "Tasks")
             "* TODO %?\t:PCLASSIC:\nDEADLINE: %^{Deadline}t\n  %i\n")
            ("w" "Work entry" entry (file+headline "~/Dropbox/org/agenda/work.org" "Events")
             "* %?\n\t:WORK:\nSCHEDULED: %^{Schedule}t\n %i\n")
            ("h" "Home entry" entry (file+headline "~/Dropbox/org/agenda/work.org" "Events")
             "* %?\n\t:HOME:\nSCHEDULED: %^{Schedule}t\n %i\n")
            ("n" "Note" entry (file+datetree "~/Dropbox/org/agenda/notes.org")
             "* %?\nEntered on %U\n  %i\n")
            ("l" "Lab notebook" entry (file+datetree "~/Dropbox/org/agenda/lab-notebook.org")
             "* %^{prompt|No Title}\nEntered on %U\n%? %i\n")))
    (setq org-agenda-files
          (quote
           ("~/Dropbox/org/agenda/notes.org" "~/Dropbox/org/agenda/tasks.org"
            "~/Dropbox/org/agenda/work.org" "~/Dropbox/org/agenda/school-work.org"
            "~/Dropbox/org/agenda/pclassic.org")))
    (setq org-mobile-inbox-for-pull "~/Dropbox/org/from-mobile.org")
    (setq org-mobile-directory "~/Dropbox/MobileOrg")
    (setq org-mobile-files '("~/Dropbox/org/agenda/notes.org" "~/Dropbox/org/agenda/tasks.org"
                             "~/Dropbox/org/agenda/work.org"))
    (setq org-modules
          (quote
           (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-drill)))
    (setq org-src-tab-acts-natively t)
    (setq org-src-window-setup (quote current-window))
    (setq org-structure-template-alist
          (quote
           (("S" "#+BEGIN_SRC emacs-lisp

  ,#+END_SRC" "")
            ("s" "#+BEGIN_SRC ?

  ,#+END_SRC" "<src lang=\"?\">

  </src>")
            ("e" "#+BEGIN_EXAMPLE
  ?
  ,#+END_EXAMPLE" "<example>
  ?
  </example>")
            ("q" "#+BEGIN_QUOTE
  ?
  ,#+END_QUOTE" "<quote>
  ?
  </quote>")
            ("v" "#+BEGIN_VERSE
  ?
  ,#+END_VERSE" "<verse>
  ?
  </verse>")
            ("V" "#+BEGIN_VERBATIM
  ?
  ,#+END_VERBATIM" "<verbatim>
  ?
  </verbatim>")
            ("c" "#+BEGIN_CENTER
  ?
  ,#+END_CENTER" "<center>
  ?
  </center>")
            ("l" "#+BEGIN_LaTeX
  ?
  ,#+END_LaTeX" "<literal style=\"latex\">
  ?
  </literal>")
            ("L" "#+LaTeX: " "<literal style=\"latex\">?</literal>")
            ("h" "#+BEGIN_HTML
  ?
  ,#+END_HTML" "<literal style=\"html\">
  ?
  </literal>")
            ("H" "#+HTML: " "<literal style=\"html\">?</literal>")
            ("a" "#+BEGIN_ASCII
  ?
  ,#+END_ASCII" "")
            ("A" "#+ASCII: " "")
            ("i" "#+INDEX: ?" "#+INDEX: ?")
            ("I" "#+INCLUDE: %file ?" "<include file=%file markup=\"?\">"))))
    (defun nates-org-mode-hook ()
      (visual-line-mode t)))
    (add-hook 'org-mode-hook 'nates-org-mode-hook)
    (setq org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i!)" "|" "DONE(d!)")))
    (use-package magit
      :config
      (setq magit-push-arguments nil)
      (setq magit-diff-use-overlays nil))
  (use-package company
      :demand
      :config
      (setq company-dabbrev-downcase nil)
      (setq company-idle-delay 0.1)
      (setq company-backends
            '((company-files
               company-capf)
              (company-keywords 
               company-dabbrev-code)))
      (setq company-dabbrev-code-modes
            '(prog-mode batch-file-mode csharp-mode css-mode 
                        erlang-mode haskell-mode jde-mode lua-mode
                        python-mode matlab-mode matlab-shell-mode))    
      (add-hook 'after-init-hook 'global-company-mode)
      :bind (("C-M-s-<tab>" . company-other-backend)))
