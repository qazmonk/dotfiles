(setq debug-on-error t)
;(setq warning-minimum-level :error)
(setq inhibit-splash-screen t)

(add-to-list 'load-path "~/.emacs.d")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(ac-config-default)


(require 'server)


;;dunno if this actually works
(unless (server-running-p)
  (server-start))

(windmove-default-keybindings 'meta)
(menu-bar-mode -1)

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
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'meta)


(add-hook 'c-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-hook 'text-mode-hook (lambda () (setq indent-tabs-mode t)
                            (flyspell-mode)
                            (auto-fill-mode)))


(setq inferior-lisp-program "/usr/local/bin/sbcl")
(add-to-list 'load-path "~/.emacs.d/slime-2.8")
(require 'slime)




(add-hook 'lisp-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))



(setq TeX-auto-save t)
(setq TeX-parse-self t)
;(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook (lambda () (auto-fill-mode -1)))
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)
(setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)"))) 
;; Use Skim as viewer, enable source <-> PDF sync
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(add-hook 'LaTeX-mode-hook (lambda ()
			     (push
			      '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
				:help "Run latexmk on file")
			      TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background
(require 'package)
(package-initialize)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)


(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

(server-start)


;;Stop autocompleting when I hit enter
;(add-hook 'css-mode-hook
;          (lambda ()
;            (define-key ac-complete-mode-map "\r" nil)))

;;Configuration for org mode
(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


;;autocomplete for C coding
(add-to-list 'load-path "~/.emacs.d/auto-complete-clang/")
(require 'auto-complete-clang)

(defun nates-lisp-mode ()
  (slime-mode)
  (auto-complete-mode t))

(add-hook 'lisp-mode-hook 'nates-lisp-mode)
(add-hook 'emacs-lisp-hook 'nates-lisp-mode)
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t) (auto-complete-mode t)))


(setq global-auto-complete-mode t)

(add-to-list 'auto-mode-alist '("\\.j\\'" . lisp-mode))






