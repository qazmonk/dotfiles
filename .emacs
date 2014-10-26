(setq debug-on-error t)

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
        (delete-file file)))
    )
  )


(setq mac-command-modiffier 'meta)
(setq mac-command-key-is-meta t)
(add-hook 'c-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-hook 'text-mode-hook (lambda () (setq indent-tabs-mode t)
                            (flyspell-mode)
                            (auto-fill-mode)))

(setq inferior-lisp-program "/usr/local/bin/sbcl")
(add-to-list 'load-path "~/.emacs.d/slime-2.8")
(require 'slime)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))


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

(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "/Users/Nate/.emacs.d/emacs-eclim/")



(defun ome-eclim-setup ()
  ;; (add-to-list 'eclim-eclipse-dirs "/opt/eclipse")
  (setq eclim-auto-save t
        eclim-executable (or (executable-find "eclim") "/opt/eclipse/eclim")
        eclimd-executable (or (executable-find "eclimd") "/opt/eclipse/eclimd")
        eclimd-wait-for-process nil
        eclimd-default-workspace "~/Google_Drive/Workspace/"
        help-at-pt-display-when-idle t
        help-at-pt-timer-delay 0.1)

  ;; Call the help framework with the settings above & activate
  ;; eclim-modex
  (help-at-pt-set-timer)

  ;; keep consistent which other auto-complete backend.
  (custom-set-faces
   '(ac-emacs-eclim-candidate-face ((t (:inherit ac-candidate-face))))
   '(ac-emacs-eclim-selection-face ((t (:inherit ac-selection-face)))))

  ;; Hook eclim up with auto complete mode
  ;;(require 'ac-emacs-eclim-source)
  ;;(ac-emacs-eclim-config)

  (require 'eclimd)

  (add-hook 'java-mode-hook
            (lambda ()
              (add-to-list 'ac-sources 'ac-source-emacs-eclim)
              (eclim-mode t))))


;; regular auto-complete initialization
(require 'auto-complete-config)
(ac-config-default)

;; add the emacs-eclim source
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)

(ome-eclim-setup)

(when (executable-find "eclipse")
  (ome-install 'eclim))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(indent-tabs-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
