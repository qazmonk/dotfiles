(tool-bar-mode -1)
(setq debug-on-error t)
;(setq warning-minimum-level :error)
(setq inhibit-splash-screen t)

(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(ac-config-default)


;;dunno if this actually work
(if (and (fboundp 'server-running-p) 
         (not (server-running-p)))
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
(add-to-list 'load-path "~/.emacs.d/auto-complete/auto-complete-clang")
(require 'auto-complete-clang)

(defun nates-lisp-mode ()
  (slime-mode)
  (auto-complete-mode t))

(add-hook 'lisp-mode-hook 'nates-lisp-mode)
(add-hook 'emacs-lisp-hook 'nates-lisp-mode)
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t) (auto-complete-mode t)))


(setq global-auto-complete-mode t)

(add-to-list 'auto-mode-alist '("\\.j\\'" . lisp-mode))



(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
(require 'merlin)

;; -- common-lisp compatibility if not added earlier in your .emacs
(require 'cl)

;; -- Tuareg mode -----------------------------------------
;; Add Tuareg to your search path
(add-to-list
 'load-path
 ;; Change the path below to be wherever you've put your tuareg installation.
 (expand-file-name "~/lib/elisp/tuareg"))
(require 'tuareg)
(setq auto-mode-alist 
      (append '(("\\.ml[ily]?$" . tuareg-mode))
          auto-mode-alist))

;; -- Tweaks for OS X -------------------------------------
;; Tweak for problem on OS X where Emacs.app doesn't run the right
;; init scripts when invoking a sub-shell
(cond
 ((eq window-system 'ns) ; macosx
  ;; Invoke login shells, so that .profile or .bash_profile is read
  (setq shell-command-switch "-lc")))

;; -- opam and utop setup --------------------------------
;; Setup environment variables using opam
(dolist
   (var (car (read-from-string
           (shell-command-to-string "opam config env --sexp"))))
 (setenv (car var) (cadr var)))
;; Update the emacs path
(setq exec-path (split-string (getenv "PATH") path-separator))
;; Update the emacs load path
(push (concat (getenv "OCAML_TOPLEVEL_PATH")
          "/../../share/emacs/site-lisp") load-path)
;; Automatically load utop.el
(autoload 'utop "utop" "Toplevel for OCaml" t)
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
(add-hook 'tuareg-mode-hook 'merlin-mode)

;;Add els dir to load path
(add-to-list 'load-path "~/.emacs.d/els")

;;column marking
(require 'column-marker)
(add-hook 'tuareg-mode-hook (lambda () (interactive) (column-marker-1 80)))


;;ocp-indent
(require 'ocp-indent)
(load-file "/Users/Nate/.opam/system/share/emacs/site-lisp/ocp-indent.el")


;;Org mode and capture
(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file "~/Dropbox/org/agenda/notes.org")
(define-key global-map "\C-cr" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done 'time)
(setq org-capture-templates
      '(("w" "Work entry" entry (file+headline "~/Dropbox/org/agenda/work.org" "Tasks")
	 "* TODO %?\n  %i\n  %a")
	("t" "Task entry" entry (file+headline "~/Dropbox/org/agenda/tasks.org" "Tasks")
	 "* TODO %?\n  %i\n  %a")
        ("n" "Note" entry (file+datetree "~/Dropbox/org/agenda/notes.org")
	 "* %?\nEntered on %U\n  %i\n  %a")
        ("l" "Lab notebook" entry (file+datetree "~/Dropbox/org/agenda/lab-notebook.org")
	 "* %?\nEntered on %U\n  %i\n  %a")))
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-files '("~/Dropbox/org/agenda/notes.org" "~/Dropbox/org/agenda/tasks.org" "~/Dropbox/org/agenda/work.org"))
(defvar org-mobile-push-timer nil
  "Timer that `org-mobile-push-timer' used to reschedule itself, or nil.")

(defun org-mobile-push-with-delay (secs)
  (when org-mobile-push-timer
    (cancel-timer org-mobile-push-timer))
  (setq org-mobile-push-timer
        (run-with-idle-timer
         (* 1 secs) nil 'org-mobile-push)))

(add-hook 'after-save-hook 
 (lambda () 
   (when (eq major-mode 'org-mode)
     (dolist (file (org-mobile-files-alist))
      (if (string= (file-truename (expand-file-name (car file)))
		   (file-truename (buffer-file-name)))
           (org-mobile-push-with-delay 30)))
   )))
;; refreshes agenda file each day
(run-at-time "00:05" 86400 '(lambda () (org-mobile-push-with-delay 1)))
(org-mobile-pull) ;; run org-mobile-pull at startup

(defun install-monitor (file secs)
  (run-with-timer
   0 secs
   (lambda (f p)
     (unless (< p (second (time-since (elt (file-attributes f) 5))))
       (org-mobile-pull)))
   file secs))

(install-monitor (file-truename
                  (concat
                   (file-name-as-directory org-mobile-directory)
                          org-mobile-capture-file))
                 5)

;; Do a pull every 5 minutes to circumvent problems with timestamping
;; (ie. dropbox bugs)
(run-with-timer 0 (* 5 60) 'org-mobile-pull)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Dropbox/org/agenda/notes.org" "~/Dropbox/org/agenda/tasks.org" "~/Dropbox/org/agenda/work.org")))
 '(org-mobile-inbox-for-pull "~/Dropbox/org/from-mobile.org"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )








