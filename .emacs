;;;;;;;;;;;;;;;;;;;
;; GENERAL STUFF ;;
;;;;;;;;;;;;;;;;;;;

(tool-bar-mode 0)
(setq debug-on-error t)
;(setq warning-minimum-level :error)
(setq inhibit-splash-screen t)

(require 'package)
(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))



;;dunno if this actually works (I don't think it does)
(require 'server)
(if (not (server-running-p))
    (server-start))

(windmove-default-keybindings 'super)
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
(setq mac-option-modifier 'super)

;;GET THOSE GOD DAMN TABS OUT OF HERE
(setq-default indent-tabes-mode nil)


(defun my-merge-imenu ()
  (interactive)
  (let ((mode-imenu (imenu-default-create-index-function))
        (custom-imenu (imenu--generic-function imenu-generic-expression)))
;;    (delete-dups (append mode-imenu custom-imenu))))
    custom-imenu))

(add-hook 'text-mode-hook (lambda () 
                            (flyspell-mode)
                            (auto-fill-mode)))

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
(global-set-key (kbd "M-`") 'jump-to-mark)


(require 'visible-mark)
(defface mark-1
  '((((type tty) (class mono))
     (:inverse-video t))
    (t (:background "turquoise2"))) "")
(defface mark-2
  '((((type tty) (class mono)))
    (t (:background "turquoise3"))) "")
(defface mark-3
  '((((type tty) (class mono)))
    (t (:background "turquoise4"))) "")
(setq visible-mark-faces (quote (mark-1 mark-2 mark-3)))
(setq visible-mark-max 3)
(global-visible-mark-mode)

(global-set-key (kbd "M-i") 'helm-imenu)
(require 'helm-config)

(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

(defun my-activate-adaptive-wrap-prefix-mode ()
    "Toggle `visual-line-mode' and `adaptive-wrap-prefix-mode' simultaneously."
    (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1)))
(add-hook 'visual-line-mode-hook 'my-activate-adaptive-wrap-prefix-mode)
;;;;;;;;;;;;;;;;;
;; PROGRAMMING ;;
;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(ac-config-default)
(setq global-auto-complete-mode t)

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
   (cons msg code)))



;;;;;;;;;
;; C++ ;;
;;;;;;;;;
(defun my-c++-mode-hook ()
  (add-to-list
   'imenu-generic-expression
   '("Function Header" 
     "^\\s-*\\([a-zA-Z0-9_:><]+\\s-+\\)*\\([a-zA-Z0-9_]+\\)([-a-zA-Z0-9[:space:]:<>,=_*&()\n]*);"
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
 (setq tab-width 4)
 (setq indent-tabs-mode nil)
 (setq column-number-mode t)
 (setq compile-command "make all")
 (local-set-key (kbd "C-c C-c") 'recompile))


(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;autocomplete for C coding
(add-to-list 'load-path "~/.emacs.d/auto-complete/auto-complete-clang")
(require 'auto-complete-clang)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;;;;;;;;;
;; LISP ;;
;;;;;;;;;;

(setq inferior-lisp-program "/usr/local/bin/sbcl")
(add-to-list 'load-path "~/.emacs.d/slime-2.8")
(require 'slime-autoloads)

(add-to-list 'slime-contribs 'inferior-slime)
(add-to-list 'slime-contribs 'slime-fancy)

(add-hook 'lisp-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

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
  (auto-complete-mode t)
  (paredit-mode t)
  (show-paren-mode t)
  (highlight-parentheses-mode t)
  (local-set-key (kbd "RET") 'electrify-return-if-match))
(defun nates-lisp-mode ()
  (slime-mode)
  (local-set-key (kbd "C-M-S-s-r")
                 (lambda ()
                   (interactive)
                   (shell-command "osascript ~/Dropbox/AppleScript/refresh-preview.scpt")))
  (set (make-local-variable 'lisp-indent-function)
       'common-lisp-indent-function)
  (nates-general-lisp-mode))

(defun nates-inferior-lisp-mode ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil)
  (local-set-key (kbd "C-M-S-s-r")
                 (lambda ()
                   (interactive)
                   (shell-command "osascript ~/Dropbox/AppleScript/refresh-preview.scpt")))
  (nates-general-lisp-mode))


(defun nates-emacs-lisp-mode ()
  (eldoc-mode t)
  (nates-general-lisp-mode))

(add-hook 'lisp-mode-hook 'nates-lisp-mode)

(add-hook 'emacs-lisp-mode-hook 'nates-emacs-lisp-mode)
(add-hook 'slime-repl-mode-hook 'nates-inferior-lisp-mode)
(add-hook 'slime-mode-hook
          (lambda ()
            (unless (slime-connected-p)
              (print (current-buffer))
              (unless (equalp (current-buffer) (get-buffer "*scratch*"))
                (save-excursion (slime))))))

(add-to-list 'auto-mode-alist '("\\.j\\'" . lisp-mode))

(eval-after-load 'slime
  `(define-key slime-mode-map (kbd "M-?") nil))

(require 'cl)

;;;;;;;;;;;
;; OCAML ;;
;;;;;;;;;;;

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


;;;;;;;;;;;;
;; MATLAB ;;
;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/matlab-emacs")
         (load-library "matlab-load")
(custom-set-variables
 '(matlab-shell-command-switches '("-nodesktop -nosplash")))
(add-hook 'matlab-mode-hook 'auto-complete-mode)
(add-hook 'matlab-mode-hook 'mlint-minor-mode)
(setq auto-mode-alist
    (cons
     '("\\.m$" . matlab-mode)
     auto-mode-alist))


;;;;;;;;;;;
;; LATEX ;;
;;;;;;;;;;;
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


(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer"
         "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))


;;Stop autocompleting when I hit enter
;(add-hook 'css-mode-hook
;          (lambda ()
;            (define-key ac-complete-mode-map "\r" nil)))

;;Set indentation rules
(customize-set-variable
 'LaTeX-indent-environment-list
 '(("verbatim" current-indentation)
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
   ("table*")))
;;;;;;;;;;;;;;
;; FLYSPELL ;;
;;;;;;;;;;;;;;

(add-hook 'flyspell-mode-hook (lambda ()
                                (define-key flyspell-mode-map (kbd "M-<f1>")
                                  #'flyspell-check-previous-highlighted-word)))


;;;;;;;;;;;;;;
;; ORG MODE ;;
;;;;;;;;;;;;;;

(require 'org)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-indent-environment-list
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
     ("table*"))) t)
 '(compilation-auto-jump-to-first-error t)
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("3dafeadb813a33031848dfebfa0928e37e7a3c18efefa10f3e9f48d1993598d3" "05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" default)))
 '(highlight-changes-colors ("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   (quote
    (("#49483E" . 0)
     ("#67930F" . 20)
     ("#349B8D" . 30)
     ("#21889B" . 50)
     ("#968B26" . 60)
     ("#A45E0A" . 70)
     ("#A41F99" . 85)
     ("#49483E" . 100))))
 '(hl-paren-colors
   (quote
    ("PaleGreen1" "SpringGreen1" "SpringGreen3" "SpringGreen4")))
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(magit-diff-use-overlays nil)
 '(mlint-programs
   (quote
    ("mlint" "mac/mlint" "/Applications/MATLAB_R2014a.app/bin/maci64/mlint")))
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/agenda/notes.org" "~/Dropbox/org/agenda/tasks.org" "~/Dropbox/org/agenda/work.org" "~/Dropbox/org/agenda/school-work.org")))
 '(org-mobile-inbox-for-pull "~/Dropbox/org/from-mobile.org")
 '(reb-re-syntax (quote string))
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0")))

(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file "~/Dropbox/org/agenda/notes.org")
(define-key global-map "\C-cr" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done 'time)
(setq org-capture-templates
      '(("W" "Work todo" entry (file+headline "~/Dropbox/org/agenda/work.org" "Tasks")
	 "* TODO %?\t:WORK:\nDEADLINE: %^{Deadline}t\n  %i\n")
	("H" "Home todo" entry (file+headline "~/Dropbox/org/agenda/tasks.org" "Tasks")
	 "* TODO %?\t:HOME:\nDEADLINE: %^{Deadline}t\n  %i\n")
	("S" "School todo" entry (file "~/Dropbox/org/agenda/school-work.org")
	 "* TODO %?\t:SCHOOL:\nDEADLINE: %^{Deadline}t\n  %i\n")
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
(setq org-agenda-custom-commands
      '(("w" tags-todo "WORK")
	("h" tags-todo "HOME")))





;;;AFTER PACKAGE INITIALIZATION 
(package-initialize)
;make path variables the same in emacs and the shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(setq opam-share
      (substring
       (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
(require 'merlin)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
