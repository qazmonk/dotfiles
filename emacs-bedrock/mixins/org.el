;;; Emacs Bedrock
;;;
;;; Mixin: Org-mode starter config

;;; Usage: Append or require this file from init.el for some software
;;; development-focused packages.
;;;
;;; Org-mode is a fantastically powerful package. It does a lot of things, which
;;; makes it a little difficult to understand at first.
;;;
;;; We will configure Org-mode in phases. Work with each phase as you are
;;; comfortable.
;;;
;;; YOU NEED TO CONFIGURE SOME VARIABLES! The most important variable is the
;;; `org-directory', which tells org-mode where to look to find your agenda
;;; files.

;;; See "org-intro.txt" for a high-level overview.

;;; Contents:
;;;
;;;  - Critical variables
;;;  - Phase 1: editing and exporting files
;;;  - Phase 2: todos, agenda generation, and task tracking
;;;  - Phase 3: extensions (org-roam, etc.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Critical variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These variables need to be set for Org-mode's full power to be unlocked!
;;;
;;; You can read the documentation for any variable with `C-h v'. If you have
;;; Consult configured (see the `base.el' file) then it should help you find
;;; what you're looking for.

;;; Phase 1 variables

;;; Phase 2 variables

;; Agenda variables
(setq org-directory "~/Documents/org/") ; Non-absolute paths for agenda and
                                        ; capture templates will look here.

;;(setq org-agenda-files '("inbox.org" "work.org"))

;; Default tags
(setq org-tag-alist '(
                      ;; locale
                      (:startgroup)
                      ("home" . ?h)
                      ("work" . ?w)
                      ("school" . ?s)
                      (:endgroup)
                      (:newline)
                      ;; scale
                      (:startgroup)
                      ("one-shot" . ?o)
                      ("project" . ?j)
                      ("tiny" . ?t)
                      (:endgroup)
                      ;; misc
                      ("meta")
                      ("review")
                      ("reading")))

;; Org-refile: where should org-refile look?
(setq org-refile-targets 'FIXME)

;;; Phase 3 variables

;; Org-roam variables
(setq org-roam-directory "~/Documents/org-roam/")
(setq org-roam-index-file "~/Documents/org-roam/index.org")

;;; Optional variables

;; Advanced: Custom link types
;; This example is for linking a person's 7-character ID to their page on the
;; free genealogy website Family Search.
(setq org-link-abbrev-alist
      '(("family_search" . "https://www.familysearch.org/tree/person/details/%s")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Phase 1: editing and exporting files
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :hook ((org-mode . visual-line-mode)  ; wrap lines at word breaks
         (org-mode . flyspell-mode))    ; spell checking!

  :bind (:map global-map
              ("C-c l s" . org-store-link)          ; Mnemonic: link → store
              ("C-c l i" . org-insert-link-global)) ; Mnemonic: link → insert
  :config
  (require 'oc-csl)                     ; citation support
  (add-to-list 'org-export-backends 'md)

  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; Make exporting quotes better
  (setq org-export-with-smart-quotes t)

  ;; Adding support for embedding html pages in iframes
  (org-link-set-parameters
   "iframe"
   :follow (lambda (path _) (browse-url path))
   :export (lambda (path desc backend _)
             (when (eq backend 'html)
               (let* ((parts (split-string path "::"))
                      (file (car parts))
                      (params (cdr parts))
                      (height (or (cadr (assoc "height" (mapcar (lambda (p) (split-string p "=")) params))) "600px"))
                      (width (or (cadr (assoc "width" (mapcar (lambda (p) (split-string p "=")) params))) "100%")))
		 (format "<iframe src=\"%s\" width=\"%s\" height=\"%s\" style=\"border: none;\"></iframe>" 
			 file width height)))))

  (defun org-iframe-complete-link ()
    "Provide completion for iframe links."
    (concat "iframe:" (read-file-name "HTML file: " nil nil t)))

  (org-link-set-parameters "iframe" :complete #'org-iframe-complete-link))

(use-package ox
  :config
  ;; use prism for syntax highlighting
  (setq org-html-htmlize-output-type nil)
  (defun my-org-html-src-block-filter (text backend info)
    "Add language-* class to source blocks for Prism.js highlighting."
    (when (eq backend 'html)
      (replace-regexp-in-string
       "<pre class=\"src src-\\([^\"]*\\)\""
       "<pre class=\"src src-\\1 language-\\1\""
       text)))

  (add-to-list 'org-export-filter-src-block-functions
               'my-org-html-src-block-filter)

  (defun my/org-babel-dumb-ctrl-c-ctrl-c-hook ()
    "Custom C-c C-c behavior for dumb copy/paste sending of org-babel blocks with no output handling."
    ;; add the :dumb tag to a bash session block to enable this mode
    (when (org-in-src-block-p)
      (let* ((info (org-babel-get-src-block-info))
             (params (nth 2 info))
             (session (cdr (assoc :session params)))
             (dumb-send (cdr (assoc :dumb params))))
	;; Only handle session blocks with :dumb true
	(when (and session 
                   (not (string= session "none"))
                   (string= dumb-send "true"))
	  (let ((body (nth 1 info))
		(session-buffer  (org-babel-sh-initiate-session session params)))
            ;; Send the code directly to the session
            (with-current-buffer session-buffer
              (goto-char (point-max))
              (insert body)
              (comint-send-input))
            ;; Switch to the session buffer
            (switch-to-buffer-other-window session-buffer)
            ;; Return t to indicate we handled it
            t))
        )))
  (add-hook 'org-ctrl-c-ctrl-c-hook #'my/org-babel-dumb-ctrl-c-ctrl-c-hook))
  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Phase 2: todos, agenda generation, and task tracking
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Yes, you can have multiple use-package declarations. It's best if their
;; configs don't overlap. Once you've reached Phase 2, I'd recommend merging the
;; config from Phase 1. I've broken it up here for the sake of clarity.
(use-package org
  :config
  ;; Instead of just two states (TODO, DONE) we set up a few different states
  ;; that a task can be in.
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w@/!)" "STARTED(s!)" "|" "DONE(d!)" "OBSOLETE(o@)")))

  ;; Refile configuration
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)

  (setq org-capture-templates
        '(("c" "Default Capture" entry (file "inbox.org")
           "* TODO %?\n%U\n%i")
          ;; Capture and keep an org-link to the thing we're currently working with
          ("r" "Capture with Reference" entry (file "inbox.org")
           "* TODO %?\n%U\n%i\n%a")
          ;; Define a section
          ("w" "Work")
          ("wm" "Work meeting" entry (file+headline "work.org" "Meetings")
           "** TODO %?\n%U\n%i\n%a")
          ("wr" "Work report" entry (file+headline "work.org" "Reports")
           "** TODO %?\n%U\n%i\n%a")))

    (setq org-agenda-custom-commands
          '(("n" "Agenda and All Todos"
             ((agenda)
              (todo)))
            ("w" "Work" agenda ""
             ((org-agenda-files '("work.org"))))))
    (setq org-agenda-files nil)
    ;; if you've set up syncing with your phone add it to the agenda
    (when (f-directory-p (f-full "~/Documents/org-mobile/"))
      (push (f-full "~/Documents/org-mobile/") org-agenda-files))
    ;; if you have a running schedule
    (when (f-directory-p (f-full "~/Documents/org-run/"))
      (push (f-full "~/Documents/org-run/") org-agenda-files))
    ;; auto save buffers after updating a TODO in agenda view
    (advice-add 'org-agenda-todo :after #'org-save-all-org-buffers)
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Phase 3: extensions (org-roam, etc.)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-roam
  :ensure t
  :config
  (org-roam-db-autosync-mode)
  ;; Dedicated side window for backlinks
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.4)
                 (window-height . fit-window-to-buffer)))
  ;; Use org roam files for agenda
  (push  (file-name-concat (getenv "HOME") "Documents/org-roam/daily/") org-agenda-files))


;; Pretty web interface for org-roam
;(use-package org-roam-ui
;  :ensure t
;  :after org-roam
;  :config
;  (setq org-roam-ui-sync-theme t
;        org-roam-ui-follow t
;        org-roam-ui-update-on-save t
;        org-roam-ui-open-on-start t))


(use-package org-journal
  :ensure t
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  :config
  (setq org-journal-dir "~/Documents/org-journal/"))

(use-package org
  :demand t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)))
  (setq org-src-preserve-indentation t)
  (setq org-startup-folded t))
