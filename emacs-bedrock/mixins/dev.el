;;; Emacs Bedrock
;;;
;;; Mixin: Development tools

;;; Usage: Append or require this file from init.el for some software
;;; development-focused packages.
;;;
;;; It is **STRONGLY** recommended that you use the base.el mixin if you want to
;;; use Eglot. Lots of completion things will work better.
;;;
;;; This will try to use tree-sitter modes for many languages. Please run
;;;
;;;   M-x treesit-install-language-grammar
;;;
;;; Before trying to use a treesit mode.

;;; Contents:
;;;  
;;;  - Built-in config for developers
;;;  - Version Control
;;;  - Common file types
;;;  - Eglot, the built-in LSP client for Emacs
;;;  - Calc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in config for developers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :config
  ;; Treesitter config

  ;; Tell Emacs to prefer the treesitter mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode))))
  ;:hook
  ;; Auto parenthesis matching
  ;((prog-mode . electric-pair-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
(use-package magit
  :ensure t
  :bind (("s-g" . magit-status)
         ("C-c g" . magit-status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Common file types
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)))

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)

;; Emacs ships with a lot of popular programming language modes. If it's not
;; built in, you're almost certain to find a mode for the language you're
;; looking for with a quick Internet search.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Eglot, the built-in LSP client for Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  ;; no :ensure t here because it's built-in

  ;; Configure hooks to automatically turn-on eglot for selected modes
  ; :hook
  ; (((python-mode ruby-mode elixir-mode) . eglot))

  :custom
  (eglot-send-changes-idle-time 0.1)

  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
  ; (add-to-list 'eglot-server-programs
  ;              '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   PYENV, manage python installs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package pyenv-mode
;;   :ensure t
;;   :config (pyenv-mode))

;;;;;;;;;;
;; conda
;;;;;;;;;;

(use-package conda
  :ensure t
  :init
  (setq conda-anaconda-home (expand-file-name "~/miniconda3"))
  (setq conda-env-home-directory (expand-file-name "~/miniconda3/envs"))
  :config
  (conda-env-initialize-interactive-shells))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   CALC
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq math-additional-units '(
  (bytes "byte" "Bytes")
  (bits "bit" "Bits")
  (bps "bit / s" "Bits per second")
  (byte "8 * bit" "Byte")
  (bit nil "Bit")
  ))
(setq math-units-table nil)


;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Unix conf
;;;
;;;;;;;;;;;;;;;;;;;;

(use-package conf-mode
  :mode "\\.service\\'"
  :mode "\\.timer\\'")

;;;;;;;;;;;;;;;;;;
;;; TRAMP
;;;;;;;;;;;;;;;;


;; A utility function for killing all tramp buffers
(defun kill-all-tramp-buffers ()
  "Kill all TRAMP buffers, including unreachable ones."
  (interactive)
  (let ((tramp-buffers (seq-filter (lambda (buf)
                                     (with-current-buffer buf
                                       (and (stringp default-directory)
                                            (file-remote-p default-directory))))
                                   (buffer-list))))
    (if tramp-buffers
        (progn
          ;; Temporarily disable TRAMP cleanup to avoid hanging
          (let ((tramp-cleanup-connection nil)
                (tramp-cleanup-all-connections nil))
            (dolist (buf tramp-buffers)
              ;; Kill buffer without running hooks that might try to access remote
              (with-current-buffer buf
                (set-buffer-modified-p nil)  ; Avoid save prompts
                (kill-buffer-hook nil))      ; Skip hooks
              (kill-buffer buf)))
          ;; Clean up TRAMP connections after killing buffers
          (when (fboundp 'tramp-cleanup-all-connections)
            (ignore-errors (tramp-cleanup-all-connections)))
          (message "Killed %d TRAMP buffer%s" 
                   (length tramp-buffers)
                   (if (= 1 (length tramp-buffers)) "" "s")))
      (message "No TRAMP buffers found"))))
