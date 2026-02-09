;;; Emacs Bedrock - Nate Chodosh
;;;
;;; Mixin: Vterm 

;;; Usage: Append or require this file from init.el for some software
;;; development-focused packages.
;;;

;;; Contents:
;;;
;;;  - Setup for running vterm with bash
;;;  - Send org src blocks to vterm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Vterm config
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Make sure to install CMake

(use-package vterm
  :ensure t
  :demand
  :bind (:map vterm-mode-map ("C-l" . vterm-send-C-l))
  :config
  (setq vterm-shell "/usr/bin/bash")
  (set-face-foreground 'vterm-color-yellow "dark orange")
  (set-face-background 'vterm-color-yellow "orange")
  (set-face-background 'vterm-color-green "dark green")
  (set-face-foreground 'vterm-color-black "gainsboro")
  (set-face-background 'vterm-color-black "dim gray")
  (setq initial-buffer-choice #'vterm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Send org src blocks to vterm
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nate/get-vterm-buffer ()
  "Get a vterm buffer, prompting if multiple exist, creating if none."
  (let ((vterm-buffers (seq-filter
                        (lambda (buf)
                          (with-current-buffer buf
                            (derived-mode-p 'vterm-mode)))
                        (buffer-list))))
    (cond
     ((null vterm-buffers)
      (vterm))
     ((= 1 (length vterm-buffers))
      (car vterm-buffers))
     (t
      (let ((name (completing-read "Send to vterm buffer: "
                                   (mapcar #'buffer-name vterm-buffers)
                                   nil t)))
        (get-buffer name))))))

(defun nate/org-send-src-block-to-vterm ()
  "Send the org src block at point to a running vterm buffer.
Unlike org-babel, this preserves shell state across blocks,
handles interactive prompts (sudo, etc.), and runs in a real terminal."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (let* ((element (org-element-at-point))
         (type (org-element-type element)))
    (unless (eq type 'src-block)
      (user-error "Not inside a src block"))
    (let* ((body (string-trim-right (org-element-property :value element)))
           (vterm-buf (nate/get-vterm-buffer)))
      (with-current-buffer vterm-buf
        (vterm-send-string body)
        (vterm-send-return))
      (display-buffer vterm-buf))))

(with-eval-after-load 'org
  (keymap-set org-mode-map "C-c t" #'nate/org-send-src-block-to-vterm))
