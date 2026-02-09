;;; Emacs Bedrock - Nate Chodosh
;;;
;;; Mixin: Vterm 

;;; Usage: Append or require this file from init.el for some software
;;; development-focused packages.
;;;

;;; Contents:
;;;
;;;  - Setup for running vterm with bash

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
