;;; nate-phone.el --- Phone-friendly minor mode for Emacs  -*- lexical-binding: t -*-

;;; Commentary:
;; A minor mode bundling phone-friendly settings:
;;   - god-mode for modifier-free input
;;   - Inverted scroll direction
;;   - Single window layout
;;
;; Toggle with M-x nate-phone-mode

;;; Code:

(require 'god-mode)


(defvar nate-phone--saved-scroll-direction nil
  "Saved value of mouse-wheel-flip-direction before phone mode.")

(defun nate-phone--update-cursor ()
  "Update cursor shape and color to reflect current god-mode state."
  (if (bound-and-true-p god-local-mode)
      (progn (setq cursor-type 'box)
             (set-cursor-color "#e06c75"))
    (progn (setq cursor-type 'bar)
           (set-cursor-color "#98c379"))))

(defun nate-phone--enable ()
  "Enable phone-friendly settings."
  (setq nate-phone--saved-scroll-direction mouse-wheel-flip-direction)
  (setq mouse-wheel-flip-direction nil)
  (delete-other-windows)
  (add-hook 'god-mode-enabled-hook  #'nate-phone--update-cursor)
  (add-hook 'god-mode-disabled-hook #'nate-phone--update-cursor)
  (god-mode-all))

(defun nate-phone--disable ()
  "Restore settings changed by phone mode."
  (setq mouse-wheel-flip-direction nate-phone--saved-scroll-direction)
  (remove-hook 'god-mode-enabled-hook  #'nate-phone--update-cursor)
  (remove-hook 'god-mode-disabled-hook #'nate-phone--update-cursor)
  (setq cursor-type t)
  (set-cursor-color "#ffffff")
  (god-mode-all -1))

(defvar nate-phone-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<home>") #'god-mode)
    map)
  "Keymap for `nate-phone-mode'.")

;;;###autoload
(define-minor-mode nate-phone-mode
  "Minor mode for comfortable phone editing.
Enables god-mode, fixes scroll direction, and collapses to a single window."
  :global t
  :lighter " Phone"
  (if nate-phone-mode
      (nate-phone--enable)
    (nate-phone--disable)))

(provide 'nate-phone)
;;; nate-phone.el ends here
