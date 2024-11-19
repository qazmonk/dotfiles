;;; Emacs Bedrock
;;;
;;; Mixin: Researcher

;;; Usage: Append or require this file from init.el for research helps. If you
;;; write papers in LaTeX and need to manage your citations or keep track of
;;; notes, this package is for you.
;;;
;;; Highly recommended to enable this mixin with the UI enhancements in
;;; `base.el', as citar works best with the Vertico completing-read interface.
;;; Also recommended is the `writer.el' mixin, which adds some nice features for
;;; spell-checking etc.

;;; Contents:
;;;
;;;  - Citation Management
;;;  - Authoring
;;;  - Note Taking: Org-Roam
;;;  - Note Taking: Denote
;;;  - Slide Making: Convert Latex equation to pngs quickly

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Critical variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These variables must be set for citar to work properly!

;; (setq citar-bibliography '("~/refs.bib")) ; paths to your bibtex files

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Citation Management
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package citar
;;   :ensure t
;;   :bind (("C-c b" . citar-insert-citation)
;;          :map minibuffer-local-map
;;          ("M-b" . citar-insert-preset))
;;   :custom
;;   ;; Allows you to customize what citar-open does
;;   (citar-file-open-functions '(("html" . citar-file-open-external)
;;                                ;; ("pdf" . citar-file-open-external)
;;                                (t . find-file))))

;; Optional: if you have the embark package installed, enable the ability to act
;; on citations with citar by invoking `embark-act'.
;(use-package citar-embark
;  :after citar embark
;  :diminish ""
;  :no-require
;  :config (citar-embark-mode))

;; (use-package citar-org-roam
;;   :diminish ""
;;   ;; To get this to work both citar *and* org-roam have to have been used
;;   :after citar org-roam
;;   :no-require
;;   :config
;;   (citar-org-roam-mode)
;;   (setq citar-org-roam-note-title-template "${author} - ${title}\n#+filetags: ${tags}"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Authoring
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Note Taking: Org-Roam
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-roam
  :ensure t
  :config
  ;; Make sure the backlinks buffer always shows up in a side window
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.4)
                 (window-height . fit-window-to-buffer)))

  (org-roam-db-autosync-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Note Taking: Denote
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   LaTeX
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'latex-mode-hook 'eglot-ensure)
(add-hook 'latex-mode-hook 'flyspell-mode)

;; change the way filenames are parsed so that file completion works inside \includegraphics{}
(defvar latex-thing-at-point-file-name-chars "-@~/[:alnum:]_.$#%,:")
(add-hook 'latex-mode-hook (lambda () (setq-local thing-at-point-file-name-chars latex-thing-at-point-file-name-chars)))


;; Utilities for making image versions of latex equations
;; If you add a mymacros.sty file to the tex installation you can reference it in all these functions
(defun latex-eq-to-png (equation output-file)
  (interactive "MEquation: \nFOuput File: ")
  (let ((file (make-temp-file "latext-to-png" nil ".tex")))
    (with-temp-file file
      (insert (format "\\documentclass[convert={outfile=%s,density=1000},border={0cm 0.2cm}]{standalone}\\usepackage{amsmath}\\usepackage{xcolor}\\usepackage{amssymb}\\IfFileExists{mymacros.sty}{\\usepackage{mymacros}}{} \\begin{document}"
		      (expand-file-name output-file)))
      (insert (format "$%s$" equation))
      (insert "\\end{document}"))
    (start-process-shell-command
     "latex-to-png"
     (get-buffer-create "*latex-to-png compilation*")
     (concat
      "cd "
      temporary-file-directory
      " && "
      "pdflatex "
      "-shell-escape " 
      file))))


(defun latex-par-eq-to-png (equation output-file)
  (interactive "MEquation: \nFOuput File: ")
  (let ((file (make-temp-file "latext-to-png" nil ".tex")))
    (with-temp-file file
      (insert (format "\\documentclass[preview,convert={outfile=%s,density=1000},border={0cm 0cm}]{standalone}\\usepackage{amsmath}\\usepackage{amssymb}\\usepackage{booktabs}\\IfFileExists{mymacros.sty}{\\usepackage{mymacros}}{} \\begin{document}"
		      (expand-file-name output-file)))
      (insert (format "\\begin{equation}%s\\end{equation}" equation))
      (insert "\\end{document}"))
    (start-process-shell-command
     "latex-to-png"
     (get-buffer-create "*latex-to-png compilation*")
     (concat
      "cd "
      temporary-file-directory
      " && "
      "pdflatex "
      "-shell-escape " 
      file))))

(defun latex-to-png (equation output-file)
  (interactive "MEquation: \nFOuput File: ")
  (let ((file (make-temp-file "latext-to-png" nil ".tex")))
    (with-temp-file file
      (insert (format "\\documentclass[preview,convert={outfile=%s,density=1000}]{standalone}\\usepackage{booktabs}\\IfFileExists{mymacros.sty}{\\usepackage{mymacros}}{} \\begin{document}"
		      (expand-file-name output-file)))
      (insert (format "%s" equation))
      (insert "\\end{document}"))
    (start-process-shell-command
     "latex-to-png"
     (get-buffer-create "*latex-to-png compilation*")
     (concat
      "cd "
      temporary-file-directory
      " && "
      "pdflatex "
      "-shell-escape " 
      file))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                  Class Management
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shuffle-region (beg end)
  "Randomize lines in region from BEG to END."
  (interactive "*r")
  (let ((lines (split-string
                (delete-and-extract-region beg end) "\n")))
    (when (string-equal "" (car (last lines 1)))
      (setq lines (butlast lines 1)))
    (apply 'insert
           (mapcar 'cdr
		   (sort (mapcar (lambda (x) (cons (random) (concat x "\n"))) lines)
			 (lambda (a b) (< (car a) (car b))))))))
